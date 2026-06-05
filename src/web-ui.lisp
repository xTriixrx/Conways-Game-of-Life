(in-package :conways-game-of-life)

(defparameter *web-asset-root*
  (asdf:system-relative-pathname :conways-game-of-life "web/"))

(defun web-asset-path (relative-path)
  "Returns the path for a web UI asset in the project web directory."
  (merge-pathnames relative-path *web-asset-root*))

(defun read-web-asset (relative-path)
  "Reads a text web UI asset from disk."
  (uiop:read-file-string (web-asset-path relative-path)))

(defclass web-renderer ()
  ((port :initarg :port :reader web-renderer-port)
   (host :initarg :host :reader web-renderer-host)
   (state :initarg :state :accessor web-renderer-state)
   (speed :initarg :speed :accessor web-renderer-speed)
   (running :initarg :running :accessor web-renderer-running)
   (quit :initform nil :accessor web-renderer-quit)
   (lock :initform (bt:make-lock "web-renderer") :reader web-renderer-lock)
   (server-socket :accessor web-renderer-server-socket)
   (server-thread :accessor web-renderer-server-thread)
   (simulation-thread :accessor web-renderer-simulation-thread)
   (latest-frame :accessor web-renderer-latest-frame)
   (websocket-clients :initform nil :accessor web-renderer-websocket-clients)
   (had-websocket-client :initform nil
                         :accessor web-renderer-had-websocket-client)))

(defun slot-value-if-bound (object slot-name)
  "Returns OBJECT's SLOT-NAME value when the slot is bound."
  (when (slot-boundp object slot-name)
    (slot-value object slot-name)))

(defun ascii-octets (string)
  "Returns STRING as ASCII octets."
  (let ((octets (make-array (length string) :element-type '(unsigned-byte 8))))
    (dotimes (i (length string) octets)
      (setf (aref octets i) (char-code (char string i))))))

(defun write-ascii (stream control &rest args)
  "Writes formatted ASCII text to a binary STREAM."
  (let ((text (apply #'format nil control args)))
    (write-sequence (ascii-octets text) stream)))

(defun read-http-line (stream)
  "Reads one CRLF-terminated ASCII line from STREAM."
  (let ((chars nil))
    (loop for byte = (read-byte stream nil nil)
          while byte
          do (cond ((= byte 10)
                    (return))
                   ((= byte 13) nil)
                   (t (push (code-char byte) chars))))
    (coerce (nreverse chars) 'string)))

(defun split-string (string char)
  "Splits STRING on CHAR."
  (let ((parts nil)
        (start 0))
    (loop for pos = (position char string :start start)
          do (if pos
                 (progn
                   (push (subseq string start pos) parts)
                   (setf start (1+ pos)))
                 (progn
                   (push (subseq string start) parts)
                   (return))))
    (nreverse parts)))

(defun parse-query (target)
  "Returns path and query alist for an HTTP target."
  (let* ((question (position #\? target))
         (path (if question (subseq target 0 question) target))
         (query (and question (subseq target (1+ question)))))
    (values path
            (loop for part in (and query (split-string query #\&))
                  for equals = (position #\= part)
                  when equals
                    collect (cons (subseq part 0 equals)
                                  (subseq part (1+ equals)))))))

(defun query-value (query key)
  "Returns KEY from QUERY alist."
  (cdr (assoc key query :test #'string=)))

(defun read-http-headers (stream)
  "Reads an HTTP request line and headers from STREAM."
  (let ((request-line (read-http-line stream))
        (headers nil))
    (loop for line = (read-http-line stream)
          until (or (null line) (string= line ""))
          do (let ((colon (position #\: line)))
               (when colon
                 (push (cons (string-downcase (subseq line 0 colon))
                             (string-trim '(#\Space #\Tab)
                                          (subseq line (1+ colon))))
                       headers))))
    (values request-line headers)))

(defun write-response (stream status content-type body)
  "Writes a complete HTTP response."
  (let ((octets (if (stringp body) (ascii-octets body) body)))
    (write-ascii stream "HTTP/1.1 ~a~%Content-Type: ~a~%Content-Length: ~a~%Cache-Control: no-store~%~%"
                 status content-type (length octets))
    (write-sequence octets stream)
    (finish-output stream)))

(defclass clack-stream-socket ()
  ((stream :initarg :stream :reader clack-stream-socket-stream)
   (read-callback :initform nil :accessor clack-stream-socket-read-callback)))

(defmethod clack.socket:read-callback ((socket clack-stream-socket))
  (clack-stream-socket-read-callback socket))

(defmethod (setf clack.socket:read-callback) (callback
                                              (socket clack-stream-socket))
  (setf (clack-stream-socket-read-callback socket) callback))

(defmethod clack.socket:close-socket ((socket clack-stream-socket))
  (ignore-errors
    (close (clack-stream-socket-stream socket))))

(defmethod clack.socket:write-sequence-to-socket ((socket clack-stream-socket)
                                                  data
                                                  &key callback)
  (write-sequence data (clack-stream-socket-stream socket))
  (finish-output (clack-stream-socket-stream socket))
  (when callback
    (funcall callback)))

(defmethod clack.socket:socket-async-p ((socket clack-stream-socket))
  nil)

(defmethod clack.socket:socket-stream ((socket clack-stream-socket))
  (clack-stream-socket-stream socket))

(defun headers-hash-table (headers)
  "Converts request HEADERS alist into the hash table expected by websocket-driver."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (header headers table)
      (setf (gethash (car header) table) (cdr header)))))

(defun make-listener-socket (host port)
  "Creates a listening TCP socket."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream
                               :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket
                                (sb-bsd-sockets:make-inet-address host)
                                port)
    (sb-bsd-sockets:socket-listen socket 16)
    socket))

(defun web-ui-url (host port)
  "Returns the URL that should be opened for the local web UI."
  (format nil "http://~a:~a/"
          (if (string= host "0.0.0.0") "127.0.0.1" host)
          port))

(defun open-web-ui-url (url)
  "Opens URL in the platform default browser."
  (ignore-errors
    #+darwin
    (uiop:launch-program (list "open" url)
                         :output nil
                         :error-output nil
                         :ignore-error-status t)
    #+linux
    (uiop:launch-program (list "xdg-open" url)
                         :output nil
                         :error-output nil
                         :ignore-error-status t)
    #+windows
    (uiop:launch-program (list "cmd" "/c" "start" "" url)
                         :output nil
                         :error-output nil
                         :ignore-error-status t)))

(defun wake-web-listener (renderer)
  "Wakes the blocking accept loop by opening and closing a local connection."
  (ignore-errors
    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                 :type :stream
                                 :protocol :tcp)))
      (unwind-protect
           (sb-bsd-sockets:socket-connect
            socket
            (sb-bsd-sockets:make-inet-address
             (if (string= (web-renderer-host renderer) "0.0.0.0")
                 "127.0.0.1"
                 (web-renderer-host renderer)))
            (web-renderer-port renderer))
        (ignore-errors
          (sb-bsd-sockets:socket-close socket))))))

(defun join-web-thread (renderer slot-name)
  "Waits for the thread in SLOT-NAME to finish unless it is the current thread."
  (let ((thread (slot-value-if-bound renderer slot-name)))
    (when (and thread
               (bt:thread-alive-p thread)
               (not (eq thread (bt:current-thread))))
      (ignore-errors
        (bt:join-thread thread)))))

(defun close-web-listener (renderer)
  "Closes RENDERER's listening socket when present."
  (let ((socket (slot-value-if-bound renderer 'server-socket)))
    (when socket
      (ignore-errors
        (sb-bsd-sockets:socket-close socket)))))

(defun close-websocket-clients (renderer)
  "Closes all active WebSocket clients and clears the active client list."
  (let ((clients nil))
    (bt:with-lock-held ((web-renderer-lock renderer))
      (setf clients (copy-list (web-renderer-websocket-clients renderer))
            (web-renderer-websocket-clients renderer) nil))
    (dolist (client clients)
      (ignore-errors
        (websocket-driver:close-connection client)))))

(defun stop-web-renderer (renderer &key (wait t))
  "Stops RENDERER, closes its socket, and optionally joins worker threads."
  (bt:with-lock-held ((web-renderer-lock renderer))
    (setf (web-renderer-running renderer) nil
          (web-renderer-quit renderer) t))
  (close-websocket-clients renderer)
  (wake-web-listener renderer)
  (close-web-listener renderer)
  (when wait
    (join-web-thread renderer 'server-thread)
    (join-web-thread renderer 'simulation-thread))
  renderer)

(defun history-enabled-p (renderer)
  "Returns whether previous-generation history is enabled for RENDERER."
  (let ((limit (game-state-history-limit (web-renderer-state renderer))))
    (or (null limit) (> limit 0))))

(defun web-config-json (renderer)
  "Returns one-shot browser configuration as JSON."
  (format nil "{\"historyEnabled\":~a}"
          (if (history-enabled-p renderer) "true" "false")))

(defun latest-frame (renderer)
  "Returns the latest packed frame for RENDERER."
  (bt:with-lock-held ((web-renderer-lock renderer))
    (web-renderer-latest-frame renderer)))

(defun websocket-clients-snapshot (renderer)
  "Returns currently connected WebSocket driver objects.
The caller must hold WEB-RENDERER-LOCK."
  (copy-list (web-renderer-websocket-clients renderer)))

(defun register-websocket-client (renderer websocket)
  "Registers WEBSOCKET as an active WebSocket client."
  (bt:with-lock-held ((web-renderer-lock renderer))
    (setf (web-renderer-had-websocket-client renderer) t)
    (pushnew websocket (web-renderer-websocket-clients renderer) :test #'eq)))

(defun unregister-websocket-client (renderer websocket)
  "Removes WEBSOCKET from active WebSocket clients."
  (bt:with-lock-held ((web-renderer-lock renderer))
    (setf (web-renderer-websocket-clients renderer)
          (remove websocket (web-renderer-websocket-clients renderer)
                  :test #'eq))))

(defun stop-web-renderer-when-without-websocket-clients (renderer)
  "Stops RENDERER after the final browser WebSocket disconnects."
  (let ((should-stop nil))
    (bt:with-lock-held ((web-renderer-lock renderer))
      (setf should-stop
            (and (web-renderer-had-websocket-client renderer)
                 (null (web-renderer-websocket-clients renderer)))))
    (when should-stop
      (stop-web-renderer renderer :wait nil))))

(defun send-websocket-frame-to-client (renderer websocket frame)
  "Sends FRAME to one WebSocket client, unregistering failed streams."
  (handler-case
      (websocket-driver:send-binary websocket frame)
    (error ()
      (unregister-websocket-client renderer websocket)
      (stop-web-renderer-when-without-websocket-clients renderer))))

(defun broadcast-websocket-frame (renderer clients frame)
  "Sends FRAME to CLIENTS."
  (dolist (client clients)
    (send-websocket-frame-to-client renderer client frame)))

(defun frame-from-update (update)
  "Returns a packed web frame from renderer-neutral UPDATE."
  (pack-world-frame (generation-update-world update)
                    (generation-update-dimension update)
                    (generation-update-generation update)
                    (generation-update-live-count update)))

(defun publish-web-frame (renderer frame)
  "Publishes FRAME as the latest frame and broadcasts it to WebSocket clients."
  (let ((clients nil))
    (bt:with-lock-held ((web-renderer-lock renderer))
      (setf (web-renderer-latest-frame renderer) frame
            clients (websocket-clients-snapshot renderer)))
    (broadcast-websocket-frame renderer clients frame)
    frame))

(defun store-update-frame (renderer update)
  "Stores UPDATE as the latest packed browser frame.
The caller must hold WEB-RENDERER-LOCK."
  (setf (web-renderer-latest-frame renderer)
        (frame-from-update update)))

(defmethod renderer-initialize ((renderer web-renderer) state)
  "Initializes WEB-RENDERER with the current world frame."
  (bt:with-lock-held ((web-renderer-lock renderer))
    (setf (web-renderer-latest-frame renderer)
          (pack-world-frame (game-state-world state)
                            (game-state-dimension state)
                            (game-state-generation state)
                            (game-state-live-count state))))
  renderer)

(defmethod renderer-apply-update ((renderer web-renderer) update)
  "Publishes a packed full-world frame for browser clients."
  (publish-web-frame renderer (frame-from-update update))
  renderer)

(defmethod renderer-close ((renderer web-renderer))
  "Stops the web renderer server and simulation loop."
  (stop-web-renderer renderer))

(defun handle-control (renderer action)
  "Applies a control ACTION to RENDERER."
  (if (string= action "stop")
      (stop-web-renderer renderer :wait nil)
      (let ((frame nil)
            (clients nil))
        (bt:with-lock-held ((web-renderer-lock renderer))
          (cond ((string= action "pause")
                 (setf (web-renderer-running renderer) nil))
                ((string= action "resume")
                 (setf (web-renderer-running renderer) t))
                ((string= action "next")
                 (setf (web-renderer-running renderer) nil)
                 (let ((update (advance-game-state (web-renderer-state renderer))))
                   (setf frame (frame-from-update update)
                         (web-renderer-latest-frame renderer) frame
                         clients (websocket-clients-snapshot renderer))))
                ((string= action "previous")
                 (setf (web-renderer-running renderer) nil)
                 (let ((update (previous-game-state (web-renderer-state renderer))))
                   (when update
                     (setf frame (frame-from-update update)
                           (web-renderer-latest-frame renderer) frame
                           clients (websocket-clients-snapshot renderer)))))))
        (when frame
          (broadcast-websocket-frame renderer clients frame)))))

(defun handle-websocket-request (renderer stream headers)
  "Upgrades STREAM to a WebSocket connection and sends binary world frames."
  (let* ((socket (make-instance 'clack-stream-socket :stream stream))
         (env (list :clack.io socket
                    :headers (headers-hash-table headers)))
         (ws (websocket-driver:make-server env)))
    (websocket-driver:on :open ws
      (lambda ()
        (register-websocket-client renderer ws)
        (let ((frame (latest-frame renderer)))
          (when frame
            (send-websocket-frame-to-client renderer ws frame)))))
    (websocket-driver:on :close ws
      (lambda (&key code reason)
        (declare (ignore code reason))
        (unregister-websocket-client renderer ws)
        (stop-web-renderer-when-without-websocket-clients renderer)))
    (websocket-driver:on :error ws
      (lambda (error)
        (format *error-output* "WebSocket error: ~a~%" error)
        (unregister-websocket-client renderer ws)
        (stop-web-renderer-when-without-websocket-clients renderer)))
    (unwind-protect
         (websocket-driver:start-connection ws)
      (unregister-websocket-client renderer ws)
      (stop-web-renderer-when-without-websocket-clients renderer))))

(defun handle-web-request (renderer stream)
  "Handles one HTTP request."
  (multiple-value-bind (request-line headers)
      (read-http-headers stream)
    (when (or (null request-line)
              (string= request-line ""))
      (return-from handle-web-request nil))
    (destructuring-bind (method target &rest ignored)
        (split-string request-line #\Space)
      (declare (ignore method ignored))
      (multiple-value-bind (path query)
          (parse-query target)
        (cond ((string= path "/")
               (write-response stream "200 OK" "text/html; charset=utf-8"
                               (read-web-asset "index.html")))
              ((string= path "/ws")
               (handle-websocket-request renderer stream headers))
              ((string= path "/app.js")
               (write-response stream "200 OK" "application/javascript; charset=utf-8"
                               (read-web-asset "app.js")))
              ((string= path "/style.css")
               (write-response stream "200 OK" "text/css; charset=utf-8"
                               (read-web-asset "style.css")))
              ((string= path "/config")
               (write-response stream "200 OK" "application/json; charset=utf-8"
                               (web-config-json renderer)))
              ((string= path "/control")
               (handle-control renderer (or (query-value query "action") ""))
               (write-response stream "200 OK" "text/plain; charset=utf-8" "ok"))
              (t
               (write-response stream "404 Not Found" "text/plain; charset=utf-8" "not found")))))))

(defun web-server-loop (renderer)
  "Accepts web renderer HTTP clients."
  (let ((socket (web-renderer-server-socket renderer)))
    (unwind-protect
         (loop until (web-renderer-quit renderer)
               do (handler-case
                      (let ((client (sb-bsd-sockets:socket-accept socket)))
                        (bt:make-thread
                         (lambda ()
                           (let ((stream (sb-bsd-sockets:socket-make-stream
                                          client
                                          :input t
                                          :output t
                                          :element-type '(unsigned-byte 8)
                                          :buffering :full)))
                             (unwind-protect
                                  (handler-case
                                      (handle-web-request renderer stream)
                                    (error (condition)
                                      (unless (web-renderer-quit renderer)
                                        (format *error-output*
                                                "Web client error: ~a~%"
                                                condition))))
                               (ignore-errors (close stream))
                               (ignore-errors (sb-bsd-sockets:socket-close client)))))
                         :name "game-of-life-web-client"))
                    (error (condition)
                      (unless (web-renderer-quit renderer)
                        (format *error-output* "Web server error: ~a~%" condition)))))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun web-simulation-loop (renderer)
  "Advances simulation state and publishes frames."
  (loop until (web-renderer-quit renderer)
        do (progn
             (let ((frame nil)
                   (clients nil))
               (bt:with-lock-held ((web-renderer-lock renderer))
                 (when (web-renderer-running renderer)
                   (let ((update (advance-game-state
                                  (web-renderer-state renderer))))
                     (setf frame (frame-from-update update)
                           (web-renderer-latest-frame renderer) frame
                           clients (websocket-clients-snapshot renderer)))))
               (when frame
                 (broadcast-websocket-frame renderer clients frame)))
             (sleep (/ 1 (max 1 (web-renderer-speed renderer)))))))

(defun start-web (&key (function #'init-random-pattern) (dimension 300)
                    (speed 10) (coverage 0.25) (history-limit 0)
                    (port 8080) (host "127.0.0.1") (running nil)
                    (open-browser t))
  "Starts a web-based renderer using packed full-frame browser updates.
Returns the WEB-RENDERER instance."
  (let* ((state (create-game-state :function function
                                   :dimension dimension
                                   :coverage (and (or (eq function 'init-random-pattern)
                                                      (eq function #'init-random-pattern))
                                                  coverage)
                                   :history-limit history-limit
                                   :track-changes (or (null history-limit)
                                                      (> history-limit 0))))
         (renderer (make-instance 'web-renderer
                                  :port port
                                  :host host
                                  :state state
                                  :speed speed
                                  :running running))
         (url (web-ui-url host port)))
    (renderer-initialize renderer state)
    (setf (web-renderer-server-socket renderer)
          (make-listener-socket host port))
    (setf (web-renderer-server-thread renderer)
          (bt:make-thread (lambda () (web-server-loop renderer))
                          :name "game-of-life-web-server"))
    (setf (web-renderer-simulation-thread renderer)
          (bt:make-thread (lambda () (web-simulation-loop renderer))
                          :name "game-of-life-web-simulation"))
    (format t "Web UI running at ~a~%" url)
    (when open-browser
      (open-web-ui-url url))
    renderer))

(defun stop-web (renderer)
  "Stops a web renderer returned by START-WEB."
  (renderer-close renderer))

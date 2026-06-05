# Conway's Game of Life

A Common Lisp implementation of Conway's Game of Life with a renderer-neutral core, a Tk UI, and a browser-based web UI. The world is stored as a flat `simple-bit-vector`, which keeps cell storage compact and gives the update logic predictable row-major access.

The project currently uses flat-world edges: cells outside the world are treated as dead.

## Project Layout

- `src/core.lisp`: world storage, generation stepping, game state, history, and renderer-neutral updates.
- `src/patterns.lisp`: built-in pattern initializers.
- `src/render-api.lisp`: package exports and renderer generic functions.
- `src/tk-ui.lisp`: Tk/Ltk renderer and keyboard controls.
- `src/web-ui.lisp`: local HTTP/WebSocket server and web renderer.
- `src/web-protocol.lisp`: packed binary web frame protocol.
- `web/`: browser UI assets served by `start-web`.
- `tests/main.lisp`: Rove tests for core behavior and patterns.
- `UI-PERFORMANCE-ANALYSIS.md`: performance notes from the UI/rendering work.

## Dependencies

The project is currently developed and tested with SBCL. The ASDF system depends on:

```lisp
(ql:quickload '(:ltk
                :cl-progress-bar
                :bordeaux-threads
                :websocket-driver))
```

The web server uses `sb-bsd-sockets`, which is provided by SBCL.

The test system also depends on:

```lisp
(ql:quickload :rove)
```

Tk rendering requires Tcl/Tk and `wish` to be available on your system path.

## Loading And Testing

From a REPL configured with Quicklisp and ASDF:

```lisp
(asdf:load-system :conways-game-of-life)
(in-package :conways-game-of-life)
(asdf:test-system :conways-game-of-life)
```

## Tk UI

Start the Tk renderer with:

```lisp
(start)
```

Useful arguments:

```lisp
(start :function #'init-random-pattern
       :dimension 200
       :speed 10
       :display-size 1200
       :coverage 0.25
       :running t
       :history-limit 100)
```

`display-size` may be:

- `nil`: use the detected screen size.
- A number: use a square window of that size.
- A list like `(1200 900)`: use explicit width and height.

Tk keyboard controls:

- `p`: pause.
- `s`: start/resume.
- `n`: advance one generation.
- `b`: go back one generation when history is enabled.
- `q`: quit gracefully.

The Tk renderer now draws live cells without cell outlines and uses a separate grid overlay to keep individual cells identifiable. It tracks generation history for the `b` command according to `:history-limit`.

## Web UI

Start the web renderer with:

```lisp
(start-web :dimension 300 :speed 10 :coverage 0.25 :port 8080)
```

By default, `start-web` opens `http://127.0.0.1:8080/` in your default browser. The simulation starts paused unless `:running t` is passed. Press Play in the browser to begin.

Useful arguments:

```lisp
(start-web :function #'init-random-pattern
           :dimension 300
           :speed 10
           :coverage 0.25
           :history-limit 0
           :port 8080
           :host "127.0.0.1"
           :running nil
           :open-browser t)
```

Web UI behavior:

- `/` serves `web/index.html`.
- `/app.js` and `/style.css` serve the browser assets.
- `/config` returns one-time UI configuration.
- `/control?action=...` handles Play/Pause/Next/Back/Stop commands.
- `/ws` streams packed binary full-world frames over WebSocket.

The web renderer uses `websocket-driver`; the project does not implement the WebSocket protocol manually. There are no polling, heartbeat, or `/frame` fallback mechanisms in the baseline. Multiple browser clients can view the same simulation, and the server shuts down automatically when all WebSocket clients close.

The browser supports mouse-wheel zoom, drag panning, Reset View, and keyboard zoom shortcuts with `+`, `-`, and `0`. Grid lines are drawn as a browser overlay when the zoom level makes individual cells large enough to inspect.

Because the current web protocol streams full-world frames, very large dense worlds can still be browser-bound: a `10000x10000` world is 12.5 MB per packed frame and expands to 400 MB of RGBA canvas data in the browser.

## Pattern Initializers

Built-in pattern functions:

- `init-random-pattern`
- `init-glider-pattern`
- `init-c-pattern`
- `init-pi-heptomino-pattern`
- `init-u-pattern`, kept as a compatibility alias for `init-pi-heptomino-pattern`
- `init-pentadecathlon-on-snacker-pattern`

Example:

```lisp
(start-web :function #'init-glider-pattern :dimension 80)
(start :function #'init-pi-heptomino-pattern :dimension 100)
```

Random coverage defaults to `0.25` and can be changed with `:coverage` when using `init-random-pattern`.

## Installation Notes

1. Install SBCL or another Common Lisp implementation.
2. Install Quicklisp.
3. Install Tcl/Tk if using the Tk renderer.
4. Clone this repository.
5. Ensure ASDF can find the `.asd` file. One common SBCL setup is:

```lisp
(push "~/Path/To/Local/CL-Projects" ql:*local-project-directories*)
```

### Debian/Ubuntu

```bash
sudo apt install tcl tk
```

### Arch

```bash
sudo pacman -S tcl tk
```

### Fedora

```bash
sudo dnf install tcl tk
```

### macOS

```bash
brew install tcl-tk
```

### WSL

Use the Linux instructions for your WSL distribution. If Tk display forwarding is needed, configure your display environment for your Windows/WSL setup.

## Performance Direction

Recent performance work moved the core to bit-vector storage, reduced UI copying, added renderer-neutral update APIs, and replaced HTTP frame polling with WebSocket streaming.

Likely next performance areas:

- Browser rendering for very large dense worlds.
- Level-of-detail rendering for zoomed-out web views.
- Tile or region-based web streaming if a future implementation can preserve the expected UX.
- HashLife or sparse algorithms for suitable worlds.
- Optional toroidal edge handling.

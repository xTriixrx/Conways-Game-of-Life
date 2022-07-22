# Conway-Game-Of-Life

<p align="center">
This is a Common Lisp implementation of the infamous Conway's Game of Life problem utilizing an array of bits and the Ltk library. Ltk is a portable set of Tk bindings for Common Lisp which provides low level GUI capabilities. This implementation has been tested with SBCL but should also work with other standard Common Lisp implementations. Currently, this implementation only supports a flat world, where edges are the end of the universe. This implementation has been written in such a way in order to take advantage of the capabilities of LISP, for example:
</p>

```Lisp
(start :function #'your-awesome-init-world-function :dimension x :speed y)
```

<p align="center">
The above function is how to start the GUI interface for initializing a blank x by x world with a custom pattern and updating at the speed y provided. This library also supports REPL interactive command line operations and output if that is desired for testing. For a list of available functions, please review the defpackage macro for the available exported functions and for examples on how to implement initialiation functions as well as creating bit arrays.
</p>

<p align="center">
Below is a .gif screen recording of the provided GUI interface progressing through a randomized world. The GUI interface also provides gdb-like keybindings to pause, start, and progress to the next generation. These features are showcased in the below .gif however is difficult to determine from a screen recording. For more information about the gdb-like keybindings, please review the Usage section. The frame rate's in the recording do not depict actual world performance as the frame rate is reduced during the conversion to a .gif.
</p>

<p align="center">
    <img src="https://github.com/xTriixrx/Conways-Game-of-Life/blob/main/imgs/game-of-life.gif" alt="Conway's Game of Life"
</p>

## Usage

<p align="center">
After installation of the package, you can load and verify the system with the following:
</p>

```Lisp
(asdf:load-system :conways-game-of-life)
(in-package :conways-game-of-life)
(asdf:test-system :conways-game-of-life)
(start)
```

<p align="center">
After running the test-system function, you should have all unit tests pass. If they fail, you may need to manually install the necessary packages to your QuickLisp environment with the following:
</p>

```Lisp
(ql:quickload :ltk)
(ql:quickload :prove)
(ql:quickload :cl-progress-bar)
```

<p align="center">
You should only have to manually install the packages once if the initial verification of the system does not work; once packages are installed your QuickLisp instance will maintain the dependencies.
</p>

<p align="center">
To run the Ltk GUI use the aforementioned #'start function with the desired arguments; the start function starts with default values to test a basic random world. As the Ltk GUI is running, you can hit the following keys to perform specific actions with the world:
</p>

* p (Pause) - Will pause the progression of generations on the current generation. If you plan on pausing, it is suggested to keep sleep times higher as their is currently no way to "unwind" the progression.
* s (Start) - Will start the progression of generations again starting on the currently paused generation.
* n (Next) - If the progression is in motion, the next command will stop the progression and perform the next progression. If the progression has already stopped, the next progression is performed.

## Installation

Below are the generic high level instructions for installing the appropriate Common Lisp environment as well as the necessary steps to run this application. Detailed commands for installing packages based on OS follows the generic instructions. 

1. Ensure sbcl (or some Common Lisp repl), QuickLisp, and Slime/Sly have been installed.

2. Ensure tcl & tk are installed on your machine. This can be tested by running the 'wish' command which should run the simple windowing shell that should be installed along side tcl and tk. Make sure 'wish' is added to your PATH.

3. Clone the repository.

4. Configure your ASDF environment to read the .asd file from the cloned repo. If you clone the repo within a local project directory for QuickLisp, it should be read automatically on instantiation of your REPL flavor.

<p align="center">
For SBCL, add the following to the bottom of your ~/.sbclrc file to create a local project directory outside of your QuickLisp installation: 
</p>

```Lisp
(push "~/Path/To/Local/CL-Projects: ql:*local-project-directories*)
```

### Linux

<p align="center">
If you are fresh installing a Common Lisp environment, their are numerous guides online to assist in getting one set up.
<a href="https://lisp-lang.org/learn/getting-started/">Learning Lisp</a>
is a good reference to get started by installing sbcl, QuickLisp, and SLIME.
</p>

### Debian-Based Distros

```Bash
sudo apt install tcl
sudo apt install tk
git clone https://github.com/xTriixrx/Conways-Game-of-Life.git
echo '(push "~/Path/To/Local/CL-Projects: ql:\*local-project-directories\*)' >> ~/.sbclrc
```

### Arch-Based Distros

```Bash
sudo pacman -S tcl
sudo pacman -S tk
git clone https://github.com/xTriixrx/Conways-Game-of-Life.git
echo '(push "~/Path/To/Local/CL-Projects: ql:\*local-project-directories\*)' >> ~/.sbclrc
```

### Fedora-Based Distros

```Bash
sudo yum install tcl
sudo yum install tk
git clone https://github.com/xTriixrx/Conways-Game-of-Life.git
echo '(push "~/Path/To/Local/CL-Projects: ql:\*local-project-directories\*)' >> ~/.sbclrc
```

### Windows

<p align="center">
More information will added once integration with Windows 10/11 has been tested. Integration instructions will be provided for both WSL 2.0 as well as standard Windows support.
</p>

### Mac OS

<p align="center">
More information will added once integration with Mac OS has been tested, please note that integration with Apple Silicon Macs may be incompatible due to the tcl/tk requirements.
</p>

## Future Updates

* Improve computational complexity
* Improve GUI capabilities
* Ability to "unwind" progression(s)
* Add toroidal array support
* Add HashLife support

### References

<a href="http://www.lispworks.com/documentation/HyperSpec/Front/index.htm">Common Lisp HyperSpec Documentation</a>

<a href="https://quickref.common-lisp.net/ltk.html">Ltk Reference Manual</a>

<a href="https://en.m.wikibooks.org/wiki/Common_Lisp/External_libraries/Ltk">Ltk Basic Examples</a>
# Conway-Game-Of-Life

<p align="center">
This is a Common Lisp implementation of the infamous Conway's Game of Life problem utilizing an array of bits and the Ltk library. Ltk is a portable set of Tk bindings for Common Lisp which provides low level GUI capabilities. This implementation has been tested with SBCL but should also work with other standard Common Lisp implementations. Currently, this implementation only supports a flat world, where edges are the end of the universe. This implementation has be written in a way to take advantage of the capabilities of LISP, for example:
</p>

```
(start :function #'your-awesome-init-world-function :dimension x :speed y)
```

<p align="center">
The above function is how to start the GUI interface for initializing a blank x by x world with a custom pattern and updating at the speed y provided. 
</p>

<p align="center">
![Conway's Game of Life](https://github.com/xTriixrx/Conways-Game-of-Life/blob/main/imgs/game-of-life.gif)
</p>

## Usage

<p align="center">
</p>

## Installation

1. Ensure tcl & tk are installed on your machine. This can be tested by running the 'wish' command which should run the simple windowing shell that should be installed along side tcl and tk. Make sure 'wish' is added to your PATH.

2. Clone the repository.

3. Configure your ASDF environment to read the .asd file from the cloned repo. If you clone the repo within a local project directory for QuickLisp, it should be read automatically on instantiation of your REPL flavor. For SBCL, add the following to the bottom of your ~/.sbclrc file to create a local project directory outside of your QuickLisp installation: (push "~/Path/To/Local/CL-Projects: ql:\*local-project-directories\*)


### Linux

### Debian-Based Distros

```
sudo apt install tcl
sudo apt install tk
```

```
git clone https://github.com/xTriixrx/Conways-Game-of-Life.git
```

### Arch-Based Distros

```
sudo pacman -S tcl
sudo pacman -S tk
```

```
git clone https://github.com/xTriixrx/Conways-Game-of-Life.git
```

### Fedora-Based Distros

```
sudo yum install tcl
sudo yum install tk
```

```
git clone https://github.com/xTriixrx/Conways-Game-of-Life.git
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
* Add toroidal array support
* Add HashLife support
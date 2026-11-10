# onin_gala

## Installation

### Requirements

This project requires:

- **[Odin](https://odin-lang.org/docs/install/)** — Download and install the Odin compiler. Make sure `odin` is available in your `$PATH`.
- **Clang and LLVM** — Required for compilation and code generation. In particular, `clang`, `llc` and `opt` must be available in your `$PATH`.

#### Installing LLVM and Clang

**Debian / Ubuntu**
```bash
sudo apt update
sudo apt install clang llvm
```

**Alpine Linux**
```bash
sudo apk add clang llvm
```

**Arch Linux**
```bash
sudo pacman -S clang llvm
```

Depending on your distribution, LLVM utilities may be versioned (e.g. `llc-18` and `opt-18`). Ensure the required executables are accessible through your `$PATH`.

### Building

Clone the repository and build the compiler:

```bash
git clone https://github.com/Tiziozero/onin_gala.git
cd onin_gala
python3 build.py
```

This will compile the project and produce the `galac` executable.

### Adding `galac` to PATH

To make `galac` accessible globally without specifying its full path, create a symbolic link in your local binary directory:

```bash
mkdir -p ~/.local/bin
ln -sf "$(realpath galac)" ~/.local/bin/galac
```

If `~/.local/bin` is not already in your `$PATH`, add it to your shell configuration (e.g. `~/.bashrc` or `~/.zshrc`):

```bash
export PATH="$HOME/.local/bin:$PATH"
```

Restart your shell or reload its configuration:

```bash
source ~/.bashrc
# or
source ~/.zshrc
```

You should now be able to invoke the compiler from anywhere:
```bash
galac
```

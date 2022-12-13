rpi
===

Work in progress.

This repository is based on https://github.com/dbuenzli/rpi-boot-ocaml
and an attempt to reuse some components for MirageOS 4.0 and the Raspberry Pi 4.


### Build for Bare Metal

```
opam pin gilbraltar-toolchain https://github.com/dinosaure/gilbraltar.git
opam pin gilbraltar https://github.com/dinosaure/gilbraltar.git
dune build --workspace dune-workspace.bare
```

### Build on a Raspberry Pi

```
dune build
```

### Cross-Compilation

We need a cross-compiler configured for the Raspberry Pi OS (corresponding GLIBC):

https://github.com/abhiTronix/raspberry-pi-cross-compilers#e-toolchain-setup-documentation

After that, an `aarch64-linux-gnu` toolchain should be available.

TODO: instructions for `gilbraltar-unix`. 

Then, use

```
dune build --workspace dune-workspace.cross
```

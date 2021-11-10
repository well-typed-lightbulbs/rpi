rpi
===

Work in progress.

This repository is based on https://github.com/dbuenzli/rpi-boot-ocaml
and attempt to re-use some components to be used with MirageOS 4.0 and the Raspberry Pi 4.


### Build


```
opam pin gilbraltar-toolchain https://github.com/dinosaure/gilbraltar.git
opam pin gilbraltar https://github.com/dinosaure/gilbraltar.git
dune build -x rpi4 _build/default.rpi4/test/main.exe
```

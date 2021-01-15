# Fungoid
Befunge-93 interpreter written in Haskell!

# Requirements
This project requires `GHC 8.8.4` and can be built using either `cabal` or `stack`.

# Installation
```bash
λ> git clone https://github.com/PureFunctor/Fungoid.git
λ> cd Fungoid
λ> [cabal | stack] install
```

# Examples
In order to run examples obtained from the [esolangs wiki](https://esolangs.org/wiki/Befunge):
```bash
λ> [cabal | stack] run Fungoid -- b93/helloworld.b93 93
λ> [cabal | stack] run Fungoid -- b93/factorial.b93 93
```

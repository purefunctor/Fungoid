# Fungoid
Befunge-93 interpreter written in Haskell!

# Requirements
This project requires `GHC 8.10.4` and can be built using `stack`.

# Installation
```bash
λ> git clone https://github.com/PureFunctor/Fungoid.git
λ> cd Fungoid
λ> stack install
```

# Examples
In order to run examples obtained from the [esolangs wiki](https://esolangs.org/wiki/Befunge):
```bash
λ> stack run Fungoid -- b93/helloworld.b93 93
λ> stack run Fungoid -- b93/factorial.b93 93
```

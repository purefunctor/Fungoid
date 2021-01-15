{ mkDerivation, base, bytestring, containers, mtl, random, stdenv
}:
mkDerivation {
  pname = "Fungoid";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring containers mtl random ];
  executableHaskellDepends = [ base bytestring ];
  homepage = "https://github.com/PureFunctor/Fungoid";
  description = "Befunge-93 interpreter written in Haskell!";
  license = stdenv.lib.licenses.mit;
}

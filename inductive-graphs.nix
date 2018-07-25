{ mkDerivation, base, bytestring, containers, doctest, safe, stdenv
, transformers
}:
mkDerivation {
  pname = "inductive-graphs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring containers safe transformers
  ];
  testHaskellDepends = [ base doctest ];
  license = stdenv.lib.licenses.bsd3;
}

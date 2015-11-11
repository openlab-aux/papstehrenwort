{ mkDerivation, base, binary, bytestring, containers, deepseq
, directory, fgl, hashable, HTTP, HUnit, hxt, knob, network
, network-uri, parsec, QuickCheck, safe, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text
, text-binary, unordered-containers
}:
mkDerivation {
  pname = "rdf4h";
  version = "1.3.5";
  sha256 = "0zhg2qixjlkmlsfypjvrcyl1nq0snnlx46wqjvv7lvp2yvanvgnz";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary deepseq fgl hashable HTTP hxt network network-uri
    parsec text text-binary unordered-containers
  ];
  executableHaskellDepends = [
    base containers network network-uri text
  ];
  testHaskellDepends = [
    base bytestring containers directory HUnit knob network network-uri
    QuickCheck safe test-framework test-framework-hunit
    test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/robstewart57/rdf4h";
  description = "A library for RDF processing in Haskell";
  license = stdenv.lib.licenses.bsd3;
}

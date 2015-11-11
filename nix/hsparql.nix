{ mkDerivation, base, bytestring, containers, HTTP, http-types
, HUnit, MissingH, mtl, network, network-uri, rdf4h, stdenv
, test-framework, test-framework-hunit, text, wai, warp, xml
}:
mkDerivation {
  pname = "hsparql";
  version = "0.2.7";
  sha256 = "19xn50in5c9fdvmnd1zbx8k9621g39jdmdbp03ka2k99l7cf400g";
  libraryHaskellDepends = [
    base bytestring HTTP MissingH mtl network network-uri rdf4h text
    xml
  ];
  testHaskellDepends = [
    base containers http-types HUnit network-uri rdf4h test-framework
    test-framework-hunit text wai warp
  ];
  homepage = "https://github.com/robstewart57/hsparql";
  description = "A SPARQL query generator and DSL, and a client to query a SPARQL server";
  license = stdenv.lib.licenses.bsd3;
}

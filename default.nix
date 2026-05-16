{ lib
, buildDunePackage
}:

buildDunePackage (finalAttrs: {
  pname = "imgmeta";
  version = with lib; pipe ./dune-project [
    readFile
    (match ".*\\(version ([^\n]+)\\).*")
    head
  ];

  src = with lib.fileset; toSource {
    root = ./.;
    fileset = unions [
      ./dune-project
    ];
  };

  env.DUNE_CACHE = "disabled";

  propagatedBuildInputs = [ ];
})

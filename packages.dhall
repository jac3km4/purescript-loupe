
let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.12.3-20190315/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.3-20190920/packages.dhall sha256:53873cf2fc4a343a41f335ee47c1706ecf755ac7c5a336e8eb03ad23165dfd28

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions

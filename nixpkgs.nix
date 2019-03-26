import (builtins.fetchGit {
  # Descriptive name to make the store path easier to identify
  name = "nixpkgs-unstable-2019-03-26";
  url = https://github.com/nixos/nixpkgs;
  # `git ls-remote https://github.com/nixos/nixpkgs-channels nixpkgs-unstable`
  rev = "796a8764ab85746f916e2cc8f6a9a5fc6d4d03ac";
}) {}

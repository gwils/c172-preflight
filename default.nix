{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  sources = {
    papa = pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "papa";
      rev = "97ef00aa45c70213a4f0ce348a2208e3f482a7e3";
      sha256 = "0qm0ay49wc0frxs6ipc10xyjj654b0wgk0b1hzm79qdlfp2yq0n5";
    };

    aviation-units = pkgs.fetchFromGitHub {
      owner = "data61";
      repo = "aviation-units";
      rev = "aee9b5c37946acfa28dc8ce96bf71643e01f28c7";
      sha256 = "0kyqn2mn5yvjngqzapqgidvpj5r664i5afkbj0di0hp2nj7f58ac";
    };

    aviation-weight-balance = pkgs.fetchFromGitHub {
      owner = "data61";
      repo = "aviation-weight-balance";
      rev = "00c65b36ff350b6577bf0f815bd902f820a4bbf0";
      sha256 = "0sxz247qwjmmkpwjc5221jsph3vbcpgjf93sdcqnnk7r8hlbxj9h";
    };

    aviation-cessna172-weight-balance = pkgs.fetchFromGitHub {
      owner = "data61";
      repo = "aviation-cessna172-weight-balance";
      rev = "69b0ed0817f2971f0feca730fbdb5ea08da90f40";
      sha256 = "0mqfqqx6h5g9fmgyg8dhjf6f6vw1w7am2y4f9mz8r55ls026faf2";
    };

    aviation-cessna172-diagrams = pkgs.fetchFromGitHub {
      owner = "data61";
      repo = "aviation-cessna172-diagrams";
      rev = "b091a48f32e3cb2c9d51578c25024154ec1aa359";
      sha256 = "1q0l4x682dxpqir834n673w3v831iazjk7kfkknd5yczbhkws7p6";
    };

    hgeometry = pkgs.fetchFromGitHub {
      owner = "noinia";
      repo = "hgeometry";
      rev = "77cc519b5c1728bc983aa3b492fbcef59f42763e";
      sha256 = "0jcd9gqqshy5hsk0mjrccn8hs46pgdkryl6fyr4m5zdcz9kyncla";
    };

  };

  modifiedHaskellPackages = haskellPackages.override {
    overrides = self: super: import sources.papa self // {
      aviation-units = import sources.aviation-units {};
      aviation-weight-balance = import sources.aviation-weight-balance {};
      aviation-cessna172-weight-balance = import sources.aviation-cessna172-weight-balance {};
      aviation-cessna172-diagrams = import sources.aviation-cessna172-diagrams {};
      hgeometry = super.callCabal2nix "hgeometry" "${sources.hgeometry}" {};
      parsers = pkgs.haskell.lib.dontCheck super.parsers;        
    };
  };

  c172-preflight = modifiedHaskellPackages.callPackage ./c172-preflight.nix {};

in

  c172-preflight


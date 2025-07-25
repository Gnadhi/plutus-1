{ inputs, pkgs, lib, project, agda-tools, metatheory, r-with-packages }:

let

  tools = project.tools {
    # "latest" cabal would be 3.14.1.0 which breaks haddock generation.
    # TODO update cabal version once haddock generation is fixed upstream.
    cabal = "3.12.1.0";
    cabal-fmt = "latest";
    haskell-language-server = "latest";
    # fourmolu 0.18.0.0 and hlint 3.10 require GHC >=9.8
    fourmolu = "0.17.0.0";
    hlint = "3.8";
    stylish-haskell = "latest";
  };

  pre-commit-check = inputs.pre-commit-hooks.lib.${pkgs.system}.run {
    src = ../.;
    hooks = {
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      cabal-fmt = {
        enable = true;
        package = tools.cabal-fmt;
      };
      stylish-haskell = {
        enable = true;
        package = tools.stylish-haskell;
        args = [ "--config" ".stylish-haskell.yaml" ];
        excludes = [ "^plutus-metatheory/src/MAlonzo" ];
      };
      fourmolu = {
        enable = false;
        package = tools.fourmolu;
        args = [ "--mode" "inplace" ];
      };
      hlint = {
        enable = false;
        package = tools.hlint;
        args = [ "--hint" ".hlint.yaml" ];
      };
      shellcheck = {
        enable = false;
        package = pkgs.shellcheck;
      };
      prettier = {
        enable = false;
        package = pkgs.prettier;
      };
      editorconfig-checker = {
        enable = true;
        package = pkgs.editorconfig-checker;
      };
      generate-malonzo-code = {
        enable = true;
        entry = "${metatheory.generate-malonzo-code}/bin/generate-malonzo-code";
        files = "^plutus-metatheory/src";
        stages = [ "pre-push" ];
        pass_filenames = false;
      };
    };
  };

  linux-pkgs = lib.optionals pkgs.hostPlatform.isLinux [
    pkgs.papi
  ];

  common-pkgs = [
    pkgs.ghcid
    agda-tools.agda
    agda-tools.agda-with-stdlib
    agda-tools.agda-mode

    metatheory.generate-malonzo-code
    metatheory.agda-with-stdlib-and-metatheory

    r-with-packages
    inputs.nixpkgs-2405.legacyPackages.${pkgs.system}.linkchecker

    tools.haskell-language-server
    tools.stylish-haskell
    tools.fourmolu
    tools.cabal
    tools.hlint
    tools.cabal-fmt

    pkgs.texliveFull
    pkgs.jekyll
    pkgs.plantuml
    pkgs.jq
    pkgs.yq
    pkgs.github-cli
    pkgs.gnused
    pkgs.awscli2
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.scriv
    pkgs.fswatch
    pkgs.yarn
    pkgs.zlib
    pkgs.cacert
    pkgs.upx
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
    pkgs.nodejs_20
  ];

  locale-archive-hook =
    lib.optionalString (pkgs.stdenv.hostPlatform.libc == "glibc")
      "export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive";

  full-shell = project.shellFor {
    name = "plutus-shell-${project.args.compiler-nix-name}";

    buildInputs = lib.concatLists [
      common-pkgs
      linux-pkgs
      pre-commit-check.enabledPackages
    ];

    withHoogle = true;

    shellHook = ''
      ${pre-commit-check.shellHook}
      ${locale-archive-hook}
      export NIX_AGDA_STDLIB=${agda-tools.NIX_AGDA_STDLIB}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
      echo -e "\n🤟 Welcome to Plutus 🤟"
    '';
  };


  quick-shell = project.shellFor {
    name = "plutus-shell-${project.args.compiler-nix-name}";
    tools = { cabal = "latest"; };
    shellHook = ''
      ${locale-archive-hook}
      export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
      echo -e "\n🤟 Welcome to Plutus 🤟"
    '';
  };


  shell = {
    ghc967 = full-shell;
    ghc984 = quick-shell;
    ghc9102 = quick-shell;
  }.${project.args.compiler-nix-name};

in

shell

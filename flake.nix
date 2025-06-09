{
  description = "lem";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    systems.url = "github:nix-systems/default";
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      imports = [
        inputs.treefmt-nix.flakeModule
      ];

      perSystem =
        { pkgs, lib, ... }:
        let
          reversi = pkgs.sbcl.buildASDFSystem rec {
            pname = "reversi";
            version = "unstable";
            src = lib.cleanSource ./.;

            nativeBuildInputs = [
              pkgs.makeWrapper
            ];

            propagatedNativeBuildInputs = [
              pkgs.tclPackages.tk
            ];

            qlBundleLibs = pkgs.stdenvNoCC.mkDerivation {
              pname = "${pname}-qlot-bundle";
              inherit src version;

              nativeBuildInputs = [
                pkgs.sbclPackages.qlot-cli
                pkgs.which
                pkgs.git
                pkgs.cacert
              ];

              installPhase = ''
                runHook preInstall

                export HOME=$(mktemp -d)
                qlot install --no-color
                qlot bundle --no-color

                ls -a

                # Unnecessary and also platform-dependent file
                rm .bundle-libs/bundle-info.sexp

                # Remove vendored .so files
                find .bundle-libs -type f '(' -name '*.so' -o -name '*.dll' ')' -exec rm '{}' ';'

                cp -r .bundle-libs $out

                runHook postInstall
              '';

              dontBuild = true;
              dontFixup = true;
              outputHashMode = "recursive";
              #outputHash = "sha256-gOClnFljwVEwXbr39JlEQ2vcZeXsI5SNXbwg6+hHB1o=";
              outputHash = "sha256-En6YnPUeCF1TXK/0rNDr5Iywj85vmyG5ZcMBlueJqPA=";
            };

            configurePhase = ''
              runHook preConfigure
              mkdir -p $out/share/reversi
              (
                cd $out/share/reversi
                cp -r $qlBundleLibs .bundle-libs
                chmod -R +w .bundle-libs
              )

              source ${inputs.nixpkgs}/pkgs/development/lisp-modules/setup-hook.sh
              buildAsdfPath

              runHook postConfigure
            '';

            buildScript = pkgs.writeText "build-reversi.lisp" ''
              (defpackage :nix-cl-user
                (:use :cl))

              (in-package :nix-cl-user)
              (load "${reversi.asdfFasl}/asdf.${reversi.faslExt}")

              ;; Avoid writing to the global fasl cache
              (asdf:initialize-output-translations
                '(:output-translations :disable-cache
                                       :inherit-configuration))

              (defvar *systems* (uiop:split-string (uiop:getenv "systems")))
              (defvar *out-path* (uiop:getenv "out"))
              (defvar *share-path* (concatenate 'string
                                    *out-path*
                                    "/share/reversi"))
              (defvar *bundle-path* (concatenate 'string
                                    *share-path*
                                    "/.bundle-libs/bundle.lisp"))

              ;; Initial load
              (let ((asdf:*system-definition-search-functions*
                       (copy-list asdf:*system-definition-search-functions*)))
                (load *bundle-path*)
                (loop :for system :in *systems*
                      :do (asdf:load-system system)))

              ;; Load the bundle on every startup
              (uiop:register-image-restore-hook
                (lambda ()
                  (load *bundle-path*))
                   nil)

              (setf uiop:*image-entry-point* #'reversi:main)
              (uiop:dump-image
                "reversi"
                :executable t
                :compression t)
            '';

            installPhase = ''
              runHook preInstall

              mkdir -p $out/bin $out/share/reversi
              mv reversi $out/bin
              wrapProgram $out/bin/reversi \
                --prefix PATH : "${lib.makeBinPath propagatedNativeBuildInputs}"

              cp -r $src $out/share/reversi

              runHook postInstall
            '';
          };
        in
        {
          treefmt = {
            projectRootFile = "flake.nix";
            programs.nixfmt.enable = true;
          };

          packages = {
            inherit reversi;
            default = reversi;
          };

          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [
              # Lisp
              pkgs.sbcl
              pkgs.sbclPackages.qlot-cli
            ];
          };
        };
    };
}

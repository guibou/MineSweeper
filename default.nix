(import ./reflex.nix {
  config .android_sdk.accept_license = true;
  config.allowBroken = true;

  haskellOverlays = [(
    selfPkgs: superPkgs:
    let
      pkgs = superPkgs.callPackage ({ pkgs }: pkgs) {};
    in
      {
      clay = pkgs.haskell.lib.doJailbreak superPkgs.clay;
    }
  )];
}).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    MineSweeper = ./.;
  };

  shells = {
    ghc = ["MineSweeper"];
  };

  android.MineSweeper = {
    executableName = "MineSweeper";
    applicationId = "org.guibou.MineSweeper";
    displayName = "Mine Sweeper";
  };
})

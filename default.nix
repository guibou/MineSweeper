(import ./reflex.nix).project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    MineSweeper = ./.;
  };

  shells = {
    ghc = ["MineSweeper"];
    ghc8_2_1 = ["MineSweeper"];
    ghcjs = ["MineSweeper"];
  };

  android.MineSweeper = {
    executableName = "MineSweeper";
    applicationId = "org.guibou.MineSweeper";
    displayName = "Mine Sweeper";
  };
})

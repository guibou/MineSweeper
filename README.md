This is a mine sweeper in Haskell Reflex

## JSaddle work (fast iterations)

```shell
nix-shell -A shells.ghc8_2_1

cabal new-repl
>>> go
```

- Open your browser to "http://localhost:3003"

## Android application

```shell
nix-build -A android.frontend
```

You will get your APK inside `result`.

## Screenshot

![MineSweeper](screenshot.png)

## TODO

- Count the number of remaining mines
- Stop the timer when the game is done
- Find a better way to set flags. For now we need to change the mode using a checkbox, that's painful.
- Runtime change of the game geometry (size / nb mines)
- Nicer display on failure
- Nice icons

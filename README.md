# Sgf-mode

Sgf-mode is an Emacs mode for viewing [Go](https://en.wikipedia.org/wiki/Go_(game)) game records in Smart Game Format ([SGF](https://en.wikipedia.org/wiki/Smart_Game_Format)). It is modified from Eric Schulte's el-go (http://eschulte.github.io/el-go/).

## Install

Add following lines to .emacs:
```
  (add-to-list 'load-path "/path/to/sgf-mode")
  (require 'sgf-mode)
```

## Usage

Open foo.sgf (e.g. sgf-mode/alphago.sgf) in sgf-mode:
```
  M-x sgf-open RET [then complete the path to foo.sgf when prompt]
```
Or first open (C-x C-f) /path/to/foo.sgf, then
```
  M-x sgf-load RET
```
Toggle guess-move mode (inspired by Android APP "[Go Dojo](https://play.google.com/store/apps/details?id=pl.happydroid.goess&hl=en)"):
```
  M-x sgf-toggle-guess-move-mode RET
```

## Key bindings

 * Enter: one move forward.
 * Space: one move back.
 * Delete: previous variation.
 * Letter keys "a"-"n": navigate through variation branches.
 * "q" to bury the buffer, "Q" to kill the buffer, "R" to reload.

## Screenshot

![](https://raw.githubusercontent.com/renxiaoyi/sgf-mode/master/sgf-mode-screen-shot.png)

#lang racket/base
;; because `raco test` will not automatically reach .pm or .pp files
(require (prefix-in burial: "../scribblings/third-tutorial-files/burial.html.pm")
         (prefix-in chess: "../scribblings/third-tutorial-files/chess.html.pm")
         (prefix-in sermon: "../scribblings/third-tutorial-files/sermon.html.pm")
         (prefix-in styles: "../scribblings/third-tutorial-files/styles.css.pp"))
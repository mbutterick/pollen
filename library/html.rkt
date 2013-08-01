#lang racket

(require (planet mb/pollen/syntax))

; for now, body is deemed a block, not inline
;todo: is this legit? Why is body inline?
(define block-tags
  '(address article aside audio blockquote body canvas dd div dl fieldset figcaption figure footer form  h1 h2 h3 h4 h5 h6 header hgroup  noscript ol output p pre section table tfoot ul video))

; for now, map is omitted because it's a Racket keyword
; for now, style, script, and link are omitted because they shouldn't be wrapped
(define inline-tags 
  '(a abbr acronym applet area b base basefont bdo big br button caption center cite code col colgroup  del dir dfn dt em embed font  frame framesethead hr html i iframe img input ins isindex kbd label legend li  menu meta noframes object optgroup option param q s samp  select small span strike strong  sub sup tbody td textarea th thead title tr tt u var xmp))

(define tags (append block-tags inline-tags))


(provide (all-defined-out))
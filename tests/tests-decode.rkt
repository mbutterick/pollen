#lang racket/base
(require pollen/decode)
(module+ test (require rackunit))

(module+ test
  (check-true (begin (register-block-tag 'barfoo) (block-xexpr? '(barfoo "foo"))))

  (check-equal? (typogrify "I had --- maybe 13 -- 20 --- hob-nobs.") "I had—maybe 13–20—hob-nobs.")
  (check-equal? (typogrify "\"Why,\" she could've asked, \"are we in O‘ahu watching 'Mame'?\"") 
                "“Why,” she could’ve asked, “are we in O‘ahu watching ‘Mame’?”"))



;; todo: make some tougher tests, it gets flaky with edge cases
(module+ test
  (check-equal? (nonbreaking-last-space '(p "Hi there")) '(p "Hi there")) ; nbsp in between last two words
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "Ø") '(p "HiØthere")) ; but let's make it visible
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_") '(p "Hi_up_there")) 
  (check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_" #:minimum-word-length 3) 
                '(p "Hi there")) 
  (check-equal? (nonbreaking-last-space '(p "Hi here" (em "ho there")) #:nbsp "Ø") '(p "Hi here" (em "hoØthere")))) 


(module+ test
  (check-equal? (wrap-hanging-quotes '(p "\"Hi\" there")) '(p (dquo "“" "Hi\" there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there")) '(p (squo "‘" "Hi' there")))
  (check-equal? (wrap-hanging-quotes '(p "'Hi' there") #:single-prepend '(foo ((bar "ino")))) 
                '(p (foo ((bar "ino")) "‘" "Hi' there")))
  ;; make sure txexpr without elements passes through unscathed
  (check-equal? (wrap-hanging-quotes '(div ((style "height:2em")))) '(div ((style "height:2em")))))


(module+ test
  (check-equal? (convert-linebreaks '("foo" "\n" "bar")) '("foo" (br) "bar"))
  (check-equal? (convert-linebreaks '("\n" "foo" "\n" "bar" "\n")) '("\n" "foo" (br) "bar" "\n"))
  (check-equal? (convert-linebreaks '((p "foo") "\n" (p "bar"))) '((p "foo") (p "bar")))
  (check-equal? (convert-linebreaks '("foo" "\n" (p "bar"))) '("foo" (p "bar")))
  (check-equal? (convert-linebreaks '("foo" "moo" "bar")) '("foo" "moo" "bar"))
  (check-equal? (convert-linebreaks '("foo" "moo" "bar") #:newline "moo") '("foo" (br) "bar"))
  (check-equal? (convert-linebreaks '("foo" "\n\n" "bar")) '("foo" "\n\n" "bar")))



(module+ test
  (check-false (paragraph-break? "foo"))
  (check-false (paragraph-break? "\n"))
  (check-false (paragraph-break? "\n \n"))
  (check-true (paragraph-break? "\n \n" #:pattern #px"^\n \n$"))
  (check-true (paragraph-break? "\n\n"))
  (check-true (paragraph-break? "\n\n\n")))


(module+ test
  (check-equal? (merge-newlines '(p "\n" "foo" "\n" "\n" "bar" (em "\n" "\n" "\n"))) 
                '(p "\n" "foo" "\n\n" "bar" (em "\n\n\n"))))



;; todo: add native support for list-xexpr
;; decode triple newlines to list items


(module+ test
  (check-equal? (prep-paragraph-flow '("\n" "foo" "\n" "\n" "bar" "\n" "ino" "\n"))
                '("foo" "\n\n" "bar" (br) "ino")))

(module+ test
  (check-equal? (wrap-paragraph '("foo" "bar")) '(p "foo" "bar"))
  (check-equal? (begin (append-block-tag 'para) (wrap-paragraph #:tag 'para '("foo" "bar"))) 
                '(para "foo" "bar"))
  (check-equal? (wrap-paragraph '((p "bar" "foo"))) '(p "bar" "foo"))
  (check-equal? (wrap-paragraph '((div "bar" "foo") "Hi" )) '(p (div "bar" "foo") "Hi")))

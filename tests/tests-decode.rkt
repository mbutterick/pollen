#lang racket/base
(require pollen/decode rackunit txexpr)

(check-true (begin (register-block-tag 'barfoo) (block-txexpr? '(barfoo "foo"))))

(check-equal? (smart-dashes "I had --- maybe 13 -- 20 --- hob-nobs.") "I had—maybe 13–20—hob-nobs.")
(check-equal? (smart-quotes "\"Why,\" she could've asked, \"are we in O‘ahu watching 'Mame'?\"") 
              "“Why,” she could’ve asked, “are we in O‘ahu watching ‘Mame’?”")



;; todo: make some tougher tests, it gets flaky with edge cases
(check-equal? (nonbreaking-last-space '(p "Hi there")) '(p "Hi " "there")) ; nbsp in between last two words
(check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "Ø") '(p "HiØ" "there")) ; but let's make it visible
(check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_") '(p "Hi_up_" "there")) 
(check-equal? (nonbreaking-last-space '(p "Hi there") #:nbsp "_up_" #:minimum-word-length 3) 
              '(p "Hi " "there")) 
(check-equal? (nonbreaking-last-space '(p "Hi here" (em "ho there")) #:nbsp "Ø") '(p "Hi here" (em "hoØ" "there")))



(check-equal? (wrap-hanging-quotes '(p "\"Hi\" there")) '(p (dquo "“" "Hi\" there")))
(check-equal? (wrap-hanging-quotes '(p "'Hi' there")) '(p (squo "‘" "Hi' there")))
(check-equal? (wrap-hanging-quotes '(p "'Hi' there") #:single-prepend '(foo ((bar "ino")))) 
              '(p (foo ((bar "ino")) "‘" "Hi' there")))
;; make sure txexpr without elements passes through unscathed
(check-equal? (wrap-hanging-quotes '(div ((style "height:2em")))) '(div ((style "height:2em"))))


(check-equal? (detect-linebreaks '("foo" "\n" "bar")) '("foo" (br) "bar"))
(check-equal? (detect-linebreaks '("\n" "foo" "\n" "bar" "\n")) '("\n" "foo" (br) "bar" "\n"))
(check-equal? (detect-linebreaks '((p "foo") "\n" (p "bar"))) '((p "foo") (p "bar")))
(check-equal? (detect-linebreaks '("foo" "\n" (p "bar"))) '("foo" (p "bar")))
(check-equal? (detect-linebreaks '("foo" "moo" "bar")) '("foo" "moo" "bar"))
(check-equal? (detect-linebreaks '("foo" "moo" "bar") #:insert "moo") '("foo" "moo" "bar"))
(check-equal? (detect-linebreaks '("foo" "\n\n" "bar")) '("foo" "\n\n" "bar"))




(check-equal? (merge-newlines '(p "\n" "foo" "\n" "\n" "bar" (em "\n" "\n" "\n"))) 
              '(p "\n" "foo" "\n\n" "bar" (em "\n\n\n")))


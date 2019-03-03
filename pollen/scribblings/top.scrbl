#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup pollen/tag))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/setup))

@title{Top}

@defmodule[pollen/top]

You'll probably never invoke this module directly. But it's implicitly imported into every Pollen markup file. And if you don't know what it does, you might end up surprised by some of the behavior you get.

@defform[(#%top . id)]{

In standard Racket, @racket[#%top] is the function of last resort, called when @racket[_id] is not bound to any value. As such, it typically reports a syntax error.
 
@examples[
(code:comment @#,t{Let's call em without defining it})
(em "Bonjour")
(code:comment @#,t{(em "Bonjour") is being converted to ((#%top . em) "Bonjour")})
(code:comment @#,t{So calling ((#%top . em) "Bonjour") will give the same result})
((#%top . em) "Bonjour")
]

In the Pollen markup environment, however, this behavior is annoying. Because when you're writing X-expressions, you don't necessarily want to define all your tags ahead of time. 

So Pollen redefines @racket[#%top]. For convenience, Pollen's version of @racket[#%top] assumes that an undefined tag should just refer to an X-expression beginning with that tag (and uses @racket[default-tag-function] to provide this behavior):

@examples[
(code:comment @#,t{Again, let's call em without defining it, but using pollen/top})
(require pollen/top)
(em "Bonjour")
(code:comment @#,t{(em "Bonjour") is still being converted to ((#%top . em) "Bonjour")})
(code:comment @#,t{But now, ((#%top . em) "Bonjour") gives a different result})
((#%top . em) "Bonjour")
]

The good news is that this behavior means you use any tag you want in your markup without defining it in advance. You can still attach a function to the tag later, which will automatically supersede @racket[#%top].

@examples[
(define (em x) `(span ((style "font-size:100px")) ,x))
(em "Bonjour")
]

The bad news is that you'll never get an ``unbound identifier'' error. These unbound identifiers will happily sail through and be converted to tags.

@examples[
(require pollen/top)
(define (em . xs) `(span ((style "font-size:100px")) ,@xs))
(code:comment @#,t{There's a typo in my tag})
(erm "Bonjour")
]

@margin-note{If you prefer the ordinary Racket-style behavior where unbound identifiers raise an error, define @racket[setup:allow-unbound-ids?] in your project to be @racket[#false].}

This isn't a bug. It's just a natural consequence of how Pollen's @racket[#%top] works. It can, however, make debugging difficult sometimes. Let's suppose my markup depends on @racket[very-important-function], which I don't import correctly.

@examples[
(require pollen/top)
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
(very-important-function "Bonjour")
]

So the undefined-function bug goes unreported. Again, that's not a bug in Pollen — there's just no way for it to tell the difference between an identifier that's deliberately undefined and one that's inadvertently undefined. If you want to guarantee that you're invoking a defined identifier, use @racket[def/c].}


@defform[(def/c id)]{Invoke @racket[_id] if it's a defined identifier, otherwise raise an error. This form reverses the behavior of @racket[#%top] (in other words, it restores default Racket behavior). 

Recall this example from before. In standard Racket, you get an undefined-identifier error.

@examples[
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
(very-important-function "Bonjour")
]

But with @racketmodname[pollen/top], the issue is not treated as an error.

@examples[
(require pollen/top)
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
(very-important-function "Bonjour")
]

By adding @racket[def/c], we restore the usual behavior, guaranteeing that we get the defined version of @racket[very-important-function] or nothing.

@examples[
(require pollen/top)
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
((def/c very-important-function) "Bonjour")
]

}

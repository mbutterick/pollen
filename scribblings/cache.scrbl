#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket pollen/world pollen/render pollen/file sugar txexpr))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Cache}

@defmodule[pollen/cache]

The slowest part of a @racket[render] is parsing and decoding the source file. Often, previewing a single source file necessarily means decoding others (for instance templates, or other source files that are linked into the main source file). But usually, only one source file is changing at a time. Therefore, Pollen stores copies of the exports of source files — namely, whatever is stored in @code[(format "~a" world:main-export)] and @code[(format "~a" world:meta-export)] — in the cache so they can be reused.


@defproc[
(cached-require
[source-path pathish?]
[key (or/c 'doc 'metas)])
(or/c txexpr? hash? integer?)]
Similar to @racket[(dynamic-require _source-path _key)], except that it first tries to retrieve the requested value out of the cache. If it's not there, or out of date, @racket[dynamic-require] is used to update the value.

The only keys supported are @racket['doc] and @racket['metas].

If you want the speed benefit of the cache, you should @bold{always} use @racket[cached-require] to get data from Pollen source files. That doesn't mean you can't still use functions like @racket[require], @racket[local-require], and @racket[dynamic-require]. They'll just be slower.


@defproc[
(reset-cache)
void?]
Clears the cache. When only the nuclear option will do.



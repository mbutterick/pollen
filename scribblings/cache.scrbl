#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/cache pollen/render pollen/file))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Cache}

@defmodule[pollen/cache]

The slowest part of a @racket[render] is parsing and decoding the source file. Often, previewing a single source file necessarily means decoding others (for instance templates, or other source files that are linked into the main source file). But usually, only one source file is changing at a time. Therefore, Pollen stores copies of the exports of source files — namely, whatever is stored in @code[(format "'~a" world:main-pollen-export)] and @code[(format "'~a" world:meta-pollen-export)] — in the cache so they can be reused.

@defproc[
(cached-require
[source-path pathish?]
[key (or/c 'doc 'metas 'mod-time)])
(or/c txexpr? hash? integer?)]
Similar to @racket[(dynamic-require _source-path _key)], except that it tries to get the requested value out of @racket[current-cache]. If it's not there, or out of date, @racket[dynamic-require] is used to update the value.

If you want the speed benefit of the cache, you should @bold{always} use @racket[cached-require] to get data from Pollen source files. That doesn't mean you can't still use functions like @racket[require], @racket[local-require], and @racket[dynamic-require]. They'll just be slower.


@defproc[
(current-cache)
(or/c #f hash?)]
A parameter that refers to the current cache. It starts with a value of @racket[#f]. It has to be initialized with @racket[make-cache].

The cache is a hash table that uses the complete path of a source file as its keys. The value associated with each of these keys is another hash table with keys @code[(format "'~a" world:main-pollen-export)], @code[(format "'~a" world:meta-pollen-export)] (for storing the exports of the source file) and @racket['mod-time] (for storing the modification time).

@defproc[
(make-cache)
hash?]
Initializes @racket[current-cache].

@defproc[
(reset-cache)
void?]
Clears @racket[current-cache]. When only the nuclear option will do.


@defproc[
(cache-ref
[source-path pathish?])
hash?]
Returns the cached value associated with the key @racket[_source-path], which will itself be a hash table. See @racket[current-cache].


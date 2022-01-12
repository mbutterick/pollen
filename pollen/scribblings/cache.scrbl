#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/cache pollen/setup (for-label racket pollen/core pollen/setup pollen/render pollen/file sugar txexpr))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Cache}

@defmodule[pollen/cache]

The slowest part of a Pollen @racket[render] is compiling a source file. Because Pollen allows source files to be edited and previewed dynamically, these files get recompiled a lot. Therefore, Pollen stores copies of the exports of source files — namely, whatever is stored in @code[(format "~a" pollen-main-export)] and @code[(format "~a" pollen-meta-export)] — in a cache so they can be reused.

In each directory of your project, Pollen writes cache files into a subdirectory called @filepath{compiled}. The files are stored on disk so they can be reused between sessions. If you delete files within a cache directory (or the whole thing), don't worry — everything will get regenerated. (However, I don't recommend trying to read or write directly to any @filepath{compiled} directory, as the implementation details of the cache are subject to change.)

@section{Preloading and reseting}

Though the cache will be populated as you use Pollen, you can also preheat it with @exec{@seclink["raco_pollen_setup"]}. This command will load all your source files into the cache. This will give you the snappiest performance during an interactive session with the project server.

If you want to reset all the compile caches, use @exec{@seclink["raco_pollen_reset"]}.

@section{Disabling the cache}

The compile cache is controlled by the @seclink["setup-overrides"]{overridable value} @racket[setup:compile-cache-active]. Thus, to disable the compile cache, add a @racket[setup] submodule to your @filepath{pollen.rkt} like so:

@codeblock|{
(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f))
}|

Pollen also caches rendered output files, so if you want to disable all caching — thus forcing everything to recompile, every time — you should also disable the render cache by overriding @racket[setup:render-cache-active]:

@codeblock|{
(module setup racket/base
  (provide (all-defined-out))
  (define compile-cache-active #f)
  (define render-cache-active #f))
}|

Be warned that this will make your rendering much slower. But you will be guaranteed an entirely fresh recompile each time, which can sometimes be useful in development.


@section{Scope of dependency tracking}

The compile cache tracks the modification date of the source file, the current setting of @secref["The_POLLEN_environment_variable"], and the modification dates of the template and @filepath{pollen.rkt} (if they exist). For @tt{poly} source files, it also tracks the @racket[current-poly-target]. It also tracks files you've listed in the optional setup value @racket[setup:cache-watchlist] and environment variables listed in the optional setup value @racket[setup:envvar-watchlist].

It does not, however, track every possible dependency. So in a complex project, it's possible to create deep dependencies that aren't noticed by the cache. In particular, Pollen does not track pagetree files as dependencies of other source files. Thus, if you change a pagetree, you'll ordinarily need to use @exec{raco pollen reset} to clear the caches.

Unfortunately, there's no way around this problem. For the cache to be useful, there has to be a limit on the horizon of dependency checking. To capture every possible dependency, the cache would have to recompile every file, every time — which would be equivalent to not caching at all.

Those who need that kind of deep dynamism can disable the cache (with the setup values @racket[setup:render-cache-active] and @racket[setup:compile-cache-active]).


@section[#:tag-prefix "cache"]{Functions}


@deftogether[(
@defproc[
(cached-doc
[source-path pathish?])
txexpr?]

@defproc[
(cached-metas
[source-path pathish?])
hash-eq?]
)]
Attempt to retrieve the requested value out of the cache. If it's not there, or out of date, @racket[dynamic-require] is used to update it from the source.

These functions are the lower-level cousins of @racket[get-doc] and @racket[get-metas], which have a more convenient interface. Unless you have a special reason, you're better off using those.

If you want the speed benefit of the cache, you should use @racket[cached-doc] and @racket[cached-metas] to get data from Pollen source files in preference to functions like @racket[require], @racket[local-require], and @racket[dynamic-require]. Those will also work. They'll just be slower.


@defproc[
(reset-cache)
void?]
Clears the cache. When only the nuclear option will do.

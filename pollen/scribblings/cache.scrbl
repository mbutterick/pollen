#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/cache pollen/setup (for-label racket pollen/setup pollen/render pollen/file sugar txexpr))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Cache}

@defmodule[pollen/cache]

The slowest part of a Pollen @racket[render] is compiling a source file. Because Pollen allows source files to be edited and previewed dynamically, these files get recompiled a lot. Therefore, Pollen stores copies of the exports of source files — namely, whatever is stored in @code[(format "~a" default-main-export)] and @code[(format "~a" default-meta-export)] — in a cache so they can be reused.

In each directory of your project, Pollen creates a subdirectory called @filepath{pollen-cache}. The files are stored on disk so they can be reused between sessions. If you delete files within a cache directory (or the whole thing), don't worry — everything will get regenerated. (However, you should not read or write to any @filepath{pollen-cache} directory, as the implementation details are subject to change.)

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

The compile cache tracks the modification date of the source file, the current setting of @secref["The_POLLEN_environment_variable"], and the modification dates of the template and @filepath{pollen.rkt} (if they exist). 

It does not, however, track every possible dependency. So in a complex project, it's possible to create deep dependencies that aren't noticed by the cache. 

Unfortunately, there's no way around this problem. For the cache to be useful, there has to be a limit on the horizon of dependency checking. To capture every possible dependency, the cache would have to recompile every file, every time — which would be equivalent to not caching at all.

Those who need that kind of deep dynamism can disable the cache.


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
Try to retrieve the requested value out of the cache. If it's not there, or out of date, @racket[dynamic-require] is used to update it from the source.

Despite their names, these functions actually rely on @racket[setup:main-export] and @racket[setup:meta-export] (which default to @id[default-main-export] and @id[default-meta-export]). Thus, if you override those names, everything will still work as expected.

If you want the speed benefit of the cache, you should @bold{always} use @racket[cached-doc] and @racket[cached-metas] to get data from Pollen source files. That doesn't mean you can't also use functions like @racket[require], @racket[local-require], and @racket[dynamic-require]. They'll just be slower.


@defproc[
(reset-cache)
void?]
Clears the cache. When only the nuclear option will do.

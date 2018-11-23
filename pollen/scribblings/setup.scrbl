#lang scribble/manual
@(require "mb-tools.rkt")
@(require scribble/eval pollen/setup racket/string (for-label racket syntax/modresolve (except-in pollen #%module-begin) pollen/setup))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/setup))

@title{Setup}

@defmodule[pollen/setup]


@section[#:tag "setup-overrides"]{How to override setup values}

The values below can be changed by overriding them in your @racket["pollen.rkt"] source file:

@itemlist[#:style 'ordered

@item{Within this file, @seclink["submodules" #:doc '(lib "scribblings/guide/guide.scrbl")]{create a submodule} called @racket[setup].}

@item{Within this submodule, use @racket[define] to make a variable with the same name as the one in @racket[pollen/setup], but without the @racket[setup:] prefix.}

@item{Assign it whatever value you like.}

@item{Repeat as needed.}

@item{(Don't forget to @racket[provide] the variables from within your @racket[setup] submodule.)}

 ]

When Pollen runs, these definitions will supersede those in @racketmodname[pollen/setup].

For instance, suppose you wanted the main export of every Pollen source file to be called @racket[van-halen] rather than @racket[doc], the extension of Pollen markup files to be @racket[.rock] rather than @racket[.pm], and the command character to be @litchar{🎸} instead of @litchar{◊}. Your @racket["pollen.rkt"] would look like this:

@fileblock["pollen.rkt" 
@codeblock{
#lang racket/base

;; ... the usual definitions and tag functions ...

(module setup racket/base
  (provide (all-defined-out))
  (define main-export 'van-halen)
  (define markup-source-ext 'rock)
  (define command-char #\🎸))
}]

Of course, you can restore the defaults simply by removing these defined values from @racket["pollen.rkt"].

Every @racket[setup:]@racket[_name] function will resolve the current value of that variable: it will return the value from the @racket[setup] submodule (if @racket[_name] was defined there), otherwise it will return the default value (which is directly available from @racket[default-]@racket[_name]). For instance, @racket[default-command-char] will always be @litchar{◊}, but in the example above, @racket[(setup:command-char)] would return @litchar{🎸}. 

@section{Values}

@defoverridable[project-server-port integer?]{
Determines the default HTTP port for the project server.}


@defoverridable[main-export symbol?]{The main X-expression exported from a compiled Pollen source file.}

@defoverridable[meta-export symbol?]{The meta hashtable exported from a compiled Pollen source file.}

@defoverridable[extension-escape-char char?]{Character for escaping output-file extensions within source-file names.}


@deftogether[(
@defoverridable[preproc-source-ext symbol?]
@defoverridable[markup-source-ext symbol?]
@defoverridable[markdown-source-ext symbol?]
@defoverridable[null-source-ext symbol?]
@defoverridable[pagetree-source-ext symbol?]
@defoverridable[template-source-ext symbol?]
@defoverridable[scribble-source-ext symbol?]
)]{File extensions for Pollen source files.}


@defoverridable[main-pagetree string?]{Pagetree that Pollen dashboard loads by default in each directory.}



@defoverridable[main-root-node symbol?]{Name of the root node in a decoded @racket[doc].}

@defoverridable[block-tags (listof symbol?)]{Tags that are treated as blocks by @racket[block-txexpr?]. Initialized to the @link["https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements"]{block-level elements in HTML5}, namely:

@racketidfont{@(string-join (map symbol->string (cdr default-block-tags)) " ")}

... plus @racket[setup:main-root-node].}



@defoverridable[command-char char?]{The magic character that indicates a Pollen command, function, or variable.}

@defoverridable[template-prefix string?]{Prefix of the default template.}


@deftogether[(
@(defoverridable newline string?)
@(defoverridable linebreak-separator string?)
@(defoverridable paragraph-separator string?)
)]    
Default separators used in decoding.


@defoverridable[render-cache-active boolean?]{Whether the render cache, which speeds up interactive sessions by reusing rendered versions of Pollen output files, is active.}

@defoverridable[compile-cache-active boolean?]{Whether the compile cache, which speeds up interactive sessions by saving compiled versions of Pollen source files, is active.}

@defoverridable[compile-cache-max-size exact-positive-integer?]{Maximum size of the compile cache.}

@defoverridable[cache-watchlist (listof (or/c path? path-string?))]{List of extra files that the cache (= render cache + compile cache, collectively) watches during a project-server session. If one of the files on the watchlist changes, the cache is invalidated (just as it would be if @racket["pollen.rkt"] changed).

If the cache can't find a certain file on the watchlist, it will be ignored. Therefore, to avoid unexpected behavior, the best policy is to pass in complete paths (or path strings). An easy way to convert a module name into a complete path is with @racket[resolve-module-path]:

@fileblock["pollen.rkt" 
@codeblock{
(module+ setup
  (require syntax/modresolve)
  (provide (all-defined-out))
  (define cache-watchlist (map resolve-module-path '("my-module.rkt"))))
}]

@pollen-history[#:added "1.4"]
}


@defoverridable[publish-directory (or/c path-string? path-for-some-system?)]{Default target for @secref{raco_pollen_publish}. A complete path is used as is; a relative path is published to the desktop.. @pollen-history[#:added "1.1"]}

@defoverridable[unpublished-path? (path? . -> . boolean?)]{@pollen-history[#:changed "1.1" @elem{Deprecated. Please use @racket[setup:omitted-path?].}]}


@defoverridable[omitted-path? (path? . -> . boolean?)]{Predicate that determines whether a path is omitted from @secref{raco_pollen_render} and @secref{raco_pollen_publish} operations. If the predicate evaluated to @racket[#t], then the path is omitted. 

@pollen-history[#:added "1.1"]}

@defoverridable[extra-published-path? (path? . -> . boolean?)]{@pollen-history[#:changed "1.1" @elem{Deprecated. Please use @racket[setup:extra-path?].}]}

@defoverridable[extra-path? (path? . -> . boolean?)]{Predicate that determines if path is rendered & published, overriding @racket[(setup:omitted-path?)] above, and Pollen's default publish settings. For instance, Pollen automatically omits files with a @racket[.rkt] extension. If you wanted to force a @racket[.rkt] file to be published, you could include it here.

@pollen-history[#:added "1.1"]}


@defoverridable[splicing-tag symbol?]{Key used to signal that an X-expression should be spliced into its containing X-expression.}


@defoverridable[poly-source-ext symbol?]{Extension that indicates a source file can target multiple output types.}


@defoverridable[poly-targets (listof symbol?)]{List of symbols that denotes the possible targets of a @racket['poly] source file.}


@defoverridable[index-pages (listof string?)]{List of strings that the project server will use as directory default pages, in order of priority. Has no effect on command-line rendering operations. Also has no effect on your live web server (usually  that's a setting you need to make in an @tt{.htaccess} configuration file).} But with this setting, you can simulate the behavior of your live server, so that internal index-page URLs work correctly.

 @defoverridable[trim-whitespace? boolean?]{Predicate that controls whether the Pollen source reader trims whitespace from the beginning of a @racket[doc] export. You might set this to @racket[#false] if you're using Pollen as a preprocessor for another programming language and you want to preserve leading whitespace accurately.

 @pollen-history[#:added "1.5"]}


@section{Parameters}

I mean @italic{parameters} in the Racket sense, i.e. values that can be fed to @racket[parameterize]. 

@defparam[current-server-port port integer? #:value default-project-server-port]{
A parameter that sets the HTTP port for the project server.}


@defparam[current-project-root path path?]{
A parameter that holds the root directory of the current project (e.g., the directory where you launched @code{raco pollen start}).}


@defparam[current-server-extras-path dir path? #:value #f]{
A parameter that reports the path to the directory of support files for the project server.}

@defparam[current-poly-target target symbol? #:value 'html]{
A parameter that reports the current rendering target for @racket[poly] source files.}


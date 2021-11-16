#lang scribble/manual
@(require "mb-tools.rkt")
@(require scribble/eval pollen/setup racket/string (for-label (except-in racket #%top) racket/runtime-path syntax/modresolve (except-in pollen #%module-begin #%top) pollen/render pollen/setup pollen/top))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/setup))

@(require (for-syntax racket/base racket/syntax pollen/setup))
@(define-syntax (defoverridable stx)
  (syntax-case stx ()
    [(_ name predicate? desc ...)
     (with-syntax* ([default-name (format-id #'here "default-~a" #'name)]
                   [value (let ([v (syntax-local-eval #'default-name)])
                            (cond
                              [(and (list? v) (andmap symbol? v) (> (length v) 5)) #`'#,'(see below)]
                              [(or (symbol? v) (list? v)) #`'#,v]
                              [(procedure? v) '(λ (path) #f)]
                              [else v]))]
                   [setup:name (format-id stx "setup:~a" #'name)])
       #`(deftogether ((defproc (setup:name) predicate?)
                       (defthing default-name predicate? #:value value))
           desc ...))]))

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



@defoverridable[main-pagetree string?]{Pagetree that Pollen dashboard loads by default in each directory.}



@defoverridable[main-root-node symbol?]{Name of the root node in a decoded @racket[doc].}

@defoverridable[block-tags (listof symbol?)]{Tags that are treated as blocks by @racket[block-txexpr?]. Initialized to the @link["https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements"]{block-level elements in HTML5}, namely:

@racketidfont{@(string-join (map symbol->string (cdr default-block-tags)) " ")}

... plus @racket[setup:main-root-node].}



@defoverridable[command-char char?]{The magic character that indicates a Pollen command, function, or variable.}


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

If the cache can't find a certain file on the watchlist, no error will arise. The file will simply be ignored. Therefore, to avoid unexpected behavior, the best policy is to use complete paths (or path strings). One way to generate a complete path to a local file is with @racket[define-runtime-path]. Another way, if you're using a module that's already installed as part of a package, is with @racket[resolve-module-path]:

@fileblock["pollen.rkt" 
@codeblock{
(module+ setup
  (provide (all-defined-out))
  (require racket/runtime-path syntax/modresolve)
  (define-runtime-path my-local-mod "my-module.rkt")
  (define my-installed-mod (resolve-module-path 'package/my-other-module))
  (define cache-watchlist (list my-local-mod my-installed-mod)))
}]

@history[#:added "1.4"]
}

@defoverridable[envvar-watchlist (listof string?)]{List of extra environment variables that are used in cache keys. Separate caches will be maintained for each distinct value of an environment variable. @secref["The_POLLEN_environment_variable"] is always used, regardless of how this value is set.

Both the names and the values of environment variables are case-insensitive, so @racket["PUB"] and @racket["pub"] and @racket["pUb"] are all treated the same.

@history[#:added "2.1"]}



@defoverridable[publish-directory (or/c path-string? path-for-some-system?)]{Default target for @secref{raco_pollen_publish}. A complete path is used as is; a relative path is published to the desktop.. @history[#:added "1.1"]}

@defoverridable[unpublished-path? (path? . -> . boolean?)]{@history[#:changed "1.1" @elem{Deprecated. Please use @racket[setup:omitted-path?].}]}


@defoverridable[omitted-path? (path? . -> . boolean?)]{Predicate that determines whether a path is omitted from @secref{raco_pollen_render} and @secref{raco_pollen_publish} operations. If the predicate evaluated to @racket[#t], then the path is omitted. 

@history[#:added "1.1"]}

@defoverridable[extra-published-path? (path? . -> . boolean?)]{@history[#:changed "1.1" @elem{Deprecated. Please use @racket[setup:extra-path?].}]}

@defoverridable[extra-path? (path? . -> . boolean?)]{Predicate that determines if path is rendered & published, overriding @racket[(setup:omitted-path?)] above, and Pollen's default publish settings. For instance, Pollen automatically omits files with a @racket[.rkt] extension. If you wanted to force a @racket[.rkt] file to be published, you could include it here.

@history[#:added "1.1"]}


@defoverridable[poly-targets (listof symbol?)]{List of symbols that denotes the possible targets of a @racket['poly] source file.}


@defoverridable[index-pages (listof string?)]{List of strings that the project server will use as directory default pages, in order of priority. Has no effect on command-line rendering operations. Also has no effect on your live web server (usually  that's a setting you need to make in an @tt{.htaccess} configuration file).} But with this setting, you can simulate the behavior of your live server, so that internal index-page URLs work correctly.

 @defoverridable[trim-whitespace? boolean?]{Predicate that controls whether the Pollen source reader trims whitespace from the beginning of a @racket[doc] export. You might set this to @racket[#false] if you're using Pollen as a preprocessor for another programming language and you want to preserve leading whitespace accurately.

 @history[#:added "1.5"]}

@defoverridable[allow-unbound-ids? boolean?]{Predicate that controls whether Pollen converts unbound identifiers into default tags by altering the behavior of @racket[#%top] in @racketmodname[pollen/top].

@history[#:added "2.0"]}

@defoverridable[external-renderer (or/c (list/c module-path? symbol?) #f)]{A module path and identifier (suitable for use with @racket[dynamic-require]) that provide a function for Pollen to call instead of @racket[render] when rendering files needed by the @seclink["Using_the_project_server"]{project server} or when running @secref["raco_pollen_render"]. The function must accept the same arguments as @racket[render-to-file] and should return the final output as a @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{string} or @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{byte string}. Pollen will always write this return value out to the output file for you.

Setting this value gives you full control over (and responsibility for) how Pollen converts the compiled @racketidfont{doc} and @racketidfont{metas} from source files into their final output. Your renderer should be able to handle any of Pollen’s @seclink["Source_formats"]{source formats} or @seclink["Utility_formats"]{utility formats}. The operation of Pollen’s @racket[render] function is not affected by setting this value, so your renderer can use it as a fallback.

 @history[#:added "3.2"]}
  
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


#lang scribble/manual
@(require "mb-tools.rkt")
@(require scribble/eval pollen/setup racket/string (for-label racket (except-in pollen #%module-begin) pollen/setup))

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

Every @racket[(setup:_name)] function will resolve the current value of that variable: it will return the value from the @racket[setup] submodule (if @racket[_name] was defined there), otherwise it will return the default value (which is directly available from @racket[setup:default-]@racket[_name]). For instance, @racket[setup:default-command-char] will always be @litchar{◊}, but in the example above, @racket[(setup:command-char)] would return @litchar{🎸}. 

@section{Values}

@defoverridable[project-server-port integer?]{
Determines the default HTTP port for the project server. Initialized to @val[setup:default-project-server-port].}

@defoverridable[main-export symbol?]{The main X-expression exported from a compiled Pollen source file. Initialized to @racket[doc].}

@defoverridable[meta-export symbol?]{The meta hashtable exported from a compiled Pollen source file. Initialized to @racket[metas].}

@defoverridable[extension-escape-char char?]{Character for escaping output-file extensions within source-file names. Initialized to @racket[#\_].}


@deftogether[(
@defoverridable[preproc-source-ext symbol?]
@defoverridable[markup-source-ext symbol?]
@defoverridable[markdown-source-ext symbol?]
@defoverridable[null-source-ext symbol?]
@defoverridable[pagetree-source-ext symbol?]
@defoverridable[template-source-ext symbol?]
@defoverridable[scribble-source-ext symbol?]
)]
File extensions for Pollen source files, initialized to the following values:

@racket[setup:default-preproc-source-ext] = @code[(format "'~a" setup:default-preproc-source-ext)]
@(linebreak)@racket[setup:default-markup-source-ext] = @code[(format "'~a" setup:default-markup-source-ext)]
@(linebreak)@racket[setup:default-markdown-source-ext] = @code[(format "'~a" setup:default-markdown-source-ext)]
@(linebreak)@racket[setup:default-null-source-ext] = @code[(format "'~a" setup:default-null-source-ext)]
@(linebreak)@racket[setup:default-pagetree-source-ext] = @code[(format "'~a" setup:default-pagetree-source-ext)]
@(linebreak)@racket[setup:default-template-source-ext] = @code[(format "'~a" setup:default-template-source-ext)]
@(linebreak)@racket[setup:default-scribble-source-ext] = @code[(format "'~a" setup:default-scribble-source-ext)]


@defoverridable[main-pagetree string?]{Pagetree that Pollen dashboard loads by default in each directory. Initialized to @filepath{index.ptree}.}



@defoverridable[main-root-node symbol?]{Name of the root node in a decoded @racket[doc]. Initialized to @code{'root}.}

@defoverridable[block-tags (listof symbol?)]{Tags that are treated as blocks by @racket[block-txexpr?]. Initialized to the @link["https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements"]{block-level elements in HTML5}, namely:

@racketidfont{@(string-join (map symbol->string (cdr setup:default-block-tags)) " ")}

... plus @racket[setup:main-root-node].}



@defoverridable[command-char char?]{The magic character that indicates a Pollen command, function, or variable. Initialized to @racket[#\◊].}

@defoverridable[template-prefix string?]{Prefix of the default template. Initialized to @code{"template"}.}


@deftogether[(
@(defoverridable newline string?)
@(defoverridable linebreak-separator string?)
@(defoverridable paragraph-separator string?)
)]    
Default separators used in decoding. The first two are initialized to @racket["\n"]; the third to @racket["\n\n"].


@defoverridable[render-cache-active boolean?]{Whether the render cache, which speeds up interactive sessions by reusing rendered versions of Pollen output files, is active. Default is active (@racket[#t]).}

@defoverridable[compile-cache-active boolean?]{Whether the compile cache, which speeds up interactive sessions by saving compiled versions of Pollen source files, is active. Default is active (@racket[#t]).}

@defoverridable[compile-cache-max-size exact-positive-integer?]{Maximum size of the compile cache. Default is 10 megabytes.}

@defoverridable[unpublished-path? (path? . -> . boolean?)]{Predicate that determines whether a path is omitted from @secref{raco_pollen_publish} operations. If the predicate is @racket[#t], then the path is omitted. The default, therefore, is @racket[#f].}


@defoverridable[splicing-tag symbol?]{Key used to signal that an X-expression should be spliced into its containing X-expression. Default is @val[setup:default-splicing-tag].}


@defoverridable[poly-source-ext symbol?]{Extension that indicates a source file can target multiple output types. Default is @racket['poly].}


@defoverridable[poly-targets (listof symbol?)]{List of symbols that denotes the possible targets of a @racket['poly] source file. Default is @racket['(html)].}


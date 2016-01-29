#lang scribble/manual
@(require "mb-tools.rkt")
@(require scribble/eval pollen/world racket/string (for-label racket (except-in pollen #%module-begin) pollen/world))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/world))

@title{World}

@defmodule[pollen/world]

Global values that are used throughout the Pollen system.

@section{Parameters}

I mean @italic{parameters} in the Racket sense, i.e. values that can be fed to @racket[parameterize]. 

@defparam[world:current-server-port port integer?]{
A parameter that sets the HTTP port for the project server. Initialized to @racket[world:default-port].}


@defparam[world:current-project-root port path?]{
A parameter that holds the root directory of the current project (e.g., the directory where you launched @code{raco pollen start}).}

@defparam[world:current-server-extras-path dir path?]{
A parameter that reports the path to the directory of support files for the project server. Initialized to @racket[#f], but set to a proper value when the server runs.}

@defparam[world:current-poly-target target symbol?]{
A parameter that reports the current rendering target for @racket[poly] source files. Initialized to @racket['html].}


@section[#:tag "world-overrides"]{World overrides}

These values can be changed by overriding them in your @racket["pollen.rkt"] source file:

@itemlist[#:style 'ordered

@item{Within this file, @seclink["submodules" #:doc '(lib "scribblings/guide/guide.scrbl")]{create a submodule} called @racket[world].}

@item{Within this submodule, use @racket[define] to make a variable with the same name as the one in @racket[pollen/world], but without the @racket[world:] prefix.}

@item{Assign it whatever value you like.}

@item{Repeat as needed.}
 ]

  When Pollen runs, these definitions will supersede those in @racket[pollen/world].

For instance, suppose you wanted the main export of every Pollen source file to be called @racket[van-halen] rather than @racket[doc], the extension of Pollen markup files to be @racket[.rock] rather than @racket[.pm], and the command character to be @litchar{🎸} instead of @litchar{◊}. Your @racket["pollen.rkt"] would look like this:

@fileblock["pollen.rkt" 
@codeblock{
#lang racket/base

;; ... the usual definitions and tag functions ...

(module world racket/base
  (provide (all-defined-out))
  (define main-export 'van-halen)
  (define markup-source-ext 'rock)
  (define command-char #\🎸))
}]

Though any of the values below can be overridden, it may not always be wise to do so. For instance, if you redefined @racket[world:fallback-template-prefix], you would simply break the fallback-template mechanism, because it would look for files that don't exist. But we don't live in a nanny state, so you are entrusted to say what you mean and accept the consequences.

Of course, you can restore the defaults simply by removing these defined values from @racket["pollen.rkt"].

These values are each equipped with a corresponding @racket[world:current-]@racket[_name] function that will return the value loaded from the @racket[world] submodule (if @racket[_name] was defined there), otherwise it returns the original value for @racket[world:]@racket[_name]. For instance, @racket[world:command-char] will always be @litchar{◊}, but in the example above, @racket[world:current-command-char] would return @litchar{🎸}. 


@defoverridable[default-port integer?]{
Determines the default HTTP port for the project server. Initialized to @racket[8080].}


@defoverridable[main-export symbol?]{The main X-expression exported from a compiled Pollen source file. Initialized to @racket[doc].}

@defoverridable[meta-export symbol?]{The meta hashtable exported from a compiled Pollen source file. Initialized to @racket[metas].}

@defoverridable[meta-tag-name symbol?]{Name of the tag used to mark metas within Pollen source.}

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

@racket[world:preproc-source-ext] = @code[(format "'~a" world:preproc-source-ext)]
@(linebreak)@racket[world:markup-source-ext] = @code[(format "'~a" world:markup-source-ext)]
@(linebreak)@racket[world:markdown-source-ext] = @code[(format "'~a" world:markdown-source-ext)]
@(linebreak)@racket[world:null-source-ext] = @code[(format "'~a" world:null-source-ext)]
@(linebreak)@racket[world:pagetree-source-ext] = @code[(format "'~a" world:pagetree-source-ext)]
@(linebreak)@racket[world:template-source-ext] = @code[(format "'~a" world:template-source-ext)]
@(linebreak)@racket[world:scribble-source-ext] = @code[(format "'~a" world:scribble-source-ext)]


@defoverridable[decodable-extensions (listof symbol?)]{File extensions that are eligible for decoding.}


@defoverridable[default-pagetree string?]{Pagetree that Pollen dashboard loads by default in each directory. Initialized to @filepath{index.ptree}.}


@defoverridable[pagetree-root-node symbol?]{Name of the root node in a decoded pagetree. It's ignored by the code, so its only role is to clue you in that you're looking at something that came out of the pagetree decoder. Initialized to @code{'pagetree-root}.}


@defoverridable[main-root-node symbol?]{Name of the root node in a decoded @racket[doc]. Initialized to @code{'root}.}

@defoverridable[block-tags (listof symbol?)]{Tags that are treated as blocks by @racket[block-txexpr?]. Initialized to the @link["https://developer.mozilla.org/en-US/docs/Web/HTML/Block-level_elements"]{block-level elements in HTML5}, namely:

@racketidfont{@(string-join (map symbol->string (cdr world:block-tags)) " ")}

... plus @racket[world:current-main-root-node].}



@defoverridable[command-char char?]{The magic character that indicates a Pollen command, function, or variable. Initialized to @racket[#\◊].}

@defoverridable[default-template-prefix string?]{Prefix of the default template. Initialized to @code{"template"}.}


@defoverridable[fallback-template-prefix string?]{Used to generate the name of the fallback template (i.e., the template used to render a Pollen markup file when no other template can be found). Prefix is combined with the output suffix of the source file. Initialized to @code{"fallback"}.}


@defoverridable[template-meta-key symbol?]{Meta key used to store a template name for that particular source file. Initialized to @racket['template].}

@deftogether[(
@(defoverridable newline string?)
@(defoverridable linebreak-separator string?)
@(defoverridable paragraph-separator string?)
)]    
Default separators used in decoding. The first two are initialized to @racket["\n"]; the third to @racket["\n\n"].

@defoverridable[dashboard-css string?]{CSS file used for the dashboard. Initialized to @filepath{poldash.css}.}

@defoverridable[paths-excluded-from-dashboard (listof path?)]{Paths not shown in the Pollen dashboard.}

@defoverridable[render-cache-active boolean?]{Whether the render cache, which speeds up interactive sessions by reusing rendered versions of Pollen output files, is active. Default is active (@racket[#t]).}

@defoverridable[compile-cache-active boolean?]{Whether the compile cache, which speeds up interactive sessions by saving compiled versions of Pollen source files, is active. Default is active (@racket[#t]).}

@defoverridable[compile-cache-max-size exact-positive-integer?]{Maximum size of the compile cache. Default is 10 megabytes.}

@defoverridable[unpublished-path? (path? . -> . boolean?)]{Predicate that determines whether a path is omitted from @secref{raco_pollen_publish} operations. If the predicate is @racket[#t], then the path is omitted. The default, therefore, is @racket[#f].}

@defoverridable[here-path-key symbol?]{Key used to store the absolute path of the current source file in its @racket[metas] hashtable. Default is @racket['here-path].}

@defoverridable[splicing-tag symbol?]{Key used to signal that an X-expression should be spliced into its containing X-expression. Default is @val[world:splicing-tag].}


@defoverridable[poly-source-ext symbol?]{Extension that indicates a source file can target multiple output types. Default is @racket['poly].}


@defoverridable[poly-targets (listof symbol?)]{List of symbols that denotes the possible targets of a @racket['poly] source file. Default is @racket['(html)].}


#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/render))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/world))

@title{World}

@defmodule[pollen/world]

A set of global values and parameters that are used throughout the Pollen system. If you don't like the defaults I've picked, change them.

All identifiers are exported with the prefix @racket[world:], and are so documented below. 

@deftogether[(
@defthing[world:main-pollen-export symbol? #:value 'doc]
@defthing[world:meta-pollen-export symbol? #:value 'metas]
)]
The two exports from a compiled Pollen source file.

@(defthing world:project-require string? #:value "project-require.rkt")
File implicitly required into every Pollen source file from its directory.

@defparam[world:check-project-requires-in-render? check? boolean?
          #:value #t]{
A parameter that determines whether the @racket[world:project-require] file is checked for changes on every pass through @racket[render]. (Can be faster to turn this off if you don't need it.) Initialized to @racket[#t].}

@defthing[world:server-extras-dir string? #:value "server-extras"]
Name of directory where server support files live.

@defparam[world:current-server-extras-path dir path?
          #:value #f]{
A parameter that reports the path to the directory of support files for the development server. Initialized to @racket[#f], but set to a proper value when @racketmodname[pollen/server] runs.}


@deftogether[(
@defthing[world:preproc-source-ext symbol? #:value 'pp]
@defthing[world:markup-source-ext symbol? #:value 'pm]
@defthing[world:markdown-source-ext symbol? #:value 'pmd]
@defthing[world:null-source-ext symbol? #:value 'p]
@defthing[world:pagetree-source-ext symbol? #:value 'ptree]
@defthing[world:template-source-ext symbol? #:value 'pt]
@defthing[world:scribble-source-ext symbol? #:value 'scrbl]
)]
File extensions for Pollen source files.


@defthing[world:decodable-extensions (listof symbol?) #:value (list world:markup-source-ext world:pagetree-source-ext)]
File extensions that are eligible for decoding.


@deftogether[(
@(defthing world:mode-auto symbol? #:value 'auto)
@(defthing world:mode-preproc symbol? #:value 'pre)
@(defthing world:mode-markup symbol? #:value 'markup)
@(defthing world:mode-markdown symbol? #:value 'markdown)
@(defthing world:mode-pagetree symbol? #:value 'ptree)
)]    
Mode indicators for the Pollen reader and parser.

@defthing[world:default-pagetree string? #:value "index.ptree"]
Pagetree that Pollen dashboard loads by default in each directory.

@defthing[world:pagetree-root-node symbol? #:value 'pagetree-root]
Name of the root node in a decoded pagetree. It's ignored by the code, so its only role is to clue you in that you're looking at something that came out of the pagetree decoder.


@defthing[world:command-marker char? #:value #\◊]
The magic character that indicates a Pollen command, function, or variable.

@defthing[world:default-template-prefix string? #:value "template"]
Prefix of the default template.

@defthing[world:fallback-template string? #:value "fallback.html.pt"]
Name of the fallback template (i.e., the template used to render a Pollen markup file when no other template can be found).

@defthing[world:template-meta-key symbol? #:value 'template]
Meta key used to store a template name for that particular source file.

@deftogether[(
@(defthing world:newline string? #:value "\n")
@(defthing world:linebreak-separator string? #:value world:newline)
@(defthing world:paragraph-separator string? #:value "\n\n")
)]    
Default separators used in decoding.

@(defthing world:dashboard-css string? #:value "poldash.css")
CSS file used for the dashboard.

@(defthing world:paths-excluded-from-dashboard (listof path?) #:value (map string->path (list "poldash.css" "compiled")))
Paths not shown in the Pollen dashboard.


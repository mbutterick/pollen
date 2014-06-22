#lang scribble/manual

@(require scribble/eval pollen/cache pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/render))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/world))

@title{World}

@defmodule[pollen/world]

A set of global values and parameters that are used throughout the Pollen system. If you don't like the defaults I've picked, change them.

All identifiers are exported with the prefix @racket[world:], and are so documented below. 


@defthing[world:default-port integer?]
@defparam[world:current-server-port port integer?]{
A parameter that sets the HTTP port for the project server. Initialized to @racket[world:default-port], which defaults to 8080.}


@deftogether[(
@defthing[world:main-pollen-export symbol?]
@defthing[world:meta-pollen-export symbol?]
)]
The two exports from a compiled Pollen source file. Initialized to @racket['doc] and @racket['metas], respectively.

@(defthing world:project-require string?)
File implicitly required into every Pollen source file from its directory. Initialized to @racket["project-require.rkt"].

@defparam[world:check-project-requires-in-render? check? boolean?]{
A parameter that determines whether the @racket[world:project-require] file is checked for changes on every pass through @racket[render]. (Can be faster to turn this off if you don't need it.) Initialized to @racket[#t].}

@defthing[world:server-extras-dir string?]
Name of directory where server support files live. Initialized to @racket["server-extras"].

@defparam[world:current-server-extras-path dir path?]{
A parameter that reports the path to the directory of support files for the project server. Initialized to @racket[#f], but set to a proper value when the server runs.}


@deftogether[(
@defthing[world:preproc-source-ext symbol?]
@defthing[world:markup-source-ext symbol?]
@defthing[world:markdown-source-ext symbol?]
@defthing[world:null-source-ext symbol?]
@defthing[world:pagetree-source-ext symbol?]
@defthing[world:template-source-ext symbol?]
@defthing[world:scribble-source-ext symbol?]
)]
File extensions for Pollen source files, initialized to the following values:

@racket[world:preproc-source-ext] = @code[(format "~a" world:preproc-source-ext)]
@(linebreak)@racket[world:markup-source-ext] = @code[(format "~a" world:markup-source-ext)]
@(linebreak)@racket[world:markdown-source-ext] = @code[(format "~a" world:markdown-source-ext)]
@(linebreak)@racket[world:null-source-ext] = @code[(format "~a" world:null-source-ext)]
@(linebreak)@racket[world:pagetree-source-ext] = @code[(format "~a" world:pagetree-source-ext)]
@(linebreak)@racket[world:template-source-ext] = @code[(format "~a" world:template-source-ext)]
@(linebreak)@racket[world:scribble-source-ext] = @code[(format "~a" world:scribble-source-ext)]


@defthing[world:decodable-extensions (listof symbol?)]
File extensions that are eligible for decoding.


@deftogether[(
@(defthing world:mode-auto symbol?)
@(defthing world:mode-preproc symbol?)
@(defthing world:mode-markup symbol?)
@(defthing world:mode-markdown symbol?)
@(defthing world:mode-pagetree symbol?)
)]    
Mode indicators for the Pollen reader and parser. Initialized to the following values:

@racket[world:mode-auto] = @code[(format "~a" world:mode-auto)]
@(linebreak)@racket[world:mode-preproc] = @code[(format "~a" world:mode-preproc)]
@(linebreak)@racket[world:mode-markup] = @code[(format "~a" world:mode-markup)]
@(linebreak)@racket[world:mode-markdown] = @code[(format "~a" world:mode-markdown)]
@(linebreak)@racket[world:mode-pagetree] = @code[(format "~a" world:mode-pagetree)]

@defthing[world:default-pagetree string?]
Pagetree that Pollen dashboard loads by default in each directory. Initialized to @racket["index.ptree"].

@defthing[world:pagetree-root-node symbol?]
Name of the root node in a decoded pagetree. It's ignored by the code, so its only role is to clue you in that you're looking at something that came out of the pagetree decoder. Initialized to @racket['pagetree-root].


@defthing[world:command-marker char?]
The magic character that indicates a Pollen command, function, or variable. Initialized to @racket[#\◊].

@defthing[world:default-template-prefix string?]
Prefix of the default template. Initialized to @racket["template"].

@defthing[world:fallback-template-prefix string?]
Used to generate the name of the fallback template (i.e., the template used to render a Pollen markup file when no other template can be found). Prefix is combined with the output suffix of the source file. Initialized to @racket["fallback"].

@defthing[world:template-meta-key symbol?]
Meta key used to store a template name for that particular source file. Initialized to @racket['template].

@deftogether[(
@(defthing world:newline string?)
@(defthing world:linebreak-separator string?)
@(defthing world:paragraph-separator string?)
)]    
Default separators used in decoding. The first two are initialized to @racket["\n"]; the third to @racket["\n\n"].

@(defthing world:dashboard-css string?)
CSS file used for the dashboard. Initialized to @racket["poldash.css"].

@(defthing world:paths-excluded-from-dashboard (listof path?))
Paths not shown in the Pollen dashboard.

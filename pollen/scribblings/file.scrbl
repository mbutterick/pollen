#lang scribble/manual

@(require scribble/eval "mb-tools.rkt" pollen/render pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup sugar pollen/file))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))

@title[#:tag "file-types"]{File}

@defmodule[pollen/file]

A utility module that provides functions for working with Pollen source and output files. In ordinary use, you probably won't need these. But if you want to do more elaborate Pollen hacking, here they are.

Pollen handles six kinds of source files:


@itemlist[
@item{@bold{Preprocessor}, with file extension @ext[pollen-preproc-source-ext]}

@item{@bold{Markup}, with file extension @ext[pollen-markup-source-ext]} 

@item{@bold{Markdown}, with file extension @ext[pollen-markdown-source-ext]} 

@item{@bold{Null}, with file extension @ext[pollen-null-source-ext]}

@item{@bold{Scribble}, with file extension @ext[pollen-scribble-source-ext]}

@item{@bold{Pagetree}, with file extension @ext[pollen-pagetree-source-ext]. This is the only source type that does not produce an output file.}

]


The functions in this module rely on file extensions specified in @racketmodname[pollen/setup]. These extensions can be overridden within a project — see @secref["setup-overrides"]. 

For each kind of Pollen source file, the corresponding output file name is derived by removing the extension from the name of the source file. So the preprocessor source file @filepath{default.css.pp} would become @filepath{default.css}. (See 
@secref["Saving___naming_your_source_file"] if this rings no bells.)

Scribble files work differently — the corresponding output file is the source file but with an @filepath{html} extension rather than @filepath{scrbl}. So @filepath["pollen.scrbl"] would become @filepath["pollen.html"].

For more about Pollen's file model, see @secref["File_formats"].

@deftogether[
(@defproc[
(preproc-source?
[val any/c]) 
boolean?]

@defproc[
(markup-source?
[val any/c]) 
boolean?]

@defproc[
(markdown-source?
[val any/c]) 
boolean?]

@defproc[
(null-source?
[val any/c]) 
boolean?]

@defproc[
(scribble-source?
[val any/c]) 
boolean?]

@defproc[
(pagetree-source?
[val any/c]) 
boolean?]
)]
Test whether @racket[_val] is a path representing a source file of the specified type, based on its file extension. Does not check whether @racket[_val] exists.

@examples[#:eval my-eval
(preproc-source? "main.css.pp")
(markup-source? "default.html.pm")
(markdown-source? "default.html.pmd")
(null-source? "index.html.p")
(scribble-source? "file.scrbl")
(pagetree-source? "index.ptree")
]


@deftogether[
(@defproc[
(->preproc-source-path
[p pathish?]) 
path?]

@defproc[
(->markup-source-path
[p pathish?]) 
path?]

@defproc[
(->markdown-source-path
[p pathish?]) 
path?]

@defproc[
(->null-source-path
[p pathish?]) 
path?]

@defproc[
(->scribble-source-path
[p pathish?]) 
path?]
)]
Convert an output path @racket[_p] into the source path of the specified type that would produce this output path. This function simply generates a corresponding source path — it does not ask whether this source path exists. (If you want a guarantee that the file exists, use @racket[get-source].)

@examples[#:eval my-eval
(define name "default.html")
(->preproc-source-path name)
(->markup-source-path name)
(->markdown-source-path name)
(->scribble-source-path name)
(->null-source-path name)
]


@deftogether[(

@defproc[
(get-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-markup-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-markdown-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-preproc-source
[p pathish?]) 
(or/c #f path?)]


@defproc[
(get-null-source
[p pathish?]) 
(or/c #f path?)]

@defproc[
(get-scribble-source
[p pathish?]) 
(or/c #f path?)]
)]
Find an existing source path that would produce the output path @racket[_p]. 

The omnibus @racket[get-source] will check source formats in this order: @racket[get-markup-source], @racket[get-markdown-source], @racket[get-preproc-source], @racket[get-null-source], and @racket[get-scribble-source]. 

The type-specific variants will, of course, only return a source file of the specified type.

In all cases, if there is no corresponding source, return @racket[#f].




@defproc[
(->output-path
[p pathish?]) 
path?]
Convert a source path @racket[_p] into its corresponding output path. This function simply generates a path for a file — it does not ask whether the file exists.

If @racket[_p] has a @seclink["The_poly_output_type"]{@id[pollen-poly-source-ext] output type}, then @racket[->output-path] uses @racket[current-poly-target] as the output-path extension.

Otherwise, there are no type-specific variants for this function because the output path of a Pollen source file is @seclink["Saving___naming_your_source_file"]{determined by its name}.

@examples[#:eval my-eval
(->output-path "main.css.pp")
(->output-path "default.html.pm")
(->output-path "index.html.p")
(->output-path "file.scrbl")
]

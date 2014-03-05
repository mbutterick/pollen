#lang scribble/manual

@(require scribble/eval pollen/render pollen/world (for-label racket (except-in pollen #%module-begin) pollen/world pollen/render web-server/templates pollen/file-tools))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@section{Rendering}

@defmodule[pollen/render]

@defproc[
(render
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]) 
bytes?]
Renders the source file at @racket[_source-path]. The rendering behavior depends on the type of source file:

A @racketmodname[pollen/pre] file is rendered without a template.

A @racketmodname[pollen/markup] or @racketmodname[pollen/markdown] file is rendered with a template. If no template is provided with @racket[_template-path], Pollen tries to find one using @racket[get-template-for]. If that doesn't work, Pollen uses the fallback template. 

Be aware that rendering with a template uses @racket[include-template] within @racket[eval]. This is necessary to allow dynamic refresh of pages. For complex pages, it can be slow the first time. Caching is used to make subsequent requests faster.

For those panicked at the use of @racket[eval], please don't be. As the author of @racket[include-template] has already advised, ``If you insist on dynamicism'' — and yes, I do insist — ``there is always @racket[eval].''
@secref["How do I use templates “dynamically\"?" #:doc '(lib "web-server/scribblings/faq.scrbl")]

@defproc[
(render-to-file
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]
[output-path (or/c #f complete-path?) #f]) 
void?]
Like @racket[render], but saves the file to @racket[_output-path], overwriting whatever is already there. If no @racket[_output-path] is provided, it's derived from @racket[_source-path] using @racket[->output-path].

@defproc[
(render-to-file-if-needed
[source-or-output-path complete-path?]
[template-path (or/c #f complete-path?) #f]
[output-path (or/c #f complete-path?) #f]) 
void?]
Like @racket[render-to-file], with two differences. First, the mandatory argument can be either a source or an output path, and the function will compute the other. Second, the render only happens if one of these conditions exist:
@itemlist[#:style 'ordered
@item{No file exists at @racket[_output-path]. (Corollary: an easy way to force a refresh of a particular @racket[_output-path] is to delete it.)}

@item{Either @racket[_source-path] or @racket[_template-path] have changed since the last trip through @racket[render].}

@item{One or more of the project requires have changed.}]

If none of these conditions exist, @racket[_output-path] is assumed to be current, and the render is skipped.




@defproc[
(render-batch
[source-paths (listof pathish?)] ...) 
void?]
Render multiple @racket[_source-paths] in one go. This can be faster than @racket[(map render _source-paths)] if your @racket[_source-paths] rely on a common set of templates. Templates may have their own Pollen source files that need to be compiled. If you use @racket[render], the templates will be repeatedly (and needlessly) re-compiled. Whereas if you use @racket[render-batch], each template will only be compiled once.


@defproc[
(get-template-for
[source-path complete-path?])
(or/c #f complete-path?)]
Find a template file for @racket[_source-path], with the following priority:

If the metas for @racket[_source-path] have a key for @code[(format "'~a" world:template-meta-key)], then use the value of this key. 

If this key doesn't exist, or if it points to a nonexistent file, look for a default template in the project directory with the name @code[(format "~a.[output extension].~a" world:default-template-prefix world:template-source-ext)]. Meaning, if @racket[_source-path] is @code[(format "intro.html.~a" world:markup-source-ext)], the default template would be @code[(format "~a.html.~a" world:default-template-prefix world:template-source-ext)].

If this file doesn't exist, use the fallback template.
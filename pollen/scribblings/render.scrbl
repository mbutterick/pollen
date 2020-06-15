#lang scribble/manual

@(require scribble/eval pollen/render pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup pollen/core web-server/templates pollen/file sugar pollen/render))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen))

@title{Render}

@defmodule[pollen/render]

@italic{Rendering} is how Pollen source files get converted into output.

@defproc[
(render
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]) 
(or/c string? bytes?)]
Renders @racket[_source-path]. The rendering behavior depends on the type of source file (for details, see @secref["File_formats" #:doc '(lib "pollen/scribblings/pollen.scrbl")]):

A @racketmodname[pollen/pre] file is rendered without a template.

A @racketmodname[pollen/markup] or @racketmodname[pollen/markdown] file is rendered with a template. If no template is specified with @racket[_template-path], Pollen tries to find one using @racket[get-template-for].

Be aware that rendering with a template uses @racket[include-template] within @racket[eval]. For complex pages, it can be slow the first time. Caching is used to make subsequent requests faster.

For those panicked at the use of @racket[eval], please don't be. As the author of @racket[include-template] has already advised, ``If you insist on dynamicism'' — and yes, I do insist — ``@link["http://docs.racket-lang.org/web-server/faq.html#%28part._.How_do_.I_use_templates__dynamically__%29"]{there is always @racket[eval].}''

@defproc[
(render-to-file
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]
[output-path (or/c #f complete-path?) #f]) 
void?]
Like @racket[render], but saves the file to @racket[_output-path], overwriting whatever was already there. If no @racket[_output-path] is provided, it's derived from @racket[_source-path] using @racket[->output-path].

@defproc[
(render-to-file-if-needed
[source-path complete-path?]
[template-path (or/c #f complete-path?) #f]
[output-path (or/c #f complete-path?) #f]) 
void?]
Like @racket[render-to-file], but the render only happens if one of these conditions exist:
@itemlist[#:style 'ordered

@item{No file exists at @racket[_output-path]. (Thus, an easy way to force a render of a particular @racket[_output-path] is to delete it.)}

@item{Either @racket[_source-path], @racket[_template-path], or the associated @filepath["pollen.rkt"] has changed since the last trip through @racket[render].}

@item{The render cache is deactivated.}]

If none of these conditions exist, @racket[_output-path] is deemed to be up to date, and the render is skipped.




@defproc[
(render-batch
[source-path pathish?] ...) 
void?]
Render multiple @racket[_source-paths] in one go. This can be faster than @racket[(for-each render _source-paths)] if your @racket[_source-paths] rely on a common set of templates. Templates may have their own source files that need to be compiled. If you use @racket[render], the templates will be repeatedly (and needlessly) re-compiled. Whereas if you use @racket[render-batch], each template will only be compiled once.

@defproc[
(render-pagenodes 
[pt-or-pt-source (or/c pathish? pagetree?)]) 
void?]
Using @racket[_pt-or-pt-source], render the pagenodes in that pagetree using @racket[render-batch].

Note that @racket[_pt-or-pt-source] is used strictly as a list of files to render, like a batch file. It is not used as the navigational pagetree for the rendered files.

@defproc[
(get-template-for
[source-path complete-path?])
(or/c #f complete-path?)]
Find a template file for @racket[_source-path], with the following priority:
@itemlist[#:style 'ordered

@item{If the @racket[metas] for @racket[_source-path] have a key for @code[(format "~a" pollen-template-meta-key)], then use the value of this key, e.g. —

@code{◊(define-meta template "my-template.html")}

If your project has @seclink["fourth-tutorial"]{multiple output targets}, you can supply a list of templates, and the template with an extension matching the current output target will be selected automatically, e.g. —

@code{◊(define-meta template (list "my-template.html" "my-template.txt" "my-template.pdf"))}




}

@item{If this key doesn't exist, or refers to a nonexistent file, look for a default template with the name @code[(format "~a.[output extension]" pollen-template-prefix)]. Meaning, if @racket[_source-path] is @code[(format "intro.html.~a" pollen-markup-source-ext)], the output path would be @code["intro.html"], so the default template would be @code[(format "~a.html" pollen-template-prefix)]. Look for this default template in the same directory as the source file, and then search upwards within successive parent directories. (Corollary: a default template in the project root will apply to all files in the project unless overridden within a subdirectory.)}

@item{If this file doesn't exist, use the fallback template as a last resort. (See @secref["Templates"
         #:tag-prefixes '("tutorial-2")
         #:doc '(lib "pollen/scribblings/pollen.scrbl")].)}
]

This function is called when a template is needed, but a @racket[_template-path] argument is missing (for instance, in @racket[render] or @racket[render-to-file]).
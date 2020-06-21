#lang scribble/manual

@(require scribble/eval pollen/render "mb-tools.rkt" pollen/setup (for-label pollen/core pollen/cache (except-in pollen/top #%top def/c) racket (except-in pollen #%module-begin) pollen/setup sugar pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title{File formats}


@section{Source formats}

@defmodulelang*[(pollen/pre pollen/markdown pollen/markup pollen/ptree)]


The Pollen language is divided into @italic{dialects} that are tailored to suit each of the core source formats.

These dialects can be invoked one of two ways: either by invoking a specific dialect in the first line of the file (also known as the @tt{#lang} line), or by using the generic @tt{#lang pollen} as the first line, and then the correct dialect will be automatically selected based on the source file extension.

If the @tt{#lang} line specifies a dialect different from the one implied by the file extension, the @tt{#lang} line will take precedence. 


For ease of use, the behavior of the Pollen language departs from the standard Racket language in several ways. The differences are noted below.

@subsection{Command syntax using ◊}

Commands must start with the special lozenge character @litchar{◊}. Other material is interpreted as plain text. See @secref["pollen-command-syntax"] for more.

You can change the command character for a project by overriding @racket[pollen-command-char].

@bold{How is this different from Racket?} In Racket, everything is a command, and plain text must be quoted.


@subsection{Any command is valid}

There are no undefined commands in Pollen. If a command has not already been defined, it's treated as a tag function. See @secref["pollen-command-syntax"] and @racketmodname[pollen/top] for more.

@bold{How is this different from Racket?} In Racket, if you try to treat an identifier as a function before defining it with @racket[define], you'll get an error.

@subsection{Standard exports}

By default, every Pollen source file exports two identifiers:

@defthing[doc xexpr?]{
Contains the output of the file. The type of output depends on the source format (about which, more below).}

@defthing[metas hasheq?]{
A table of key–value pairs with extra information that is extracted from the source. These @racket[metas] will always contain the key @racket['#,pollen-here-path-key], which returns a string representation of the full path to the source file. Beyond that, the only @racket[metas] are the ones that are specified within the source file (see the source formats below for more detail on how to specify metas).}

As usual, you can use @racket[require], @racket[local-require], or @racket[dynamic-require] to retrieve these values. But within a Pollen project, the faster way is to use @racket[get-doc] and @racket[get-metas].

Pollen source files also make the @racket[metas] hashtable available through a submodule, unsurprisingly called @racket[metas]. So rather than importing a source file with @racket[(require "source.html.pm")], you can @racket[(require (submod "source.html.pm" metas))]. Accessing the metas this way avoids fully compiling the source file, and thus will usually be faster.

@margin-note{The Pollen rendering system relies on these two exported identifiers, but otherwise doesn't care how they're generated. Thus, the code inside your Pollen source file could be written in @tt{#lang racket} or @tt{#lang whatever}. As long as you @racket[provide] those two identifiers and follow Pollen's file-naming conventions, your source file will be renderable.}

@bold{How is this different from Racket?} In Racket, you must explicitly @racket[define] and then @racket[provide] any values you want to export.

@subsection{Custom exports}

Any value or function that is defined within the source file using @racket[define] is automatically exported.

@bold{How is this different from Racket?} In Racket, you must explicitly @racket[provide] any values you want to export. Unlike Racket, every Pollen source file impliedly uses @racket[(provide (all-defined-out))].


@subsection{The @filepath{pollen.rkt} file}

If a file called @filepath{pollen.rkt} exists in the same directory with a source file, or in a parent directory of that source file, it's automatically imported when the source file is compiled.

@bold{How is this different from Racket?} In Racket, you must explicitly import files using @racket[require].

@subsection{Preprocessor (@(format ".~a" pollen-preproc-source-ext) extension)}

Invoke the preprocessor dialect by using @code{#lang pollen/pre} as the first line of your source file, or by using @code{#lang pollen} with a file extension of @code{@(format ".~a" pollen-preproc-source-ext)}. These forms are equivalent:


@racketmod[#:file "sample.css.pp" pollen
_...source...
]

@racketmod[#:file "sample.css" pollen/pre
_...source...
]

When no dialect is explicitly specified by either the @tt{#lang} line or the file extension, Pollen will default to using the preprocessor dialect. For instance, this file will be treated as preprocessor source:

@racketmod[#:file "test.yyz" pollen
_...source...
]

Of course, you're better off specifying the preprocessor dialect explicitly rather than relying on this default behavior.

The output of the preprocessor dialect, provided by @racket[doc], is plain text. For this reason, the preprocessor will convert everything it finds to text in the least surprising way possible.



@subsection{Markdown (@(format ".~a" pollen-markdown-source-ext) extension)}

Invoke the Markdown dialect by using @code{#lang pollen/markdown} as the first line of your source file, or by using @code{#lang pollen} with a file extension of @code{@(format ".~a" pollen-markdown-source-ext)}. These forms are equivalent:


@racketmod[#:file "sample.txt.pmd" pollen
_...source...
]

@racketmod[#:file "sample.txt" pollen/markdown
_...source...
]

The output of the Markdown dialect, provided by @racket[doc], is a tagged X-expression.


@subsection{Markup (@(format ".~a" pollen-markup-source-ext) extension)}

Invoke the Pollen markup dialect by using @code{#lang pollen/markup} as the first line of your source file, or by using @code{#lang pollen} with a file extension of @code{@(format ".~a" pollen-markup-source-ext)}. These forms are equivalent:


@racketmod[#:file "about.html.pm" pollen
_...source...
]

@racketmod[#:file "about.html" pollen/markup
_...source...
]

The output of the Pollen markup dialect, provided by @racket[doc], is a tagged X-expression.

@subsection{Pagetree  (@(format ".~a" pollen-pagetree-source-ext) extension)}


Invoke the pagetree dialect by using @code{#lang pollen/ptree} as the first line of your source file, or by using @code{#lang pollen} with a file extension of @code{@(format ".~a" pollen-pagetree-source-ext)}. These forms are equivalent:


@racketmod[#:file "main.ptree" pollen
_...source...
]

@racketmod[#:file "main.rkt" pollen/ptree
_...source...
]



The output of the pagetree dialect, provided by @racket[doc], is a @racket[pagetree?] that is checked for correctness using @racket[validate-pagetree].




@section{Utility formats}

These aren't source formats because they don't contain a @tt{#lang pollen} line. But for convenience, they get special handling by the Pollen project server.



@subsection{Scribble  (@(format ".~a" pollen-scribble-source-ext) extension)}

Scribble files are recognized by the project server and can be compiled and previewed in single-page mode.


@subsection{Null (@(format ".~a" pollen-null-source-ext) extension)}

Files with the null extension are simply rendered as a copy of the file without the extension, so @filepath{index.html.p} becomes @filepath{index.html}. 

This can be useful if you're managing your project with Git. Most likely you'll want to ignore @filepath{*.html} and other file types that are frequently regenerated by the project server. But if you have isolated static files — for instance, a @filepath{index.html} that doesn't have source associated with it — they'll be ignored too. You can cure this problem by appending the null extension to these static files, so they'll be tracked in your source system without actually being source files. 

The null extension is also useful for templates — @filepath{template.html} and @filepath{template.html.p} will work the same way.



@section{Escaping output-file extensions within source-file names}

Pollen relies extensively on the convention of naming source files by adding a source extension to an output-file name. So the Pollen markup source for @filepath{index.html} would be @filepath{index.html.pm}. 

This convention occasionally flummoxes other programs that assume a file can only have one extension. If you run into such a situation, you can  @italic{escape} the output-file extension with the underscore character @litchar{_}.

So instead of @filepath{index.html.pm}, your source-file name would be @filepath{index_html.pm}. When this source file is rendered, it will automatically be converted into @filepath{index.html} (meaning, the escaped extension will be converted into a normal file extension).

This alternative-naming scheme is automatically enabled in every project. You can also set the escape character on a per-project basis (by overriding @racket[defaul-extension-escape-char]). Pollen will let you choose any character, but of course it would be unwise to pick one with special meaning in your filesystem (for instance, @litchar{/}).

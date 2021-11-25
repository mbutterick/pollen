#lang scribble/manual
@(require pollen/private/version
(for-label pollen/cache pollen/core pollen/template pollen/decode pollen/unstable/typography pollen/setup pollen/template/html))

@title[#:tag "version-notes"]{Version notes (@|pollen:version|)}

@section{What the version number means}

Consistent with Racket's @seclink["Package_Concepts" #:doc '(lib "pkg/scribblings/pkg.scrbl")]{version-numbering system}, the first digit reflects major updates to Pollen that break backward compatibility. The second digit reflects feature updates that don't affect existing features.

Inconsistent with this system, Pollen's version also appends a build number, which is the age of the software in days and minutes. (The official version reported in Pollen's @filepath{info.rkt} is just the major + minor digits.)

@section{Source code}

Pollen's source code is @link["http://github.com/mbutterick/pollen"]{available from this Git repo}. The @tt{MASTER} branch of the repo will always contain the most recent stable version. 

Racket's @link["http://pkg.racket-lang.org"]{package catalog} relies on this branch, so if you get your updates with @tt{raco pkg update pollen}, you'll get the most recent updates from this branch. 

I will add Git tags to commits where the major or minor version changed, so if you need to rebuild an earlier version, you can.

@section{Development policy}

Beyond keeping the commit history available, I make no promise to maintain the public interface in its current state. I will avoid unnecessary upheaval, of course. But my goal is to make the system more capable and stable, not to permanently burden it with my current level of ignorance.

@section{Changelog}

@subsection{Version 3.2}

Added @racket[setup:external-renderer].

@subsection{Version 3.1}

Downgraded the following @racket[pollen/setup] values from configurable to fixed: @racket[here-path-key], @racket[extension-escape-char].

@subsection{Version 3.0}

Changed rendering model to share a namespace between sequential renders, improving speed.
 
Added @racket[--force] switch to @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

Added @racket[--dry-run] switch to @secref["raco_pollen_publish" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

Downgraded the following @racket[pollen/setup] values from configurable to fixed: @racket[splicing-tag], @racket[preproc-source-ext], @racket[markup-source-ext], @racket[markdown-source-ext], @racket[null-source-ext], @racket[pagetree-source-ext], @racket[template-source-ext], @racket[scribble-source-ext], @racket[poly-source-ext], @racket[cache-dir-name], @racket[cache-subdir-name], @racket[template-prefix], @racket[fallback-template-prefix], @racket[template-meta-key], @racket[main-export], @racket[meta-export], @racket[meta-tag-name], @racket[define-meta-name].


@subsection{Version 2.2}

Added @racket[--null] and @racket[--dry-run] switches to @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

Extended the @racket[define-meta] form to allow multiple key–value pairs.

Changed handling of @racket[current-metas] so that values can be updated by tag functions during the evaluation of a source file.

Switched to MIT license.

@subsection{Version 2.1}

Added @racket[setup:envvar-watchlist].

@seclink["raco-pollen" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{@racketfont{raco pollen}}: Introduced support for parallel processing by adding @racket[--parallel] and @racket[--jobs] options to @secref["raco_pollen_setup" #:doc '(lib "pollen/scribblings/pollen.scrbl")] and @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")].


@subsection{Version 2.0}

Dropped support for Racket versions earlier than 6.3.

Added @racket[setup:allow-unbound-ids?].


@subsection{Version 1.5}

Added @racket[setup:trim-whitespace?].


@subsection{Version 1.4}

Added @racket[setup:cache-watchlist], @racket[for/splice], @racket[for*/splice], @racket[current-metas].


@subsection{Version 1.3} 

Various optimizations and bugfixes.


@subsection{Version 1.2} 

@seclink["raco-pollen" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{@racketfont{raco pollen}}: Added @racket[--local] option to @secref["raco_pollen_start" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

Various optimizations and bugfixes.

@subsection{Version 1.1} 

@bold{New features}

@seclink["raco-pollen" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{@racketfont{raco pollen}}: Added @racket[--recursive] option to @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")].

@racketmodname[pollen/setup]: @racket[setup:omitted-path?] replaces @racket[setup:unpublished-path?], and @racket[setup:extra-path?] replaces @racket[setup:extra-published-path?]. These settings are used during both render and publish operations. (The old names will still work, however.)

Added @racket[setup:publish-directory], which can be used to set a default publish directory.



@subsection{Version 1.0} 

@bold{New features}

@racketmodname[pollen/cache]: @racket[cached-doc] and @racket[cached-metas] are the preferred way to retrieve data from a Pollen source file.

The @racketmodname[pollen/core] module has been added, the new home for functions that once lived in @racketmodname[pollen/template]. Also new splicing tag @racket[\@] and @racket[when/splice] macro.

@racketmodname[pollen/setup] replaces @racketfont{pollen/world}. @racketmodname[pollen/setup] values can be overridden with a @racket[setup] submodule. Parameter names are no longer prefixed.

@bold{Backward incompatibilities}

@racketmodname[pollen/cache]: @racketfont{cached-require} is deprecated. Use @racket[cached-doc] or @racket[cached-metas].

@racketmodname[pollen/template]: functions like @racket[get-doc], @racket[get-metas], and the @racket[select] functions have been moved to @racketmodname[pollen/core]. @racketfont{when/block} is deprecated in favor of @racket[when/splice].

@racketmodname[pollen/template]: @racketfont{detect-paragraphs} and @racketfont{detect-linebreaks} have been renamed @racket[decode-paragraphs] and @racket[decode-linebreaks]. Typography functions like @racket[smart-quotes] and @racket[smart-dashes] have been moved to @racketmodname[pollen/unstable/typography]. @racket[register-block-tags] is gone — override @racket[default-block-tags] instead.

@racketmodname[pollen/file]: refined to a more carefully curated set of functions.

@racketmodname[pollen/template]: Pollen now supports any output formats, so this module has been broken into format-specific versions. For now, the only submodule is @racketmodname[pollen/template/html], the home of @racket[->html].

@racketfont{pollen/world}: superseded by @racketmodname[pollen/setup] (see above). The @racket[config] submodule in a @filepath{pollen.rkt} will no longer work.


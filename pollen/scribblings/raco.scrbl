#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/setup (for-label (except-in racket ...) pollen/setup))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title[#:tag "raco-pollen"]{Using @exec{raco pollen}}


Racket provides centralized command-line options through @exec{raco} (short for @exec{racket command}, see @other-doc['(lib "scribblings/raco/raco.scrbl")]). 

Once you install Pollen, you can access the following Pollen-specific commands through @racket[raco] using the subcommand @exec{raco pollen}.

@section{Making sure @exec{raco pollen} works}

Open a terminal window and type:

@terminal{
> raco pollen test}

If @exec{raco pollen} is installed correctly, you'll see:

@terminal{raco pollen is installed correctly}

But if you get:

@terminal{raco: Unrecognized command: pollen}

You'll need to fix the problem before proceeding, most likely by reinstalling Pollen (see @secref["Installation" #:doc '(lib "pollen/scribblings/pollen.scrbl")]).

If your error is like this:

@terminal{Unrecognized command: raco}

You have a deeper problem with your Racket installation (often a misconfiguration of @code{PATH}).

@section{@exec{raco pollen}}

Same as @exec{raco pollen help}.

@section{@exec{raco pollen help}}

Displays a list of available commands.


@section{@exec{raco pollen start}}

Start the project server from the current directory using the default port, which is the value of the parameter @racket[current-server-port] (by default, port @id[default-project-server-port]).

This command can be invoked with two optional arguments.

@racket[raco pollen start _path] will start the project server from @racket[_path] rather than the current directory (making @racket[_path] its root directory).

@terminal{
> raco pollen start ~/path/to/project/}

@racket[raco pollen start _path _port] will start the project server in @racket[_path] using @racket[_port] rather than @racket[current-server-port]. This is useful if you want to have multiple project servers running simultaneously.

@terminal{
> raco pollen start ~/path/to/project/
> raco pollen start ~/path/to/project/scribblings 8088}

If you want to start in the current directory but with a different port, use @litchar{.} as the path:

@terminal{
> raco pollen start . 8088}

@section{@exec{raco pollen render}}

Render all preprocessor source files and then all pagetree files found in the current directory. If none of these files are found, a pagetree will be generated for the directory (which will include all source files) and then rendered.

This command can be invoked with extra arguments.

@racket[raco pollen render _directory] will perform the render described above, but in the specified directory. By default, only files in the immediate directory are rendered. 

Adding the optional @exec{-r} or @exec{--recursive} switch will also render subdirectories recursively. Certain directories are automatically omitted from recursive renders, including Pollen caches and source-control directories (like @tt{.git} and @tt{.svn}). You can omit other paths by overriding @racket[default-omitted-path?]. You can override these omissions — that is, force a path to be included in a recursive render — by overriding @racket[default-extra-path?].

Alternatively, the command can take a variable number of path arguments. @racket[raco pollen render _path ...] will render only the paths specified in @racket[_path ...]. Consistent with the usual command-line idiom, this can be a single path, a list of paths, or a pattern:

@terminal{
> raco pollen render foo.html.pm
> raco pollen render foo.html.pm bar.html.pm zam.css.pp
> raco pollen render *.html.pm}

Paths can also be specified as output rather than input paths, and the corresponding source paths will be discovered:

@terminal{
> raco pollen render foo.html
> raco pollen render foo.html bar.html zam.css}

The optional @exec{-t} or @exec{--target} switch specifies the render target for multi-output source files. If the target is omitted, the renderer will use whatever target appears first in @racket[(setup:poly-targets)].


@terminal{
> raco pollen render -t pdf foo.poly.pm}

See also @seclink["raco-pollen-render-poly"].

@bold{Warning}: In all cases, the newly rendered output file will overwrite any previous output file.

@section{@exec{raco pollen publish}}

Make a copy of the project directory on the desktop, but without any source files or other Pollen-related files. (This function is pretty lame, and I invite suggestions for improvement.)

@racket[raco pollen publish _project-dir] will publish the project in @racket[_project-dir] onto the desktop in a folder called @racket[publish]. @bold{Warning}: if @racket[publish] already exists on the desktop, it will be overwritten.

@racket[raco pollen publish _project-dir _dest-dir] will publish the project in @racket[_project-dir] to @racket[_dest-dir] rather than the desktop. @bold{Warning}: if @racket[_dest-dir] already exists, it will be overwritten by the newly published directory.

If you're already in your project directory and want to publish somewhere other than the desktop, use @racket[raco pollen publish _. _dest-dir].

By default, this command will automatically overwrite the destination directory. Adding the optional @exec{-c} or @exec{--confirm} switch will ask for confirmation if the destination already exists.

You can determine the default publishing destination for a project by overriding @racket[default-publish-directory].

Certain files and directories are automatically omitted from the published directory, including Racket and Pollen sources, Pollen caches, and source-control directories (like @tt{.git} and @tt{.svn}). You can omit other files by overriding @racket[default-omitted-path?]. You can override these omissions — that is, force a path to be published — by overriding @racket[default-extra-path?].


@section{@exec{raco pollen setup}}

Finds Pollen source files in the current directory, compiles them, and loads the results into the @seclink["Cache" #:doc '(lib "pollen/scribblings/pollen.scrbl")]. This will give you the snappiest performance during an interactive session with the project server. 

Can also be invoked as @racket[raco pollen setup _directory], which will set up a different project @racket[_directory].


@section{@exec{raco pollen reset}}

Resets Pollen's @seclink["Cache" #:doc '(lib "pollen/scribblings/pollen.scrbl")] by deleting the cache directories, including @tt{compiled} directories created by Racket. Use this when you need a fresh start in life.

Can also be invoked as @racket[raco pollen reset _directory], which will reset a different project @racket[_directory].

@section{@exec{raco pollen version}}

Would you believe this prints the Pollen version number.

@section{The @exec{POLLEN} environment variable}

Pollen recognizes a @exec{POLLEN} environment variable on the command line, which can be used to pass through any value you like. This value can be used within your project files with @racket[(getenv "POLLEN")], which if not set, returns @racket[#f]. Take this file, for instance:

@fileblock["test.txt.pp" @codeblock{
#lang pollen
Result is ◊or[(getenv "POLLEN")]{nothing}
}]

The @exec{POLLEN} environment variable will change how it's rendered:

@terminal{
> raco pollen render test.txt ; cat test.txt
rendering test.txt.pp
rendering: /test.txt.pp as /test.txt
Result is nothing

> POLLEN=DEBUG raco pollen render test.txt ; cat test.txt
rendering test.txt.pp
rendering: /test.txt.pp as /test.txt
Result is DEBUG

}



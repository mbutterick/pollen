#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/world (for-label (except-in racket ...) pollen/world))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title[#:tag "raco-pollen"]{Using @exec{raco pollen}}


Racket provides centralized command-line options through @exec{raco} (short for @exec{racket command}, see @other-doc['(lib "scribblings/raco/raco.scrbl")]). 

Once you install Pollen, you can access the following Pollen-specific commands through @racket[raco] using the subcommand @exec{raco pollen}.

@section{Making sure @racket[raco pollen] works}

Open a terminal window and type:

@terminal{
> raco pollen test}

If @racket[raco pollen] is installed correctly, you'll see:

@terminal{raco pollen is installed correctly}

But if you get:

@terminal{raco: Unrecognized command: pollen}

You'll need to fix the problem before proceeding, most likely by reinstalling Pollen (see @secref["Installation" #:doc '(lib "pollen/scribblings/pollen.scrbl")]).

If your error is like this:

@terminal{Unrecognized command: raco}

You have a deeper problem with your Racket installation (often a misconfiguration of @code{PATH}).

@section{@racket[raco pollen]}

Same as @racket[raco pollen help].

@section{@racket[raco pollen help]}

Displays a list of available commands.


@section{@racket[raco pollen start]}

Start the project server from the current directory using the default port, which is the value of the parameter @racket[world:current-server-port] (by default, port @(format "~a" world:default-port)).

This command can be invoked with two optional arguments.

@racket[raco pollen start _path] will start the project server in @racket[_path] rather than the current directory.

@terminal{
> raco pollen start ~/path/to/project/}

@racket[raco pollen start _path _port] will start the project server in @racket[_path] using @racket[_port] rather than @racket[world:current-server-port]. This is useful if you want to have multiple project servers running simultaneously.

@terminal{
> raco pollen start ~/path/to/project/
> raco pollen start ~/path/to/project/scribblings 8088}

If you want to start in the current directory but with a different port, use @litchar{.} as the path:

@terminal{
> raco pollen start . 8088}

@section{@racket[raco pollen render]}

Render all preprocessor source files and then all pagetree files found in the current directory. If none of these files are found, a pagetree will be generated for the directory (which will include all source files) and then rendered.

This command can be invoked with extra arguments.

@racket[raco pollen render _directory] will perform the render described above, but in the specified directory.

Alternatively, the command can take a variable number of path arguments. @racket[raco pollen render _path ...] will render only the paths specified in @racket[_path ...]. Consistent with the usual command-line idiom, this can be a single path, a list of paths, or a pattern:

@terminal{
> raco pollen render foo.html.pm
> raco pollen render foo.html.pm bar.html.pm zam.css.pp
> raco pollen render *.html.pm}


@section{@racket[raco pollen publish]}

Make a copy of the project directory on the desktop, but without any source files or other Pollen-related files. (This function is pretty lame, and I invite suggestions for improvement.)

@racket[raco pollen publish _project-dir] will publish the project in @racket[_project-dir] onto the desktop in a folder called @racket[publish]. @bold{Warning}: if @racket[publish] already exists on the desktop, it will be overwritten.

@racket[raco pollen publish _project-dir _dest-dir] will publish the project in @racket[_project-dir] to @racket[_dest-dir] rather than the desktop. @bold{Warning}: if @racket[_dest-dir] already exists, it will be overwritten by the newly published directory.

If you're already in your project directory and want to publish somewhere other than the desktop, use @racket[raco pollen publish _. _dest-dir].





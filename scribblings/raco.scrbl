#lang scribble/manual

@(require scribble/eval pollen/world (for-label racket pollen/world))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))


@title[#:tag "raco-pollen"]{Using @exec{raco pollen}}


Racket provides centralized command-line options through @racket[raco] (short for @code{racket command}, see @other-doc['(lib "scribblings/raco/raco.scrbl")]). 

Once you install Pollen, you can access the following Pollen-specific commands through @racket[raco] using the subcommand @exec{raco pollen}.

@section{Making sure @racket[raco pollen] works}

Open a terminal window and type:

@verbatim{
> raco pollen test}

If @racket[raco pollen] is installed correctly, you'll see:

@verbatim{raco pollen is installed correctly}

But if you get:

@verbatim{raco: Unrecognized command: pollen}

You'll need to fix the problem before proceeding, most likely by reinstalling Pollen (see @racket[Installation]).

@margin-note{Pro tip: I have an alias in my @racketfont{.bash_profile} like so: @racketfont{alias polcom=@literal{'}raco pollen@literal{'}}}

@section{@racket[raco pollen]}

Same as @racket[raco pollen help].

@section{@racket[raco pollen help]}

Displays a list of available commands.


@section{@racket[raco pollen start]}

Starts the project server from the current directory using the default port, which is the value of the parameter @racket[world:current-server-port] (by default, port @(format "~a" world:default-port)).

This command can be invoked with two optional arguments.

@racket[raco pollen start _path] will start the project server in @racket[_path] rather than the current directory.

@verbatim{
> raco pollen start ~/path/to/project/}

@racket[raco pollen start _path _port] will start the project server in @racket[_path] using @racket[_port] rather than @racket[world:current-server-port]. This is useful if you want to have multiple project servers running simultaneously.

@verbatim{
> raco pollen start ~/path/to/project/
> raco pollen start ~/path/to/project/scribblings 8088}

If you want to start in the current directory but with a different port, use @litchar{.} as the path:

@verbatim{
> raco pollen start . 8088}

@section{@racket[raco pollen render]}

Renders all preprocessor source files and then all pagetree files found in the current directory.

This command can be invoked with extra arguments.

@racket[raco pollen render _directory] will render the preprocessor source files and pagetree files in the specified directory.

Alternatively, the command can take a variable number of path arguments. @racket[raco pollen render _path...] will render only the paths specified in @racket[_path...]. Consistent with the usual command-line idiom, this can be a single path, a list of paths, or a pattern:

@verbatim{
> raco pollen render foo.html.pm
> raco pollen render foo.html.pm bar.html.pm zam.css.pp
> raco pollen render *.html.pm}


@section{@racket[raco pollen clone]}

Makes a copy of the project directory on the desktop, and removes any source files or other Pollen-related files.

@racket[raco pollen clobe _directory-path] will perform the same copying and filtering, but using @racket[_directory-path] as the destination rather than the desktop.





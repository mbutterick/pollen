#lang scribble/manual

@(require scribble/eval pollen/render pollen/setup pollen/private/version "mb-tools.rkt" (for-label racket (except-in pollen #%module-begin) pollen/setup sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))

@(define (short-version) (car (regexp-match #px"^\\d.\\d" (version))))

@title{Installation}

@section{Prerequisites}

Pollen will run on Mac OS, Linux, or Windows.

Pollen is not a self-contained GUI program like Adobe InDesign. It's a software package that runs atop the Racket language environment (also a free download).

Your three main tools in Pollen will be a text editor (for those starting out, I recommend @other-doc['(lib "scribblings/drracket/drracket.scrbl")]), a terminal window, and a web browser. The terminal commands you'll be using are simple, but if you haven't used your terminal window before, this is the moment to learn where it is. (On Mac OS, your terminal window is called Terminal; on Windows it's called the Windows Command Processor.)

After the initial download, Pollen does not require a network connection.

@section{How to install}

@itemlist[

@item{@link["http://download.racket-lang.org/"]{Download and install Racket}, which includes DrRacket. (Of course, you're welcome to use your preferred text editor, but the tutorials will assume you're using DrRacket.)}

@item{Linux and OS X users: update your system @envvar{PATH} to include @filepath{/path-to-racket-installation/bin/}. Then, from the terminal, you'll be able to run @exec{racket} and @exec{raco} (see @other-doc['(lib "scribblings/raco/raco.scrbl")]).}


@item{OS X users who haven't altered your @envvar{PATH} before: don't panic. You need to add the full path to Racket's @racket[bin] directory to the @filepath{/etc/paths} file on your system. You can do this with the following terminal command (which will ask you for your password):

@terminal{sudo sh -c 'echo "/Applications/Racket v@(short-version)/bin" >> /etc/paths'}

Of course, @filepath{/Applications/Racket v@(short-version)/bin} should be changed if necessary to represent the actual location and version of your Racket installation.}


@item{To check that you did it correctly, try typing @exec{racket} on your command line, and you should see something like this:

@terminal{~ : racket
Welcome to Racket v.@(version).
>
}

Type @exec{ctrl+D} to exit.

But if you get an error like this:

@terminal{Unrecognized command: racket}

You have a deeper problem with your Racket installation that needs adjustment before continuing (usually a misconfiguration of @code{PATH}).

}


@item{Then, from the command line, install Pollen:
@commandline{raco pkg install pollen}

To check that it worked, try typing @exec{raco pollen test} on the command line, and you should see this:

@terminal{~ : raco pollen test
raco pollen is installed correctly
~ :
}

But if you get:

@terminal{raco: Unrecognized command: pollen}

You'll need to fix the problem before proceeding, most likely by reinstalling Pollen.


@margin-note{Windows users, when you see instructions that reference @exec{raco}, I'll trust you to convert into the appropriate command for your system. Assuming defaults, it's likely to be @filepath{C:\Program Files\Racket\raco} (include the surrounding quotes in the command).}


}

@item{Alternatively, you can install Pollen from inside DrRacket with the @menuitem["File" "Install Package ..."] command, and then relaunching DrRacket.}

@item{Either way, Pollen's documentation will be automatically installed.}

@item{After that, you can also update the package from the command line:
@commandline{raco pkg update --update-deps pollen}

Updating is optional. Major updates may have backward-incompatible changes, so you might want to consult the current @secref["version-notes"] before plunging in. The documentation for the newest version of Pollen is @link["http://pkg-build.racket-lang.org/doc/pollen/"]{available online} and refreshed daily. 

}

]

@section{Beyond that}

Pollen doesn't install anything on your machine other than the Racket packages it relies on. It does not gather any information about you or your project. Your data belongs to you. I won't know that you're using Pollen unless you tell me.

Pollen's built-in @seclink["Using_the_project_server"
         #:doc '(lib "pollen/scribblings/pollen.scrbl")]{project web server} is a real web server, however. Be mindful if you're using it on a machine visible on a public network. 

         This project server is primarily a development & previewing tool. You do not need it to deploy Pollen projects (which generally compile down to a set of static files).

In general, I subscribe to the view that software should let you do what you want, not enroll you in a nanny state. Pollen is, in part, a programming language. Like all programming languages, it will let you do things that are incredibly clever. And also miserably stupid. But that is how we learn.

I've been using Pollen daily for several years (and will continue to do so, because my main work is writing). I've made Pollen available because a) I'm certain that others have had the same frustrations that I have, and b) feature suggestions and bug reports make it more useful for everyone.

I hope you enjoy using it. If you get stuck on something not covered here, see @secref["Getting_more_help" #:doc '(lib "pollen/scribblings/pollen.scrbl")].


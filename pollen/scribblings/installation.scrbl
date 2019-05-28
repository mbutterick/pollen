#lang scribble/manual

@(require scribble/eval pollen/render pollen/setup pollen/private/version "mb-tools.rkt" (for-label racket (except-in pollen #%module-begin) pollen/setup sugar))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/file))

@(define (short-version) (car (regexp-match #px"^\\d+.\\d+" (version))))

@title{Installation}

@section{Prerequisites}

Pollen will run on Mac OS, Linux, or Windows.

Pollen is not a self-contained GUI program like Adobe InDesign. It's a software package that runs atop the Racket language environment (also a free download).

Your three main tools in Pollen will be a text editor (for those starting out, I recommend @other-doc['(lib "scribblings/drracket/drracket.scrbl")]), a terminal window, and a web browser. The terminal commands you'll be using are simple, but if you haven't used your terminal window before, this is the moment to learn where it is. (On Mac OS, your terminal window is called Terminal; on Windows it's called the Windows Command Processor.)

After the initial download, Pollen does not require a network connection.

@section{How to install}

@itemlist[

@item{@link["http://download.racket-lang.org/"]{Download and install Racket}, which includes DrRacket. (Of course, you're welcome to use your preferred text editor, but the tutorials will assume you're using DrRacket.)}

@item{Update the @envvar{PATH} envi­ron­ment vari­able on your system to include the direc­tory that holds the racket appli­ca­tion. On @bold{Mac OS} and @bold{Linux}, this path will be some­thing like @exec{"/path/to/racket/bin"}. On @bold{Windows}, it’ll be some­thing like @filepath{"C:\Program Files\Racket71\"}. Then, from the terminal, you’ll be able to run @exec{racket} and @exec{raco} (see @other-doc['(lib "scribblings/raco/raco.scrbl")]).}

@item{@bold{Windows} users who haven’t altered your @envvar{PATH} before: don’t panic. To add the Racket command-line programs to your Windows 10 @envvar{PATH}, click the Windows search box, type the word @exec{path}, and then click on @onscreen{Edit the system environment variables}. Click on the @onscreen{Environment Variables} button. In the top window, which contains your user variables, find @exec{Path} and double-click it to open. Click the @onscreen{New} button and either use the @onscreen{Browse} button to select your Racket directory, or manually enter its path. Restart your Windows terminal (either the Command Prompt or PowerShell) and now @exec{racket} and @exec{raco} should work.

Alternatively, follow @link["https://www.opentechguides.com/how-to/article/windows-10/113/windows-10-set-path.html"]{these instruc­tions}.}

@item{@bold{Mac OS} users who haven't altered your @envvar{PATH} before: don't panic. Follow @link["https://beautifulracket.com/setting-the-mac-os-path.html"]{these instructions}.}

@item{@bold{Linux}, @bold{Mac OS}, and @bold{Windows} users: try typing @exec{racket} on your command line, and you should see something like this:

@terminal{~ : racket
Welcome to Racket v.@(version).
>
}

If so, all is well. Type @exec{ctrl+D} to exit (or @exec{(exit)} on Windows).

But if you get an error like this:

@terminal{Unrecognized command: racket}

You have a deeper problem with your Racket installation that needs adjustment before continuing (usually a misconfiguration of @code{PATH}).

}


@item{Then install Pollen. Your first option is to install it using @exec{raco} on the command line:

@terminal{raco pkg install pollen}

To check that it worked, try typing @exec{raco pollen test} on the command line, and you should see this:

@terminal{~ : raco pollen test
raco pollen is installed correctly
~ :
}

But if you get:

@terminal{raco: Unrecognized command: pollen}

You'll need to fix the problem before proceeding, most likely by reinstalling Pollen.

}

@item{Your other option is to install Pollen from inside DrRacket. Use the menu command @menuitem["File" "Install Package ..."]. Type @exec{pollen} in the box and click @exec{Install}. When it's done, relaunch DrRacket.}

@item{Either way, Pollen's HTML documentation will be automatically installed. One way to reach the documentation:

@terminal{raco docs pollen}}

@item{After that, you can also update the package from the command line:
@terminal{raco pkg update --update-deps pollen}

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


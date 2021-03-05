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

@item{Update the @envvar{PATH} envi­ron­ment vari­able on your system to include the direc­tory that holds the racket appli­ca­tion. On @bold{Mac OS} and @bold{Linux}, this path will be some­thing like @filepath{/path/to/racket/bin}. On @bold{Windows}, it’ll be some­thing like @filepath{C:\Program Files\Racket}. Then, from the terminal, you’ll be able to run @exec{racket} and @exec{raco} (see @other-doc['(lib "scribblings/raco/raco.scrbl")]).}

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

I hope you enjoy using it.


@section{Getting more help}


@subsection{Bugs and feature requests}

Can be submitted as @link["https://github.com/mbutterick/pollen/issues"]{GitHub issues} at the main Pollen source repository.

@subsection{Questions & discussion}

For general tips and how-to questions, use the @link["https://github.com/mbutterick/pollen-users/issues"]{pollen-users discussion group} (on GitHub). I'll also use that list to post major changes and new features. 

You need a GitHub account to post. If you don't have one, don't panic — they're free and easy to set up with an email address. @link["https://github.com/mbutterick/pollen-users"]{Instructions here}.

(BTW, the former ``pollenpub'' Google Group is now deprecated.)



@subsection{Can I see the source for @italic{Practical Typography} or @italic{Typography for Lawyers}?}

Yes, a tutorial project based on the previous version of @link["http://typographyforlawyers.com/"]{@italic{Typography for Lawyers}} is available by installing the @link["https://docs.racket-lang.org/pollen-tfl/"]{pollen-tfl} package the same way you installed Pollen. 

The current versions of @italic{Practical Typography} & @italic{Typography for Lawyers} are generated from a single set of Pollen source files, which is a complication that makes them less suitable for an introductory tutorial. Still, even though this tutorial project is based on an earlier version, the coding techniques are very close to what I still use. Learn with confidence.


@subsection{Utilities & libraries}

@link["https://github.com/malcolmstill/pollen-count"]{pollen-count}: enumeration and cross-referencing library by Malcolm Still

@link["https://github.com/lijunsong/pollen-mode"]{pollen-mode}: Emacs mode for Pollen by Junsong Li

@link["https://github.com/basus/pollen-mode"]{Pollen mode}: Emacs mode for Pollen by Shrutarshi Basu

@link["https://docs.racket-lang.org/pollen-component/"]{Pollen Component}: Component-based development for Pollen by Leandro Facchinetti

@link["https://docs.racket-lang.org/css-expr/"]{CSS-expressions}: S-expression-based CSS by Leandro Facchinetti

@link["https://github.com/lijunsong/pollen-rock"]{Pollen Rock}: rendering server and an in-browser editor for Pollen

@link["https://github.com/appliedsciencestudio/talks/tree/master/mxnet"]{Polllen as a front end for Reveal.js} by Dave Liepmann. Reveal.js is a library that allows you to create slide presentations in pure HTML/CSS that run in the browser.


@subsection{More projects & guides}

@link["https://digitalwords.net"]{Digital Words} by Júda Ronén [@link["https://gitlab.com/rwmpelstilzchen/digitalwords.net"]{source}]

@link["https://thelocalyarn.com/excursus/secretary"]{Secretary of Foreign Relations} by Joel Dueck [@link["https://github.com/otherjoel/try-pollen"]{source}]

@link["https://github.com/fasiha/pollen-guide"]{A Poor Guide to Pollen} by Ahmed Fasih

@link["https://youtu.be/20GGVNBykaw"]{The World's Most Dangerous Racket Programmer} and @link["https://youtu.be/IMz09jYOgoc"]{Like a Blind Squirrel in a Ferrari}: short talks about Pollen that I gave at RacketCons 2013 and 2014, respectively.

@link["http://mstill.io"]{mstill.io blog} by Malcolm Still [@link["https://github.com/malcolmstill/mstill.io"]{source}]



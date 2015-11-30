# dotemacs

jidaikobo-shibata's dotemacs.

## Ussage

    cd ~/.emacs.d/
    git clone git@github.com:jidaikobo-shibata/dotemacs.git jidaikobo

add few lines to init.el:

    ;;; load jidaikobo/jidaikobo.init.el
    (when (file-exists-p "~/.emacs.d/jidaikobo/jidaikobo.init.el")
        (load "~/.emacs.d/jidaikobo/jidaikobo.init.el"))

## Installation global (gtags) and Packages from shell

Macintosh example:

    sudo port install global
    emacs --batch -l ~/.emacs.d/jdiaikobo/jidaikobo.init.el

## little bit useful

after load jidaikobo.init.el.

    (when (or noninteractive (is-once-in-a-day)) (package-refresh-contents))

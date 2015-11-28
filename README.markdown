# dotemacs

jidaikobo-shibata's dotemacs.

## Ussage

    cd ~/.emacs.d/
    git clone git@github.com:jidaikobo-shibata/dotemacs.git jidaikobo

add few lines to init.el:

    ;;; load jidaikobo/jidaikobo.init.el
    (when (file-exists-p "~/.emacs.d/jidaikobo/jidaikobo.init.el")
        (load "~/.emacs.d/jidaikobo/jidaikobo.init.el"))

## little bit useful

after load jidaikobo.init.el.

    (when (is-once-in-a-day) (package-refresh-contents))

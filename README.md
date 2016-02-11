# dotemacs

jidaikobo-shibata's dotemacs.

## Ussage

    cd ~/.emacs.d/
    git clone git@github.com:jidaikobo-shibata/dotemacs.git jidaikobo

or "Donwload Zip"

add few lines to init.el:

    (when (file-exists-p "~/.emacs.d/jidaikobo/jidaikobo.init.el")
    	(setq-default my-work-dir (expand-file-name "~/"))
    	(setq-default my-fetch-app-dir (expand-file-name "~/"))
    	(setq-default is-deactivate-region t)
    	(setq-default is-use-tabbar t)
    	(setq-default is-use-elscreen nil)
    	(load "~/.emacs.d/jidaikobo/jidaikobo.init.el"))

## Installation global (gtags) and Packages from shell

Macintosh example:

    sudo port install global
    emacs --batch -l ~/.emacs.d/jdiaikobo/jidaikobo.init.el

## little bit useful

after load jidaikobo.init.el.
or use [once-in-a-day](https://gist.github.com/jidaikobo-shibata/33e072cea6aa96a19f58).

    (when (or noninteractive (is-once-in-a-day)) (package-refresh-contents))

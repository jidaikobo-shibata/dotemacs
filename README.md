# dotemacs

jidaikobo-shibata's dotemacs.

## Ussage

```:terminal
cd ~/.emacs.d/
git clone git@github.com:jidaikobo-shibata/dotemacs.git jidaikobo
```
or "Donwload Zip"

add few lines to init.el:

```elisp:init.el
(when (file-exists-p "~/.emacs.d/jidaikobo/jidaikobo.init.el")
(load "~/.emacs.d/jidaikobo/jidaikobo.init.el"))
```
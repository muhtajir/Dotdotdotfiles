;; Generic base16 theme into which base16 environment vars have been inserted

(require 'base16-theme)

(defvar base16-generic-colors
  '(:base00 "#262626"
    :base01 "#3a3a3a"
    :base02 "#4e4e4e"
    :base03 "#8a8a8a"
    :base04 "#949494"
    :base05 "#dab997"
    :base06 "#d5c4a1"
    :base07 "#ebdbb2"
    :base08 "#d75f5f"
    :base09 "#ff8700"
    :base0A "#ffaf00"
    :base0B "#afaf00"
    :base0C "#85ad85"
    :base0D "#83adad"
    :base0E "#d485ad"
    :base0F "#d65d0e"))

(add-to-list 'default-frame-alist '(font . "mononoki-12"))

(deftheme base16-generic)
(base16-theme-define 'base16-generic base16-generic-colors)
(provide-theme 'base16-generic)

(provide 'init-base16-generic-theme)

;; vim: ft=lisp

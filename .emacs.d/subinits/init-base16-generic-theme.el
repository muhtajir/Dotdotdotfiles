;; Generic base16 theme into which base16 environment vars have been inserted

(require 'base16-theme)

(defvar base16-generic-colors
  '(:base00 "#1d2021"
    :base01 "#3c3836"
    :base02 "#504945"
    :base03 "#665c54"
    :base04 "#bdae93"
    :base05 "#d5c4a1"
    :base06 "#ebdbb2"
    :base07 "#fbf1c7"
    :base08 "#fb4934"
    :base09 "#fe8019"
    :base0A "#fabd2f"
    :base0B "#b8bb26"
    :base0C "#8ec07c"
    :base0D "#83a598"
    :base0E "#d3869b"
    :base0F "#d65d0e"))

(add-to-list 'default-frame-alist '(font . "mononoki-12"))

(deftheme base16-generic)
(base16-theme-define 'base16-generic base16-generic-colors)
(provide-theme 'base16-generic)

(provide 'init-base16-generic-theme)

(require 'package)

(setq packages (quote
                (slime
                 flycheck-clj-kondo
                 cider
                 clojure-mode
                 pug-mode
                 tabbar
                 doom-themes
                 doom-modeline
                 web-mode
                 auto-complete
                 jinja2-mode
                 dockerfile-mode
                 php-mode
                 magit
                 terraform-mode
                 coffee-mode
                 flycheck
                 tide
                 yaml-mode
                 all-the-icons
                 smart-mode-line
                 typescript-mode
                 neotree
                 dracula-theme
                 solarized-theme)))

(defun install-missing-packages (package-list)
  (package-refresh-contents)

  (dolist (package package-list)
    (when (not (require package nil 'noerror))
      (package-install package))))

(defun require-packages ()
  (require 'neotree)
  (require 'typescript-mode)
  (require 'smart-mode-line)
  (require 'yaml-mode)
  (require 'doom-modeline))

(defun setup-theme ()

  ;; setup dracula theme for both editor and emacs in general
  ;; now its solarized light
  (load-theme 'solarized-light t)
  (load-theme 'doom-solarized-light t)

  ;; setup editor font family as some Source Code Pro
  (set-default-font "Source Code Pro Regular")

  ;; setup themes for neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 30)
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config)

  ;; setup modeline
  (doom-modeline-mode 1))

(defun setup-editor-behavior ()
  (setq make-backup-files nil)
  (global-display-line-numbers-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-top-list '(2 4 6 8 10))
  (electric-indent-mode 0)
  (tool-bar-mode -1)

  (if (not (display-graphic-p))
    (xterm-mouse-mode)))

(setq tide-format-options
  '(:indentSize 2
    :tabSize 2
    :convertTabsToSpaces t))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (auto-complete-mode)
  (tide-hl-identifier-mode +1)
  (local-set-key [mouse-9] #'tide-jump-to-definition)
  (local-set-key [mouse-8] #'tide-jump-back))

(defun setup-key-bindings ()
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x |") #'split-window-right)
  (global-set-key (kbd "C-x -") #'split-window-below)
  (global-set-key (kbd "C-x C-1") 'neotree-toggle)
  (global-set-key (kbd "C-x C-d") 'neotree-dir))

(defun setup-hooks ()
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'server-switch-mode #'raise-frame))

(defun setup-slime ()
  (setq inferior-lisp-program (executable-find "sbcl")))

(defun setup-all ()
  (setup-key-bindings)
  (setup-hooks)
  (setup-theme)
  (setup-editor-behavior)

  (server-start))

(provide 'my-custom-things)

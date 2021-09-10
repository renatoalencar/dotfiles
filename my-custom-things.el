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
		 company
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
                 solarized-theme
                 tuareg
                 lsp-mode
                 lsp-ui
                 lsp-pyright
                 git-gutter
                 fuzzy-finder)))

(defun add-melpa-to-package-archives ()
  "Add melpa.org to package lists."

  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

(defun install-missing-packages (package-list)
  "Install packages from PACKAGE-LIST that aren't installed."

  (add-melpa-to-package-archives)
  (package-refresh-contents)

  (dolist (package package-list)
    (when (not (require package nil 'noerror))
      (package-install package))))

(defun require-packages ()
  "Require needed runtime packages."

  (require 'neotree)
  (require 'typescript-mode)
  (require 'smart-mode-line)
  (require 'yaml-mode)
  (require 'doom-modeline)
  (require 'flycheck-clj-kondo)
  (require 'lsp-mode))

(defun basename (name)
  "Get a file or directory name base path from NAME.

   Example:
   (basename \"/tmp/foo\")
   ;; \"/tmp/\"

   (basename \"/tmp/bar/\")
   ;; \"/tmp/\"
  "

  (let ((filename (if (directory-name-p name)
                   (directory-file-name name)
                   name)))
    (file-name-directory filename)))

(defun find-git-directory (directory)
  "Goes recursively upwards on the directory tree until it finds a
   .git directory and returns its basename."
  (cond ((string= directory "/") nil)
        ((file-exists-p (concat directory ".git")) directory)
        (t (find-git-directory (basename directory)))))

(defun setup-theme ()
  "Setup themes editor and neotree."

  ;; setup dracula theme for both editor and emacs in general
  ;; now its solarized light
  ;;(load-theme 'solarized-light t)
  (load-theme 'doom-gruvbox t)

  ;; setup editor font family as some Source Code Pro
  ;; disabled because it's giving and error on Gnome
  ;; (set-default-font "Source Code Pro")

  ;; setup themes for neotree
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-width 30)
  (setq doom-themes-neotree-file-icons t)
  (doom-themes-neotree-config)

  ;; setup modeline
  (doom-modeline-mode 1))

(defun setup-editor-behavior ()
  "Setup editor behavior,like identation and global modes."

  (setq make-backup-files nil)
  (global-display-line-numbers-mode)
  (setq-default indent-tabs-mode nil)
  (setq-default tab-top-list '(2 4 6 8 10))
  (electric-indent-mode 0)
  (tool-bar-mode -1)

  (global-flycheck-mode)
  (global-git-gutter-mode +1)

  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
  (company-mode +1)
  (tide-hl-identifier-mode +1)
  (local-set-key [mouse-9] #'tide-jump-to-definition)
  (local-set-key [mouse-8] #'tide-jump-back))

(defun setup-python ()
  "Setup python configuration, like LSP and more."

  (require 'lsp-pyright)
  (lsp-deferred))

(defun custom-fuzzy-finder ()
  "Run fuzzy-finder at the root at the project."

  (fuzzy-finder :directory
                (or (find-git-directory default-directory)
                    default-directory)))

(defun setup-fuzzy-finder ()
  "Setup executable and configuration for fuzzy finder."

  (setq fuzzy-finder-executable-path "~/.fzf/bin/fzf"))

(defun setup-key-bindings ()
  "Setup several custom key bindings."

  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x |") #'split-window-right)
  (global-set-key (kbd "C-x -") #'split-window-below)
  (global-set-key (kbd "C-x C-1") 'neotree-toggle)
  (global-set-key (kbd "C-x C-d") 'neotree-dir)
  (global-set-key (kbd "C-x p") #'custom-fuzzy-finder))

(defun setup-hooks ()
  "Add hooks for several modes."

  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'server-switch-mode #'raise-frame)
  (add-hook 'python-mode-hook #'setup-python))

(defun setup-slime ()
  (setq inferior-lisp-program (executable-find "sbcl")))

(defun setup-all ()
  (setup-key-bindings)
  (setup-hooks)
  (setup-theme)
  (setup-editor-behavior)

  (server-start))

(provide 'my-custom-things)

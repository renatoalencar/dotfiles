(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (magit terraform-mode coffee-mode flycheck tide yaml-mode all-the-icons smart-mode-line typescript-mode neotree dracula-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'neotree)
(require 'typescript-mode)
(require 'smart-mode-line)
(require 'yaml-mode)

(load-theme 'dracula t)
(set-default-font "Source Code Pro Semibold")
(global-set-key (kbd "C-x C-1") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq sml/theme 'powerline)
(sml/setup)

(setq make-backup-files nil)

(global-display-line-numbers-mode)
(setq-default indent-tabs-mode nil)
(setq-default tab-top-list '(2 4 6 8 10))
(electric-indent-mode 0)


(require 'pasquale-mode "/home/renato/.emacs.d/pasquale-mode.el")

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
  (tide-hl-identifier-mode +1)
  (local-set-key [mouse-9] #'tide-jump-to-definition)
  (local-set-key [mouse-8] #'tide-jump-back))

(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(server-start)

(add-hook 'server-switch-mode #'raise-frame)

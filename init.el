;; ===========================================================================================
;; PACKAGE MANAGEMENT
;; ===========================================================================================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ===========================================================================================
;; BASIC SETTINGS
;; ===========================================================================================
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(ido-mode 1)
(ido-everywhere 1)
(set-face-attribute 'default nil :font "Iosevka ExtraLight Extended" :height 150)

(setq auto-save-default nil)   ;; Disable auto-saving
(setq make-backup-files nil)   ;; Disable backup~ files
(setq create-lockfiles nil)    ;; Disable .#lock files

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(load-theme 'doom-material-dark t)

;; ===========================================================================================
;; Formatting
;; ===========================================================================================
(defun format-current-buffer ()
  "Format current buffer with clang-format (for C/C++) or black (for Python)."
  (interactive)
  (when buffer-file-name
    (save-buffer) ;; save before formatting
    (cond
     ((derived-mode-p 'c-mode 'c++-mode)
      (shell-command (format "clang-format -i %s" (shell-quote-argument buffer-file-name))))
     ((derived-mode-p 'python-mode)
      (shell-command (format "black %s" (shell-quote-argument buffer-file-name)))))
    (revert-buffer t t t))) ;; reload buffer after formatting

(global-set-key (kbd "C-c f") 'format-current-buffer)

;; ===========================================================================================
;; C
;; ===========================================================================================
(use-package lsp-mode
  :hook ((c-mode c++-mode) . lsp)
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company
  :config
  (global-company-mode))  ;; autocomplete

(use-package flycheck
  :init (global-flycheck-mode)) ;; inline errors

;; ===========================================================================================
;; Org Setup
;; ===========================================================================================
(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'overview)

(setq org-indent-indentation-per-level 3)
(add-hook 'org-mode-hook 'org-indent-mode)

(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight normal :height 1.5))))
 '(org-level-2 ((t (:inherit default :height 1.4))))
 '(org-level-3 ((t (:inherit default :height 1.3))))
 '(org-level-4 ((t (:inherit default :height 1.2))))
 '(org-level-5 ((t (:inherit default :height 1.1))))
 '(org-level-6 ((t (:inherit default :height 1.1))))
 '(org-level-7 ((t (:inherit default :height 1.1))))
 '(org-level-8 ((t (:inherit default :height 1.1)))))

;; Open video files
(add-to-list 'org-file-apps '("\\.mp4\\'" . "mpv %s"))
(add-to-list 'org-file-apps '("\\.mkv\\'" . "mpv %s"))

;; Default org-babel header args for Python
(setq org-babel-default-header-args:python
      '((:results . "output")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell      . t)
   (C          . t)
   (python     . t)))

(setq org-confirm-babel-evaluate nil)

;; ===========================================================================================
;; Evil Mode (Vim keybindings)
;; ===========================================================================================
(use-package evil
  :init
  (setq evil-want-integration t) ;; required by evil-collection
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)  ;; let C-u scroll up like in Vim
  (setq evil-want-C-i-jump nil)  ;; avoid TAB issues
  :config
  (evil-mode 1)

  ;; Restore Emacs-like keybindings in insert & normal state
  (define-key evil-insert-state-map (kbd "C-a") #'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") #'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-n") #'next-line)
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
)

;; (use-package evil-numbers
;;   :after evil
;;   :config
;;   (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;   (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Optional: Better Evil integration across modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ===========================================================================================
;; LaTeX Previews
;; ===========================================================================================
(with-eval-after-load 'org
  (setq org-preview-latex-default-process 'dvisvgm)
  ;; Bigger previews
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2)))

;; Live latex preview
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

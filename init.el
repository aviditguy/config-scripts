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
(show-paren-mode 1)
(electric-pair-mode 1)
(set-face-attribute 'default nil :font "Monospace" :height 150)

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

;; ===========================================================================================
;; Org Setup
;; ===========================================================================================
(setq org-hide-emphasis-markers t)
(setq org-startup-folded 'overview)
(setq org-preview-latex-image-directory ".ltximg/")

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

(defun my-org-add-date ()
  "Insert today's date as a top-level Org heading."
  (interactive)
  (let ((today (format-time-string "<%d-%m-%Y>")))
    ;; Insert at top of buffer
    (goto-char (point-min))
    ;; Only insert if the date is not already there
    (unless (looking-at-p (concat "^\\** " today))
      (insert (format "** %s\n\n" today)))))

(defun my-org-add-todo-helper (task)
  "Insert a TODO heading with TASK and a start time."
  (interactive "sEnter task: ")
  (let ((start-time (format-time-string "%H:%M")))
    ;; Insert TODO heading
    (insert (format "*** TODO %s [Start: %s]\n" task start-time))
    (org-indent-line)))

(defun my-org-add-todo (task)
  "Add a TODO task under today's date heading."
  (interactive "sEnter task: ")
  (my-org-add-date) ;; ensure today's heading exists
  ;; Move to the end of today's heading
  (goto-char (point-min))
  (re-search-forward (format-time-string "<%d-%m-%Y>") nil t)
  (forward-line)
  ;; Insert task
  (my-org-add-todo-helper task))

(defun my-org-todo-timestamps ()
  "Add end time when TODO is marked DONE, avoid duplicates."
  (when (and (string= org-state "DONE")
             (org-at-heading-p))
    (save-excursion
      (goto-char (line-end-position))
      ;; Only insert [End: …] if it doesn't already exist
      (unless (save-excursion
                (re-search-backward "\\[End: .*\\]" (line-beginning-position) t))
        (insert (format " [End: %s]" (format-time-string "%H:%M")))))))

;; Hook the end-time function
(add-hook 'org-after-todo-state-change-hook 'my-org-todo-timestamps)

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

;; ===========================================================================================
;; Useful Packages
;; ===========================================================================================
(use-package doom-themes
  :config (load-theme 'doom-material-dark t))

;; Smart garbage collector (reduces lag)
(use-package gcmh
  :init (gcmh-mode 1))

(use-package avy
  :ensure t
  :bind (("C-c SPC" . avy-goto-char)
         ("C-c w"   . avy-goto-word-1)
         ("C-c l"   . avy-goto-line)))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

;; ===========================================================================================
;; Custom Functions and Keybindings
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

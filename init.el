;; ===============================
;; PACKAGE MANAGEMENT
;; ===============================
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; ===============================
;; BASIC SETTINGS
;; ===============================
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq ring-bell-function 'ignore)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
(ido-mode 1)
(ido-everywhere 1)
;;(electric-pair-mode 1)
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

;; Choose Theme using F5
(load-theme 'doom-material-dark t)

(defvar my/all-themes (custom-available-themes))
(defvar my/current-theme-index -1)

(defun my/cycle-all-themes ()
  "Disable current theme and load the next available one."
  (interactive)
  ;; Disable any currently enabled themes
  (mapc #'disable-theme custom-enabled-themes)
  ;; Increment index
  (setq my/current-theme-index (% (1+ my/current-theme-index) (length my/all-themes)))
  ;; Load next theme
  (load-theme (nth my/current-theme-index my/all-themes) t)
  (message "Loaded theme: %s" (nth my/current-theme-index my/all-themes)))

(global-set-key (kbd "<f5>") 'my/cycle-all-themes)

;; =====================================================================
;; Formatting
;; =====================================================================
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

;; =====================================================================
;; Org Setup
;; =====================================================================
(setq org-hide-emphasis-markers t)

(setq org-startup-folded 'overview)

(font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(custom-set-faces
 '(org-level-1 ((t (:inherit default :weight normal
			     :height 1.6))))
 '(org-level-2 ((t (:inherit default :height 1.5))))
 '(org-level-3 ((t (:inherit default :height 1.4))))
 '(org-level-4 ((t (:inherit default :height 1.3))))
 '(org-level-5 ((t (:inherit default :height 1.2))))
 '(org-level-6 ((t (:inherit default :height 1.1))))
 '(org-level-7 ((t (:inherit default :height 1.1))))
 '(org-level-8 ((t (:inherit default :height 1.1)))))

;; Open video files
(add-to-list 'org-file-apps '("\\.mp4\\'" . "mpv %s"))
(add-to-list 'org-file-apps '("\\.mkv\\'" . "mpv %s"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)))

(setq org-confirm-babel-evaluate nil)


;; =====================================================================
;; LaTeX Previews
;; =====================================================================
(with-eval-after-load 'org
  (setq org-preview-latex-default-process 'dvisvgm)
  ;; Bigger previews
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2)))

;; Live latex preview
(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

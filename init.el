;;; Basic UI Config
(setq inhibit-startup-message 1)
(setq visible-bell t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)

;;; Setting up a font
(set-face-attribute 'default nil :font "Iosevka Light" :height 170)

;;; Enable Line Number
(column-number-mode)
(global-display-line-numbers-mode t)

;;; Disable Line number for certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Add frame borders and window dividers
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

;;; Configuring Package Manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(require 'use-package)
(setq use-package-always-ensure t)

;;; Install Evil Mode (Vim) for Emacs
(use-package evil
	:init
	:config
	(evil-mode 1))


;;; Org Mode Config
(defun org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Set faces for heading levels
(dolist (face '((org-level-1 . 1.2)
		(org-level-2 . 1.1)
		(org-level-3 . 1.05)
		(org-level-4 . 1.0)
		(org-level-5 . 1.1)
		(org-level-6 . 1.1)
		(org-level-7 . 1.1)
		(org-level-8 . 1.1)))
(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
  org-hide-emphasis-markers t))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; Load Org Babel languages
(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python     . t)
     (C          . t)
     (shell      . t)
     (js         . t)      ;; for Node.js
     (lisp       . t)      ;; Common Lisp
     (rust       . t)))    ;; Rust (requires ob-rust)

  ;; Don't ask for confirmation before running code
  (setq org-confirm-babel-evaluate nil)
(setq org-src-tab-acts-natively t)         ;; TAB indents code inside #+begin_src blocks
(setq org-edit-src-content-indentation 0)  ;; No extra indentation inside source blocks

  ;; Display results inline
  (setq org-src-window-setup 'current-window
        org-babel-inline-result-wrap "«%s»"
        org-babel-default-header-args
        '((:results . "output replace")
          (:exports . "both"))))
(use-package ob-rust
  :after org)

(require 'org-tempo)
(setq org-structure-template-alist
      '(("sh" . "src sh")
        ("el" . "src emacs-lisp")
        ("lisp" . "src lisp")
        ("cl" . "src common-lisp")
        ("c" . "src c")
        ("cpp" . "src cpp")
        ("py" . "src python")
        ("js" . "src js")
        ("ts" . "src typescript")
        ("bash" . "src bash")
        ("rust" . "src rust")
        ("rb" . "src ruby")))

(setq org-startup-with-latex-preview t)
;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-format-latex-options
      '(:foreground default :background default :scale 1.5
        :html-foreground "Black" :html-background "Transparent" :html-scale 1.0
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
(custom-set-faces
 ;; Add padding and background for source blocks
 '(org-block ((t (:background "#1e1e1e" :extend t :box (:line-width 6 :color "#1e1e1e")))))
 ;; Add padding and background for results blocks
 '(org-block-begin-line ((t (:background "#1a1a1a" :foreground "#999999" :extend t :box (:line-width 6 :color "#1a1a1a")))))
 '(org-block-end-line ((t (:background "#1a1a1a" :foreground "#999999" :extend t :box (:line-width 6 :color "#1a1a1a"))))))

(setq org-latex-listings 'minted)
(setq org-latex-packages-alist '(("" "minted")))
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory=%o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory=%o %f")) ; Run twice

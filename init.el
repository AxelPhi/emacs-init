;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External package sources
(require 'package)

;; marmalade seems to be down right now
;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; list of packages to sync
(setq package-list
      '(
	auto-complete
	auto-complete-nxml
	color-theme-sanityinc-solarized
	json-mode
	json-reformat
	markdown-mode
	yaml-mode
	dockerfile-mode
	elpy
	))

(package-initialize)

;; refresh package list if it is not already available
(when (not package-archive-contents) (package-refresh-contents))

;; install packages from the list that are not yet installed
(dolist (pkg package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure behavior

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

;; Use some more memory for undo
(setq undo-outer-limit 26127069)

;; Bind "indent-region" shortcuts
(defun indent-buffer()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key (kbd "C-'") 'indent-buffer)
(global-set-key (kbd "C-#") 'indent-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure modes

;; Autocomplete
(require 'auto-complete)
(ac-config-default)
(require 'auto-complete-nxml)
(auto-complete-mode t)

;; python Mode
(require 'elpy)
(elpy-enable)


;; XML editing
(setq auto-mode-alist (cons '("\\.xml$" . nxml-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.xsl$" . nxml-mode) auto-mode-alist))
(autoload 'xml-mode "nxml" "XML editing mode" t)
(global-set-key [C-return] 'completion-at-point)

;; Markdown editing
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'after-save-hook
			'check-parens
			nil t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Layout and defaults

;; Color theme selection
(load-theme 'tango-dark t)

;; Use default syntax highlighting
(global-font-lock-mode 1)

;; Load whitespace.el
(require 'whitespace)
(setq whitespace-style (quote
			(face trailing tab-mark empty)))
(global-whitespace-mode 1)


;; Set window dimensions
(add-to-list 'default-frame-alist '(height . 43))
(add-to-list 'default-frame-alist '(width . 120))

;; "y" and "n" are enough to answer "yes" or "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Hide unused GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Startup with empty buffer
(setq inhibit-startup-screen t
      initial-scratch-message "")

;; Highlight paranthesis pairs
(show-paren-mode t)

;; Yes, I'd like to see column numbers
(column-number-mode t)

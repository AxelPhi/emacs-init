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
	dbus
	docker-compose-mode
	dockerfile-mode
	elpy
	helm
	jinja2-mode
	js2-mode
	json-mode
	json-reformat
	markdown-mode
	yaml-mode
	web-mode
	workgroups2
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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
)

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
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'docker-compose-mode)

(require 'auto-complete-nxml)
(require 'docker-compose-mode)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Helm mode
(require 'helm-config)
(helm-mode 1)

;; Workspaces mode
(require 'workgroups2)
(workgroups-mode 1)
(global-set-key (kbd "<pause>")     'wg-reload-session)
(global-set-key (kbd "C-S-<pause>") 'wg-save-session)
(global-set-key (kbd "s-z")         'wg-switch-to-workgroup)
(global-set-key (kbd "s-/")         'wg-switch-to-previous-workgroup)
(setq wg-session-file "~/.emacs.d/.emacs_workgroups")

;; Setup js2 mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; Custom functions
(defun xml-format ()
      (interactive)
      (save-excursion
        (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
      )
    )

;;; save & shutdown when we get an "end of session" signal on dbus 
(require 'dbus)

(defun my-register-signals (client-path)
  "Register for the 'QueryEndSession' and 'EndSession' signals from
Gnome SessionManager.

When we receive 'QueryEndSession', we just respond with
'EndSessionResponse(true, \"\")'.  When we receive 'EndSession', we
append this EndSessionResponse to kill-emacs-hook, and then call
kill-emacs.  This way, we can shut down the Emacs daemon cleanly
before we send our 'ok' to the SessionManager."
  (setq my-gnome-client-path client-path)
  (let ( (end-session-response (lambda (&optional arg)
                                 (dbus-call-method-asynchronously
                                  :session "org.gnome.SessionManager" my-gnome-client-path
                                  "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse" nil
                                  t "") ) ) )
         (dbus-register-signal
          :session "org.gnome.SessionManager" my-gnome-client-path
          "org.gnome.SessionManager.ClientPrivate" "QueryEndSession"
          end-session-response )
         (dbus-register-signal
          :session "org.gnome.SessionManager" my-gnome-client-path
          "org.gnome.SessionManager.ClientPrivate" "EndSession"
          `(lambda (arg)
             (add-hook 'kill-emacs-hook ,end-session-response t)
             (kill-emacs) ) ) ) )

;; DESKTOP_AUTOSTART_ID is set by the Gnome desktop manager when emacs
;; is autostarted.  We can use it to register as a client with gnome
;; SessionManager.
(dbus-call-method-asynchronously
 :session "org.gnome.SessionManager"
 "/org/gnome/SessionManager" 
 "org.gnome.SessionManager" "RegisterClient" 'my-register-signals
 "Emacs server" (getenv "DESKTOP_AUTOSTART_ID"))

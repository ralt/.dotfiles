(defvar *projects* (make-hash-table :test 'equal))

(puthash "cso" "/home/flmar/lxc/total-cso/var/www/total-cso" *projects*)
(puthash "labs" "/home/flmar/lxc/cera-labs/var/www/cera-labs" *projects*)
(puthash "mpi" "/home/flmar/lxc/cera-mesprojetsimmobiliers/var/www/cera-mesprojetsimmobiliers" *projects*)

(require 'package)
(add-to-list 'package-archives
         '("marmalade" .
           "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("e6h" . "http://www.e6h.org/packages/"))
(package-initialize)

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
      '(markdown-mode
      color-theme
      color-theme-solarized
      ace-jump-mode
      color-theme
      monokai-theme
      paredit
      php-mode
      restclient
      js2-mode
      js2-refactor
      discover
      discover-js2-refactor
      autopair
      web-mode
      org
      go-mode
      jabber
      dtrt-indent
      auto-complete
      ac-helm
      helm
      helm-cmd-t
      helm-dash
      base16-theme
      powerline
      magit)
      "List of packages needs to be installed at launch")

(defun has-package-not-installed ()
    (loop for p in packages-list
                  when (not (package-installed-p p)) do (return t)
                          finally (return nil)))
(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;disable annoying GUI
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight tabulations
(setq-default highlight-tabs t)

;; Remove useless whitespaces before saving a file
;;(add-hook 'before-save-hook 'whitespace-cleanup)

;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)

;80 characters ftw
(add-to-list 'default-frame-alist '(width . 80))

;show column number
(column-number-mode t)
;(global-linum-mode t)

;mute emacs sounds (the annoying beep when reaching EOF)
;this is only set on windows, so first check if it exists
(if (fboundp 'set-message-beep)
    (set-message-beep 'silent))

;change the default system-encodings to utf-8
(prefer-coding-system 'utf-8)
(setq
  coding-system-for-read 'utf-8
  coding-system-for-write 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Set locale to UTF8
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;...by any means necessary

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "/usr/local/bin/sbcl")

(show-paren-mode t)

(require 'tls)

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (setq start-irc-password (read-passwd "ZNC password: "))
  (erc :server "vm.margaine.com" :port 6667
       :password start-irc-password
       :nick "Ralt"
       :full-name "Ralt"))

(setq erc-autojoin-channels-alist '(("freenode.net" "#lisp" "#promises" "#sojavascript" "#lispweb" "#emacs" "#reckict" "#drupal-commerce")))

(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-extra-arguments nil)

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gnb.intranet"
      smtpmail-smtp-server "smtp.gnb.intranet"
      smtpmail-smtp-service 25
      smtpmail-starttls-credentials '(("smtp.gnb.intranet" 25 nil nil))
      smtpmail-auth-credentials '(("smtp.gnb.intranet" 25 "flmar" nil))
      smtpmail-debug-info t)

(setq gnus-select-method '(nnimap "smile"
                                  (nnimap-address "bluemind.smile.fr")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

(setq user-mail-address "florian.margaine@smile.fr")
(setq user-full-name "Florian Margaine")

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;###autoload
(define-derived-mode drupal-mode php-mode "Drupal"
  "Major mode for Drupal coding.\n\n\\{drupal-mode-map}"
  (setq c-basic-offset 2)
  (setq indent-tabs-mode nil)
  (setq fill-column 78)
  (setq show-trailing-whitespace t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (c-set-offset 'case-label '+)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+) ; for FAPI arrays and DBTNG
  (c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ; for DBTNG fields and values
  (run-mode-hooks 'drupal-mode-hook))
(provide 'drupal-mode)

;; drupalcs
(defun my-php-hook-function ()
  (set (make-local-variable 'compile-command)
       (format "phpcs --report=emacs --standard=Drupal %s" (buffer-file-name))))

(add-hook 'drupal-mode-hook 'my-php-hook-function)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(custom-safe-themes (quote ("de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ecb-auto-activate t)
 '(erc-modules (quote (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(jabber-alert-message-hooks (quote (jabber-message-awesome libnotify-jabber-notify jabber-message-echo jabber-message-scroll)) t)
 '(jabber-alert-presence-hooks nil)
 '(mu4e-headers-fields (quote ((:human-date . 12) (:maildir . 10) (:flags . 6) (:from . 22) (:subject)))))

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(add-hook 'php-mode-hook 'my-php-mode-stuff)

(defun my-php-mode-stuff ()
  (local-set-key (kbd "<f1>") 'my-php-function-lookup))

(defun my-php-function-lookup ()
  (interactive)
  (let* ((function (symbol-name (or (symbol-at-point)
                                    (error "No function at point."))))
         (buf (url-retrieve-synchronously (concat "http://php.net/manual-lookup.php?pattern=" function))))
    (with-current-buffer buf
      (goto-char (point-min))
      (let (desc)
        (when (re-search-forward "<div class=\"methodsynopsis dc-description\">\\(\\(.\\|\n\\)*?\\)</div>" nil t)
          (setq desc
                (replace-regexp-in-string
                 " +" " "
                 (replace-regexp-in-string
                  "\n" ""
                  (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1)))))

          (when (re-search-forward "<p class=\"para rdfs-comment\">\\(\\(.\\|\n\\)*?\\)</p>" nil t)
            (setq desc
                  (concat desc "\n\n"
                          (replace-regexp-in-string
                           " +" " "
                           (replace-regexp-in-string
                            "\n" ""
                            (replace-regexp-in-string "<.*?>" "" (match-string-no-properties 1))))))))

        (if desc
            (message desc)
          (message "Could not extract function info. Press M-F1 to go the description."))))
    (kill-buffer buf)))

;;; Jabber notifications
(defvar libnotify-program "/usr/bin/notify-send")

(defun notify-send (title message)
  (start-process "notify" " notify" libnotify-program "--expire-time=4000" title message))

(defun libnotify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via libnotify"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (notify-send (format "(PM) %s"
                       (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (notify-send (format "%s" (jabber-jid-displayname from))
             text)))

(add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "C-x f") 'fiplr-find-file)

(global-set-key (kbd "C-c i") 'magit-status)


(defun goto-project (project)
  (interactive "sEnter project name: ")
  (cd (gethash project *projects*)))

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))
(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups )))

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove all backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
(setq auto-save-default nil)

(global-set-key (kbd "C-x o") 'switch-window)

(define-key global-map (kbd "RET") 'newline-and-indent)

(autopair-mode)

(define-key global-map (kbd "<C-tab>") 'php-complete-function)

(setq gnus-show-all-headers t)
(setq message-cite-reply-position 'above)

(add-hook 'makefile-mode-hook 'indent-tabs-mode)

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun unident-closure ()
  (let ((syntax (mapcar 'car c-syntactic-context)))
    (if (and (member 'arglist-cont-nonempty syntax)
             (or
              (member 'statement-block-intro syntax)
              (member 'brace-list-intro syntax)
              (member 'brace-list-close syntax)
              (member 'block-close syntax)))
        (save-excursion
          (beginning-of-line)
          (delete-char (* (count 'arglist-cont-nonempty syntax)
                          c-basic-offset))))))

(add-hook 'php-mode-hook (lambda ()
                           (add-hook 'c-special-indent-hook 'unident-closure)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; paste fix
(setq interprogram-paste-function 'x-selection-value)

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-default-notes-file "~/Org/notes.org")
(define-key global-map "\C-cc" 'org-capture)
(setq org-agenda-files (list "~/Org/work.org"))

(js2r-add-keybindings-with-prefix "C-c C-r")

(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-min-indent-superiority 50)

(require 'helm-config)
(require 'helm-cmd-t)

;; Necessary before emacs 24.4
(unless (fboundp 'hash-table-values)
  (defun hash-table-values (hashtable)
    (let (allvals)
      (maphash (lambda (kk vv) (setq allvals (cons vv allvals))) hashtable)
      allvals)))

(unless (fboundp 'hash-table-values)
  (defun hash-table-values (hashtable)
    (let (allvals)
      (maphash (lambda (_kk vv) (push vv allvals)) hashtable)
    allvals)))

(unless (fboundp 'hash-table-keys)
  (defun get-hash-keys (hashtable)
    "Return all keys in hashtable."
    (let (allkeys)
      (maphash (lambda (kk _vv) (push kk allkeys)) hashtable)
      allkeys)))

(defun helm-run-cmd-t-in-correct-folder ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (dolist (path (hash-table-values *projects*))
      (when (string= path (substring file-name 0 (length path)))
        (helm :sources (list (helm-cmd-t-get-create-source-dir path)))))))

(global-set-key (kbd "M-r") 'helm-cmd-t)
(global-set-key (kbd "M-t") 'helm-run-cmd-t-in-correct-folder)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-g s") 'helm-do-grep)

(require 'ac-helm)
(auto-complete-mode t)
(global-set-key (kbd "C-:") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

(load-theme 'base16-default t)

(when (fboundp 'w3m)
  (progn
    (require 'w3m)
    (setq w3m-home-page "https://www.google.com")))

(defun drupal-doc ()
  (interactive)
  (setq-local helm-dash-docsets '("Drupal" "PHP")))

(global-set-key (kbd "C-h C-k f") 'helm-dash)
(global-set-key (kbd "C-h C-k g") 'helm-dash-at-point)

(defun jwintz/dash-path (docset)
  (if (string= docset "OpenGL_2")
      (concat (concat helm-dash-docsets-path "/") "OpenGL2.docset")
    (if (string= docset "OpenGL_3")
        (concat (concat helm-dash-docsets-path "/") "OpenGL3.docset")
      (if (string= docset "OpenGL_4")
          (concat (concat helm-dash-docsets-path "/") "OpenGL4.docset")
        (if (string= docset "Emacs_Lisp")
            (concat (concat helm-dash-docsets-path "/") "Emacs Lisp.docset")
          (concat
            (concat
             (concat
              (concat helm-dash-docsets-path "/")
              (nth 0 (split-string docset "_")))) ".docset"))))))

(defun jwintz/dash-install (docset)
  (unless (file-exists-p (jwintz/dash-path docset))
    (helm-dash-install-docset docset)))

(setq helm-dash-docsets-path (format "%s/.emacs.d/docsets" (getenv "HOME")))

(jwintz/dash-install "PHP")
(jwintz/dash-install "Drupal")

(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome")
(setq browse-url-browser-function 'w3m-browse-url)
(setq current-browser "google-chrome")

(defun switch-browser ()
  (interactive)
  (if (string= current-browser "w3m")
      (progn
        (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome")
        (setq current-browser "google-chrome")
        (message "Switching to google-chrome"))
    (progn
      (setq browse-url-browser-function 'w3m-browse-url)
      (setq current-browser "w3m")
      (message "Switching to w3m"))))

(require 'powerline)
(powerline-default-theme)

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

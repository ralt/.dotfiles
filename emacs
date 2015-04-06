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
      company
      monokai-theme
      paredit
      php-mode
      js2-mode
      js2-refactor
      autopair
      web-mode
      org
      go-mode
      dtrt-indent
      rainbow-delimiters
      powerline
      aggressive-indent
      magit
      async
      epl
      f
      smex
      gh
      gist
      git-commit-mode
      git-rebase-mode
      htmlfontify
      makey
      multi-term
      names
      projectile
      web-mode)
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
(add-hook 'before-save-hook 'whitespace-cleanup)

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
(setq inferior-lisp-program "/usr/bin/sbcl")
(setenv "SBCL_HOME" "/usr/lib/sbcl")

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

(setq erc-autojoin-channels-alist '(("freenode.net" "#lisp" "#promises" "#sojavascript" "#lispweb" "#emacs" "#lispkit")))

(setq starttls-use-gnutls t)
(setq starttls-gnutls-program "gnutls-cli")
(setq starttls-extra-arguments nil)

(setq mm-discouraged-alternatives '("text/html" "text/richtext"))

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
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(custom-safe-themes
   (quote
    ("4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" "de2c46ed1752b0d0423cde9b6401062b67a6a1300c068d5d7f67725adc6c3afb" "41b6698b5f9ab241ad6c30aea8c9f53d539e23ad4e3963abff4b57c0f8bf6730" "51bea7765ddaee2aac2983fac8099ec7d62dff47b708aa3595ad29899e9e9e44" "f41fd682a3cd1e16796068a2ca96e82cfd274e58b978156da0acce4d56f2b0d5" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "e53cc4144192bb4e4ed10a3fa3e7442cae4c3d231df8822f6c02f1220a0d259a" "978ff9496928cc94639cb1084004bf64235c5c7fb0cfbcc38a3871eb95fa88f6" "ae8d0f1f36460f3705b583970188e4fbb145805b7accce0adb41031d99bd2580" "9bac44c2b4dfbb723906b8c491ec06801feb57aa60448d047dbfdbd1a8650897" "1affe85e8ae2667fb571fc8331e1e12840746dae5c46112d5abb0c3a973f5f5a" "e80a0a5e1b304eb92c58d0398464cd30ccbc3622425b6ff01eea80e44ea5130e" "60f04e478dedc16397353fb9f33f0d895ea3dab4f581307fbf0aa2f07e658a40" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ecb-auto-activate t)
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(jabber-alert-message-hooks
   (quote
    (jabber-message-awesome libnotify-jabber-notify jabber-message-echo jabber-message-scroll)) t)
 '(jabber-alert-presence-hooks nil)
 '(mu4e-headers-fields
   (quote
    ((:human-date . 12)
     (:maildir . 10)
     (:flags . 6)
     (:from . 22)
     (:subject))))
 '(org-agenda-files (quote ("~/Org/tasks.org")))
 '(org-support-shift-select t)
 ;; '(smtpmail-smtp-server "smtp.gmail2.com")
 ;; '(smtpmail-smtp-service 587))
 )
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

;; Ask "y" or "n" instead of "yes" or "no". Yes, laziness is great.
(fset 'yes-or-no-p 'y-or-n-p)

;; Remove all backup files
(setq make-backup-files nil)
(setq backup-inhibited t)
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.


;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(global-set-key (kbd "C-x o") 'switch-window)

(define-key global-map (kbd "RET") 'newline-and-indent)

(autopair-mode)

(define-key global-map (kbd "<C-tab>") 'php-complete-function)

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

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-f") 'ido-find-file)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)

(setq browse-url-browser-function 'eww-browse-url)
(setq current-browser "eww")

(defun switch-browser ()
  (interactive)
  (if (string= current-browser "eww")
      (progn
        (setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "chromium-browser")
        (setq current-browser "chromium-browser")
        (message "Switching to chromium-browser"))
    (progn
      (setq browse-url-browser-function 'eww-browse-url)
      (setq current-browser "eww")
      (message "Switching to eww"))))

;(require 'powerline)
;(powerline-default-theme)

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

;; (add-to-list 'load-path "/home/florian/mu-0.9.9.6/mu4e")

;; (require 'mu4e)

;; ;; default
;; (setq mu4e-maildir "~/Maildir")

;; ;; setup some handy shortcuts
;; ;; you can quickly switch to your Inbox -- press ``ji''
;; ;; then, when you want archive some messages, move them to
;; ;; the 'All Mail' folder by pressing ``ma''.

;; (setq mu4e-maildir-shortcuts
;;     '( ("/Work/inbox"               . ?i)
;;        ("/Work/sent"   . ?s)
;;        ("/Personal/inbox" . ?u)
;;        ("/Personal/sent" . ?a)))

;; ;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "mbsync -a")

;; (setq mu4e-headers-skip-duplicates t)
;; (setq mu4e-hide-index-messages t)
;; (setq mu4e-view-show-addresses t)

;; ;; something about ourselves
;; (setq
;;    user-mail-address "florian.margaine@commerceguys.com"
;;    user-full-name  "Florian Margaine"
;;    )

;; ;; sending mail -- replace USERNAME with your gmail username
;; ;; also, make sure the gnutls command line utils are installed
;; ;; package 'gnutls-bin' in Debian/Ubuntu

;; (require 'smtpmail)

;; (defvar my-mu4e-account-alist
;;   '(("Work"
;;      (mu4e-sent-folder "/Work/sent")
;;      (user-mail-address "florian.margaine@commerceguys.com")
;;      (smtpmail-local-domain "commerceguys.com")
;;      (smtpmail-default-smtp-server "smtp.gmail1.com")
;;      (smtpmail-smtp-server "smtp.gmail1.com")
;;      (mu4e-compose-signature
;;       (concat
;;        "Florian Margaine\n"
;;        "\n"
;;        "Drupal dev @ Commerce Guys\n"
;;        "http://commerceguys.com\n"
;;        "\n"
;;        "Tel: +33 (0) 9 81 89 67 42\n")))
;;     ("Personal"
;;      (mu4e-sent-folder "/Personal/sent")
;;      (user-mail-address "florian@margaine.com")
;;      (smtpmail-local-domain "margaine.com")
;;      (smtpmail-default-smtp-server "smtp.gmail2.com")
;;      (smtpmail-smtp-server "smtp.gmail2.com")
;;      (mu4e-compose-signature
;;       (concat
;;        "Florian Margaine\n")))))

;; (defun my-mu4e-set-account ()
;;   "Set the account for composing a message."
;;   (let* ((account
;;           (if mu4e-compose-parent-message
;;               (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
;;                 (string-match "/\\(.*?\\)/" maildir)
;;                 (match-string 1 maildir))
;;             (completing-read (format "Compose with account: (%s) "
;;                                      (mapconcat #'(lambda (var) (car var))
;;                                                 my-mu4e-account-alist "/"))
;;                              (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
;;                              nil t nil nil (caar my-mu4e-account-alist))))
;;          (account-vars (cdr (assoc account my-mu4e-account-alist))))
;;     (if account-vars
;;         (mapc #'(lambda (var)
;;                   (set (car var) (cadr var)))
;;               account-vars)
;;       (error "No email account found"))))

;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; ;; alternatively, for emacs-24 you can use:
;; (setq message-send-mail-function 'smtpmail-send-it
;;      smtpmail-stream-type 'starttls
;;      smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
;(setq mail-user-agent 'mu4e-user-agent)

;(setq mu4e-update-interval 60) ;; every 1 minute
;(setq mu4e-use-fancy-chars t)

;(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

;; show images
;(setq mu4e-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; need this to convert some e-mails properly
;(setq mu4e-html2text-command "html2text -utf8 -width 72")

;(global-set-key (kbd "C-c m") 'mu4e)

(when (require 'multi-term nil t)
  (global-set-key (kbd "C-c t") 'multi-term)
  (global-set-key (kbd "<C-next>") 'multi-term-next)
  (global-set-key (kbd "<C-prior>") 'multi-term-prev)
  (setq multi-term-buffer-name "term"
        multi-term-program "/bin/bash"))


(when (require 'term nil t) ; only if term can be loaded..
  (setq term-bind-key-alist
        (list (cons "C-c C-c" 'term-interrupt-subjob)
              (cons "C-n" 'next-line)
              (cons "M-f" 'term-send-forward-word)
              (cons "M-b" 'term-send-backward-word)
              (cons "C-c C-j" 'term-line-mode)
              (cons "C-c C-k" 'term-char-mode)
              (cons "M-DEL" 'term-send-backward-kill-word)
              (cons "M-d" 'term-send-forward-kill-word)
              (cons "<C-left>" 'term-send-backward-word)
              (cons "<C-right>" 'term-send-forward-word)
              (cons "C-r" 'term-send-reverse-search-history)
              (cons "M-p" 'term-send-raw-meta)
              (cons "M-y" 'term-send-raw-meta)
              (cons "C-y" 'term-send-raw))))

(when (require 'term nil t)
  (defun term-handle-ansi-terminal-messages (message)
    (while (string-match "\eAnSiT.+\n" message)
      ;; Extract the command code and the argument.
      (let* ((start (match-beginning 0))
             (command-code (aref message (+ start 6)))
             (argument
              (save-match-data
                (substring message
                           (+ start 8)
                           (string-match "\r?\n" message
                                         (+ start 8))))))
        ;; Delete this command from MESSAGE.
        (setq message (replace-match "" t t message))

        (cond ((= command-code ?c)
               (setq term-ansi-at-dir argument))
              ((= command-code ?h)
               (setq term-ansi-at-host argument))
              ((= command-code ?u)
               (setq term-ansi-at-user argument))
              ((= command-code ?e)
               (save-excursion
                 (find-file-other-window argument)))
              ((= command-code ?x)
               (save-excursion
                 (find-file argument))))))

    (when (and term-ansi-at-host term-ansi-at-dir term-ansi-at-user)
      (setq buffer-file-name
            (format "%s@%s:%s" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))
      (set-buffer-modified-p nil)
        (setq default-directory (if (string= term-ansi-at-host (system-name))
                                    (concatenate 'string term-ansi-at-dir "/")
                                  (format "/%s@%s:%s/" term-ansi-at-user term-ansi-at-host term-ansi-at-dir))))
    message))

(global-set-key (kbd "C-x s") 'imenu)

(defun full-auto-save ()
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (and (buffer-file-name) (buffer-modified-p))
          (basic-save-buffer)))))
(add-hook 'auto-save-hook 'full-auto-save)

(setq temporary-file-directory "/home/florian/.emacs-auto-save")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq bindings '(("C-z" . nil) ; prevents accidental scares
                 ("C-z z" . suspend-frame)))

(defun bind-key (binding)
  (let ((key (car binding))
        (func (cdr binding)))
    (global-set-key (kbd key) func)))

(mapcar 'bind-key bindings)

;; (add-hook 'mu4e-compose-mode-hook
;;           (defun my-setup-epa-hook ()
;;             (epa-mail-mode)))

;; (add-hook 'mu4e-view-mode-hook
;;           (defun my-view-mode-hook ()
;;             (epa-mail-mode)))

;; (setq mu4e-view-show-images t)
;; (setq mu4e-html2text-command "w3m -dump -T text/html")
;; (setq mu4e-view-prefer-html t)

(load-library "iso-transl")

;; name the signatures "signature.asc"
(defadvice mml2015-sign (after mml2015-sign-rename (cont) act)
  (save-excursion
    (search-backward "Content-Type: application/pgp-signature")
    (goto-char (point-at-eol))
    (insert "; name=\"signature.asc\"; description=\"Digital signature\"")))

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
      (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
          ;; then bury the *compilation* buffer, so that C-x b doesn't go there
          (bury-buffer "*compilation*")
          ;; and return to whatever were looking at before
          (replace-buffer-in-windows "*compilation*"))
        ;; Always return the anticipated result of compilation-exit-message-function
        (cons msg code)))

(set-frame-parameter nil 'fullscreen 'maximized)

(setq browse-url-browser-function 'eww-browse-url)

(eval-after-load "eww"
  '(progn (define-key eww-mode-map "f" 'eww-lnum-follow)
          (define-key eww-mode-map "F" 'eww-lnum-universal)))

(add-to-list 'auto-mode-alist (cons "\\.paren\\'" 'lisp-mode))
(add-hook 'lisp-mode-hook
          #'(lambda ()
              (when (and buffer-file-name
                         (string-match-p "\\.paren\\>" buffer-file-name))
                (unless (slime-connected-p)
                  (save-excursion (slime)))
                (trident-mode +1))))

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode-enable)
(add-hook 'emacs-lisp-mode #'rainbow-delimiters-mode-enable)

(require 'trident-mode)
(trident-add-keys-with-prefix "C-c C-e")

(setq ispell-dictionary "francais")

;(load "~/quicklisp/log4slime-setup.el")
;(global-log4slime-mode 1)

(setq-default indent-tabs-mode nil)

(ido-mode)
(put 'narrow-to-page 'disabled nil)


(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(add-hook 'emacs-lisp-mode-hook #'(lambda ()
                                    (enable-paredit-mode)
                                    (prettify-symbols-mode t)
                                    (push '(">=" . ?≥) prettify-symbols-alist)
                                    (push '("<=" . ?≤) prettify-symbols-alist)
                                    (rainbow-delimiters-mode t)))

(add-hook 'eval-expression-minibuffer-setup-hook #'(lambda ()
                                                     (enable-paredit-mode)
                                                     (prettify-symbols-mode t)
                                                     (push '(">=" . ?≥) prettify-symbols-alist)
                                                     (push '("<=" . ?≤) prettify-symbols-alist)
                                                     (rainbow-delimiters-mode t)))

(add-hook 'ielm-mode-hook #'(lambda ()
                              (enable-paredit-mode)
                              (prettify-symbols-mode t)
                              (push '(">=" . ?≥) prettify-symbols-alist)
                              (push '("<=" . ?≤) prettify-symbols-alist)
                              (rainbow-delimiters-mode t)))

(add-hook 'lisp-mode-hook #'(lambda ()
                              (enable-paredit-mode)
                              (prettify-symbols-mode t)
                              (push '(">=" . ?≥) prettify-symbols-alist)
                              (push '("<=" . ?≤) prettify-symbols-alist)
                              (rainbow-delimiters-mode t)))

(add-hook 'lisp-interaction-mode-hook #'(lambda ()
                                          (enable-paredit-mode)
                                          (prettify-symbols-mode t)
                                          (push '(">=" . ?≥) prettify-symbols-alist)
                                          (push '("<=" . ?≤) prettify-symbols-alist)
                                          (rainbow-delimiters-mode t)))

(load-theme 'monokai)

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]"))))

(setq eshell-history-size 1024)
(setq eshell-prompt-regexp "^[^#$]*[#$] ")

(load "em-hist")           ; So the history vars are defined
(if (boundp 'eshell-save-history-on-exit)
    (setq eshell-save-history-on-exit t)) ; Don't ask, just save
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)
(if (boundp 'eshell-ask-to-save-history)
    (setq eshell-ask-to-save-history 'always)) ; For older(?) version
;(message "eshell-ask-to-save-history is %s" eshell-ask-to-save-history)

(defun eshell/ef (fname-regexp &rest dir) (ef fname-regexp default-directory))


;;; ---- path manipulation

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat "["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "green"))
      )))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize ((lambda (p-lst)
            (if (> (length p-lst) 3)
                (concat
                 (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                                            (substring elm 0 1)))
                            (butlast p-lst 3)
                            "/")
                 "/"
                 (mapconcat (lambda (elm) elm)
                            (last p-lst 3)
                            "/"))
              (mapconcat (lambda (elm) elm)
                         p-lst
                         "/")))
          (split-string (pwd-repl-home (eshell/pwd)) "/")) 'face `(:foreground "yellow"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize "# " 'face 'default))))
(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(setq eshell-highlight-prompt nil)

(setq magit-log-show-gpg-status t)

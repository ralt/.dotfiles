(require 'package)
(add-to-list 'package-archives
         '("marmalade" .
           "http://marmalade-repo.org/packages/") t)
(package-initialize)

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
      '(markdown-mode
      color-theme
      color-theme-solarized
      ace-jump-mode
      color-theme
      ido-better-flex
      paredit
      php-mode
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

;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)

;80 characters ftw
(add-to-list 'default-frame-alist '(width . 80))

;before saving, remove all trailing whitespace (god forbid)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;show column number
;(column-number-mode t)
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
;...by any means necessary

(setq inferior-lisp-program "sbcl")
(add-to-list 'load-path "~/.emacs.d/elpa/slime")  ; your SLIME directory
(require 'slime)
(slime-setup '(slime-fancy))

(ido-mode)
(setq ido-enable-flex-matching t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

<<<<<<< HEAD
=======
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(put 'scroll-left 'disabled nil)

>>>>>>> b37f57c1523415c7d0195bd9d4f7b0dd03afc58d
(show-paren-mode t)

(require 'tls)

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.margaine.com" :port 6667
           :nick "Ralt" :full-name "Ralt"))

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
                                  (nnimap-address "zimbra.smile.fr")
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
  (run-mode-hooks 'drupal-mode-hook)
)
(provide 'drupal-mode)

;; drupalcs
(defun my-php-hook-function ()
  (set (make-local-variable 'compile-command)
       (format "phpcs --report=emacs --standard=Drupal %s" (buffer-file-name))))

(add-hook 'drupal-mode-hook 'my-php-hook-function)

(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . drupal-mode))
(add-to-list 'auto-mode-alist '("\\.\\(php\\|inc\\)$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.info" . conf-windows-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
<<<<<<< HEAD
 '(browse-url-browser-function (quote browse-url-default-browser))
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ecb-auto-activate t))
=======
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(jabber-alert-message-hooks (quote (jabber-message-awesome libnotify-jabber-notify jabber-message-echo jabber-message-scroll)))
 '(jabber-alert-presence-hooks nil))
>>>>>>> db2159d99e251a2ae3bceb530401f67610824827
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(add-to-list 'load-path "~/.emacs.d/misc/")

(require 'powerline)
(powerline-default-theme)

(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(load-theme 'solarized-light t)

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
<<<<<<< HEAD

(require 'popwin)
(popwin-mode 1)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
=======
(put 'upcase-region 'disabled nil)


;;; Jabber notifications
(defvar libnotify-program "/usr/bin/notify-send")

(defun notify-send (title message)
  (start-process "notify" " notify"
		 libnotify-program "--expire-time=4000" title message))

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
<<<<<<< HEAD
  (winner-mode 1))

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-char-mode)
;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
=======
  (winner-mode 1)
>>>>>>> db2159d99e251a2ae3bceb530401f67610824827
>>>>>>> b37f57c1523415c7d0195bd9d4f7b0dd03afc58d

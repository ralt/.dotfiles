(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
    '(rainbow-mode
      fill-column-indicator
      clojure-mode
      highlight-indentation
      highlight-symbol
      markdown-mode
      php-mode
      drupal-mode
      php-extras
      dsvn
      sr-speedbar
      magit
      )
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
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" .
	       "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Prevent extraneous tabs
(setq-default indent-tabs-mode nil)

;80 characters ftw
(add-to-list 'default-frame-alist '(width . 80))

;disable annoying GUI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode nil)

;before saving, remove all trailing whitespace (god forbid)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;show column number
(column-number-mode t)
(global-linum-mode t)

(evil-mode)

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

;; drupalcs
(defun my-php-hook-function ()
  (set (make-local-variable 'compile-command)
       (format "phpcs --report=emacs --standard=Drupal %s" (buffer-file-name))))

(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))

(add-hook 'php-mode-hook 'my-php-hook-function)

(require 'ido)
(ido-mode t)

(setq linum-format "%d ")

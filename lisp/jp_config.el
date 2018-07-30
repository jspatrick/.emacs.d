;;colors
;(load-theme 'solarized-dark t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

;;only cycle through buffers with files
(setq bs-cycle-configuration-name "files")

;;turn off the alarm bell
(setq visible-bell 1)
(set-keyboard-coding-system nil)

;;dired setup
(put 'dired-find-alternate-file 'disabled nil)

;; shell setup
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; delete selected text if typing
(delete-selection-mode t)

;; highlight during searching
(setq query-replace-highlight t)

;; highlight incremental search
(setq search-highlight t)

;; Automatically reload files after they've been modified
(global-auto-revert-mode 1)
(put 'upcase-region 'disabled nil)

; fix clipboard issues
(set-clipboard-coding-system (quote x-ctext-unix))
(setq x-select-enable-clipboard t)

;; Supress the GNU startup message
(setq inhibit-startup-message t)


;don't autosave or make backups
(setq make-backup-files nil) ; stop creating those backup~ files
(setq auto-save-default nil) ; stop creating those #auto-save# files

;; No tabs-- use spaces when indenting (doesn't affect Makefiles,
;; does affect text files and code, doesn't affect existing tabs).
;; The use of setq-default means this only affects modes that don't
;; overwrite this setting.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; turn off scroll bar and tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

; show line numbers
(require `linum)
(global-linum-mode 1)

;;--------------------MARKDOWN--------------------
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;--------------------CSS--------------------
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

;;--------------------XML--------------------
(defun nxml-mode-additional-keys ()
    "Key bindings to add to `nxml-mode'."
    (require 'sgml-mode)
    (define-key nxml-mode-map (kbd "M-/") 'nxml-complete)
    (define-key nxml-mode-map (kbd "\t") 'sgml-indent-line)
    )

(add-hook
 'nxml-mode-hook
 'nxml-mode-additional-keys
)

(require 'sgml-mode)
(setq auto-mode-alist
      (append '(("\\.xml" . sgml-mode))
              auto-mode-alist))

;;--------------------MAIL---------------------
(setq mail-user-agent 'message-user-agent)
(setq user-full-name "John Patrick")

;;--------------------ORG-----------------------
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-agenda-files (list "~/todo.org"))


;;--------------------HTML/DJANGO--------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(setq web-mode-engines-alist
      '(("django"    . "\\.html\\'"))
)

;;--------------------JAVASCRIPT----------------
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;--------------------PYTHON--------------------

(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset nil)
 
(defun python-mode-additional-keys ()
    "Key bindings to add to `python-mode'."
    (define-key python-mode-map (kbd "C-c c") 'python-compile)
    (define-key python-mode-map (kbd "C-c t") 'python-pytest)
    )

(add-hook 'python-mode-hook 'python-mode-additional-keys)
(require 'python-django)


;--------------------C++--------------------
(setq-default c-basic-offset 4 c-default-style "linux")
(setq-default tab-width 4 indent-tabs-mode t)
(setq-default c-default-style)


(defun my-c-mode-hook ()
  (setq c-basic-offset 4)
  (setq c-default-style "k&r" c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)

;; get C and C++ editting to work on the proper files
(setq auto-mode-alist
      (append '(("\\.c$" . c++-mode)
                ("\\.h$" . c++-mode)
                ("\\.C$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.tcc$" . c++-mode)
                ("\\.mel$"  . c++-mode)
                ("\\.c[+][+]$" . c++-mode))
              auto-mode-alist))


;yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;buffer extension
(require 'buffer-extension)
(global-set-key (kbd "<f8>") 'copy-buffer-file-name-as-kill)

(require 'misc)


(message "loading home setup")

;OSX specific mods.  Put in an 'if' in case we change home systems
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)
	(setq mac-option-key-is-meta nil)
	(setq mac-command-key-is-meta t)
	(setq mac-command-modifier 'meta)
	(setq mac-option-modifier nil))


(setq user-mail-address "jspatrick@gmail.com")

(require 'flymake-jslint)
;; Make sure we can find the lintnode executable
(setq lintnode-location (concat toolchest-directory "/lintnode"))
;; JSLint can be... opinionated
(setq lintnode-jslint-excludes (list 'nomen 'undef 'plusplus 'onevar 'white))
;; Start the server when we first open a js file and start checking
(add-hook 'js-mode-hook
          (lambda ()
            (lintnode-hook)))


(defun jp-goto-reality-pickem ()
  "go to rp root"
  (interactive "")
  (dired "/Users/jp/dev/reality-pickem/reality-pickem-root/")
  )

(global-set-key (kbd "C-x r") 'jp-goto-reality-pickem)



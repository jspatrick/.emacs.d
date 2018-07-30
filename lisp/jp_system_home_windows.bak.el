(let* ((cygwin-root "c:/cygwin")
	   (cygwin-bin (concat cygwin-root "/bin")))
  (when (and (eq 'windows-nt system-type)
			 (file-readable-p cygwin-root))
    
	(setq exec-path (cons cygwin-bin exec-path))
	(setq exec-path (cons cygwin-bin exec-path))
	(setenv "PATH" (concat cygwin-bin ";" (getenv "PATH")))
    
	;; NT-emacs assumes a Windows shell. Change to bash.
	(setq shell-file-name "bash")
	(setenv "SHELL" shell-file-name) 
	(setq explicit-shell-file-name shell-file-name) 
    
      ;; This removes unsightly ^M characters that would otherwise
      ;; appear in the output of java applications.
      (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)))
    

(require 'cygwin-mount)
(cygwin-mount-activate)
(load "setup-cygwin")

;; (add-hook 'comint-output-filter-functions
;;     'shell-strip-ctrl-m nil t)
;; (add-hook 'comint-output-filter-functions
;;     'comint-watch-for-password-prompt nil t)
;; (setq explicit-shell-file-name "bash.exe")
;; ;; For subprocesses invoked via the shell
;; ;; (e.g., "shell -c command")
;; (setq shell-file-name explicit-shell-file-name)

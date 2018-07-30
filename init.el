;;test
;;;;;;;;;John Patrick's emacs config file;;;;;;;;;;
;; This initailization is split into several phases.

;; The config loads and configures various modes and settings.
;; The tools phase loads my custom global functions
;; The keybindings phase loads all custom keybindings

;; Finally, I have different tools and settings depending upon where I'm working.
;; I use jp_system_SYSTEMNAME.el files for system-specific/site-specifc setup.

; add ~/.emacs.d directory to load-path

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(require 'package)
(package-initialize)
(add-to-list 'package-archives 
			 '("melpa-stable". "http://stable.melpa.org/packages/") t)

;; Bug fix for package 
(defadvice package-compute-transaction
  (before package-compute-transaction-reverse (package-list requirements) activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements)
)

(when (< emacs-major-version 24)
  ;;For important compatability libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages"))
)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; The root toolchest directory
(defvar toolchest-directory "~/toolchest")


(load "jp_config")
(load "jp_tools")
(load "jp_keybindings")

;--------------------LOAD SYSYTEM-SPECIFIC CONFIG--------------------

;; Get the name of our current system.
(defvar my-system-name 
  (cond ((memq system-type '(gnu/linux)) "home_linux")
		;;Are we in OSSX
		((memq system-type '(darwin)) "home_osx")
		;;Are we in Windows?
		((memq system-type '(windows-nt)) "home_windows")
		))
;; Throw an error if we can't ID the current system
(if (eq my-system-name nil)
	(throw "no system" "Cannot identify the current system")
)

  
;; Find the load file and load it
(let ((system-load-file (concat "jp_system_" my-system-name )))
  (if (locate-file system-load-file load-path load-suffixes)	  
	  (progn
		(message (concat "loading: " system-load-file))
		(load system-load-file)
		)

	)
)

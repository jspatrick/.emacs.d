;;----------SPI STUFF-------------------
(message "loading spi setup")

(setq user-mail-address "jspatrick@imageworks.com")
;; Commands for working with MEL files
(defun grep-for-procs()
 "Grep through directory for mel procedure commands"
 (interactive)
 (grep "grep -n -e \"[[:space:]]proc[[:space:]]\" *.mel"))

(defvar maya-version-by-show 
  '(("htr" . "2012") ("smf" . "2009") ("cl2" . "2013") ("kil" . "2013"))
  "show versions")

(defvar maya-version 
  "2013"
  "Maya version used")

(defvar current-show
  "lon"
  "The show Im working on")

  ;;-------------CUE3 mode!
(require 'cue3)
(setq cue3-search-history 
      (cons 
       (concat 
        current-show 
        "-.*((s|z|plate)render|lfxprep|chopit|convert|split|sceneprep|jspatrick)") 
       cue3-search-history)
)


(defun get-maya-version(&optional show)
  "Return the maya version string for show"
  (let ((tmp (assoc show maya-version-by-show)))
    (if tmp
        (cdr tmp)
      maya-version)))


(defun get-file-type-dir(ftype)
    (interactive 
     (list
      (completing-read "Chose one: " '("py" "mpy" "mel"))))

    (let ((showDir (concat "/shots/" current-show "/home/dev/jspatrick/")))

      (if (equal ftype "com")
          (concat showDir "python/common/"))

      (if (equal ftype "mpy")
          (concat showDir "maya" maya-version "_prod/python/common/"))
      )
)
           

(defun find-file-py (fpath)
    "Open file in pyCom"
    (interactive (list 
                  (read-file-name "File:" (concat "/shots/" current-show "/home/dev/jspatrick/python/common/"))))
    (find-file fpath)
)


(defun jp-dired-mel (&optional show &optional live)
 "Open the desired dev area"
  (interactive "sEnter a show (spi):\nsLive Mode? (n):")
  (let ((devtmp "dev/jspatrick/"))

    (if (equal (length show) 0)
        (setq show "spi"))
    
    (if (equal live "y")
        (setq devtmp ""))
    
    (dired (concat "/shots/" show "/home/" devtmp "maya" 
                   (get-maya-version show) "_prod/scripts/tools"))))

(defun jp-dired-char (&optional live &optional char)
  "Open the desired dev area"
  (interactive "sLive Mode? (n):\nsChar:")
  (let ((devtmp "dev/jspatrick/")
        (show current-show))
    
    (if (equal live "y")
        (setq devtmp ""))
    
    (dired (concat "/shots/" show "/home/" devtmp "maya" 
                   (get-maya-version show) "_prod/scripts/chars/" char))))


(defun jp-dired-py (&optional show &optional live)
 "Open the desired dev area"
  (interactive "sEnter a show (spi):\nsLive Mode? (n):")
  (let ((devtmp)
        (path))

    (if (equal (length show) 0)
        (setq show "spi"))
    
    (if (equal live "y")
        (setq devtmp "")
      (setq devtmp "dev/jspatrick/"))

    (setq path (concat "/shots/" show "/home/" devtmp 
                             "maya" (get-maya-version show) "_prod/python/common"))

    (dired path)
    )
  )

(defun jp-dired-pycom (&optional show &optional live)
  "Dired to the python/common area"
  (interactive "sEnter a show (spi):\nsLive Mode? (n):")
  (let ((devtmp "dev/jspatrick/"))

    (if (equal (length show) 0)
        (setq show "spi"))
    
    (if (equal live "y")
        (setq devtmp ""))
    (dired (concat "/shots/" show "/home/" devtmp "python/common"))))

(defun spawn-setshot-shell (name)
  "Invoke shell setshot to the name"pp
  (interactive "MShow of shell buffer to create: ")
  (let ((setshotCommand (concat "setshot " name "/dev.home")))
    (pop-to-buffer (get-buffer-create name))
    (shell (current-buffer))
    (process-send-string nil (concat "echo '" setshotCommand "'\n"))
    )
  )

;shortcuts
(global-set-key (kbd "M-j") 'cue3-list-jobs)
(global-set-key (kbd "C-x m") 'jp-dired-mel)
(global-set-key (kbd "C-x m") 'jp-dired-mel)
(global-set-key (kbd "C-x p") 'jp-dired-py)
(global-set-key (kbd "C-x c") 'jp-dired-char)
(global-set-key (kbd "C-x P") 'jp-dired-pycom)

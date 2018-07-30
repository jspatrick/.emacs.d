(ffap-bindings)
(global-set-key (kbd "C-c f") 'ffap-other-window)
(global-set-key (kbd "C-<tab>") 'ac-start)

(global-set-key (kbd "C-c 3") 'comment-region)
(global-set-key (kbd "C-c #") 'uncomment-region)


(global-set-key (kbd "M-C-<") 'bs-cycle-next)
(global-set-key (kbd "M-C->") 'bs-cycle-previous)
;; todo: customize the cycle configuration to visit shell, python, etc

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-s") 'replace-string)
(global-set-key (kbd "M-r") 'replace-regexp)
(global-set-key (kbd "C-x M-r") 'search-forward-regexp)
(global-set-key (kbd "M-B") 'rename-buffer)
(global-set-key (kbd "C-x j") 'python-django-open-project)

(global-set-key (kbd "C-c C-p") 'python-shell-switch-to-shell)


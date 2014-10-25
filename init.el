; Helpful python emacs setup:
; http://www.youtube.com/watch?v=0cZ7szFuz18&list=WLRWY_nnLzduOW16lec5L-ssd31pUij1nE

;;For emacsclient
(require 'server)
(or (server-running-p)
    (server-start))

;; (add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/lisp")
(setq inhibit-splash-screen t)

;;transparency
(if window-system 
(set-frame-parameter (selected-frame) 'alpha '(90 90)))
(add-to-list 'default-frame-alist '(alpha 90 90))

(if (daemonp)
    (add-hook 'before-make-frame-hook
	 (lambda () (set-frame-parameter (selected-frame) 'alpha '(90 90)))
	 (lambda () (add-to-list 'default-frame-alist '(alpha 90 90)))))

;;automatically revert modified buffers
(global-auto-revert-mode 1)

;;copy and paste to osx clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;spaces instead of tabs
(setq-default indent-tabs-mode nil)

(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-chunksize 500)

;;use unix style line endings
(setq default-buffer-file-coding-system 'utf-8-unix)

;;show me the column number
(setq column-number-mode t)

;;Kill toolbar, menubar and scrollbar
(if (boundp tool-bar-mode)
    (tool-bar-mode -1))
(if (boundp menu-bar-mode)
    (menu-bar-mode -1))
(if (boundp scroll-bar-mode)
    scroll-bar-mode -1)


;; Turn off beep
(setq visible-bell nil)
;;turn off all alarms
(setq ring-bell-function 'ignore)

;;backups should go to one directory
;;so as not to pollute a source tree
(setq backup-directory-alist `(("." . "~/.saves")))

;A function to only install packages that are not already installed
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)));

;A list of things to install
;A single quote tells emacs not to evaluate the expression inside the list
(setq to-install '(python-mode auto-complete jedi autopair))

(require 'package)
(package-initialize)
;;;;Takes too long to do this every start up.
;;Required for Jedi.el
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
             
;;Required for python-mode
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
             
;; (package-refresh-contents)

;execute the install-if-needed function on every package in the 
;to-install-list
(mapc 'install-if-needed to-install)

;;invoke buffer menue instead of list-buffers
;;so that the buffer window doesn't override my other buffer frames
(global-set-key "\C-x\C-b" 'buffer-menu)

;Load the installed packages
(require 'python-mode)
(require 'auto-complete)
(require 'autopair)
(require 'flymake)


(setq
 ac-auto-start 2
 ac-override-local-map nil
 ac-use-menu-map t
 ac-candidate-limit 20)

;;================Latex================
(add-to-list 'auto-mode-alist ' ("\\.tex\\'" . LaTeX-mode))
(add-hook 'LaTeX-mode-hook '(flyspell-mode t))

;;================Flyspell================
(dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

;;================CMake================
;; (setq load-path (cons (expand-file-name "/dir/with/cmake-mode") load-path)) 
(load "cmake-mode.el")
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;;================Python================
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(setq py-electric-colon-active t);not sure what this does check
(add-hook 'python-mode-hook 'autopair-mode)
(add-hook 'python-mode-hook 'flymake-activate)
(add-hook 'python-mode-hook 'auto-complete-mode)

;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly. With the same interpreter you're using.

;; if you need to change your python intepreter, if you want to change it
;; (setq jedi:server-command
(add-hook 'python-mode-hook
         (lambda ()
         (jedi:setup)
         (jedi:ac-setup)
            (local-set-key "\C-cd" 'jedi:show-doc)
            (local-set-key (kbd "M-SPC") 'jedi:complete)
            (local-set-key (kbd "M-.") 'jedi:goto-definition)))

;;================Fonts===============
;; (set-default-font
;;  "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
;; (setq default-frame-alist '((font . "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")))
(set-face-attribute 'default nil :height 140)

;; Flymake settings for Python
(defun flymake-python-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(defun flymake-activate ()
  "Activates flymake when real buffer and you have write access"
  (if (and
       (buffer-file-name)
       (file-writable-p buffer-file-name))
      (progn
        (flymake-mode t)
        ;; this is necessary since there is no flymake-mode-hook...
        (local-set-key (kbd "C-c n") 'flymake-goto-next-error)
        (local-set-key (kbd "C-c p") 'flymake-goto-prev-error))))

;;================C++===============
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.txx\\'" . c++-mode))

(defun my-c-mode-common-hook ()
  (setq c-default-style "linux"
	c-basic-offset 2
	c-indent-level 2
	indent-tabs-mode nil)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;from http://stackoverflow.com/questions/19532430/emacs-auto-complete-clang
;;forl clang autocomplete
;; (require 'yasnippet)
;; (yas-global-mode 1)

(require 'auto-complete-config)
(ac-config-default)
(require 'auto-complete-clang)

;;================Misc===============
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

;;================EMACS BASH COMPLETION===================
(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
  'bash-completion-dynamic-complete)

;;================PYTHON VIRTUAL ENVIRONMENT==============
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.virtualenvs")

;;work on python2 by default
(venv-workon "env1")
;;show the venv on the mode line
(setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))

;;================DOCVIEW SETTINGS==============
(setq doc-view-continuous 1)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(load-theme 'wombat t)

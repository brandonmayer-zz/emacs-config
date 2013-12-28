; Helpful python emacs setup:
; http://www.youtube.com/watch?v=0cZ7szFuz18&list=WLRWY_nnLzduOW16lec5L-ssd31pUij1nE

(add-to-list 'load-path "~/.emacs.d/")
(setq inhibit-splash-screen t)

;;use unix style line endings
(setq default-buffer-file-coding-system 'utf-8-unix)

;;Kill toolbar
(tool-bar-mode -1)

;;auto-hide the menue bar
;;TODO: Not working on my windows box.
(load "active-menu.el")
(require 'active-menu)
;; (autoload 'active-menu
;;            "active-menu"
;;            "Show menu only when mouse is at the top of the frame."
;;            t)
(menu-bar-mode -99)

(require 'package)
(package-initialize)
;;;;Takes too long to do this every start up.
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/"))
             
;; ;;Required for python-mode
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
             
;; (package-refresh-contents)

;A function to only install packages that are not already installed
(defun install-if-needed (package)
  (unless (package-installed-p package)
    (package-install package)));

;A list of things to install
;A single quote tells emacs not to evaluate the expression inside the list
(setq to-install '(python-mode auto-complete jedi autopair))

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

;;================IPython===============
;;Not working up to snuff yet
(require 'ipython)
(setq
 python-shell-interpreter "C:\\Python27\\Scripts\\ipython.exe"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;;yanked from http://www.jesshamrick.com/2012/09/18/emacs-as-a-python-ide/
; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
;; ; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;; ; try to automagically figure out indentation
;; (setq py-smart-indentation t)


;; ;; Jedi settings
(require 'jedi)
;; It's also required to run "pip install --user jedi" and "pip
;; install --user epc" to get the Python side of the library work
;; correctly.
;; With the same interpreter you're using.

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
(set-default-font
 "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")

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

; Turn beep off
(setq visible-bell nil)








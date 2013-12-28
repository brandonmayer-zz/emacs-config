;;                                                     -*- Emacs-Lisp -*-

;;; active-menu.el --- Show menubar only when the mouse is at the top of
;;;                    the frame.

;;
;; Copyright (C) 
;; 2002-2007 Claus Brunzema <mail@cbrunzema.de>
;;           Stefan Kamphausen <http://www.skamphausen.de>
;; 2007      Stefan Kamphausen <http://www.skamphausen.de>
;;           Claus Brunzema <mail@cbrunzema.de>
;; Version: 2.0.0
;; $Id:  $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; -----------------------------------------------------------------------


;;; Commentary:

;; Prerequisites:
;;
;; We made active-menu with the following emacsen:
;; XEmacs Value: 21.4 (patch 20) "Double Solitaire" XEmacs Lucid
;; Gnu Emacs 21.4.1
;; Please let us know if other versions work.


;; Installation:
;;
;; put this file in your load-path and the following in your init
;; file (~/.emacs or ~/.xemacs/init.el) if you want to use the
;; customisation facility:

;; (require 'active-menu)
;;
;; If you turn active-menu on and off frequently, you might want to use
;;
;; (autoload 'active-menu
;;           "active-menu"
;;           "Show menu only when mouse is at the top of the frame."
;;           t)
;;
;; instead. Now you can toggle active-menu with M-x active-menu. There
;; are two different backends available one uses timers to check the
;; current mouse position and one tracks the mouse movements.  The
;; former is probably better suited for Gnu Emacs whereas the latter
;; has been in daily use for years in various versions of XEmacs and
;; can be considered stable there.

;; If your frames change the height when the menu is toggled, try to
;; find a suitable value for `active-menu-frame-compensation'. If your
;; lines keep jumping up and down adjust
;; `active-menu-scroll-compensation'.



;; History:
;; 2007-03/04  Stefan Kamphausen (mostly)
;;         * Friendly fork/take-over.
;;         * Gnu Emacs compat.
;;         * Totally new timer backend.
;;         * Code-reorg and renaming for both backends.
;;         * New Webhome
;;         * Scroll compensation
;;         * Version 2.0.0
;; 2002-07-13  Claus Brunzema
;;         * More customisation stuff
;;         * Version 1.0.0
;; 2002-07-12  Claus Brunzema
;;         * Restricted toggling of the menubar to the
;;           currently selected frame
;;         * Customisation fixes
;;         * Redraw fixes
;; 2002-06-15  Claus Brunzema
;;         * Small cleanups
;; 2002-06-07  Claus Brunzema
;;         * Fixed and documented the require-way
;; 2002-06-06  Claus Brunzema
;;         * Code cleanup.
;;         * Version 0.9.7 released into public
;; 2002-06-02  Claus Brunzema
;;         * Frame compensation
;; 2002-05-30  Claus Brunzema
;;         * Big rewrite by Claus Brunzema
;; 2002-05-28  Stefan Kamphausen
;;         * Idea and initial implementation

;; ToDo:
;;
;; - save and restore menubar-visible-p
;; - maybe show different menus if you hit different areas on the top
;;   border.

;; Bugs and Ugliness:
;;
;; - The menubar stays visible if you pop up some menus and click
;;   outside all menus to cancel any selection. It is removed after
;;   the first mouse move or key press.
;; - Interaction with accelerate-menu looks, er, strange (but you can
;;   get used to it).
;; - Gnu Emacs with tracking does not yet have the
;;   post-command-function 
;; - Gnu Emacs with tracking showed an error once when wiggling the
;;   mouse quickly. 
;; - See FIXMEs in the code below.
;; - XEmacs reduces the size of the top window when removing the
;;   menubar but does not increase it again when showing it.  This
;;   leads to a shrinking top window when using active menu
;;   repeatedly. 
;; - Gnu Emacs calculates (mouse-pixel-position) relativ to the
;;   selected frame whereas XEmacs calculates it relative to the
;;   window the mouse is in.  If that were not the case we could just
;;   use that function everywhere.
;; - Mouse events in Gnu Emacs take care of the fringe wheras
;;   `mouse-pixel-position' does not.  Try something like the
;;   following in Gnu Emacs and watch the echo area
;;  (defun synthesize-mouse-motion-event ()
;;    (let* ((mouse-pos (mouse-pixel-position))
;;           (mouse-x (cadr mouse-pos))
;;           (mouse-y (cddr mouse-pos))
;;           (buffer-pos (point)))
;;      (list 'mouse-movement
;;            (list
;;             (selected-window)
;;             buffer-pos
;;             (cons mouse-x mouse-y)
;;             nil))))
;;  (defun display-mouse-information (event)
;;    (interactive "e")
;;    (message "%s  -- %s" event (synthesize-mouse-motion-event)))
;;  (setq track-mouse t)
;;  (define-key global-map [mouse-movement] 'display-mouse-information)
;; - But then... the mouse event in Gnu Emacs has it's y-coordinate
;;   relative to the window, which does not match the behaviour of
;;   `mouse-pixel-position'.
;; - At least in Gu Emacs there seems to be no way of finding out the
;;   whether the mouse has left the frame or whether it is on the menu
;;   bar.
;; - From reading newsgroup postings I (SK) get the impression that
;;   using  [mouse-movement]-binding is not the way to go.   That's
;;   why I started the timer-backend in the first place.  But
;;   then... the tracking backend seems to work quite well.

;;; Code:

(require 'timer)

;; ===================================================================
;; Customization
;; ===================================================================
(defgroup active-menu nil
  "Show menubar only if the mouse is at the top of the frame."
  :link '(url-link :tag "Homepage" 
                   "http://www.skamphausen.de/cgi-bin/ska/active-menu")
  :link '(emacs-commentary-link :tag "Commentary in active-menu.el"
                                "active-menu.el")
  :prefix "active-menu-"
  :group 'mouse
  :group 'gui)

(defcustom active-menu-activated nil
  "*When t, active-menu is activated."
  :type 'boolean
  :initialize #'(lambda (symbol value)
                  (setq active-menu-activated nil))
  :set #'(lambda (symbol value)
           (if value
               (turn-on-active-menu)
             (turn-off-active-menu)))
  :group 'active-menu)

(defcustom active-menu-backend 'track
  "*The backend to use for active menu (timer or track).
In Gnu Emacs creating motion events for mouse movement seems not very
common.  But the same has been in daily use in XEmacs for years now
and works like charm.  Thus, you get the choice: use the track backend
which tracks mouse movement or use the timer backend which checks for
the mouse position every now and then (`active-menu-timeout')."
  :type '(choice (const timer) (const track))
  :group 'active-menu)

(defcustom active-menu-frame-compensation 1
  "*When the menubar is activated, the frame is shrinked by that many
  lines to compensate for the automatic expansion. This is, I admit
  it, an ugly hack. There has to be a way to calculate this number
  automatically. If you know how to do it, send mail to
  mail@cbrunzema.de quick, please."
  :type 'integer
  :group 'active-menu)

(defcustom active-menu-scroll-compensation 2
  "*Adjust this to avoid you current line jumping up and down."
  :type 'integer
  :group 'active-menu)


(defcustom active-menu-sensitive-pixels 6
  "*The pixelrange of the sensitive area for active-menu."
  :type 'integer
  :group 'active-menu)


(defcustom active-menu-timeout 0.5
  "*When using the timer backend this gives the repitition time."
  :type 'number
  :group 'active-menu)


;; ===================================================================
;; Variables
;; ===================================================================
(defvar active-menu-xemacs-p (featurep 'xemacs))

;; ===================================================================
;; Compatibility
;; ===================================================================
(if active-menu-xemacs-p
  (progn
    ;; XEmacs
    (defun active-menu-redraw-frame (frame no-preempt)
      (redraw-frame frame no-preempt))
    (defun active-menu-menubar-visiple-p (frame)
      (specifier-instance menubar-visible-p frame))
    (defun active-menu-make-menubar-visible (frame)
      (set-specifier menubar-visible-p t frame))
    (defun active-menu-make-menubar-invisible (frame)
      (set-specifier menubar-visible-p nil frame))
    (defun active-menu-make-motion-event ()
      (mouse-position-as-motion-event))
    (defun active-menu-need-to-show-menubar-p (&optional event)
      (let ((event (or event (mouse-position-as-motion-event))))
        (and (or
               (and (motion-event-p event)
                    (<= (event-y-pixel event) active-menu-sensitive-pixels))
               (not event)))))
            ;; need to check for focus in emacs here)))))
    )
  (progn
    ;; Gnu Emacs
    (defun active-menu-menubar-visiple-p (frame)
      (> (frame-parameter frame 'menu-bar-lines) 0))
    (defun active-menu-redraw-frame (frame no-preempt)
      (redraw-frame frame))
    (defun active-menu-make-menubar-visible (frame)
      (modify-frame-parameters frame 
                               (list
                                (cons 'menu-bar-lines 1))))
    (defun active-menu-make-menubar-invisible (frame)
      (modify-frame-parameters frame 
                               (list
                                (cons 'menu-bar-lines 0))))
    ;; FIXME: is this correct?
    (defun active-menu-make-motion-event ()
      (let* ((mouse-pos (mouse-pixel-position))
         (mouse-x (cadr mouse-pos))
         (mouse-y (cddr mouse-pos))
         (buffer-pos (point)))
    (list 'mouse-movement
          (list
           (selected-window)
           buffer-pos
           (cons mouse-x mouse-y)
           nil))))
    (defun active-menu-need-to-show-menubar-p (&optional event)
      (let* ((event (or event (active-menu-make-motion-event)))
             (y-pos (cdr (posn-x-y (second event))))
             (window-top (second (window-edges (posn-window (second event))))))
        (and
          (zerop window-top)
          (<= y-pos active-menu-sensitive-pixels))))
    ))


;; ===================================================================
;; Internal (Common) Functions
;; ===================================================================
(defun active-menu-show-menubar ()
  "Show the menubar."
  (let ((frame (selected-frame)))
    (unless (active-menu-menubar-visiple-p frame)
      (active-menu-make-menubar-visible frame)
      (set-frame-height frame (- (frame-height)
                                 active-menu-frame-compensation))
      (ignore-errors 
    (scroll-up active-menu-scroll-compensation))
      (active-menu-redraw-frame frame t))))

(defun active-menu-hide-menubar ()
  "Hide the menubar."
  (let ((frame (selected-frame)))
    (when (active-menu-menubar-visiple-p frame)
      (active-menu-make-menubar-invisible frame)
      (set-frame-height frame
                        (+ (frame-height) active-menu-frame-compensation))
;      (ignore-errors 
;   (scroll-down active-menu-scroll-compensation))
      (active-menu-redraw-frame frame t))))


;; ===================================================================
;; The Timer Backend
;; ===================================================================

(defvar active-menu-timer nil
  "Timer to use to check the mouse position.")

(defun active-menu-timer-function ()
  "Function to be called by timer event.
This will check the mouse position and then show the menubar or hide
it depending on the mouse position."
  (if (active-menu-need-to-show-menubar-p)
    (progn
      (active-menu-show-menubar))
    (progn
      (active-menu-hide-menubar)))
  (active-menu-timer-start))

(defun active-menu-cancel-timer ()
  (when active-menu-timer
    (progn (cancel-timer active-menu-timer)
           (setq active-menu-timer nil))))

(defun active-menu-timer-start ()
  "Start active menu using the timer backend.
Don't use this."
  (interactive)
  (active-menu-cancel-timer)
  (setq active-menu-timer (run-with-timer 
               active-menu-timeout 
               nil 
               'active-menu-timer-function)))


(defun active-menu-timer-stop ()
  "Stop the timer backend of active menu.
Don't use this."
  (interactive)
  (active-menu-cancel-timer)
  (active-menu-show-menubar))


;; ===================================================================
;; The Track Backend
;; ===================================================================

(defvar active-menu-original-mouse-motion-handler nil)

(defun active-menu-track-menubar-maybe-show (event) 
  "Hide or show menubar according to the mouse position."
  (interactive "e")
  (if (active-menu-need-to-show-menubar-p event)
      (active-menu-show-menubar)
    (active-menu-hide-menubar)))


(defun active-menu-track-post-command-hook-function ()
  ;; this is needed to eventually remove the menu after an item
  ;; is selected (and the mouse isn't moved after the click).
  ;; FIXME: can we remove this function from the hook after calling
  ;; it?
  ;; FIXME is this correct in Emacs?
  (active-menu-track-menubar-maybe-show
   (active-menu-make-motion-event)))

(defun active-menu-track-mouse-motion-handler (event)
  (when (motion-event-p event)
    (active-menu-track-menubar-maybe-show event))
  (funcall active-menu-original-mouse-motion-handler event))

(defun active-menu-track-install-handler-and-hook ()
  "Install mouse handler and post-command-hook for active-menu.
Don't use this, use `turn-on-active-menu' instead.
This function is only used for XEmacs."
  (setq active-menu-original-mouse-motion-handler mouse-motion-handler)
  (setq mouse-motion-handler #'active-menu-track-mouse-motion-handler)
  (add-hook 'post-command-hook
            #'active-menu-track-post-command-hook-function)
  (active-menu-track-menubar-maybe-show (mouse-position-as-motion-event))
  (setq active-menu-activated t))

(defun active-menu-track-remove-handler-and-hook ()
  "Remove mouse handler and post-command-hook for active-menu.
Don't use this, use `turn-off-active-menu' instead"
  (setq mouse-motion-handler active-menu-original-mouse-motion-handler)
  (remove-hook 'post-command-hook
               #'active-menu-track-post-command-hook-function)
  (active-menu-show-menubar)
  (setq active-menu-activated nil))


(defun active-menu-track-start ()
  "Start the track-backend of active menu.
Don't use this."
  (interactive)
  (unless active-menu-activated
    (if active-menu-xemacs-p
        (active-menu-track-install-handler-and-hook)
      (progn
    ;; FIXME store old settings?
        (setq track-mouse t)
        (define-key global-map [mouse-movement] 
          'active-menu-track-menubar-maybe-show)
        (setq active-menu-activated t)))))

(defun active-menu-track-stop ()
  "Stop the track-backend of active menu.
Don't use this."
  (interactive)
  (when active-menu-activated
    (if active-menu-xemacs-p
        (active-menu-track-remove-handler-and-hook)
      (progn
    ;; FIXME: restore old settings? see follow-mouse.el
        (setq track-mouse nil)
        (global-unset-key [mouse-movement])
        (active-menu-show-menubar)
        (setq active-menu-activated nil)))))

;; ===================================================================
;; The User Interface
;; ===================================================================
(defun turn-on-active-menu ()
  "Turn on active-menu (you guessed it)."
  (interactive)
  (cond
   ((eq active-menu-backend 'timer)
    (active-menu-timer-start))
   ((eq active-menu-backend 'track)
    (active-menu-track-start))
   (t
    (error "Invalid backend for active menu"))))

(defun turn-off-active-menu ()
  "Turn on active-menu (you guessed it)."
  (interactive)
  (cond
   ((eq active-menu-backend 'timer)
    (active-menu-timer-stop))
   ((eq active-menu-backend 'track)
    (active-menu-track-stop))
   (t
    (error "Invalid backend for active menu"))))

;;;###autoload
(defun active-menu (&optional arg)
  "Toggle active menu.
With arg, turn active-menu on iff arg is positive."
  (interactive "P")
  (if (or (and arg (> (prefix-numeric-value arg) 0))
          (and (null arg) (null active-menu-activated)))
      (turn-on-active-menu)
    (turn-off-active-menu)))

(provide 'active-menu)

;;; active-menu.el ends here

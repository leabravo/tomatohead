;;; tomatohead.el --- Super Simple Pomodoro!

;; Copyright (c) 2017 Leandro Bravo

;; Author: Leandro Bravo <leabravo@gmail.com>
;; Version: 0.1
;; Keywords: tools, convenience
;; URL: https://github.com/leabravo/tomatohead

;; Package-Requires: ((emacs "24"))

;;; Commentary:

;; This is a simple implementation of a Pomodoro Timer which uses the
;; header for visualizing progress.
;; Usage M-x tomatohead-mode

;;; Code:


(eval-when-compile (require 'cl))


(defgroup tomatohead nil
  "Group for TomatoHead customizations."
  :group 'frames)

(defcustom tomatohead-num 1
  "Qty of iterations."
  :group 'tomatohead)

(defcustom tomatohead-work 1500
  "Amnt of work time."
  :group 'tomatohead)

(defcustom tomatohead-break 600
  "Amnt of break time."
  :group 'tomatohead)

(defcustom tomatohead-lgbreak 900
  "Amnt of long break."
  :group 'tomatohead)

(defcustom tomatohead-pomonum 4
  "Amnt of Pomodoros."
  :group 'tomatohead)

(defcustom tomatohead-pomoatm 0
  "Current Pomodoro."
  :group 'tomatohead)

(defcustom tomatohead-perc 0
  "Percentaje of current char.")

(defcustom tomatohead-timer nil
  "Default nil timer.")


(defun tomatohead-qty-of-chars (iter time)
  "Quantity of characters to display.
A = 'window-total-width' function returns the width of the
window in characters,
B = '(/ (* ITER 1.0) TIME)' calculates the percentage of
the current iteration respect to the Pomodoro time.
A times B gives the % of the window (in chars).  Truncate
to fetch only the integer part."
  (truncate (* (/ (* iter 1.0) time) (window-total-width))))


(defun tomatohead-perc-of-char (iter chars time)
  "Substract the non decimal part from the current percentaje of.
completion in order to get the current percentaje of the char-
-acter processed i.e 3.76 - 3 = .76 of the current char compl-
-eted (times 100 to make int) ITER CHARS TIME"
  (truncate (* 100 (- (* (/ (* iter 1.0) time) (window-total-width)) chars))))


(defun tomatohead-work ()
    "TODO add parameter for time and times 60 it."
    (setq tomatohead-perc (tomatohead-perc-of-char tomatohead-num (tomatohead-qty-of-chars tomatohead-num tomatohead-work) tomatohead-work))
    (setq current-char
          (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 25))
                 "")
                ((and (> tomatohead-perc 25) (<= tomatohead-perc 50))
                 "░")
                ((and (> tomatohead-perc 50) (<= tomatohead-perc 75))
                 "▒")
                ((and (> tomatohead-perc 75) (<= tomatohead-perc 100))
                 "▓")
                ))

    (setq header-line-format
          ;; Create a string of squares and append the current char at the end.
          (concat (make-string (if (= (tomatohead-qty-of-chars tomatohead-num tomatohead-work) 0)
                                   0
                                 (tomatohead-qty-of-chars tomatohead-num tomatohead-work))
                               ?█)
                  current-char))

    (setq tomatohead-num (+ 1 tomatohead-num))
    (force-mode-line-update)
    ;; Check if work pomodoro is finished.
    (if (eq 1500 tomatohead-num)
        (progn
          ;; Add one to the completed pomodoros counter.
          (setq tomatohead-pomoatm (+ tomatohead-pomoatm 1))
          ;; Check if we reached the last pomodoro.
          (if (eq tomatohead-pomonum tomatohead-pomoatm)
              (progn
                (cancel-timer tomatohead-timer)
                (setq tomatohead-perc 100)
                (setq tomatohead-num (- tomatohead-lgbreak 1))
                (set-face-attribute 'header-line nil
                                    :background "#000000"
                                    :foreground "#FFFFFF")
                (setq tomatohead-timer
                      (run-at-time "0 sec" 1 'tomatohead-long-break)))
            ;; If it was not the last pomodoro, perform a short break.
            (progn
              (cancel-timer tomatohead-timer)
              (setq tomatohead-perc 100)
              (setq tomatohead-num (- tomatohead-break 1))
              (set-face-attribute 'header-line nil
                                  :background "#65000B"
                                  :foreground "#55AB55")
              (setq tomatohead-timer
                    ;; Starting at zero seconds, each second.
                    (run-at-time "0 sec" 1 'tomatohead-break)))))))


(defun tomatohead-break ()
  "BREAK!."
  (setq tomatohead-perc (tomatohead-perc-of-char tomatohead-num (tomatohead-qty-of-chars tomatohead-num tomatohead-break) tomatohead-break))
  (setq current-char
        (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 25))
               "")
              ((and (> tomatohead-perc 25) (<= tomatohead-perc 50))
               "░")
              ((and (> tomatohead-perc 50) (<= tomatohead-perc 75))
               "▒")
              ((and (> tomatohead-perc 75) (<= tomatohead-perc 100))
               "▓")
              ))
  
  (setq header-line-format
        (concat (make-string (if (= (tomatohead-qty-of-chars tomatohead-num tomatohead-break) 0)
                                 0
                               (tomatohead-qty-of-chars tomatohead-num tomatohead-break))
                             ?█)
                current-char))

  (setq tomatohead-num (- tomatohead-num 1))
  (force-mode-line-update)
  (if (eq tomatohead-num 0)
      (progn
        (cancel-timer tomatohead-timer)
        (setq tomatohead-perc 0)
        (setq tomatohead-num 1)
        (set-face-attribute 'header-line nil
                            :background "#65000B"
                            :foreground "#DF3232")
        (setq tomatohead-timer
              ;; Starting at zero seconds, each second.
              (run-at-time "0 sec" 1 'tomatohead-work)))))


(defun tomatohead-long-break ()
  "Long BREAK!."
  (setq tomatohead-perc (tomatohead-perc-of-char tomatohead-num (tomatohead-qty-of-chars tomatohead-num tomatohead-lgbreak) tomatohead-lgbreak))
  (setq current-char
        (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 25))
               "")
              ((and (> tomatohead-perc 25) (<= tomatohead-perc 50))
               "░")
              ((and (> tomatohead-perc 50) (<= tomatohead-perc 75))
               "▒")
              ((and (> tomatohead-perc 75) (<= tomatohead-perc 100))
               "▓")
              ))

  (setq header-line-format
        (concat (make-string (if (= (tomatohead-qty-of-chars tomatohead-num tomatohead-lgbreak) 0)
                                 0
                               (tomatohead-qty-of-chars tomatohead-num tomatohead-lgbreak))
                             ?█)
                current-char)) ;; Make a string out of the qty. of current-char.

  (setq tomatohead-num (- tomatohead-num 1))
  (force-mode-line-update)
  (if (eq tomatohead-num 0)
      (progn (cancel-timer tomatohead-timer)
             (setq tomatohead-pomoatm 0) ;; Reset the counter back to 0
             (setq header-line-format nil)
             (tomatohead-mode -1))))


(defun tomatohead-start ()
  "Start the Pomodoro specifying WORK and BREAK."
  (set-face-attribute 'header-line nil
                      :background "#65000B"
                      :foreground "#DF3232")
  
  (setq tomatohead-timer
        (run-at-time "0 sec" 1 'tomatohead-work)))


;;;###autoload
(define-minor-mode tomatohead-mode
  "Tomatohead pomodoro"
  :global t
  :group 'tomatohead
  (if tomatohead-mode
      (tomatohead-start)
    (if tomatohead-timer
        (progn
          (cancel-timer tomatohead-timer)
          (setq header-line-format nil)
          (setq tomatohead-mode nil)
          (setq tomatohead-pomonum 0)
          (setq tomatohead-perc 0)
          (setq tomatohead-num 0)
          (setq tomatohead-pomoatm 0)))))

(provide 'tomatohead)


;;; tomatohead.el ends here

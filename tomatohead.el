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

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:


(eval-when-compile (require 'cl))


(defgroup tomatohead nil
  "Group for TomatoHead customizations."
  :group 'applications)

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
  "Percentage of current char.")

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
  "Fetch the decimal part of the number representing the elapsed percentage.
This function calculates the ratio between the current ITER and the total
timer and scales the 'window-total-width' (CHARS)  with the obtained number.
Then by discarding the integer part of it, we can deduce the fraction of the
elapsed TIME multiplying it by 100 to make an integer."
  (truncate (* 100 (- (* (/ (* iter 1.0) time) (window-total-width)) chars))))

(defface tomatohead-while-working
  '((t :background "#65000B"
       :foreground "#DF3232"))
  "Header line Face for the current Pomodoro work session."
  :group 'tomatohead)

(defface tomatohead-while-sbreak
  '((t :background "#65000B"
       :foreground "#55AB55"))
  "Header line Face for the current Pomodoro short break."
  :group 'tomatohead)

(defface tomatohead-while-lbreak
  '((t :background "#00005F"
       :foreground "#0087AF"))
  "Header line Face for the current Pomodoro long break."
  :group 'tomatohead)


(defun tomatohead-work ()
    "TODO add parameter for time and times 60 it."
    (set-face-attribute 'header-line nil
                        :background nil
                        :foreground nil
                        :inherit 'tomatohead-while-working)

    (setq tomatohead-perc (tomatohead-perc-of-char
                           tomatohead-num
                           (tomatohead-qty-of-chars tomatohead-num tomatohead-work)
                           tomatohead-work))
    (setq current-char
          (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 20))
                 "")
                ((and (> tomatohead-perc 20) (<= tomatohead-perc 40))
                 "░")
                ((and (> tomatohead-perc 40) (<= tomatohead-perc 60))
                 "▒")
                ((and (> tomatohead-perc 60) (<= tomatohead-perc 80))
                 "▓")
                ((and (> tomatohead-perc 80) (<= tomatohead-perc 100))
                 "█")
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
    (if (eq tomatohead-work tomatohead-num)
        (progn
          ;; Add one to the completed pomodoros counter.
          (setq tomatohead-pomoatm (+ tomatohead-pomoatm 1))
          ;; Check if we reached the last pomodoro.
          (if (eq tomatohead-pomonum tomatohead-pomoatm)
              (progn
                (cancel-timer tomatohead-timer)
                (setq tomatohead-perc 100)
                (setq tomatohead-num (- tomatohead-lgbreak 1))
                (setq tomatohead-timer
                      (run-at-time "0 sec" 1 'tomatohead-long-break)))
            ;; If it was not the last pomodoro, perform a short break.
            (progn
              (cancel-timer tomatohead-timer)
              (setq tomatohead-perc 100)
              (setq tomatohead-num (- tomatohead-break 1))
              (setq tomatohead-timer
                    ;; Starting at zero seconds, each second.
                    (run-at-time "0 sec" 1 'tomatohead-break)))))))


(defun tomatohead-break ()
  "BREAK!."
  (set-face-attribute 'header-line nil
                      :inherit 'tomatohead-while-sbreak)
  (setq tomatohead-perc (tomatohead-perc-of-char tomatohead-num (tomatohead-qty-of-chars tomatohead-num tomatohead-break) tomatohead-break))
  (setq current-char
        (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 20))
               "")
              ((and (> tomatohead-perc 20) (<= tomatohead-perc 40))
               "░")
              ((and (> tomatohead-perc 40) (<= tomatohead-perc 60))
               "▒")
              ((and (> tomatohead-perc 60) (<= tomatohead-perc 80))
               "▓")
              ((and (> tomatohead-perc 80) (= tomatohead-perc 100))
               "█")
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
        (setq tomatohead-timer
              ;; Starting at zero seconds, each second.
              (run-at-time "0 sec" 1 'tomatohead-work)))))


(defun tomatohead-long-break ()
  "Long BREAK!."
  (set-face-attribute 'header-line nil
                      :inherit 'tomatohead-while-lbreak)
  (setq tomatohead-perc (tomatohead-perc-of-char tomatohead-num (tomatohead-qty-of-chars tomatohead-num tomatohead-lgbreak) tomatohead-lgbreak))
  (setq current-char
        (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 20))
               "")
              ((and (> tomatohead-perc 20) (<= tomatohead-perc 40))
               "░")
              ((and (> tomatohead-perc 40) (<= tomatohead-perc 60))
               "▒")
              ((and (> tomatohead-perc 60) (<= tomatohead-perc 80))
               "▓")
              ((and (> tomatohead-perc 80) (<= tomatohead-perc 100))
               "█")
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
  (setq tomatohead-timer
        (run-at-time "0 sec" 1 'tomatohead-work)))


;;;###autoload
(define-minor-mode tomatohead-mode
  "Tomatohead pomodoro"
  :global nil
  :group 'tomatohead
  (if tomatohead-mode
      (tomatohead-start)
    (setq tomatohead-mode nil)
    (if tomatohead-timer
        (progn
          (cancel-timer tomatohead-timer)
          (setq tomatohead-mode nil)
          (setq tomatohead-pomonum 4)
          (setq tomatohead-perc 0)
          (setq tomatohead-num 0)
          (setq tomatohead-pomoatm 0)
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (setq header-line-format nil)
              (setq tomatohead-mode nil)))))))

(provide 'tomatohead)


;;; tomatohead.el ends here

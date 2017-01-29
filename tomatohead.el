;;; tomatohead.el --- Super Simple Pomodoro!

;; Copyright (c) 2017 Leandro Bravo

;; Author: Leandro Bravo <leabravo@gmail.com>
;; Version: 0.1
;; Keywords: Pomodoro, Productivity
;; URL: https://github.com/leabravo/tomatohead

;;; Commentary:

;; This is a simple implementation of a Pomodoro Timer which uses the
;; header for visualizing progress.
;; Usage (tomato-head-comence lenght-of-work lenght-of-break)

;;; Code:

(eval-when-compile (require 'cl))

(defvar tomatohead-num 1)

(defvar tomatohead-work 1500)
(defvar tomatohead-break 600)
(defvar tomatohead-lgbreak 900)
(defvar tomatohead-pomonum 4)
(defvar tomatohead-pomoatm 0)
(defvar tomatohead-perc 0)
(defvar tomatohead-timer nil)

(defgroup tomatohead nil
  "Group for TomatoHead customizations."
  :group 'frames)

(defun qty-of-chars (iter time)
  "Count the quantity of characters based on the percentaje of.
the Pomodoro that's been consumed.  Truncate to only fetch
by one percent at a time.  ITER TIME."
  (truncate (* (/ (* iter 1.0) time) (window-total-width))))

(defun perc-of-char (iter chars time)
  "Substract the non decimal part from the current percentaje of.
completion in order to get the current percentaje of the char-
-acter processed i.e 3.76 - 3 = .76 of the current char compl-
-eted (times 100 to make int) ITER CHARS TIME"
  (truncate (* 100 (- (* (/ (* iter 1.0) time) (window-total-width)) chars))))


(defun tomatohead-work ()
    "TODO add parameter for time and times 60 it."
    ;;(symbol-plist 'header-line-format)
    ;;(setq tomatohead-num 600)
    ;;(sit-for 0.1)
    (setq tomatohead-perc (perc-of-char tomatohead-num (qty-of-chars tomatohead-num tomatohead-work) tomatohead-work))
    (setq current-char
          (cond ((and (eq tomatohead-perc 0) (<= tomatohead-perc 25))
                 "")
                ((and (> tomatohead-perc 25) (<= tomatohead-perc 50))
                 "░")
                ((and (> tomatohead-perc 33) (<= tomatohead-perc 66))
                 "▒")
                ((and (> tomatohead-perc 66) (<= tomatohead-perc 100))
                 "▓")
                ))
    (setq header-line-format
          (concat (make-string (if (= (qty-of-chars tomatohead-num tomatohead-work) 0)
                                   0 (qty-of-chars tomatohead-num tomatohead-work)) ?█)
                  current-char))
    (setq tomatohead-num (+ 1 tomatohead-num))
    (force-mode-line-update)
    (if (eq 1500 tomatohead-num)
        (progn
          (setq tomatohead-pomoatm (+ tomatohead-pomoatm 1))
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
  (setq tomatohead-perc (perc-of-char tomatohead-num (qty-of-chars tomatohead-num tomatohead-break) tomatohead-break))
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
        (concat (make-string (if (= (qty-of-chars tomatohead-num tomatohead-break) 0)
                                 0 (qty-of-chars tomatohead-num tomatohead-break)) ?█)
                current-char));(number-to-string num))
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
  (setq tomatohead-perc (perc-of-char tomatohead-num (qty-of-chars tomatohead-num tomatohead-lgbreak) tomatohead-lgbreak))
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
        (concat (make-string (if (= (qty-of-chars tomatohead-num tomatohead-lgbreak) 0)
                                    0 (qty-of-chars tomatohead-num tomatohead-lgbreak)) ?█)
                             current-char)) ; Make a string out of the qty. of current-char.
  (setq tomatohead-num (- tomatohead-num 1))
  (force-mode-line-update)
  (if (eq tomatohead-num 0)
      (progn (cancel-timer tomatohead-timer)
             (setq tomatohead-pomoatm 0) ;; Reset the counter back to 0
             (setq header-line-format nil)
             (tomatohead-mode -1))))


(defun tomatohead-start ()
  "Start the Pomodoro specifying WORK and BREAK."
  (interactive)
  (set-face-attribute 'header-line nil
                      :background "#65000B"
                      :foreground "#DF3232")
  
  (setq tomatohead-timer
        (run-at-time "0 sec" 1 'tomatohead-work)))

;;;###autoload
(define-minor-mode tomatohead-mode
  :global t
  :group 'tomatohead
  (if tomatohead-mode
      (tomatohead-start)
    (tomatohead-mode -1)))


(provide 'tomatohead)


;;; tomatohead.el ends here

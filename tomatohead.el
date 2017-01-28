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

(defvar num 1)

(defvar work 1500)
(defvar break 600)
(defvar lgbreak 1000)
(defvar pomonum 1)
(defvar pomoatm 0)
(defvar perc 0)
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
    (interactive)
    ;;(symbol-plist 'header-line-format)
    ;;(setq num 600)
    ;;(sit-for 0.1)
    (setq perc (perc-of-char num (qty-of-chars num work) work))
    (setq current-char
          (cond ((and (eq perc 0) (<= perc 25))
                 "")
                ((and (> perc 25) (<= perc 50))
                 "░")
                ((and (> perc 33) (<= perc 66))
                 "▒")
                ((and (> perc 66) (<= perc 100))
                 "▓")
                ))
    (setq header-line-format
          (concat (make-string (if (= (qty-of-chars num work) 0)
                                   0 (qty-of-chars num work)) ?█)
                  current-char))
    (setq num (+ 1 num))
    (redraw-modeline)
    (if (eq 1500 num)
        (progn
          (setq pomoatm (+ pomoatm 1))
          (if (eq pomonum pomoatm)
              (progn
                (cancel-timer tomatohead-timer)
                (setq perc 100)
                (setq num (- lgbreak 1))
                (set-face-attribute 'header-line nil
                                    :background "#000000"
                                    :foreground "#FFFFFF")
                (setq tomatohead-timer
                      (run-at-time "0 sec" 0.01 'tomatohead-long-break)))
            (progn
              (cancel-timer tomatohead-timer)
              (setq perc 100)
              (setq num (- break 1))
              (set-face-attribute 'header-line nil
                                  :background "#65000B"
                                  :foreground "#55AB55")
              (setq tomatohead-timer
                    ;; Starting at zero seconds, each second.
                    (run-at-time "0 sec" 0.01 'tomatohead-break)))))))

(defun tomatohead-break ()
  "BREAK!."
  (interactive)
  (setq perc (perc-of-char num (qty-of-chars num break) break))
  (setq current-char
        (cond ((and (eq perc 0) (<= perc 25))
               "")
              ((and (> perc 25) (<= perc 50))
               "░")
              ((and (> perc 50) (<= perc 75))
               "▒")
              ((and (> perc 75) (<= perc 100))
               "▓")
              ))
  (setq header-line-format
        (concat (make-string (if (= (qty-of-chars num break) 0)
                                 0 (qty-of-chars num break)) ?█)
                current-char));(number-to-string num))
  (setq num (- num 1))
  (redraw-modeline)
  (if (eq num 0)
      (progn (cancel-timer tomatohead-timer)
             (tomatohead-mode -1))))

(defun tomatohead-long-break ()
  "Long BREAK!."
  (interactive)
  (setq perc (perc-of-char num (qty-of-chars num lgbreak) lgbreak))
  (setq current-char
        (cond ((and (eq perc 0) (<= perc 25))
               "")
              ((and (> perc 25) (<= perc 50))
               "░")
              ((and (> perc 50) (<= perc 75))
               "▒")
              ((and (> perc 75) (<= perc 100))
               "▓")
              ))
  (setq header-line-format
        (concat (make-string (if (= (qty-of-chars num lgbreak) 0)
                                    0 (qty-of-chars num lgbreak)) ?█)
                             current-char));(number-to-string num))
  (setq num (- num 1))
  (redraw-modeline)
  (if (eq num 0)
      (progn (cancel-timer tomatohead-timer)
             (setq pomoatm 0) ;; Reset the counter back to 0
             (setq header-line-format nil)
             (tomatohead-mode -1))))
        
(defun tomatohead-start ()
  "Start the Pomodoro specifying WORK and BREAK."

  (set-face-attribute 'header-line nil
                      :background "#65000B"
                      :foreground "#DF3232")
  
  (setq tomatohead-timer
        (run-at-time "0 sec" 0.01 'tomatohead-work)))

;;;###autoload
(define-minor-mode tomatohead-mode
  :global t
  :group 'tomatohead
  :lighter (number-to-string num)
  (if tomatohead-mode
      (progn
        (tomatohead-start))))


(provide 'tomatohead-mode)

;;; tomatohead.el ends here

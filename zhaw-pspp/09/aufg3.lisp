;;
;; Factorial with range
;;

(defun factorial (n) 
    (apply #'* (range 1 (+ n 1)))) 

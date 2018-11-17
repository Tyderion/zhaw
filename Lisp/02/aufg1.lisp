;;
;; Mapping over a list
;;
(defun map-list (f seq)
  (cond ((null seq) nil)
        (t (cons (funcall f (car seq))
                 (map-list f (cdr seq))))))

;; or as an iterative implementation
;(defun map-list (f seq) 
;    (loop for el in seq collect (funcall f el)))


;;
;; Sum of all list elements
;;
(defun list-sum (seq) 
  (cond ((null seq) 0)
        (t (+ (car seq) (list-sum (cdr seq))))))


;;
;; Product of all list elements
;;
(defun list-mult (seq) 
  (cond ((null seq) 1)
        (t (* (car seq) (list-mult (cdr seq))))


;;
;; Abstraction
;;
(defun reduce-list (f init seq)
  ; ...
  init)


;;
;; Another frequently used list abstraction
;; Find a better name
;;
(defun func (f seq) 
  (cond ((null seq) nil)
        ((funcall f (car seq))
         (cons (car seq) (func f (cdr seq))))
        (t (func f (cdr seq)))))





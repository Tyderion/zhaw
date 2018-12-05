;; Aufgabe 1
; 1a
(- (* 10 (+ 4 3 3 1)) 20)

; 1b)
; Nur ausprobieren

; 1c
(first (rest (first (rest (first '((w (x y)))))))) ; ((w (x y)))
(first (rest '((w u) y z))) ; ((w u) y z)
(first (rest (rest (first '((w (x) y) u))))) ; ((w (x) y) u)
(first (first (first (first '((((y))) w))))); ((((y))) w)

; mit kombo car/cdr
(cadadr (car '((w (x y)))))
(cadr '((w u) y z))
(caddar '((w (x) y) u))
(caaaar '((((y))) w))



;; Aufgabe 2
; 2a)
; mit mapcar
(defun mlist-double (lst) (mapcar #'(lambda (arg) (* arg 2)) lst))
; mit Rekursion
(defun list-double (lst) ( 
    if (null lst) '() 
    (cons (* 2 (first lst)) (rlist-double (rest lst)))))

; 2b)
(defun sign (n) (
    if (< n 0) -1 
    (if (> n 0) 1 0)))

; 2c)
; mit mapcar
(defun mlist-sign (lst) (mapcar #'sign lst))
; mit Rekursion
(defun list-sign (lst) ( 
    if (null lst) '() 
    (cons (sign (first lst)) (rlist-sign (rest lst)))))


;; Aufgabe 3 
; wie mit mapcar
(defun map-list (f lst)
 (if (null lst) nil
 ;; else
 (cons (funcall f (car lst))
 (map-list f (cdr lst)))))


 ; double
 (defun list-double (lst) (map-list #'(lambda (x) (* x 2)) lst))
 ; sqr
 (defun list-sqr (lst) (map-list #'(lambda (x) (* x x)) lst))


;; Aufgabe 4
; Manuelle implementationen
; Sum
(defun list-sum (lst) 
    (if (null lst) 0
        ;; else
        (+ (first lst) (list-sum (rest lst)))))

; Product
(defun list-mult (lst) 
    (if (null lst) 1
        ;; else
        (* (first lst) (list-sum (rest lst)))))

 ; all-true
(defun all-true (lst) 
    (if (null lst) T
        ;; else
        (and (first lst) (all-true (rest lst)))))

;; Mit eigenem Reduce
;; Reduce
(defun red (start combiner lst) 
    (if (null lst) start
        ;; else
        (red (funcall combiner start (first lst)) combiner (rest lst))))

;; list-sum
(defun rlist-sum (lst) (red 0 #'+ lst))
(defun rlist-sum2 (lst) (red 0 #'(lambda (x y) (+ x y)) lst))

;; list-mult
(defun rlist-mult (lst) (red 1 #'* lst))
(defun rlist-mult2 (lst) (red 1 #'(lambda (x y) (* x y)) lst))

;; all-true
(defun rall-true (lst) (red T #'(lambda (x y) (and x y T)) lst))
;;
;; Some fragments for the PSPP exercises 
;; Functional Programming (1)
;; (more in pspp_basics.lisp)
;; clisp.exe -E UTF-8
(load "pspp_basics.lisp")

(defun repeat (times value)
  (mapcar (lambda (n) value)
          (range times)))
    

;; Aufgabe 1
(repeatedly 5 (lambda (n) 'Hello))
(repeatedly 5 (lambda (n) (+ 1 (random 6))))
(repeatedly 5 (lambda (n) (concatenate 'string "id" (write-to-string n))))

;; Always, ist eine Funktion höherer Ordnung weil sie eine Funktion zurück gibt.
(defun always (val) (lambda (&rest r) val))

;; Hello * 5: 
(repeatedly 5 (always 'hello))

;; Aufgabe 2
(setfun num (dispatch #'parse-int #'parse-float #'identity))

;; Num konvertiert strings zu ints falls es ein int ist, wenn nicht zu float und wenn es kein float ist, wird der string zurückgegeben (identity)
(num "123") ; 123
(num "12.3") ; 12.3
(num "asdf") ; "asdf"

;; Add
(defun add (&rest things) (reduce #'+ (mapcar #'num things) :initial-value 0))
(add "1.3" 2 "3")
(add 100 1.2 "1.3")

(defun num-args (&rest args)
  (mapcar #'num args))
    
;;; Aufgabe 3: Partielle Anwendung
;; Endrekursiv bedeutet, dass am Ende der Rekursion *NUR* der rekrusive Aufruf steht (also z.b. '(fac 1 (* 2 1))' und nicht '(* 2 (fac 1))'
;; Endrekursive funktionen werden von den meisten Compilern gleich effizient wie ein loop umgesetzt, es gibt also keinen Stackoverflow
(defun factorial (n &optional (fact 1))
  (if (<= n 1) fact
      (factorial (- n 1) (* n fact))))
(defun trampoline (fun &rest args)
  (let ((result (apply fun args)))
    (loop while (functionp result) do
      (setq result (funcall result)))
    result))

(defun factorial-lazy (n &optional (fact 1))
  (if (<= n 1) fact
      (partial #'factorial (- n 1) (* n fact))))

(setfun fac3 (factorial-lazy 3))
;; Dann ausführen
(fac3)

;; mit trampoline
(trampoline #'fac3)
(trampoline (factorial-lazy 100))

;;; Aufgabe 4: Quicksort

(defun sort-list (lst) 
  (cond 
    ((null lst) '())
    (t (let ((head (first lst)) (tail (rest lst))) 
      (append 
        (sort-list (filter-list #'(lambda (x) (<= x head)) tail))
        (cons head (sort-list (filter-list #'(lambda (x) (> x head)) tail))))))))
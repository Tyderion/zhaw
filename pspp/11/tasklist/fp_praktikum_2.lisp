;; (load "fp_praktikum_2.lisp")
;; Read text file into string
;;
(defun file-to-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))


;; Split string at character
;;
(defun string-split (c strg &optional (start 0))
  (let ((end (position c strg :start start)))
    (cond (end (cons (subseq strg start end)
                     (string-split c strg (+ end 1))))
          (t (list (subseq strg start))))))


;; eigentlich ist die Bezeichnung autocurry für die folgende Funktion nicht ganz
;; korrekt, es ist eher ein autopartial
;;
(defun autocurry (f numargs)
  (lambda (&rest args)
    (cond ((>= (length args) numargs) 
            (apply f args))
          ((>= (length args) 1)
            (lambda (&rest restargs) (apply f (append args restargs))))
          (t (error "Function must be called with at least one argument")))))


;; Makro zum vereinfachten Binden einer Funktion an ein Symbol
;;
(defmacro setfun (symb fun)
  `(prog1 ',symb (setf (symbol-function ',symb) ,fun)))


;; Pipeline: Funktion erstellen, die aus der Verkettung einer Reihe von 
;; Funktionen gebildet wird 
;; 
(defun pipeline (&rest funcs)
  (lambda (&rest args)
    (car (reduce
          (lambda (ar f) (list (apply f ar)))
          funcs
          :initial-value args))))


;; Partielle Anwendung einer Funktion
;;
(defun partial (f &rest args)
  (lambda (&rest more-args)
    (apply f (append args more-args))))



;; Aufgabe 1)
(ql:quickload "cl-json")
(with-input-from-string (s (file-to-string "tasks.json")) (json:decode-json s))

; with-input-from-string konvertiert einen string zu einem input-stream und gibt ihn weiter an das zweite argument (json:decode-json s)
; Welche Datentypen werden verwendet: Symbole für keys, strings und zahlen (und listen) für werte. eine cons-zelle für jedes key-value paar
; Welche Bezeichnungen wurden verändert: dueDate -> DUE-DATE (camelCase -> camel-case). weil es symbole sind gibt es keine gross/kleinschreibung


(defun read-json (path) (with-input-from-string (s (file-to-string path)) (json:decode-json s)))

(defun getprop-fn (key list) (cdr (car (remove-if-not #'(lambda (e) (eq key (car e))) list))))

(setfun getprop (autocurry #'getprop-fn 2))

(getprop :tasks (read-json "tasks.json")) ; Liefert den tasks array vom tasks.json file
(getprop :tasks) ; Lifert eine funktion zurück, welche noch das listen argument erwartet und dann daraus den wert von :tasks zurück gibt. partiell angewendete getprop-fn durch auto-curry

(defvar *tasks* (getprop :tasks (read-json "tasks.json")))

(defun filter-fn (f seq)
  (remove-if-not f seq))
(defun reject-fn (f seq)
 (remove-if f seq))


(setfun filter (autocurry #'filter-fn 2))
(setfun reject (autocurry #'reject-fn 2))


(defun prop-eq (prop val)
 (pipeline (getprop prop) (partial #'equal val)))

 ;; prop-eq gibt eine funktion zurück, welche mit einer liste von props als zusätzliches argument diejenigen elemente zurück gibt, für welche (equal (getprop prop) val) gelten, z.b. :member "Scott" => (:member . "Scott")

(filter (prop-eq :member "Scott") *tasks*) ; gibt alle tasks von scott zurück

(filter (prop-eq :member "Scott")) ; Gibt eine funktion zurück, welche aus einer liste von tasks die von scott heraussucht (liste muss noch übergeben werden)

(defun pick-fn (attrs obj)
 (remove-if-not #'(lambda (el) (member (car el) attrs)) obj))

(setfun pick (autocurry #'pick-fn 2))
(setfun forall (autocurry #'mapcar 2))

; pick nimmt alle props mit den gegebenen namen (in attrs) aus der liste, also (pick '(:thing) '((:thing . 1) (:thing2 .2))) würde ((:thing .1)) ergeben. es "pickt" die gewollten eigenschaften heraus

(forall (pick '(:title :due-date)) *tasks*)

(defvar *date* "11/15/2013")

(string-split #\/ *date*)

(defun date-to-universal-simple (d) 
  (let ((sd (mapcar #'parse-integer (string-split #\/ d))))
  (encode-universal-time 0 0 0 (second sd) (first sd) (third sd))
  ) 
)

(defun date-to-universal (d) 
  (multiple-value-bind (m d y)
    (values-list (mapcar #'parse-integer (string-split #\/ d)))
    (encode-universal-time 0 0 0 d m y)
  )
)

(defun sort-by-fn (f seq)
 (sort (copy-list seq)
 (lambda (a b) (< (funcall f a) (funcall f b)))))

(setfun sort-by (autocurry #'sort-by-fn 2)) 

(defun open-tasks (name)
 (pipeline
 (getprop :tasks)
 (filter (prop-eq :member name))
 (reject (prop-eq :complete t))
 (forall (pick '(:id :due-date :title :priority)))
 (sort-by (pipeline (getprop :due-date) #'date-to-universal))))


 ;; Alle offene tasks von scott: 
 (funcall (open-tasks "Scott") (read-json "tasks.json"))


 (filter-fn #'(lambda (x) (eq (getprop :id x) '(:id . 104))) *tasks*)
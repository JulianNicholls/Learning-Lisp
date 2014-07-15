;;; Lists

;;
;; Recursive list length
;;

(defun list-len (L)   ; It seems to be an idiom to use capitals for args
  (if (null L)
    0
    (1+ (list-len (cdr L)))))

;;
;; Recursive sum
;;

(defun list-sum1 (L)
  (if (= 1 (list-len L))    ; Premature optimisation? i.e. 1 before null
    (car L)                 ; BROKEN FOR EMPTY LIST!
    (+ (car L) (list-sum1 (cdr L)))))

(defun list-sum2 (L)
  (if (null L)              ; More idiomatic? i.e. at empty
    0
    (+ (first L) (list-sum2 (rest L)))))

;;
;; Recursive N'th member
;; I forgot the degenerate case of a nil list at first
;;

(defun list-nth1 (N L)
  (if (null L)
    nil
    (if (zerop N)
      (car L)
      (list-nth1 (1- N) (cdr L)))))

(defun list-nth2 (N L)
  (cond                   ; Rewrite using cond
    ((null L)  nil)
    ((zerop N) (car L))
    (t         (list-nth2 (1- N) (cdr L)))))

;;
;; Recursive last, nil test not forgotten
;;

(defun list-last1 (L)
  (if (null L)
    nil
    (if (= 1 (list-len L))
      (car L)
      (list-last1 (rest L)))))

(defun list-last2 (L)
  (cond                     ; Rewrite using cond
    ((null L)           nil)
    ((= 1 (list-len L)) (car L))
    (t                  (list-last2 (rest L)))))

;;
;; List membership
;;

(defun list-member (M L)
  (cond
    ((null L)      nil)
    ((eql M (car L)) t)      ; = means number, eq means symbol, eql means either
    (t             (list-member M (rest L)))))

;;
;; list-append
;;

(defun list-append (L1 L2)
  (if (null L1)
    L2
    (cons (first L1) (list-append (rest L1) L2))))

;;
;; list-butlast
;;

(defun list-butlast (L)
  (if (= 1 (list-len L))
    nil
    (cons (first L) (list-butlast (rest L)))))

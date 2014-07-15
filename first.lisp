;;; testing.lisp
;;; by Philip Fong, originally

;;; Introductory comments are led in by ;;;
;;; Function headers by ;;
;;; Incline comments by ;

;;
;; Triple a number
;;

(defun triple (x)
  "compute three times x"   ; Inline comments
  (* 3 x))                  ; can go here

;;
;; Negate the sign of a number
;;

(defun negate (x)
  "Negate a value"        ; That's a documentation string
  (- x))

;;
;; Factorial
;;

(defun rfactorial (n)     ; Renamed to avoid clash with built-in
  "Compute a factorial recursively"
  (if (= n 1)
    1
    (* n (rfactorial (- n 1)))))

;;
;; N'th triangular number
;;

(defun rtriangle (n)
  "Recursive"
  (if (= n 1)
    1
    (+ n (rtriangle (1- n)))))   ; (1- n) is a synonym for (- n 1)

(defun ctriangle (n)
  "Calculated n(n+1)/2"
  (/ (* n (1+ n)) 2))           ; (1+ n) is a synonym for (+ n 1)

;;
;; Power
;;

(defun rpower (b e)
  (if (= e 0)
    1
    (* b (rpower b (- e 1)))))

;;
;; Recursive Fibonacci, starts to show slowdown at around (rfib1 40).
;;

(defun rfib1 (n)
  (if (or (= n 1) (zerop n))    ; (zerop n) is a synonym for (= n 0)
    1
    (+ (rfib1 (- n 1)) (rfib1 (- n 2)))))

;;
;; Second recursive factorial with let.
;;

(defun rfib2 (n)
  (if (or (zerop n) (= n 1))
    1
    (let
      ((F1 (rfib2 (1- n)))
       (F2 (rfib2 (- n 2))))
      (+ F1 F2)
    )
  )
)

;;
;; Binomial
;;

(defun binom (n r)
  (if (or (zerop r) (= r n))
    1
    (+ (binom (1- n) (1- r)) (binom (1- n) r))))

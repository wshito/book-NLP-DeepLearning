(defpackage nlp-dl.ch02
  (:use :cl)
  (:export :make-n-gram))

(in-package :nlp-dl.ch02)

;; The Definition of Standard Characters
;; http://www.lispworks.com/documentation/HyperSpec/Body/02_ac.htm

(defmacro extract-nonascii (str)
  "Standarc Character（主にAscii文字）を取り除いた文字列を返す．"
  `(remove-if #'standard-char-p ,str))


;; 文字列からn-gramのリストを作成して返す．
;; non-asciiキーワードがtならAscii文字を取り除く．
;; stringキーワードがtなら結果を文字列のリストにして返す．
(defun make-n-gram (str n &key (non-ascii t) (string nil))
  "N-gramを生成する．"
  (when non-ascii (setf str (extract-nonascii str)))
  ;; 1-gram
  (if (= n 1) (map 'list #'string (coerce str 'list))
      ;; n-gram
      (let* ((len (length str)))
	(if string
	    (loop for i from 0 to (- len 1)
	       collect (subseq str i (min (+ i n) len)))
	    (loop for i from 0 to (- len 1)
	       collect (intern (subseq str i (min (+ i n) len))))))))


(defun make-bag (lst)
  "リスト内の要素の出現回数を返す．"
  (let ((ht (make-hash-table)))
    (labels ((%make-bag (lst)
	     (if lst
		 (let ((ele (car lst)))
		   (multiple-value-bind (val exists) (gethash ele ht)
		     (if exists
			 (setf (gethash ele ht) (1+ val))
			 (setf (gethash ele ht) 1)))
		   (%make-bag (cdr lst)))
		 ht)))
      (%make-bag lst))
    (loop
       for key being each hash-key of ht
       for val being each hash-values of ht
       collect (cons key val))))






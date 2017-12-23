(defpackage nlp-dl.ch02
  (:use :cl))

(in-package :nlp-dl.ch02)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro write-char> (ch stream) ; for stream chaining
  "Returns the output stream for stream chaining as
   (write-char> ch3 (write-char> ch2 (write-char> ch1 (make-string-output-stream))))"
  (let ((s (gensym)))
    `(let ((,s ,stream))
       (progn (write-char ,ch ,s)
	      ,s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.1 文字の処理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; The Definition of Standard Characters
;; http://www.lispworks.com/documentation/HyperSpec/Body/02_ac.htm

(defmacro extract-nonascii (str)
  "Standarc Character（主にAscii文字）を取り除いた文字列を返す．"
  `(remove-if #'standard-char-p ,str))


;; 文字列からn-gramのリストを作成して返す．
;; non-asciiキーワードがtならAscii文字を取り除く．
;; stringキーワードがtなら結果を文字列のリストにして返す．
(defun make-n-gram-from-str (str n &key (non-ascii t) (string nil))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 2.2.2 単語の処理
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; unicodeの調べ方
;; (char-code #\鼂)
;; unicodeを16進数に変換
;; (format t "~x" (char-code #\鼂))
;; unicodeから文字
;; (format t "~:c" (code-char #x9F02))

(defmacro chartype (ch)
  "1文字分のunicodeポイントコードを取り，文字種のシンボルを返す"
    `(let ((unicode (char-code ,ch)))
       (cond
	 ((and (< #x002F unicode) (< unicode #x003A)) 'number)
	 ((and (< #x0040 unicode) (< unicode #x005B)) 'alphabet) ; 大文字
	 ((and (< #x0060 unicode) (< unicode #x007B)) 'alphabet) ; 小文字
	 ((and (< #x3040 unicode) (< unicode #x30A0)) 'hiragana)
	 ((and (< #x309F unicode) (< unicode #x30FF)) 'katakana)
	 ((and (<= #x4E00 unicode) (<= unicode #x9FA0)) 'kanji)
	 ((and (< #xFF65 unicode ) (< unicode #xFF9F)) 'hankaku-kana)
	 (t 'punctuation)))))

(defun split-chartype (in &key (string nil))
  "Inputストリームを取り文字種で分割したリストを返す．`:string`が`t`なら文字列の，`nil`ならシンボルのリストを返す．"
  (macrolet ((%make-result (stream result)
	       `(push (if string (get-output-stream-string ,stream)
			  (intern (get-output-stream-string ,stream)))
		      ,result)))
    (labels ((%split (in type nextch word res) ; type for current char
	       (if (null nextch) (nreverse (%make-result word res))
		   (let ((next-type (chartype nextch)))
		     ;; (format t "ch=~:c~%" nextch) ; for debug
		     (if (eql type next-type)
			 (%split in type (read-char in nil)
				 (write-char> nextch word) res)
			 (progn ; type != next-type
			   ;; skip punctuation
			   (loop while (eql next-type 'punctuation)
			      do
				(setf nextch (read-char in nil))
				(when (null nextch) (return))
				(setf next-type (chartype nextch)))
			   (if (null nextch) 
			       ;; end of the stream while looking for
			       ;; non-punctuation
			       (nreverse (%make-result word res))
			       ;; new word found
			       (%split in next-type (read-char in nil)
				       (write-char> nextch
						    (make-string-output-stream))
				       (%make-result word res)))))))))
      ;; main routine
      (let ((ch (read-char in nil)))
	(if (null ch) nil
	    (progn ; skip the punctuation first
	      (loop while (eql (chartype ch) 'punctuation)
		 do (setf ch (read-char in nil)))
	      ;; (format t "1st ch=~:c~%" ch) ; for debug
	      (%split in (chartype ch) (read-char in nil)
		      (write-char> ch (make-string-output-stream))
		      nil)))))))

(defun make-n-gram (n lst)
  "リストからN-Gramを作成して返す．"
  (if (> 2 n) lst
      (labels ((%nhead (n lst res)
		 (if (null lst) (nreverse res)
		     (if (= n 1) (nreverse (push (car lst) res))
			 (%nhead (1- n) (cdr lst) (push (car lst) res))))))
	(loop for x on lst  ; on shifts the lst content like maplist
	     collect (%nhead n x nil)))))

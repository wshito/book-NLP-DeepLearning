(load "ch02.lisp")

(use-package :nlp-dl.ch02)

(defparameter *sample* "自然言語処理(natural language processing)のうちで基本的かつ実用的な技術は，自然言語テキストの入力と編集を支援する技術でしょう．")

(defparameter *すもも* "すもももももももものうち")


(make-n-gram-from-str *sample* 2)
(make-n-gram-from-str *sample* 3)
(make-n-gram-from-str *sample* 4)

(make-bag (make-n-gram-from-str *sample* 2))
(make-bag (make-n-gram-from-str *すもも* 3))

;;; 文字種で分割
(split-chartype (make-string-input-stream *sample*))
(split-chartype (make-string-input-stream *すもも*))

;;; 文字種からN-Gramを作成
(make-n-gram 2 (split-chartype (make-string-input-stream *sample*)))
(make-n-gram 3 (split-chartype (make-string-input-stream *sample*)))

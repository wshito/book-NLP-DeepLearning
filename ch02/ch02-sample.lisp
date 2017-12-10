(load "ch02.lisp")

(use-package :nlp-dl.ch02)

(defparameter *sample* "自然言語処理(natural language processing)のうちで基本的かつ実用的な技術は，自然言語テキストの入力と編集を支援する技術でしょう．")

(defparameter *すもも* "すもももももももものうち")

(make-n-gram *sample* 2)
(make-n-gram *sample* 3)
(make-n-gram *sample* 4)

(make-bag (make-n-gram *sample* 2))
(make-bag (make-n-gram *すもも* 3))



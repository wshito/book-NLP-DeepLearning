(load "ch02.lisp")

(in-package :nlp-dl.ch02)

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

;;; 単語から1-of-N表現と対応する単語リストを作成
(make-1-of-n (mecab "隣の客はよく食う客だ．客はうるさい"))
(make-1-of-n (mecab "明日は明日の風が吹く．"))

# ローカルで，mecabイメージを先に生成しておくこと．
FROM mecab

MAINTAINER W.Shito (@waterloo_jp)

RUN apk --no-cache --update add --virtual build-dependencies git

#  
# Clone cl-mecab
#
RUN cd /root/.roswell/local-projects \
  && git clone https://github.com/carrotflakes/cl-mecab.git

#
# Clean up
#
RUN apk del build-dependencies

RUN ros run -- --eval '(progn (ql:quickload :cl-mecab))'

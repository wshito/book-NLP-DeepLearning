ros:

# mecab-baseイメージを生成．Alpine Linuxにmecabをインストール
# しただけもの．
.PHONY: mecab-base
mecab-base: dockers/mecab-base/Dockerfile
	cd dockers/mecab-base && \
	docker build -t mecab-base . && \
	cd -

# Install the image of wshito/sbcl-swank
# wshito/sbcl-swankイメージはSBCLのミニマム環境．起動と同時に
# SBCLとSwankサーバを立ち上げる．
.PHONY: sbcl-swank
sbcl-swank:
	docker pull wshito/sbcl-swank

# Runs SBCL through roswell with swank server on the port 4005.
#
# https://raw.githubusercontent.com/daewok/slime-docker/master/resources/docker-sbcl-seccomp.json
.PHONY: ros
ros:
	docker run --security-opt seccomp=dockers/docker-sbcl-seccomp.json -it -p 4005:4005 --rm cl-mecab

# mecabイメージを生成
# sbcl-swankイメージにmecabを組み込んだもの．起動時にSBCLとSwank
# サーバーを立ち上げる．構成物は，SBCL, Swankサーバー，mecab
.PHONY: mecab
mecab: dockers/mecab/Dockerfile
	cd dockers/mecab && \
	docker build -t mecab . && \
	cd -

# cl-mecabイメージを生成
# mecabイメージにcl-mecabを組み込んだもので，起動時にSBCLとSwank
# サーバーを立ち上げる．構成物は，SBCL, Swankサーバー，mecab，cl-mecab
.PHONY: cl-mecab
cl-mecab: dockers/cl-mecab/Dockerfile
	cd dockers/cl-mecab && \
	docker build -t cl-mecab . && \
	cd -

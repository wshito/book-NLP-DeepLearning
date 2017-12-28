lisp:

.PHONY: mecab-base
mecab-base: dockers/mecab-base/Dockerfile
	cd dockers/mecab-base && \
	docker build -t mecab-base . && \
	cd -

# Install the image of wshito/sbcl-swank
.PHONY: sbcl-swank
sbcl-swank:
	docker pull wshito/sbcl-swank

# Runs SBCL through roswell with swank server on the port 4005.
#
# https://raw.githubusercontent.com/daewok/slime-docker/master/resources/docker-sbcl-seccomp.json
.PHONY: ros
ros: 
	docker run --security-opt seccomp=dockers/docker-sbcl-seccomp.json -it -p 4005:4005 --rm wshito/sbcl-swank

.PHONY: mecab
mecab: dockers/mecab/Dockerfile
	cd dockers/mecab && \
	docker build -t mecab . && \
	cd -

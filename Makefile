mecab-base: dockers/mecab-base/Dockerfile
	cd dockers/mecab-base && \
	docker build -t mecab-base . && \
	cd -

FROM wshito/sbcl-swank

MAINTAINER W.Shito (@waterloo_jp)

RUN apk --no-cache --update add \
    bash \
    ca-certificates \
    libstdc++
    
RUN apk --no-cache --update add --virtual build-dependencies \
    build-base \
    curl \
    file \
    git \
    wget \
    openssl
#  
# Install MeCab
#
RUN MECAB_URL="https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE" \
  && IPADIC_URL="https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM" \
  && wget -q -O - ${MECAB_URL} | tar -xzf - -C /tmp \
  && cd /tmp/mecab-[0-9]* \
  && ./configure --enable-utf8-only --with-charset=utf8 \
  && CPUCOUNT=$(getconf _NPROCESSORS_ONLN)  \
  && make -j ${CPUCOUNT} \
  && make install \
#  
# Install IPA dic
#
  && wget -q -O - ${IPADIC_URL} | tar -xzf - -C /tmp \
  && cd /tmp/mecab-ipadic-* \
  && ./configure --with-charset=utf8 \
  && make -j ${CPUCOUNT} \
  && make install \
#
# Install Neologd
#
  && cd /tmp \
  && git clone --depth 1 https://github.com/neologd/mecab-ipadic-neologd.git \
  && mecab-ipadic-neologd/bin/install-mecab-ipadic-neologd -n -y \
#
# Clean up
#
  && apk del build-dependencies \
  && rm -rf \
      /tmp/mecab-[0-9]* \
      /tmp/mecab-ipadic-* \
      /tmp/mecab-ipadic-neologd

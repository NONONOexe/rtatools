FROM rocker/geospatial:4.2.2

# install japanese language libraries
ENV LANG ja_JP.UTF-8
ENV LC_ALL ja_JP.UTF-8
RUN sed -i '$d' /etc/locale.gen \
  && echo "ja_JP.UTF-8 UTF-8" >> /etc/locale.gen \
    && locale-gen ja_JP.UTF-8 \
    && /usr/sbin/update-locale LANG=ja_JP.UTF-8 LANGUAGE="ja_JP:ja"
RUN /bin/bash -c "source /etc/default/locale"
RUN ln -sf  /usr/share/zoneinfo/Asia/Tokyo /etc/localtime

# install japanese fonts
RUN apt-get update && apt-get install -y \
  fonts-ipaexfont \
  fonts-noto-cjk

# install library for pdftools
RUN apt-get install -y libpoppler-cpp-dev

# set working directory
RUN echo "setwd(\"/home/rstudio/rtatools/\")" > /home/rstudio/.Rprofile

# MeCab
RUN curl -fsSL "https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE" \
  -o mecab-0.996.tar.gz ;\
  tar xf mecab-0.996.tar.gz ;\
  cd mecab-0.996 ;\
  ./configure --with-charset=utf8 ;\
  make ;\
  make install ;\
  ldconfig

# IPAdic
RUN curl -fsSL "https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM" \
  -o mecab-ipadic-2.7.0-20070801.tar.gz ;\
  tar xf mecab-ipadic-2.7.0-20070801.tar.gz ;\
  cd mecab-ipadic-2.7.0-20070801 ;\
  ./configure --with-charset=utf-8 ;\
  make ;\
  make install ;\
  ldconfig

# install R packages
RUN install2.r -d TRUE -e -n -1 \
  celestial \
  conflicted \
  patchwork \
  pdftools \
  styler \
  tictoc \
  openxlsx
RUN installGithub.r \
  uribo/jpmesh \
  uribo/jpndistrict
RUN Rscript -e "install.packages('RMeCab', repos = 'https://rmecab.jp/R', type = 'source')"

# clean tmp files
RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds ;\
  rm -rf mecab-0.996.tar.gz* ;\
  rm -rf mecab-ipadic-2.7.0-20070801*

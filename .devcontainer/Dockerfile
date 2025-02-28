FROM rocker/tidyverse:4.4

# packages necessary for the multi-variant analysis tutorial
RUN apt-get update && apt-get install -y  \
    libjpeg-dev cmake pandoc libharfbuzz-dev libxml2-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libv8-dev libcairo2-dev libtiff5-dev 
RUN Rscript -e 'install.packages(c("languageserver","rmarkdown","httpgd"))'
RUN Rscript -e 'install.packages(c("ragg","performance","flextable","gtsummary","randomForest","equatiomatic","pROC"))'
RUN Rscript -e 'install.packages(c("lme4","see","DHARMa","ggeffects","effects","sjPlot","broom.helpers","ggstats","car","vip"))'
RUN Rscript -e 'install.packages(c("gtsummary","flextable","tidyverse"))'
RUN Rscript -e 'install.packages(c("carData","dplyr","caret","gridExtra"))'


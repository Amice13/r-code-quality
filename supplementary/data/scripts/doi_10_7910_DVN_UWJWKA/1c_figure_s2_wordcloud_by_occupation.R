#--------------
# Figure S2. word-cloud maps for appendix 
#==============
library(tidyverse)
library(wordcloud)
library(tm)

png(file.path(figure_dir,'wordcloud_employed.png'), width=5, height = 5, units='in', res = 300)
wordcloud(
	words = wordcloud_occupation[type == 'employed',OccupationText], 
	freq = wordcloud_occupation[type == 'employed',N], min.freq = 2,
	max.words= 200, random.order=FALSE, rot.per = 0.1)
dev.off()

png(file.path(figure_dir,'wordcloud_notinlabor.png'), width=5, height = 5, units='in', res = 300)
wordcloud(
	words = wordcloud_occupation[type == 'notinlabor',OccupationText], 
	freq = wordcloud_occupation[type == 'notinlabor',N], min.freq = 2,
	max.words=200, random.order=FALSE, rot.per=0.1)
dev.off()

png(file.path(figure_dir,'wordcloud_unemployed.png'), width=5, height = 5, units='in', res = 300)
wordcloud(
	words = wordcloud_occupation[type == 'unemployed',OccupationText], 
	freq = wordcloud_occupation[type == 'unemployed',N], min.freq = 2,
	max.words=200, random.order=FALSE, rot.per=0.1)
dev.off()

png(file.path(figure_dir,'wordcloud_missing.png'), width=5, height = 5, units='in', res = 300)
wordcloud(
	words = wordcloud_occupation[type == 'missing',OccupationText], 
	freq = wordcloud_occupation[type == 'missing',N], min.freq = 2,
	max.words=200, random.order=FALSE, rot.per=0.35)
dev.off()


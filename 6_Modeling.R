rm(list=ls())
setwd("~")

############################################
# Eduardo Fierro Farah                     #
# eff254                                   #
# May 2 - 2017                             #
# Text description                         #
# DS-GA 3001-004, Text as Data             #
# Prof, Arthur Spirling                    #
# Teaching Assistant: Mr Patrick Chester   #
############################################

require(ggplot2)
require(doBy)
require(stringr)
require(quanteda)
require(readtext)
require(topicmodels)

###############
# Directories #
###############

# Output TXT location
dir1 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/TXTsOriginal/"

# Location of Main.csv / Custom Stop Words list
dir2 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/"

# Location for Graphs Output
dir3 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Graphs/"

# Location of Corpus Creator function
dir4 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Code/"

#################################
# Import data and create corpus #
#################################

source(paste0(dir4, "4_CorpusCreator.R"))
corpus <- corpus_creator(dir2, dir1)
my_dfm <- corpus[[1]]
iniciativas <- corpus[[2]]
rm(corpus)

zeros <- (iniciativas$totals == 0)
iniciativas <- subset(iniciativas, zeros==F)
my_dfm <- subset(my_dfm, zeros==F)

rm(zeros)

#########
# LDA's #
#########

#### THIS TAKES A LONG TIME. CAREFULL!!!! 

# K=5
lda_model_k5 <- LDA(my_dfm, k = 5, method = "Gibbs",  control = list(seed = 10012))
save(lda_model_k5 , file=paste0(dir2, "lda_model_k5.RData"))
rm(lda_model_k5)

# K=10
lda_model_k10 <- LDA(my_dfm, k = 10, method = "Gibbs",  control = list(seed = 10012))
save(lda_model_k10 , file=paste0(dir2, "lda_model_k10.RData"))
rm(lda_model_k10)

# K=15
lda_model_k15 <- LDA(my_dfm, k = 10, method = "Gibbs",  control = list(seed = 10012))
save(lda_model_k15 , file=paste0(dir2, "lda_model_k15.RData"))
rm(lda_model_k15)

# K=20
lda_model_k20 <- LDA(my_dfm, k = 20, method = "Gibbs",  control = list(seed = 10012))
save(lda_model_k20 , file=paste0(dir2, "lda_model_k20.RData"))
rm(lda_model_k20)

# K=30
#lda_model_k30 <- LDA(my_dfm, k = 30, method = "Gibbs",  control = list(seed = 10012))
#save(lda_model_k30 , file=paste0(dir2, "lda_model_k30.RData"))
#rm(lda_model_k30)

# K=40
lda_model_k40 <- LDA(my_dfm, k = 40, method = "Gibbs",  control = list(seed = 10012))
save(lda_model_k40 , file=paste0(dir2, "lda_model_k40.RData"))
rm(lda_model_k40)
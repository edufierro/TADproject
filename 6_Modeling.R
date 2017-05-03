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

#######
# PCA #
#######



rm(list=ls())
setwd("~")

############################################
# Eduardo Fierro Farah                     #
# eff254 / akash                           #
# May 2 - 2017                             #
# STM models                               #
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
require(doBy)
require(stm)

###############
# Directories #
###############

# Local directoy for graphs output: 
dir1 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Graphs"

# Location of Data (Available on Github)
dir2 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/"

########
# Data #
########

load(paste0(dir2, "FilteredCorpus.RData"))


topics = iniciativas$tema
topic_names = c()
unique_topics = c()

for(i in 1:length(topics))
{
  topic_names = append(topic_names, strsplit(topics[i], "[0-9].-"))
  tmp = unlist(topic_names[i])
  tmp = tmp[-1]
  
  unique_topics = append(unique_topics, tmp)
  topic_names[i] = list(tmp)
}

unique_topics = unique(unique_topics)

# Creating data frame for dummies
dummy = data.frame(matrix(0, nrow=length(topics), ncol=length(unique_topics)))
colnames(dummy) = unique_topics

for(i in 1:nrow(dummy))
{
  tmp = unlist(topic_names[i])
  for(j in 1:length(tmp))
  {
    dummy[i,tmp[j]] = 1
  }
}

# DID NOT RUN: TRIED IN 3 COMPUTERS. RAN OUT OF MEMORY IN ALL. 
stm_pp <- stm(my_dfm, K= 0, prevalence=~ iniciativas$partido_po, init.type = "Spectral")
stm_topics <- stm(my_dfm, K= 0, prevalence=~ as.matrix(dummy), init.type = "Spectral")

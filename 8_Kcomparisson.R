rm(list=ls())
setwd("~")

########################################################
# Eduardo Fierro Farah  / Akash Kadel                  #
# eff254 / ak6201                                      #
# May 9 - 2017                                         #
# Model Results                                        #
# DS-GA 3001-004, Text as Data                         #
# Prof, Arthur Spirling                                #
# Teaching Assistant: Mr Patrick Chester               #
# Github Repo: https://github.com/edufierro/TADproject #
########################################################

require(ggplot2)
require(doBy)
require(stringr)
require(quanteda)
require(readtext)
require(topicmodels)
require(doBy)

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

#############
# Functions #
#############

# All functions are an adaptation from ldatuning
# For estimating the scores individually.
# Author: Nikita Moor
# Source: https://github.com/nikita-moor

CaoJuan2009 <- function(model) {
    # topic-word matrix
    m1 <- exp(model@beta)
    # pair-wise cosine distance
    pairs <- utils::combn(nrow(m1), 2)
    cos.dist <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      # dist <- lsa::cosine(x, y)
      dist <- crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
      return(dist)
    })
    # metric
    metric <- sum(cos.dist) / (model@k*(model@k-1)/2)
    return(metric)
}

Arun2010 <- function(model, dtm) {
  # length of documents (count of words)
  len <- slam::row_sums(dtm)
  # evaluate metrics
    # matrix M1 topic-word
    m1 <- exp(model@beta) # rowSums(m1) == 1
    m1.svd <- svd(m1)
    cm1 <- as.matrix(m1.svd$d)
    # matrix M2 document-topic
    m2   <- model@gamma   # rowSums(m2) == 1
    cm2  <- len %*% m2    # crossprod(len, m2)
    norm <- norm(as.matrix(len), type="m")
    cm2  <- as.vector(cm2 / norm)
    # symmetric Kullback-Leibler divergence
    divergence <- sum(cm1*log(cm1/cm2)) + sum(cm2*log(cm2/cm1))
    return ( divergence )
}

Deveaud2014 <- function(model) {
    ### original version
    # topic-word matrix
    m1 <- exp(model@beta)
    # prevent NaN
    if (any(m1 == 0)) { m1 <- m1 + .Machine$double.xmin }
    # pair-wise Jensen-Shannon divergence
    pairs  <- utils::combn(nrow(m1), 2)
    jsd <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      ### standard Jensen-Shannon divergence
      # m <- (x + y) / 2
      # jsd <- 0.5 * sum(x*log(x/m)) + 0.5 * sum(y*log(y/m))
      ### divergence by Deveaud2014
      jsd <- 0.5 * sum(x*log(x/y)) + 0.5 * sum(y*log(y/x))
      return(jsd)
    })

#     ### optimized version
#     m1   <- model@beta
#     m1.e <- exp(model@beta)
#     pairs  <- utils::combn(nrow(m1), 2)
#     jsd <- apply(pairs, 2, function(pair) {
#       x   <- m1[pair[1], ]
#       y   <- m1[pair[2], ]
#       x.e <- m1.e[pair[1], ]
#       y.e <- m1.e[pair[2], ]
#       jsd <- ( sum(x.e*(x-y)) + sum(y.e*(y-x)) ) / 2
#       return(jsd)
#     })

    # metric
    metric <- sum(jsd) / (model@k*(model@k-1))
    return(metric)
}

##########
# Scores #
##########

scoresDeveaudk5 <- c(NA, NA, NA)
load(paste0(dir2, "lda_model_k5.RData"))
scoresDeveaudk5[1] <- CaoJuan2009(lda_model_k5)
scoresDeveaudk5[2] <- Arun2010(lda_model_k5, my_dfm)
scoresDeveaudk5[3] <- Deveaud2014(lda_model_k5)
# For memory sake
rm(lda_model_k5)

scoresDeveaudk10 <- c(NA, NA, NA)
load(paste0(dir2, "lda_model_k10.RData"))
scoresDeveaudk10[1] <- CaoJuan2009(lda_model_k10)
scoresDeveaudk10[2] <- Arun2010(lda_model_k10, my_dfm)
scoresDeveaudk10[3] <- Deveaud2014(lda_model_k10)
# For memory sake
rm(lda_model_k10)

scoresDeveaudk20 <- c(NA, NA, NA)
load(paste0(dir2, "lda_model_k20.RData"))
scoresDeveaudk20[1] <- CaoJuan2009(lda_model_k20)
scoresDeveaudk20[2] <- Arun2010(lda_model_k20, my_dfm)
scoresDeveaudk20[3] <- Deveaud2014(lda_model_k20)
# For memory sake
rm(lda_model_k20)

scoresDeveaudk30 <- c(NA, NA, NA)
load(paste0(dir2, "lda_model_k30.RData"))
scoresDeveaudk30[1] <- CaoJuan2009(lda_model_k30)
scoresDeveaudk30[2] <- Arun2010(lda_model_k30, my_dfm)
scoresDeveaudk30[3] <- Deveaud2014(lda_model_k30)
# For memory sake
rm(lda_model_k30)

scoresDeveaudk40 <- c(NA, NA, NA)
load(paste0(dir2, "lda_model_k40.RData"))
scoresDeveaudk40[1] <- CaoJuan2009(lda_model_k40)
scoresDeveaudk40[2] <- Arun2010(lda_model_k40, my_dfm)
scoresDeveaudk40[3] <- Deveaud2014(lda_model_k40)
# For memory sake
rm(lda_model_k40)

#########
# Table #
#########

data <- data.frame(rbind(scoresDeveaudk5, scoresDeveaudk10, scoresDeveaudk20, scoresDeveaudk30, scoresDeveaudk40))
names(data) <- c("CaoJuan2009", "Arun2010", "Deveaud2014")
print(data)
rm(scoresDeveaudk5, scoresDeveaudk10, scoresDeveaudk20, scoresDeveaudk30, scoresDeveaudk40)


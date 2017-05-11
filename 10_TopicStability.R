rm(list=ls())
setwd("~")

########################################################
# Eduardo Fierro Farah  / Akash Kadel                  #
# eff254 / ak6201                                      #
# May 3 - 2017                                         #
# Topic Stability. K=30                                #
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
require(plyr)

###############
# Directories #
###############

# Local directoy for graphs output: 
dir1 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Graphs"

# Location of Data (Available on Github)
dir2 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/"

##############
# Locad Data #
##############

load(paste0(dir2, "FilteredCorpus.RData"))

#########################################
# Re-run another k=30 topic model LDA() #
#########################################

# CAREFULL: Run only once: 

# lda_model_k30_2 <- LDA(my_dfm, k = 30, method = "Gibbs",  control = list(seed = 12345))
# save(lda_model_k30_2 , file=paste0(dir2, "lda_model_k30_Stab.RData"))
# rm(lda_model_k30_2)

###################
# Topic stability #
###################

load(paste0(dir2, "lda_model_k30.RData"))
load(paste0(dir2, "lda_model_k30_Stab.RData"))

# Code from Hmw3 - Eduardo Fierro
words_topics_m1 <- t(lda_model_k30@beta)
words_topics_m2 <- t(lda_model_k30_2@beta)

# Function copied from lab3: Cosine Similarity
norm_vec <- function(x) sqrt(sum(x^2))
cosineSimilarity <- function(x,y){
	dot_product = x %*% y
	result = (x %*% y) / (norm_vec(x) * norm_vec(y))
	return(result)
}


main_data <- data.frame() # This will contain similarity scores. 

# Loop trhough all topics of both matrixes
pb <- txtProgressBar(min=1, max=ncol(words_topics_m2), style=3)
for(x in 1:ncol(words_topics_m2)){
	for(y in 1:ncol(words_topics_m1)){
		similarity <- cosineSimilarity(words_topics_m2[,x], words_topics_m1[,y])
		data <- data.frame(t(c(x, y, similarity)))
		names(data) <- c("TopicM2", "TopicM1", "Similarity")
		main_data <- rbind.fill(main_data, data)
		rm(data, similarity)
	}
	rm(y)
	setTxtProgressBar(pb, x)
}
rm(x, pb)

# For some reason, when applying "data.frame", makes everything factor()
main_data[] <- lapply(main_data, as.character)
main_data[] <- lapply(main_data, as.numeric)

# Subset to pairs with max similarity 
# (There must be a more fancy way to do it)
secondary_data <- data.frame() # Results will be stored here
for(x in 1:ncol(words_topics_m2)){
	temp <- subset(main_data, main_data$TopicM2 == x)
    temp <- temp[which.max(temp$Similarity),]
    secondary_data <- rbind.fill(secondary_data, temp)
    rm(temp)
}
rm(x)

top10_terms_m1 <- get_terms(lda_model_k30, 10)
top10_terms_m2 <- get_terms(lda_model_k30_2, 10)

secondary_data$common_words <- as.numeric(NA)

for(x in 1:ncol(top10_terms_m2)){
	m2_words <- top10_terms_m2[,x]
	index_m1 <- secondary_data$TopicM1[x]
	m1_words <- top10_terms_m1[, index_m1]
	  # Sum a boolean vector (T/F) --> (1/0 as default):
	secondary_data$common_words[x] <- sum(m2_words %in% m1_words) 
	rm(m2_words, index_m1, m1_words)
}
rm(x)

# Get average: 
avg <-  sum(secondary_data$common_words) / nrow(secondary_data) # AVG: 5.866667

print(secondary_data)

# Topics that didn't made it: 
seq(1:30)[seq(1:30) %in% unique(secondary_data$TopicM1) == F]

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

zeros <- (iniciativas$totals == 0)
iniciativas <- subset(iniciativas, zeros==F)
my_dfm <- subset(my_dfm, zeros==F)

rm(zeros)

# Save corpus once, because it takes a while to load. THIS is the version that is saved, ready for model
# save(iniciativas, my_dfm, file=paste0(dir2, "FilteredCorpus.RData"))

#######################################
# Similarity and Distances by Parties #
#######################################

data_cosine <- aggregate(iniciativas$text, list(iniciativas$partido_po), paste, collapse=" ")
names(data_cosine) <- c("party", "text")
custom_stop_words <- readLines(paste0(dir2, "OwnDict.txt"))
cosine_dfm <- dfm(data_cosine$text, remove= custom_stop_words, tolower=FALSE, remove_punct = TRUE)

#Make Morena the benchmark: 
similarity <- textstat_simil(cosine_dfm, c("text2"), margin = "documents", method="cosine")
similarity_data <- data.frame(head(similarity, 20)) # There is no data.frame coersion, had to use "head()"
names(similarity_data) <- "cosine"
similarity_data$party <- as.character(NA)
similarity_data$party[2:nrow(similarity_data)] <- subset(data_cosine$party, data_cosine$party!="MORENA")
similarity_data$party[is.na(similarity_data$party)] <- "MORENA"

similarity <- textstat_dist(cosine_dfm, c("text2"), margin = "documents", method="manhattan")
similarity_data$manhattan_distance <- head(similarity, 20)

similarity <- textstat_dist(cosine_dfm, c("text2"), margin = "documents")
similarity_data$euclidian_distance <- head(similarity, 20)

similarity_data <- similarity_data[order(-similarity_data$euclidian_distance),]

data_graph <- similarity_data[,1:2] 
names(data_graph) <- c("value", "party")
data_graph$index <- "Cosine Similarity"

data_temp <- similarity_data[,2:3]
names(data_temp) <- c("party", "value")
data_temp$index <- "Manhattan Distance"
data_graph <- rbind(data_graph, data_temp)
rm(data_temp)

data_temp <- similarity_data[,c(2,4)]
names(data_temp) <- c("party", "value")
data_temp$index <- "Euclidian Distance"
data_graph <- rbind(data_graph, data_temp)
rm(data_temp)

data_graph <- subset(data_graph, data_graph$party!="MORENA")

graf <- ggplot(data_graph, aes(x= party, y=value)) + 
         geom_bar(aes(fill= party), stat = "identity") + 
         facet_wrap(~ index, ncol=1, scales="free") + 
         theme_bw() + 
         scale_fill_manual(values=c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", 
                                    "#1d91c0", "#225ea8", "#253494", "#081d58", "black"),  name="Political Party")  + 
         labs(y="Value", x="Party", title="Meassures of Distances and SImilarity by political party \n Benchmark: MORENA") + 
         theme(axis.title.x = element_text(face="bold", color="black", size=12), 
	          axis.title.y = element_text(face="bold", color="black", size=12), 
	          axis.text=element_text(angle = 45, hjust = 1, size=5), 
	    	  plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 7),
	    	  strip.background = element_rect(fill="white"))	    	  
ggsave(paste(dir3, "3_DistancesMORENA.png", sep="/"), plot= graf,  width = 16, height = 12)

rm(data_graph, graf)

#######
# TTR #
#######

data_ttr <- data.frame(ttr=textstat_lexdiv(cosine_dfm, measure = "TTR"), party=data_cosine$party)
number <- iniciativas
number$total <- 1
number <- summaryBy(total ~ partido_po, FUN=sum, keep.names=T, data= number)
similarity_data <- merge(similarity_data, data_ttr, by="party")
similarity_data <- merge(similarity_data, number, by.x="party", by.y="partido_po")
rm(data_ttr, number)

graf <- ggplot(similarity_data, aes(x= total, y= ttr)) + 
        geom_point(aes(color= party)) + 
        theme_bw() + 
        labs(y="TTR", x="Presented bills", title="TTR and Number of bills presented by political party") + 
        scale_color_manual(values=c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", 
                                    "#1d91c0", "#225ea8", "#253494", "#081d58", "black"),  name="Political Party") +
        theme(axis.title.x = element_text(face="bold", color="black", size=12), 
	          axis.title.y = element_text(face="bold", color="black", size=12), 
	          axis.text=element_text(angle = 45, hjust = 1, size=5), 
	    	  plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 7),
	    	  strip.background = element_rect(fill="white"))	    	
ggsave(paste(dir3, "4_TTRnumber.png", sep="/"), plot= graf,  width = 8, height = 6)


# Tidy up:
rm(cosine_dfm, graf, similarity_data, data_cosine, similarity, custom_stop_words)



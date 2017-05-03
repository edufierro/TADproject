############################################
# Eduardo Fierro Farah                     #
# eff254                                   #
# May 2 - 2017                             #
# Corpus Creator                           #
# DS-GA 3001-004, Text as Data             #
# Prof, Arthur Spirling                    #
# Teaching Assistant: Mr Patrick Chester   #
############################################

## Function to import text and creat the corpus

require(quanteda)
require(readtext)

corpus_creator <- function(dirfiles, dirtext){
	
	# Function to creat project's corpus. 
	# Takes @ dirfiles, local dir MainData and stop_words
	#       @ dirtext, local dir to text files
	
	# Returns corpus - list of 2 files: 
	#          1.- DFM (quanteda) for text files
	#          2.- Metadata for the DFM
	
    data_main <- read.csv(paste0(dirfiles, "Main.csv"), stringsAsFactors=F)
    files <- list.files(dirtext)
    files_numeric <- as.numeric(gsub(".txt", "", files))
    data_main <- subset(data_main, data_main$num %in% files_numeric)
    
    data_main$tipo <- data_main$onclicks <- data_main$estatus <- data_main$turnado <- data_main$legislatura <- NULL
    data_main$partido_po[data_main$partido_po=="-"] <- "NA/Independent/No Party"
    data_main$partido_po[data_main$partido_po=="Sin Partido"] <- "NA/Independent/No Party"
    data_main$partido_po[data_main$partido_po=="Ind."] <- "NA/Independent/No Party"
    
    # Read Text
    setwd(dirtext)
    iniciativas <- readtext("*.txt", docvarsfrom=c("filenames"))
    setwd("~")
    
    iniciativas <- data.frame(iniciativas)
    iniciativas <- merge(iniciativas, data_main, by.x="docvar1", by.y="num", all.x=T, all.y=T)
    names(iniciativas)[names(iniciativas)=="docvar1"] <- "num"
    
    custom_stop_words <- readLines(paste0(dirfiles, "OwnDict.txt"))
    custom_stop_words <- c(stopwords("spanish"), custom_stop_words)
    corpus <- dfm(iniciativas$text, remove=custom_stop_words, tolower=FALSE, remove_punct = TRUE)
    
    list_to_return <- list(corpus, iniciativas)
    return(list_to_return)

}
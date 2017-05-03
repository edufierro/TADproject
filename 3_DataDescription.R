rm(list=ls())
setwd("~")

############################################
# Eduardo Fierro Farah                     #
# eff254                                   #
# April 11 - 2017                          #
# Data Description - Party/Time            #
# DS-GA 3001-004, Text as Data             #
# Prof, Arthur Spirling                    #
# Teaching Assistant: Mr Patrick Chester   #
############################################

require(ggplot2)
require(doBy)
require(stringr)

###############
# Directories #
###############

# Output TXT location
dir1 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/TXTsOriginal/"

# Location of Main.csv
dir2 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/"

# Location for Graphs Output
dir3 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Graphs/"

##################################################
##################################################
############# Data Main Despcription #############
##################################################
##################################################

data_main <- read.csv(paste0(dir2, "Main.csv"), stringsAsFactors=F)

############
# By Dates #
############

data_graph <- data_main
dates <- str_split(data_graph$fecha, "/")
dates <- do.call(rbind, dates)
dates <- data.frame(dates)
names(dates) <- c("day", "month", "year")
dates[] <- lapply(dates, as.character)
data_graph <- cbind(data_graph, dates)
rm(dates)
data_graph$date <- as.Date(data_graph$fecha, "%d/%m/%Y")
data_graph$total <- 1
data_graph <- summaryBy(total ~ date, data=data_graph, keep.names=T, FUN=sum)
data_graph$to_group = 1

graf <- ggplot(data= data_graph, aes(x= date, y= total)) +  
	    geom_line(na.rm=TRUE, size=1, color="#253494") +
	    geom_point(na.rm=TRUE, size=2, color="#253494") +
	    theme_bw() + 
	    labs(x="Date", y="Total bills presented", title="Bills presented by day (LXIII Legislatura) \n Updated to April 6, 2017") + 
	    theme(axis.title.x = element_text(face="bold", color="black", size=10), 
	          axis.title.y = element_text(face="bold", color="black", size=12), 
	          axis.text=element_text(angle = 45, hjust = 1, size=12), 
	    	  plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 12),
	    	  strip.background = element_rect(fill="#41b6c4")) 
ggsave(paste(dir3, "1_GeneralByDate.png", sep="/"), plot= graf,  width = 14, height = 8)

rm(graf, data_graph)


######################
# By political party #
######################

data_graph <- data_main
data_graph$partido_po[data_graph$partido_po=="-"] <- "NA/Independent/No Party"
data_graph$partido_po[data_graph$partido_po=="Sin Partido"] <- "NA/Independent/No Party"
data_graph$partido_po[data_graph$partido_po=="Ind."] <- "NA/Independent/No Party"

data_graph <- data_graph[c("partido_po", "presentada")]
data_graph$total <- 1

data_graph <- summaryBy(total ~ partido_po, FUN=sum, keep.names=T, data=data_graph)
data_graph <- data_graph[order(-data_graph$total),]
data_graph$partido_po_factor <- factor(seq(from=1, to=nrow(data_graph)), labels=data_graph$partido_po)

graf <- ggplot(data_graph, aes(x="", y= total)) + 
        geom_bar(aes(fill= partido_po_factor), stat = "identity") +
        scale_fill_manual(values=c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58", "black"),  name="Political Party") + 
        coord_polar("y") + 
       theme_bw() + 
	    labs(x="", y="", title="Bills by political party \n  September 1, 2015 - April 11, 2017") + 
	    theme(plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 12),
	    	  strip.background = element_rect(fill="#41b6c4")) 
ggsave(paste(dir3, "2_GeneralByPoliticalParty.png", sep="/"), plot= graf,  width = 14, height = 8)

rm(graf, data_graph)

##############################
# Missing files in text data #
##############################

files <- list.files(dir1)
files_numeric <- as.numeric(gsub(".txt", "", files))

missing <- subset(data_main, (data_main$num %in% files_numeric) == F) # 32 files

table(missing$partido_po) # They are avenly spread, not worth doing a graph
table(missing$fecha) # Centered in later days. 
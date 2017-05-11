rm(list=ls())
setwd("~")

########################################################
# Eduardo Fierro Farah  / Akash Kadel                  #
# eff254 / ak6201                                      #
# May 3 - 2017                                         #
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


######################
# LDA; k=30 Analysis #
######################

load(paste0(dir2, "FilteredCorpus.RData"))
load(paste0(dir2, "lda_model_k30.RData"))

top10_terms <- get_terms(lda_model_k30, 10)
print(top10_terms)
rm(top10_terms)

##################################
# Top topics per political party #
##################################

distribution <- lda_model_k30@gamma
distribution <- data.frame(distribution)
names(distribution) <- c(paste0("Topic", seq(from=1, to=ncol(distribution))))
distribution$parties <- iniciativas$partido_po
distribution <- summaryBy(Topic1 + Topic2 + Topic3 + Topic4 + Topic5 + Topic6 + Topic7 + Topic8 + Topic9 + Topic10 + Topic11 + Topic12 + Topic13 + Topic14 + Topic15 + Topic16 + Topic17 + Topic18 + Topic19 + Topic20 + Topic21 + Topic22 + Topic23 + Topic24 + Topic25 + Topic26 + Topic27 + Topic28 + Topic29 + Topic30  ~ parties, FUN=mean, keep.names=T, data=distribution)

# Data to graph: 
data_plot <- distribution
data_plot <- reshape(data_plot, idvar="parties", direction="long", sep="",  varying=names(data_plot)[2:ncol(data_plot)])
names(data_plot) <- c("parties", "topic", "score")

unlabaled <- c(4, 16, 17, 19, 25, 26, 28)
data_plot$type <- "Valid Topics"
data_plot$type[data_plot$topic %in% unlabaled] <- "Unlabaled/Congressional Lingo"

graf <- ggplot(data_plot, aes(x= topic, y= score)) + 
         geom_bar(aes(fill= type), stat = "identity") + 
         facet_wrap(~ parties) + 
         theme_bw() +
         scale_fill_manual(values = c("#edf8b1", "#253494"), name="Type of Topic") + 
         labs(y="Percent", x="Topics (Number)", title="Topic distribution by party") + 
         theme(axis.title.x = element_text(face="bold", color="black", size=12), 
	          axis.title.y = element_text(face="bold", color="black", size=12), 
	          axis.text=element_text(angle = 45, hjust = 1, size=12), 
	    	  plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 12),
	    	  strip.background = element_rect(fill="white"))
ggsave(paste(dir1, "5_TopicDistribution.png", sep="/"), plot= graf,  width = 14, height = 8)

rm(graf, unlabaled, distribution)

#####################################
# Top topics per political party V2 #
#####################################

data_plot2 <- subset(data_plot, data_plot$type=="Valid Topics")

graf <- ggplot(data_plot2, aes(x= topic, y= score)) + 
         geom_bar(aes(fill= parties), stat = "identity", color="black") + 
         facet_wrap(~ parties) + 
         theme_bw() +
         scale_fill_manual(values = c("#ffffd9", "#edf8b1", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#253494", "#081d58", "black"), name="Party") + 
         labs(y="Percent", x="Topics (Number)", title="Topic distribution by party") + 
         theme(axis.title.x = element_text(face="bold", color="black", size=12), 
	          axis.title.y = element_text(face="bold", color="black", size=12), 
	          axis.text=element_text(angle = 45, hjust = 1, size=12), 
	    	  plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 12),
	    	  legend.position="none",
	    	  strip.background = element_rect(fill="white"))
ggsave(paste(dir1, "6_TopicDistributionSubset.png", sep="/"), plot= graf,  width = 14, height = 8)

rm(graf)

# Get the numbers: 
women <- subset(data_plot, data_plot$topic==20)
women <- women[order(-women$score),]
print(women)

rm(women, data_plot2, data_plot)

#######################################
# Where do they differ? MORENA vs PRD #
#######################################

distribution <- lda_model_k30@gamma
distribution <- data.frame(distribution)
names(distribution) <- c(paste0("Topic", seq(from=1, to=ncol(distribution))))
distribution$parties <- iniciativas$partido_po
distribution <- summaryBy(Topic1 + Topic2 + Topic3 + Topic4 + Topic5 + Topic6 + Topic7 + Topic8 + Topic9 + Topic10 + Topic11 + Topic12 + Topic13 + Topic14 + Topic15 + Topic16 + Topic17 + Topic18 + Topic19 + Topic20 + Topic21 + Topic22 + Topic23 + Topic24 + Topic25 + Topic26 + Topic27 + Topic28 + Topic29 + Topic30  ~ parties, FUN=mean, keep.names=T, data=distribution)

distribution <- subset(distribution, distribution$parties=="MORENA" | distribution$parties=="PRD")
distribution <- reshape(distribution, idvar="parties", direction="long", sep="",  varying=names(distribution)[2:ncol(distribution)])
row.names(distribution) <- seq(from=1, to=nrow(distribution))
names(distribution) <- c("party", "topic", "P_")
distribution <- reshape(distribution, idvar="topic", direction="wide", sep="",  timevar="party")
distribution$dif_morenaMprd <- distribution$P_MORENA - distribution$P_PRD 
morena_prd <- distribution

# Add mean use among all corpus: 
distribution <- lda_model_k30@gamma
distribution <- data.frame(distribution)
names(distribution) <- c(paste0("Topic", seq(from=1, to=ncol(distribution))))
distribution$parties <- iniciativas$partido_po
distribution$all <- "all"
distribution <- summaryBy(Topic1 + Topic2 + Topic3 + Topic4 + Topic5 + Topic6 + Topic7 + Topic8 + Topic9 + Topic10 + Topic11 + Topic12 + Topic13 + Topic14 + Topic15 + Topic16 + Topic17 + Topic18 + Topic19 + Topic20 + Topic21 + Topic22 + Topic23 + Topic24 + Topic25 + Topic26 + Topic27 + Topic28 + Topic29 + Topic30 ~ all, FUN=mean, keep.names=T, data=distribution)

morena_prd$mean <- t(distribution[1,2:31])

# FUNCTION FROM: http://stackoverflow.com/questions/11335836/increase-number-of-axis-ticks
number_ticks <- function(n) {function(limits) pretty(limits, n)}
graf <- ggplot(morena_prd, aes(x= topic, y= dif_morenaMprd)) + 
        geom_bar(aes(fill= mean), stat = "identity", color="black") + 
         theme_bw() +
         scale_x_continuous(breaks=number_ticks(30)) + 
         scale_fill_continuous(low="#edf8b1", high="#081d58", name="Mean use \n among corpus") + 
         labs(y="Difference on percentage use (MORENA - PRD)", x="Topics (Number)", title="Difference of use by topic between left-wing parties \n Morena - PRD ") + 
         theme(axis.title.x = element_text(face="bold", color="black", size=12), 
	          axis.title.y = element_text(face="bold", color="black", size=12), 
	          axis.text=element_text(angle = 45, hjust = 1, size=12), 
	    	  plot.title = element_text(face="bold", color = "black", size=16), 
	    	  legend.text=element_text(face="bold", color="black", size= 12),
	    	  strip.background = element_rect(fill="white"))
ggsave(paste(dir1, "7_TopicMoprenaMinusPRD.png", sep="/"), plot= graf,  width = 14, height = 8)

# To get the numbers: 
print(morena_prd[order(abs(-morena_prd$dif_morenaMprd)),])

rm(graf, distribution, number_ticks, morena_prd)


rm(list=ls())
setwd("~")

############################################
# Eduardo Fierro Farah                     #
# eff254                                   #
# April 11 - 2017                          #
# PDF TO TXT - Scrapper from SIL - Mexico  #
# DS-GA 3001-004, Text as Data             #
# Prof, Arthur Spirling                    #
# Teaching Assistant: Mr Patrick Chester   #
############################################

require(pdftools) # Require  "poppler", available from brew
require(stringr)

################
# Directiories #
################

# PDF's Location
dir1 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/PDFsOriginal/"

# Output TXT location
dir2 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/TXTsOriginal/"

# Location of Main.csv
dir3 <- "/Users/eduardofierro/Google Drive/SegundoSemestre/TextAsData/Project/Data/"

###################
# FROM PDF TO TXT #
###################

files <- list.files(dir1)
pb <- txtProgressBar(min=1, max=length(files), style=3)
for (x in 1:length(files)){
	filename <- str_split(files[x], "[.]")[[1]][1]
	txt <- pdf_text(paste0(dir1, files[x]))
	writeLines(txt, paste0(dir2, filename, ".txt"))
	rm(txt, filename)
	setTxtProgressBar(pb, x)  
}
rm(x)

#################
# Missing files #
#################

# I got some errors, check which files are missing: 

pdfs_names <- list.files(dir1)
txt_names <- list.files(dir2)

main_data <- read.csv(paste0(dir3, "Main.csv"))

pdfs_names <- str_split(pdfs_names, "[.]")
pdfs_names <- do.call(rbind, pdfs_names)
pdfs_names <- as.numeric(pdfs_names[,1])

txt_names <- str_split(txt_names, "[.]")
txt_names <- do.call(rbind, txt_names)
txt_names <- as.numeric(txt_names[,1])

table(pdfs_names %in% txt_names) # Apparently none. Must be emtpy then or something

table(main_data$num %in% txt_names) # 32 not found. We already knew. OK. 


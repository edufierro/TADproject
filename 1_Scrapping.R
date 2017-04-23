rm(list=ls())
setwd("~")

############################################
# Eduardo Fierro Farah                     #
# eff254                                   #
# April 11 - 2017                          #
# Project Scrapper - SIL Mexico            #
# DS-GA 3001-004, Text as Data             #
# Prof, Arthur Spirling                    #
# Teaching Assistant: Mr Patrick Chester   #
############################################

require(XML)
require(plyr)
require(bitops)
require(RCurl)
require(stringr)
require(httr)
require(xlsx)
require(R2HTML)
require(readxl)

################
# Directiories #
################

# PDFS & DATA DIR ON LOCAL MACHINE
dir1 <- "_______________/Project/Data/PDFsOriginal/"

# MAIN DATA DIR
dir2 <- "_______________/Project/Data/"

########################
# First Table and URLs #
########################

# All initiatives of the LXIII Legislatura

main_table <- data.frame()

# 4361 files

pb <- txtProgressBar(min=1, max=291, style=3)
for (x in 1:291){
	url <- paste0("http://sil.gobernacion.gob.mx/Busquedas/Basica/ResultadosBusquedaBasica.php?SID=0caed04e90fba161f2df2ea821936146&Serial=521688abdeef83a15307bac0f50d983e&Reg=4361&Origen=BB&Paginas=15&pagina=", x, "&Orden=42")
	page.doc <- content(GET(url))
	page.doc <- htmlParse(page.doc)
	tables <- readHTMLTable(page.doc)
    tables <- as.data.frame(tables[[7]])
    tables[] <- lapply(tables, as.character)
    tables <- subset(tables, tables$No. != "")
    onclikcs <- xpathApply(page.doc, "//*/a", xmlGetAttr, "onclick")
    onclikcs <- do.call(rbind, onclikcs)
    onclikcs <- subset(onclikcs, str_detect(onclikcs[,1], "pp_ContenidoAsuntos"))
    onclikcs <- do.call(rbind, regmatches(onclikcs, gregexpr('"[^"]*"', onclikcs)))[,1]
    onclikcs <- gsub('\"', "" , onclikcs)
    tables <- cbind(tables, onclikcs)
    names(tables) <- c("num", "tipo", "denominacion", "clasificacion", "presentada", "fecha", "por", "partido_po", "legislatura", "turnado", "estatus", "tema", "onclicks")
    main_table <- rbind.fill(main_table, tables)	
    rm(tables, onclikcs, page.doc, url)
    setTxtProgressBar(pb, x)  
}
close(pb)
rm(pb, x)

main_table$onclicks <- as.character(main_table$onclicks)
write.csv(main_table, paste(dir2, "Main.csv", sep="/"), row.names=F)
write.xlsx(main_table, paste(dir2, "Main.xlsx", sep="/"), row.names=F)

# 4361 files --> OK.

########################
# By URL, download PDF #
########################

main_table <- read.csv(paste0(dir2, "Main.csv"))
setwd(dir1)
files <- list.files()
files <- gsub(".pdf", "", files)
setwd("~")

contains <- as.character(main_table$num) %in% files

for (x in 1:nrow(main_table)){
	if(contains[x]==FALSE){
		url <- paste0("http://sil.gobernacion.gob.mx", main_table$onclicks[x])
		page.doc <- content(GET(url))
		page.doc <- htmlParse(page.doc)
		pdf_url <- xpathSApply(page.doc, "//*/@href")[2]
		download.file(pdf_url, method="curl", destfile=paste0(dir1, main_table$num[x], ".pdf"),  mode="wb")
		rm(page.docm,url)
	}   
}
close(pb)
rm(x)

# FINAL: 4329 files (missing 32: 0.73%. NOT BAD!)



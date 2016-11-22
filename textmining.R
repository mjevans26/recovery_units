load(".RData")
library(pdftools)
library(stringr)
library(rvest)
library(dplyr)

#rus$BIOPS <- 0
#files <- list.files("/datadrive/data/PDF_OCR/section_7a2", recursive = TRUE, pattern = ".pdf", full.names = FALSE)
#files2 <- list.files("/datadrive/data/PDF_OCR/section_7a2_cbd", recursive = TRUE, pattern = ".pdf", full.names = FALSE)
biops <- data.frame(species = character(), file = character(), stringsAsFactors = FALSE)
for(i in files){
  pdf <- pdf_text(paste("/datadrive/data/PDF_OCR/section_7a2", i, sep = "/"))
  pdf <- paste(pdf, collapse = " ")
  #rus$BIOPS[str_detect(pdf[1], rus$Scientific)] <- rus$BIOPS[str_detect(pdf[1], rus$Scientific)] + 1
  if(TRUE %in% str_detect(pdf[1], rus$Scientific)){
    spp <- grep("", str_extract(pdf[1], rus$Scientific), value = TRUE)
    datfr <- data.frame(species = spp, file = i, stringsAsFactors = FALSE)
    biops <- rbind(biops, datfr)
  }
}


for(i in files2){
  pdf <- pdf_text(paste("/datadrive/data/PDF_OCR/section_7a2_cbd", i, sep = "/"))
  pdf <- paste(pdf, collapse = " ")
  #rus$BIOPS[str_detect(pdf[1], rus$Scientific)] <- rus$BIOPS[str_detect(pdf[1], rus$Scientific)] + 1
  if(TRUE %in% str_detect(pdf[1], rus$Scientific)){
    spp <- grep("", str_extract(pdf[1], rus$Scientific), value = TRUE)
    datfr <- data.frame(species = spp, file = i, stringsAsFactors = FALSE)
    biops <- rbind(biops, datfr)
  }
}

quit(save = "yes")
list<- compare$Scientific
list[c(23, 55, 30, 40, 52)] <- c("Purshia", "Urocitellus brunneus", "Lessingia germanorum", "Euphydryas editha bayensis", NA)

for(sp in list[23:52]){
print(sp)
spp_pg <- filter(TECP_domestic, grepl(sp, Scientific_Name), Federal_Listing_Status == "Endangered"|Federal_Listing_Status == "Threatened")$Species_Page
link <- read_html(spp_pg)%>%
  html_nodes('a')%>%
  #html_attr("href")%>%
  str_subset("Reclassification|Reclassify")
print(length(link))
  if(length(link)>0){
    print(link)}}
  for(i in 1:length(link)){
    download.file(url = paste("http://ecos.fws.gov", link[i], sep = ""), destfile = paste("FiveYrRev/",sp,i,".pdf", sep = ""), mode = "wb")
  }}
}
filter(five_year_review_table, Species%in%list)
str_split(five_year_review_table$Doc_Link[1], "/")[[1]][4]
lapply(list, function(x) download.file(url = paste("https://defend-esc-dev.org/esadocs/ive_year_review/")))
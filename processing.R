library(dplyr)
library(stringr)
library(tidyr)

county_stats <- read.csv("C:/Users/mevans/repos/esasummary/ESAListings/inst/extdata/US_counties_attrib.csv", header = TRUE)
county_stats$GEOID <- as.character(county_stats$GEOID)
county_stats$GEOID <- ifelse(nchar(county_stats$GEOID) == 4,
                             paste0("0", county_stats$GEOID),
                             county_stats$GEOID)

esacounties <- select(county_stats, ALAND, GEOID)%>%
  right_join(esacounties, by = "GEOID")

#get total range area for species
species$Area <- as.vector(group_by(esacounties, Scientific)%>%
  summarize(area=sum(ALAND))%>%
  select(Scientific, area)%>%
  right_join(species, by = "Scientific")%>%
  select(area))

compare$Office <- as.vector(
  select(spp_plans, Species_Scientific_Name, Plan_Lead_Office__FWS_)%>%
    right_join(compare, by = c("Species_Scientific_Name" = "Scientific"))%>%
    select(Plan_Lead_Office__FWS_)
)[c(1:28,31:36,39:59),]


plan_dates$Common <- sapply(plan_dates$Common_NameScientific_Name, function(x) 
                                              str_trim(strsplit(x, "\n")[[1]][1])
       )

plan_dates$Scientific <- sapply(plan_dates$Common_NameScientific_Name, function(x) 
  str_trim(strsplit(x, "\n")[[1]][4])
)

dat <- select(plan_dates, Plan_Name, Plan_Date, Scientific)%>%
  right_join(spp_plans, by = c("Scientific" = "Species_Scientific_Name", "Plan_Name" = "Plan_Title"))

rus <- read.csv("Species.csv", header = TRUE, sep = ",")

compare <- rus
compare[nrow(compare)+1,c(1:3)]<-c("Zapus hudsonius luteus", "New Mexico meadow jumping mouse", "Mammals")
compare[nrow(compare)+1,c(1:3)]<-c("Oncorhynchus apache", "Apache trout", "Fish")
compare[nrow(compare)+1,c(1:3)]<-c("Caretta caretta", "Pacific loggerhead sea turtle", "Reptiles")
compare[nrow(compare)+1,c(1:3)]<-c("Ovis canadensis nelsoni", "Peninsular bighorn sheep", "Mammals")
compare[nrow(compare)+1,c(1:3)]<-c("Gopherus polyphemus", "Gopher tortoise", "Reptiles")
compare[nrow(compare)+1,c(1:3)]<-c("Rana sevosa", "dusky gopher frog", "Amphibians")
compare[nrow(compare)+1,c(1:3)]<-c("Bufo houstonensis", "Houston toad", "Amphibians")
compare[nrow(compare)+1,c(1:3)]<-c("Euphydryas editha taylori", "Taylors (=whulge) Checkerspot", "Insects")
compare[nrow(compare)+1,c(1:3)]<-c("Euphydryas editha bayensis", "Bay checkerspot butterly", "Insects")
compare[nrow(compare)+1,c(1:3)]<-c("Lycaeides argyrognomon lotis", "Lotis blue butterfly", "Insects")
compare[nrow(compare)+1,c(1:3)]<-c("Euphilotes enoptes smithi", "Smith's blue butterfly", "Insects")
compare[nrow(compare)+1,c(1:3)]<-c("Chorizanthe howelii", "Howell's spineflower", "Plants")
compare[nrow(compare)+1,c(1:3)]<-c("Chorianthe pungens var. hartwegiana", "Ben Lomond spineflower", "Plants")
compare[nrow(compare)+1,c(1:3)]<-c("Myotis grisescens", "Gray bat", "Mammals")
compare[nrow(compare)+1,c(1:3)]<-c("Graptemys oculifera", "Ringed map turtle", "Reptiles")
compare[nrow(compare)+1,c(1:3)]<-c("Strix occidentalis lucida", "Mexican spotted owl", "Birds")
compare[nrow(compare)+1,c(1:3)]<-c("Gila cypha", "Humpback chub", "Fish")

compare$Units[is.na(compare$Units)] <- 0
compare$Range[is.na(compare$Range)] <- 
  
sapply(1:nrow(compare), function(x, y, z) {
  if(is.na(y$Region[x])){
    y$Region[x] <- z$Lead Region[z$Species == y$Scientific[x]]}}, y= compare, z = species_info_table)
    try(y$Range[x] <- species$count[species$Scientific == y$Scientific[x]])}}, y = compare)

#create 'counties' dataset
counties<-group_by(esacounties, GEOID)%>%
  summarise(count = n())

#counties$Species <- sapply(counties$GEOID, function(x,y) y$Scientific[y$GEOID == x], y = esacounties)

counties <- dplyr::left_join(counties, select(county_attrib, GEOID, INTPTLAT, INTPTLON, NAME),by = "GEOID")


#create species dataset
species <- dplyr::group_by(esacounties, Scientific)%>%
  summarise(count = n())%>%
  arrange(count)


dat$Group <- sapply(dat$Species_Taxonomic_Group, function(x)
  if(x == "Ferns and Allies"|x == "Flowering Plants"|x == "Conifers and Cycads"|x == "Lichens"){
    "Plants and Lichens"}
  else if(x == "Snails"|x=="Clams"){
    "Molluscs"}else{x})

anti_join(filter(compare, is.na(Region)), spp_plans, by = c("Scientific" = "Species_Scientific_Name"))

#unnest full into rows for each species/consultation
test <- select(full, activity_code, ESOffice, start_date, timely_concl, consult_type, consult_complex, spp_ev_ls, n_jeop, n_admo, n_nofx)%>%
  unnest(spp_ev_ls)

test$Scientific <- vapply(1:length(test$Scientific), function(x, y) {
  s<- strsplit(y[x], "[()]")[[1]]
  if(length(s) == 2){
  str_trim(s[2])}else if (length(s) == 4){paste(str_trim(s)[c(2,4)], collapse = " ")}else{""}}, y = test$spp_ev_ls, character(1))


compare$Date <- left_join(compare, select(plan_dates, Scientific, Plan_Date), by = "Scientific")[c(1:10,17:34,36:41,43:56),7]
compare[30,9] <- "08/08/2003"
compare[39,9] <- "06/29/2010"

compare$admo <- as.data.frame(filter(test,!is.na(n_admo), n_admo>0)%>%
  group_by(Scientific)%>%
  summarise(admo = n())%>%
  select(Scientific, admo)%>%
  right_join(compare, by = "Scientific")%>%
  select(admo))


for(i in 1:nrow(compare)){
  if(compare$Units[i]>0){
  compare$admo[i] <-filter(test, Scientific == compare$Scientific[i], 
         as.Date(start_date, format = "%Y-%m-%d")>= compare$ymd[i],
         !is.na(n_admo), n_admo>0)%>%
        nrow()
#    group_by(Scientific)%>%
#    summarise(cons = n())%>%
#    select(cons)%>%
#    unlist(use.names=FALSE)
  }else{
    compare$admo[i] <-filter(test, Scientific == compare$Scientific[i],
                              !is.na(n_admo), n_admo>0)%>%
      nrow()
#      group_by(Scientific)%>%
#      summarise(cons = n()) 
#      select(cons)%>%
#      unlist(use.names=FALSE)
  } 
}

#check for species in consultation df
for(name in compare$Scientific){
  if(!(name%in%five_year_review_table$Species)){paste(print(name%in%five_year_review_table$Species), print(name), sep = " ")}}

for(name in us$Office){
  if(!(name%in%office_cont$Office)){paste(print(name%in%office_cont$Office), print(name), sep = " ")}}
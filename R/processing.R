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

#get total range area for species based on counties
species$Area <- as.vector(group_by(esacounties, Scientific)%>%
  summarize(area=sum(ALAND))%>%
  select(Scientific, area)%>%
  right_join(species, by = "Scientific")%>%
  select(area))

clean_data2 <- as.data.frame(select(spp_plans, Species_Scientific_Name, Plan_Lead_Office__FWS_)%>%
    group_by(Species_Scientific_Name)%>%
    summarise(Office = first(Plan_Lead_Office__FWS_))%>%
    right_join(clean_data, by = c("Species_Scientific_Name" = "Scientific")))

temp <- as.data.frame(group_by(expenditures, scientific)%>%
                        summarise(Total = mean(Fed_tot))%>%
                        right_join(temp, by = c("scientific" = "Species_Scientific_Name")))


plan_dates$Common <- sapply(plan_dates$Common_NameScientific_Name, function(x) 
                                              str_trim(strsplit(x, "\n")[[1]][1]))

plan_dates$Scientific <- sapply(plan_dates$Common_NameScientific_Name, function(x) 
  str_trim(strsplit(x, "\n")[[1]][4]))

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

species2 <- filter(TECP_domestic, Federal_Listing_Status == "Endangered"|Federal_Listing_Status == "Threatened", Lead_Region != "NMFS")%>%
  select(Scientific_Name, Species_Group, Lead_Region, Federal_Listing_Status)%>%
  inner_join(., species, by = c("Scientific_Name" = "Scientific"))

species2$Species_Group <- sapply(species2$Species_Group, function(x)
  if(x == "Ferns and Allies"|x == "Flowering Plants"|x == "Conifers and Cycads"|x == "Lichens"){
    "Plants and Lichens"}
  else if(x == "Snails"|x=="Clams"){
    "Molluscs"}else{x})

species2[37, 5:6] <- c(2, 5429470554)
species2[38, 5:6] <- c(1, 7083927512)
species2[39, 5:6] <- c(30, 152222337622 - 7083927512 - 5429470554)
#roseate tern
species2[1131, "Area"] <- 88790046016
species2[1130, 5:6] <- c(4, 126597256825 - 88790046016)
#rana muscosa
species2[1251, 5:6] <- c(20, 39862735170)
species2[1252, 5:6] <- c(28, 64115867954 - 39862735170)
#Loggerhead
species2[177, 5:6] <- c(84, 168262400000)
species2[178, 5:6] <- c(7, 40726910000)
#remove threatened pop of gray wolf, piping plover, unknown green sea turtle pops 
species2 <- species2[-c(173, 207, 212, 213, 215, 217), ]

species2$Priority <- filter(spp_plans, Species_Listing_Status == "Threatened"|Species_Listing_Status=="Endangered")%>%
                                     select(Species_Scientific_Name, Species_Recovery_Priority_Number)%>%
                                      distinct()%>%
                                     right_join(species2, by = c("Species_Scientific_Name" = "Scientific_Name"))%>%
                                     select(Species_Recovery_Priority_Number)
species2$Priority <- as.integer(gsub("C", "", species2$Priority2))
species2$Priority2 <- NULL
species2[2:4, ] <- as.factor(species2[2:4, ])

TECP_domestic <- filter(TECP_table, U_S__or_ForeignListed == "US"|U_S__or_ForeignListed == "US/Foreign", 
                        Federal_Listing_Status == "Threatened"|Federal_Listing_Status == "Endangered")

species2 <- species2[!(species2$Scientific %in% compare$Scientific), ]

species2 <- species2[species2$Scientific %in% TECP_domestic$Scientific_Name, ]

t <- select(miss_recover, Scientific, number)%>%
  distinct()

for(i in species2$Scientific){
  if (i %in% t$Scientific){
    species2$Priority[species2$Scientific == i] <- t$number[t$Scientific==i]
  }
}

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

compare$frate <- compare$fcons/compare$cons

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
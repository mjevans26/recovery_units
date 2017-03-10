#chi square test for distribution of taxonomic groups
str(chisq.test(group_cont[, 2:3]))
chisq.test(group_cont[c(1:8,10), 2:3])

plot_ly(filter(group_cont, Group != "Plants and Lichens"), x = ~Group)%>%
  add_trace(type = "bar", x = ~Group, y = ~RUs.p, name = "R Units")%>%
  add_trace(type = "bar", x = ~Group, y = ~All.p, name = "All")

#Wilcoxon rank test for difference in median range sizes
wilcox.test(x = species$count, y = rus$Range, alternative = "l", paired = FALSE)
wilcox.test(x = species$Area, y = filter(compare, Units>0)$Area, alternative = "l", paired = FALSE)

plot_ly(alpha = 0.75)%>%
  add_histogram(data = species, x = ~log(Area), histnorm = "probability", name = "All", xbins = list(start = 15, end = 35, size =0.5))%>%
  add_histogram(data = filter(compare, Units>0), x = ~log(Area), histnorm = "probability", name = "RUs", xbins = list(start = 15, end = 35, size =0.5))%>%
  layout(barmode = "overlay")

#Regional offices
all <- group_by(spp_plans, Plan_Lead_Office__FWS_)%>%
  summarize(plans = n())

us <-filter(compare, Units>0)%>%
  group_by(Office)%>%
  summarize(units = n())

office_cont <- left_join(all, us, by = c("Plan_Lead_Office__FWS_"="Office"))
names(office_cont) <- c("Office", "plans","units")
office_cont$units[is.na(office_cont$units)]<- 0
office_cont <- mutate(office_cont, p = units/plans)

chisq.test(office_cont[ ,2:3])
plot(office_cont$p)
tanova<-aov(p$units~Office, data = office_cont)
#temporal trends

#percent of formal consultations RUs vs non

n_formal_ru <- nrow(filter(test, grepl("Formal", consult_type), Scientific%in%compare$Scientific[compare$Units>0]))
n_ru <- nrow(filter(compare, Units >0))

n_formal <- nrow(filter(test, grepl("Formal", consult_type)))
n_all <- nrow(plan_dates)

chisq.test(x = c(n_ru, n_all), y = c(n_formal_ru, n_formal))

chisq.test()
#All
plot_ly(type = "box")%>%
  add_trace(y = table(test$Scientific, test$consult_type)[,1]/table(test$Scientific), name = "All")%>%
  add_trace(y = compare$fcons[compare$Units>0]/compare$cons[compare$Units>0], type = "box", name = "RUs")
# Head-to-head
plot_ly(type = "box")%>%
  add_trace(y = compare$fcons[compare$Units>0]/compare$cons[compare$Units>0], type = "box", name = "RUs")%>%
  add_trace(y = compare$fcons[compare$Units == 0]/compare$cons[compare$Units == 0], type = "box", name = "Non")

#Gila trout vs. Apache Trout

#loggerhead atlantic vs. pacific

#preble's meadow jumping mouse vs. new mexico meadow jumping mouse

#sierra nevada bighorn vs. peninsular bighorn

#desert tortoise vs. gopher tortoise

#california red-legged frog vs. dusky gopher frog 

#Arroyo southwestern toad vs. Houston toad

#Quino checkerspot vs. taylor's checkerspot vs. bay checkerspot

#karner blue vs Lotis blue

#El Segundo blue vs. smith's blue

# robust vs. monterrey spineflower

#indiana vs. gray

#bog vs. ringed map turtle

#northern vs. mexican spotted owl

#bonytail vs. humpback chub
taxa_stats <- group_by(goddamn, Group)%>%summarise(Area_md = median(Area, na.rm = TRUE), 
                                                   Area_mn = mean(Area, na.rm = TRUE),
                                                   Area_sd = sd(Area, na.rm = TRUE),
                                                   Prior_md = median(Priority, na.rm = TRUE), 
                                                   Prior_mn = mean(Priority, na.rm = TRUE),
                                                   Prior_sd = sd(Priority, na.rm = TRUE),
                                                   Scholar_md = median(scholar, na.rm = TRUE),
                                                   Scholar_mn = mean(scholar, na.rm = TRUE),
                                                   Scholar_sd = sd(scholar, na.rm = TRUE),
                                                   count_all = n())
boxplot(
  compare$Area[compare$Group == "Plants" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Plants"],
  compare$Area[compare$Group == "Insects" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Insects"],
  compare$Area[compare$Group == "Amphibians" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Amphibians"],
  compare$Area[compare$Group == "Birds" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Birds"],
  compare$Area[compare$Group == "Reptiles" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Reptiles"],
  compare$Area[compare$Group == "Fish" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Fish"],
  compare$Area[compare$Group == "Mammals" & compare$group == "R"] - taxa_stats$Area_md[taxa_stats$Group == "Mammals"])

z_score <- function(df, var, Group){
  v <- c()
  for (i in unique(Group)){
    z <- (log(df$var[df$Group == i]) - mean(log(df$var[df$Group == i]), na.rm = TRUE))/sd(log(df$var[df$Group == i]), na.rm = TRUE) 
    v<- append(v, z, after = length(v))
  }
  return(v)
}

for(i in c("Plants", "Insects", "Amphibians", "Birds", "Reptiles", "Fish", "Mammals", "Arachnids", "Crustaceans", "Molluscs")){
  #goddamn$Area_z[goddamn$Group == i] <- (goddamn$Area[goddamn$Group == i] - taxa_stats$Area_md[taxa_stats$Group == i])/taxa_stats$Area_sd[taxa_stats$Group == i]
  #goddamn$Priority_z[goddamn$Group == i] <- (goddamn$Priority[goddamn$Group == i] - taxa_stats$Prior_mn[taxa_stats$Group == i])/taxa_stats$Prior_sd[taxa_stats$Group == i]
  temp$Total_z[temp$Group == i] <- (log(temp$Total[temp$Group == i]) - mean(log(temp$Total[temp$Group == i]), na.rm = TRUE))/sd(log(temp$Total[temp$Group == i]), na.rm = TRUE)
}



that <- aggregate(scholar ~ cut(ymd, "5 year")*Group, goddamn, mean)

for (i in c("1980", "1985", "1990", "1995", "2000", "2005", "2010","2015")){
  #if(!is.na(goddamn$Year[i])){
    #goddamn$Scholar_z[i] <- (goddamn$scholar[i] - median(goddamn$scholar[goddamn$Group == goddamn$Group[i] & goddamn$Year == goddamn$Year[i]], na.rm = TRUE))/sd(goddamn$scholar[goddamn$Group == goddamn$Group[i] & goddamn$Year == goddamn$Year[i]], na.rm = TRUE)
    #goddamn$Scholar_z[i] <- (goddamn$scholar[i] - median(goddamn$scholar[goddamn$Year == goddamn$Year[i]], na.rm=TRUE))/sd(goddamn$scholar[goddamn$Year == goddamn$Year[i]], na.rm=TRUE)
    goddamn$Scholar_z[!is.na(goddamn$scholar) & is.na(goddamn$Scholar_z) & !is.na(goddamn$Year) & goddamn$Year == paste(i,"01-01",sep ="-")] <- (goddamn$scholar[!is.na(goddamn$scholar) & is.na(goddamn$Scholar_z) & !is.na(goddamn$Year) & goddamn$Year == paste(i,"01-01",sep ="-")] - median(goddamn$scholar[goddamn$Year == paste(i,"01-01",sep ="-")], na.rm=TRUE))/sd(goddamn$scholar[goddamn$Year == paste(i,"01-01",sep ="-")], na.rm=TRUE)
    #}else{
    #goddamn$Scholar_z[i] <- (goddamn$scholar[i] - median(goddamn$scholar[goddamn$Group == goddamn$Group[i]], na.rm = TRUE))/sd(goddamn$scholar[goddamn$Group == goddamn$Group[i]], na.rm = TRUE)
  #}
} 
paste(i,"01-01",sep =)

goddamn$Scholar_z[!is.na(goddamn$scholar) & is.na(goddamn$Scholar_z) & goddamn$Year == paste(i,"01-01",sep =)] <- (goddamn$scholar - median(goddamn$scholar[goddamn$Year == goddamn$Year[i]], na.rm=TRUE))/sd(goddamn$scholar[goddamn$Year == goddamn$Year[i]], na.rm=TRUE)
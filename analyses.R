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


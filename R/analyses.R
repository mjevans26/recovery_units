library(dplyr)
library(ggplot2)
library(lme4)
library(mclogit)
library(multcomp)
library(party)
library(plotly)
library(pROC)
library(shiny)
library(stringr)
library(tidyr)

# set output directory for tables/figures
outdir = 'C:/Users/mevans/OneDrive - Defenders of Wildlife/Analyses/RUs'

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
taxa_stats <- group_by(clean_data, Group)%>%summarise(Area_md = median(Area, na.rm = TRUE), 
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
  #clean_data$Area_z[clean_data$Group == i] <- (clean_data$Area[clean_data$Group == i] - taxa_stats$Area_md[taxa_stats$Group == i])/taxa_stats$Area_sd[taxa_stats$Group == i]
  #clean_data$Priority_z[clean_data$Group == i] <- (clean_data$Priority[clean_data$Group == i] - taxa_stats$Prior_mn[taxa_stats$Group == i])/taxa_stats$Prior_sd[taxa_stats$Group == i]
  temp$Total_z[temp$Group == i] <- (log(temp$Total[temp$Group == i]) - mean(log(temp$Total[temp$Group == i]), na.rm = TRUE))/sd(log(temp$Total[temp$Group == i]), na.rm = TRUE)
}



that <- aggregate(scholar ~ cut(ymd, "5 year")*Group, clean_data, mean)

for (i in c("1980", "1985", "1990", "1995", "2000", "2005", "2010","2015")){
  #if(!is.na(clean_data$Year[i])){
    #clean_data$Scholar_z[i] <- (clean_data$scholar[i] - median(clean_data$scholar[clean_data$Group == clean_data$Group[i] & clean_data$Year == clean_data$Year[i]], na.rm = TRUE))/sd(clean_data$scholar[clean_data$Group == clean_data$Group[i] & clean_data$Year == clean_data$Year[i]], na.rm = TRUE)
    #clean_data$Scholar_z[i] <- (clean_data$scholar[i] - median(clean_data$scholar[clean_data$Year == clean_data$Year[i]], na.rm=TRUE))/sd(clean_data$scholar[clean_data$Year == clean_data$Year[i]], na.rm=TRUE)
    clean_data$Scholar_z[!is.na(clean_data$scholar) & is.na(clean_data$Scholar_z) & !is.na(clean_data$Year) & clean_data$Year == paste(i,"01-01",sep ="-")] <- (clean_data$scholar[!is.na(clean_data$scholar) & is.na(clean_data$Scholar_z) & !is.na(clean_data$Year) & clean_data$Year == paste(i,"01-01",sep ="-")] - median(clean_data$scholar[clean_data$Year == paste(i,"01-01",sep ="-")], na.rm=TRUE))/sd(clean_data$scholar[clean_data$Year == paste(i,"01-01",sep ="-")], na.rm=TRUE)
    #}else{
    #clean_data$Scholar_z[i] <- (clean_data$scholar[i] - median(clean_data$scholar[clean_data$Group == clean_data$Group[i]], na.rm = TRUE))/sd(clean_data$scholar[clean_data$Group == clean_data$Group[i]], na.rm = TRUE)
  #}
} 
paste(i,"01-01",sep =)

clean_data$Scholar_z[!is.na(clean_data$scholar) & is.na(clean_data$Scholar_z) & clean_data$Year == paste(i,"01-01",sep =)] <- (clean_data$scholar - median(clean_data$scholar[clean_data$Year == clean_data$Year[i]], na.rm=TRUE))/sd(clean_data$scholar[clean_data$Year == clean_data$Year[i]], na.rm=TRUE)

# FIGURES
# add orca command line utility to R environmental path
Sys.setenv("PATH" = paste(Sys.getenv("PATH"), "C:\\Users\\mevans\\AppData\\Local\\Programs\\orca", sep = .Platform$path.sep))

# define common axis formatting parameters
ax <- list(
  titlefont = list(family = 'serif', color = 'black', size = 16),
  showgrid = FALSE,
  zerolinewidth = 2,
  tickfont = list(family = 'serif', color = 'black', size = 12)
)

# Timeline of plan designations
fig1 <- plot_ly(timeline, x = ~substr(Year, 1, 4), y = ~num,
        type = "scatter", mode = "lines")%>%
  layout(
    yaxis = append(list(title = "Recovery plans<br>designating recovery units",
                        linewidth = 2),
                        ax),
    xaxis = append(list(title = 'Year',
                        tickangle = 45), ax),
    margin = list(r = 50)
    )

orca(fig1, file = "fig1.png", format = 'png', scale = 20)

fig3 <- plot_ly(data = group_cont, x = ~ All.d, y = ~RUs.d)%>%
  add_trace(type = "scatter", mode = "markers",
            size = ~All, sizes = c(50,500),
            text = ~paste(All, "species with Recovery Plans are", Group, "(", round(All.d * 100, 2),"%)",
                          "<br> and", RUs, "species with Recovery Units are", Group, "(", round(RUs.d * 100, 2),"%)"),
            hoverinfo = 'text',
            marker = list(color = brewer.pal(10, 'Spectral'),
                          line = list(color = brewer.pal(10, 'Spectral'))),
            showlegend = FALSE)%>%
  add_trace(type = "scatter", mode = "lines", x = c(0, 0.25), y = c(0, 0.25),
            showlegend = FALSE, line = list(color = 'black', width = 1, dash = 'dash'),
            text = "1:1 Line", hoverinfo = 'text')%>%
  add_annotations(text = ~Group,
                  font = list(color = "black", family = 'serif', size = 8),
                  showarrow = TRUE,
                  arrowcolor = 'black',
                  arrowhead = 0,
                  arrowsize = 0.5,
                  ax = c(20, 25, 20, 25, 20, 20, 20, 30, -45, 35),
                  ay = c(20, -20, 20, -20, 20, -20, -10, -15, 45, 0),
                  showlegend = FALSE)%>%
  layout(#title = "Distribution of Taxonomic Groups",
    barmode = "stack",
    xaxis = append(list(title = "Proportion of all species"), ax),
    yaxis = append(list(title = "Proportion of species with recovery units"), ax),
    legend = list(x = 0.1, y = 1))

orca(fig3, file = 'fig3.png', format = 'png', scale = 20, width = 600, height = 300)

fig5 <- plot_ly(data = filter(group_cont, Group != "Crustaceans", Group != "Molluscs"),
        x = ~ p, y = ~pM)%>%
  add_trace(type = "scatter", mode = "markers",
            size = ~Biops, sizes = c(50,500),
            marker = list(symbol = "circle",
                          color = rainbow(10, alpha = 1)[c(1:3,5:7,9:10)],
                          line = list(color = 'black')),
            text = ~paste(Biops*pM, "BiOps for", Group, "mentioned Recovery Units(", round(pM * 100, 2),"%)",
                          "<br> and", Biops*pA, "used Recovery Units in jeopardy analysis (",
                          round(pA * 100, 2),"%)"),
            hoverinfo = 'text',
            name = "Mentioned")%>%
  add_trace(type = "scatter", mode = "lines",
            x = c(0, 0.2), y = c(0.4494, 0.4494+(0.2*2.3839)),
            showlegend = TRUE,
            line = list(color = 'black', width = 1, dash = 'dash'),
            text = "Mention trend", hoverinfo = 'text',
            name = 'Mention trend')%>%
  add_trace(y = ~pA, type = "scatter", mode = "markers",
            size = ~Biops, sizes = c(10,500),
            marker = list(symbol = "square",
                          color = rainbow(10, alpha = 1)[c(1:3,5:7,9:10)],
                          line = list(color = 'black')),
            name = "Used")%>% 
  add_trace(type = "scatter", mode = "lines", x = c(0, 0.2), y = c(0.2291, 0.2291+(0.2*2.703)),
            showlegend = TRUE, line = list(color = 'black', width = 1, dash = 'dot'),
            text = "Use trend", hoverinfo = 'text',
            name = 'Use trend')%>%
  add_annotations(text = ~Group,
                  font = list(family = 'serif', color = "black"),
                  showarrow = TRUE,
                  arrowcolor = '#00000000',
                  arrowsize = ~ 0.1,
                  ax = c(30, 0, 0, -40, 0, 0, 40, 0),
                  ay = c(20, 20, 25, 20, 40, 50, 20, 30),
                  showlegend = FALSE)%>%
  layout(#title = "Recovery Units in Biological Opinions",
         barmode = "stack",
         xaxis = append(list(title = "Odds of Recovery Unit Designation"), ax),
         yaxis = append(list(title = "Proportion of BiOps", range = c(0,1.1)), ax),
         legend = list(x = 0.8, y = 0, bordercolor = 'black', borderwidth = 1))

orca(fig5, file = "fig5.png", format = 'png', scale = 20)

# read in recovery unit attribute table from GIS
ruData <- read.csv(file = 'data-raw/units.csv', header = TRUE, stringsAsFactors = FALSE)

group_by(ruData, Species)%>%
  summarize(min = min(Acres), med = median(Acres), max = max(Acres), n = n())%>%
  write.csv(file ='C:/Users/mevans/OneDrive - Defenders of Wildlife/Analyses/RUs/summary_stats.csv')
  DT::datatable(
    colnames = c('Species', 'min', 'median', 'max', 'N'),
    style = 'compact',
    options = list(
      searching = FALSE,
      paging = FALSE
    )
  )%>%
  DT::formatRound(c('min', 'med', 'max'), 0)
  
# boxplots of z-scores by ru vs all species 
fig2 <- plot_ly(data = select(clean_data, group, Area_z, Scholar_z)%>%
          gather(metric, value, -group))%>%
  add_trace(type = 'box', boxpoints = 'all', jitter = 0.5, pointpos =0,
            x = ~metric, y = ~value,
            color = ~group, colors = c('orange', 'blue'))%>%
  layout(boxmode = 'group',
         xaxis = append(list(ticktext = list('Range size', 'Citations'),
                             tickvals = list('Area_z', 'Scholar_z'),
                             tickmode = 'array'),
                        ax),
         yaxis = append(list(title = 'Z-score', linewidth = 2), ax),
         legend = list(x = 0.6, y = 1, 
                       font = list (family = 'serif', color = 'black'))
         )
orca(fig2, file = 'fig2.png', format = 'png', scale = 20, width = 600, height = 300)

fig6 <- plot_ly(data = df, type = "scatter", mode = "lines")%>%
  add_trace(name = "Mentioned",
            x = ~xm, y = ~m,
            line = list(color = "blue"),
            hoverinfo = 'text',
            text = ~paste("After", xm, "years, odds =", m))%>%
  add_trace(name = "95% CI",
            x = ~xm, y = ~mU,
            line = list(color = "blue", dash = "dash"),
            hoverinfo = 'none')%>%
  add_trace(name = "Lower",
            x = ~xm, y = ~mL,
            line = list(color = "blue", dash = "dash"),
            hoverinfo = 'none',
            showlegend = FALSE)%>%
  add_trace(name = "Used",
            x = ~xa, y = ~a,
            line = list(color = "black"),
            hoverinfo = "text",
            text = ~paste("After", xa, "years, odds =", a))%>%
  add_trace(name = "95% CI",
            x = ~xa, y = ~aU,
            line = list(color = "black", dash = "dash"),
            hoverinfo = 'none')%>%
  add_trace(name = "Lower",
            x = ~xa, y = ~aL,
            line = list(color = "black", dash = "dash"),
            hoverinfo = 'none',
            showlegend = FALSE)%>%
  layout(xaxis = append(list(title = "Years since unit designation"), ax),
         yaxis = append(list(title = "Odds of use or mention", range = c(0, 4)), ax),
         legend = list(x = 0.8, y = 1.1,
                       font = list(family = 'serif', color = 'black', size = 12)))
orca(fig6, file = 'fig6.png', format = 'png', scale = 10, width = 600, height = 300)

fig4a <- plot_ly(type = "scatter", mode = "lines")%>%
  add_trace(data = auc[2:4], x = ~specificities, y = ~sensitivities,
            line = list(color = "black"), name = "Full model")%>%
  add_trace(data = auc_noof[2:4], x = ~specificities, y = ~sensitivities,
            line = list(color = "black", dash = 'dash'), name = "No office")%>%
  add_trace(data = auc_nogr[2:4], x = ~specificities, y = ~sensitivities,
            line = list(color = "black", dash = 'dot'), name = "No taxa")%>%
  add_trace(data = auc_nogrof[2:4], x = ~specificities, y = ~sensitivities,
            line = list(color = "black", dash = 'longdashdot'), name = "No office & taxa")%>%
  layout(xaxis = append(list(title = "Specificity", autorange = "reversed", zeroline = FALSE), ax),
         yaxis = append(list(title = "Sensitivity", linewidth = 2), ax),
         legend = list(x = 0.7, y = 0.1,
                       font = list(family = 'serif', color = 'black', size = 12)))

orca(fig4a, file = 'fig4a.png', format = 'png', scale = 10, width = 600, height = 300)

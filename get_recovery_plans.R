#get_recovery_plan_links
links <- data.frame("Species" = character(0), "Link" = character(0))
for(i in badlist$Scientific){
  print(i)
  url <- TECP_domestic$Species_Page[TECP_domestic$Scientific_Name == i][1]
  page <- read_html(url)
  a_nodes <- try(html_nodes(page, "a"))
  if(class(a_nodes) == "try-error") err_res("No ,<a> nodes")
  hrefs <- html_attr(a_nodes, "href")
  r_plans <- hrefs[grepl("recovery_plan", hrefs)]
  print(r_plans)
  links <- rbind(links, data.frame(rep(i, length(r_plans)), r_plans))
}

species2$Date <- recovery_plan_table$Date[match(species2$Scientific, recovery_plan_table$Species)]

#get_recovery_plan_date
for(i in 1:nrow(species2)){
  if(is.na(species2$Date[i])){
    try(date <- TECP_domestic$Species_Page[TECP_domestic$Scientific_Name == species2$Scientific[i]][1]%>%
    read_html()%>%
    html_nodes("table")%>%
    html_table(.[grep("recovery", .)])[[1]][1,1])
    species2$Date[i] <- date
  }
}

compare[56,14] <- nrow(filter(test, Scientific == "Thamnophis gigas"))
compare[56,15] <- nrow(filter(test, Scientific == "Thamnophis gigas", consult_type == "Formal Consultation"))
compare[56,16] <- "R"
compare[56,17] <- 293/480
compare[56,12] <- sum(county_stats$ALAND[county_stats$GEOID %in% unique(filter(esacounties, Scientific == "Thamnophis gigas")%>%select(GEOID))$GEOID])
compare[56,8] <- 2
compare[56,11] <- 17
goddamn <- rbind(compare[ ,c(1,3,4,7,8,10,12,16)], species2[ , c(1,2,3,4,6,7,8,10)])
goddamn$Priority <- as.integer(goddamn$Priority)
goddamn$group[goddamn$group == "C"] <- "A"
goddamn$Group[goddamn$Group == "Plants and Lichens"] <- "Plants"
goddamn$Group[goddamn$Group == "Fishes"] <- "Fish"
goddamn$group <- droplevels(goddamn$group)
goddamn$Group <- droplevels(goddamn$Group)
goddamn$scholar <- integer(length = nrow(goddamn))

#scrape Google Scholar (doesn't work because somehow google blocks this?)
agent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0"
agent <- "Mozilla/5.0 (Windows NT 6.3; Trident/7.0; .NET4.0E; .NET4.0C; rv:11.0) like Gecko"
for(i in sample(which(is.na(goddamn$scholar)), 50, replace = FALSE)){
  text <- NA
  scientific <- str_split(goddamn[i, 1], " ")[[1]]
  scientific <- paste(scientific[!grepl("\\(", scientific)], collapse = "+")
  year <- format(goddamn[i, 6], "%Y")
  url <- paste("http://scholar.google.com/scholar?q=\"", scientific, "\"+genetic&hl=en&as_sdt=0%2C9&as_ylo=&as_yhi=",year, sep = "")
  text <- try(httr::GET(url, user_agent = agent)%>%
    httr::content("parsed")%>%
    rvest::html_nodes("#gs_ab_md")%>%
    rvest::html_text())
  if(is.na(text)){goddamn$scholar[i] <- NA}
  else if(text == " "){goddamn$scholar[i] <- 0}
  else{
  #sometimes text reads 'About XXX resuts', sometimes 'xxx results'
  text<- str_split(text, pattern = " ")[[1]]
  cites <- text[grep("result", text)-1]
  goddamn$scholar[i] <- as.numeric(gsub(",", "", cites))
  }
  Sys.sleep(30)
}

httr::GET(url, authenticate("mje12002", "Cougars26"))%>%
  content("parsed")%>%
  html_nodes("a")

url <- "http://web.b.ebscohost.com.ezproxy.lib.uconn.edu/ehost/results?sid=01835d90-d711-4c01-9218-d23bc8d5f183%40sessionmgr104&vid=0&hid=128&bquery=(SU+genetic)+AND+(AB+strix+occidentalis+caurina)&bdata=JmRiPWFwaCZjbGkwPURUMSZjbHYwPTAwMDAwMS0yMDExMTImdHlwZT0xJnNpdGU9ZWhvc3QtbGl2ZSZzY29wZT1zaXRl"
sess <- html_session(url)
form <- html_form(sess)[[1]]
filled_form <- set_values(form, 'username' = "mje12002", 'password' = "Cougars26")
submit_form(sess, filled_form)
page <- jump_to(sess, url)
doc <- read_html(page)
  html_nodes(x = doc, "body")%>%
  html_text()
  
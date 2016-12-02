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
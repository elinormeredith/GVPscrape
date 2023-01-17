# This code scrapes data from GVP website into an Excel spreadsheet
#download SelectorGadget for Chrome

#install packages and load libraries
install.packages("rvest")
install.packages("dplyr")
library(rvest)
library(dplyr)
library(readxl)
library(dplyr)

#select what page you want
#here we get the html data of the GVP website
link <- "https://volcano.si.edu/volcanolist_holocene.cfm" #link to the GVP volcanoes list 
download.file(link, destfile = "scrapedpage.html", quiet=TRUE) #download the html
page <- read_html("scrapedpage.html") #take html code

#select what you want on the page and copy the tags as links from the page
#here we want a list of the urls for each volcano
name <- page %>% html_nodes("td") %>% html_text() #names of volcanoes
volcano_links <- page %>% html_nodes("td, a") %>% html_attr("href") #lists everything to click on page
volcano_links <- na.omit(volcano_links) #omit blanks
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")#create urls
volcano_links <- data.frame(volcano_links = volcano_links) #turn into dataframe
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links)) #specify only volcanoes
volcano_links <- as.list(volcano_links$volcano_links) #list

#subsetting volcanoes - recommended to run this as it makes it run quicker
#volcano_links <- volcano_links[101:200]

#below are functions - run which info is needed from the volcanoes
#geology summary of the volcano
get_geo_summary = function(volcano_links){
volcano_page = read_html(volcano_links)
  geo_summary = volcano_page %>% html_nodes("tr:nth-child(2) p") %>%
    html_text() %>% paste(collapse = ",")
  return(geo_summary)
}

#tectonic setting of the volcano
get_t_setting = function(volcano_links){
  volcano_page = read_html(volcano_links)
 tectonic_setting = volcano_page %>% html_nodes(".shaded:nth-child(2)") %>%
    html_text() %>% paste(collapse = ",")
  return(tectonic_setting)
}

#volcano latitude co-ordinate
get_LAT = function(volcano_links){
  volcano_page = read_html(volcano_links)
  LAT = volcano_page %>% html_nodes(".clear:nth-child(1)") %>%
    html_text() %>% paste(collapse = ",")
  return(LAT)
}

#volcano longitude co-ordinate
get_LON = function(volcano_links){
  volcano_page = read_html(volcano_links)
  LON = volcano_page %>% html_nodes(".clear:nth-child(2)") %>%
    html_text() %>% paste(collapse = ",")
  return(LON)
}

#list of article references for the volcano
get_references = function(volcano_links){
  volcano_page = read_html(volcano_links)
  references = volcano_page %>% html_nodes("p+ p") %>%
    html_text() %>% paste(collapse = ",")
  return(references)
}

#elevation of volcano
get_elevation = function(volcano_links){
  volcano_page = read_html(volcano_links)
 elevation = volcano_page %>% html_nodes(".clear:nth-child(4)") %>%
    html_text() %>% paste(collapse = ",")
  return(elevation)
}

#country of volcano
get_country = function(volcano_links){
  volcano_page = read_html(volcano_links)
  country = volcano_page %>% html_nodes(".shaded:nth-child(1)") %>%
    html_text() %>% paste(collapse = ",")
  return(country)
}

#volcano GVP number
get_v_number = function(volcano_links){
  volcano_page = read_html(volcano_links)
  v_number = volcano_page %>% html_nodes(".clear:nth-child(6)") %>%
    html_text() %>% paste(collapse = ",")
  return(v_number)
}

#name of volcano
get_v_name = function(volcano_links){
  volcano_page = read_html(volcano_links)
  v_name = volcano_page %>% html_nodes("#ProfileHolocene h3") %>%
    html_text() %>% paste(collapse = ",")
  return(v_name)
}

#date of last eruption
get_last_eruption = function(volcano_links){
  volcano_page = read_html(volcano_links)
  last_eruption = volcano_page %>% html_nodes(".shaded~ .shaded+ .shaded") %>%
    html_text() %>% paste(collapse = ",")
  return(last_eruption)
}

#list of bulletin reports - note this is a lot of info
get_bulletin_reports = function(volcano_links){
  volcano_page = read_html(volcano_links)
  bulletin_reports = volcano_page %>% html_nodes("h5, .tab, .varFigCaption , .varReports .varSummary ") %>%
    html_text() %>% paste(collapse = ",")
  return(bulletin_reports)
}

#to extract the info from our volcano links, run the relevant lines
v_name = sapply(volcano_links, FUN = get_v_name)
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = sapply(volcano_links, FUN = get_bulletin_reports)

#put all data into a dataframe - remove those not extracted
alldata = data.frame(name = v_name, number = v_number, country = country, LAT = LAT, LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, 
                     key_references = references)

#save the data
write.csv(alldata,"GVPinfo.csv", row.names = FALSE)


#Making searches within the GVP report data
#filter to a search - change the search terms, for example lava flow impacts
lava_records_summary = alldata %>%
  filter(str_detect(alldata$summary, "lava"))
lava_impact_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "impact"))
lava_destroy_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "destroy"))

#bind them together
all_records_summary <- rbind(lava_records_summary, lava_impact_records_summary, lava_destroy_records_summary)

#remove duplicates
all_records_filtered <- all_records_summary %>%
  distinct()

#export
final = data.frame(volcano = all_records_filtered$name, record = all_records_filtered$bulletin_and_latest_reports)
write.csv(final,"list_impacts.csv", row.names = FALSE)

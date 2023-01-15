#download SelectorGadget for Chrome

install.packages("rvest")
install.packages("dplyr")

library(rvest)
library(dplyr)

link= "https://volcano.si.edu/volcano.cfm?vn=284040"

#take html code
page = read_html(link)

#select what you want on the page and copy the tags from the page
#here we want the text on the page
#html text pulls out the string from the text in the tags
name = page %>% html_nodes("h5, .varSummary, .varFigCaption , .tab") %>% html_text()
Miyakejima = data.frame(text = name)
Miyakejima












#download SelectorGadget for Chrome

install.packages("rvest")
install.packages("dplyr")

library(rvest)
library(dplyr)

link= "https://volcano.si.edu/volcanolist_holocene.cfm"
download.file(link, destfile = "scrapedpage.html", quiet=TRUE)

#take html code
page = read_html("scrapedpage.html")

#select what you want on the page and copy the tags from the page
#here we want the text on the page
#html text pulls out the string from the text in the tags
name = page %>% html_nodes("td") %>% html_text()
volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)


#test run
#volcano_links = "https://volcano.si.edu/volcano.cfm?vn=283001"
#volcano_links = head(volcano_links)

#subsetting
#volcano_links <- volcano_links[101:200]

get_geo_summary = function(volcano_links){
volcano_page = read_html(volcano_links)
  geo_summary = volcano_page %>% html_nodes("tr:nth-child(2) p") %>%
    html_text() %>% paste(collapse = ",")
  return(geo_summary)
}

get_t_setting = function(volcano_links){
  volcano_page = read_html(volcano_links)
 tectonic_setting = volcano_page %>% html_nodes(".shaded:nth-child(2)") %>%
    html_text() %>% paste(collapse = ",")
  return(tectonic_setting)
}

get_LAT = function(volcano_links){
  volcano_page = read_html(volcano_links)
  LAT = volcano_page %>% html_nodes(".clear:nth-child(1)") %>%
    html_text() %>% paste(collapse = ",")
  return(LAT)
}

get_LON = function(volcano_links){
  volcano_page = read_html(volcano_links)
  LON = volcano_page %>% html_nodes(".clear:nth-child(2)") %>%
    html_text() %>% paste(collapse = ",")
  return(LON)
}

get_references = function(volcano_links){
  volcano_page = read_html(volcano_links)
  references = volcano_page %>% html_nodes("p+ p") %>%
    html_text() %>% paste(collapse = ",")
  return(references)
}

get_elevation = function(volcano_links){
  volcano_page = read_html(volcano_links)
 elevation = volcano_page %>% html_nodes(".clear:nth-child(4)") %>%
    html_text() %>% paste(collapse = ",")
  return(elevation)
}

get_country = function(volcano_links){
  volcano_page = read_html(volcano_links)
  country = volcano_page %>% html_nodes(".shaded:nth-child(1)") %>%
    html_text() %>% paste(collapse = ",")
  return(country)
}

get_v_number = function(volcano_links){
  volcano_page = read_html(volcano_links)
  v_number = volcano_page %>% html_nodes(".clear:nth-child(6)") %>%
    html_text() %>% paste(collapse = ",")
  return(v_number)
}

get_v_name = function(volcano_links){
  volcano_page = read_html(volcano_links)
  v_name = volcano_page %>% html_nodes("#ProfileHolocene h3") %>%
    html_text() %>% paste(collapse = ",")
  return(v_name)
}

get_last_eruption = function(volcano_links){
  volcano_page = read_html(volcano_links)
  last_eruption = volcano_page %>% html_nodes(".shaded~ .shaded+ .shaded") %>%
    html_text() %>% paste(collapse = ",")
  return(last_eruption)
}
get_bulletin_reports = function(volcano_links){
  volcano_page = read_html(volcano_links)
  bulletin_reports = volcano_page %>% html_nodes("h5, .tab, .varFigCaption , .varReports .varSummary ") %>%
    html_text() %>% paste(collapse = ",")
  return(bulletin_reports)
}


geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = sapply(volcano_links, FUN = get_bulletin_reports)

alldata = data.frame(name = v_name, number = v_number, country = country, 
                     #LAT = LAT,
                     LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
data101_200 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data101_200,"data101_200.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[201:300]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data201_300 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data201_300,"data201_300.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[301:400]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data301_400 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data301_400,"data301_400.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[401:500]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data401_500 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data401_500,"data401_500.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[501:600]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data501_600 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data501_600,"data501_600.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[601:700]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data601_700 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data601_700,"data601_700.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[701:800]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data701_800 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data701_800,"data701_800.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[801:900]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data801_900 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data801_900,"data801_900.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[901:1000]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data901_1000 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data901_1000,"data901_1000.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[1001:1100]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data1001_1100 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data1001_1100,"data1001_1100.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[1101:1200]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data1101_1200 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data1101_1200,"data1101_1200.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[1201:1300]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data1201_1300 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data1201_1300,"data1201_1300.csv", row.names = FALSE)

volcano_links = page %>% html_nodes("td, a") %>% html_attr("href") 
volcano_links <- na.omit(volcano_links)
volcano_links <- volcano_links %>% paste("https://volcano.si.edu/", ., sep ="")
volcano_links <- data.frame(volcano_links = volcano_links)
volcano_links <- volcano_links %>% filter(grepl('https://volcano.si.edu/volcano.', volcano_links))
volcano_links = as.list(volcano_links$volcano_links)
volcano_links <- volcano_links[1301:1331]
geo_summary = sapply(volcano_links, FUN = get_geo_summary)
v_name = sapply(volcano_links, FUN = get_v_name)
v_number = sapply(volcano_links, FUN = get_v_number)
country = sapply(volcano_links, FUN = get_country)
last_eruption = sapply(volcano_links, FUN = get_last_eruption)
tectonic_setting = sapply(volcano_links, FUN = get_t_setting)
references = sapply(volcano_links, FUN = get_references)
LAT = sapply(volcano_links, FUN = get_LAT)
LON = sapply(volcano_links, FUN = get_LON)
elevation = sapply(volcano_links, FUN = get_elevation)
bulletin_reports = elevation = sapply(volcano_links, FUN = get_bulletin_reports)
elevation = sapply(volcano_links, FUN = get_elevation)
data1301_1331 = data.frame(name = v_name, number = v_number, country = country, LAT = LAT,LON = LON, elevation = elevation, last_eruption = last_eruption, volcano_type = tectonic_setting, summary = geo_summary, bulletin_and_latest_reports = bulletin_reports, key_references = references)
write.csv(data1301_1331,"data1301_1331.csv", row.names = FALSE)

library(readxl)
library(dplyr)
volcano_list <- read_excel("volcano_list.xlsx")

test = left_join(volcano_list, data, by="name")
data = rbind(data100,data101_200,data201_300,data301_400,data401_500,data501_600,data601_700,data701_800,data801_900,data901_1000,data1001_1100,data1101_1200,data1201_1300,data1301_1331)


names = data$name
df = data.frame(name = names)

#filter to lava flow impacts
lava_records = alldata %>%
  filter(str_detect(alldata$bulletin_and_latest_reports, "lava"))
lava_impact_records = lava_records %>%
  filter(str_detect(lava_records$bulletin_and_latest_reports, "impact"))
lava_damage_records = lava_records %>%
  filter(str_detect(lava_records$bulletin_and_latest_reports, "damage"))
lava_destroy_records = lava_records %>%
  filter(str_detect(lava_records$bulletin_and_latest_reports, "destroy"))
lava_flow_destroy_records = lava_records %>%
  filter(str_detect(lava_records$bulletin_and_latest_reports, "lava flow destroy"))
lava_inundated_records = lava_records %>%
  filter(str_detect(lava_records$bulletin_and_latest_reports, "inundated"))
lava_buried_records = lava_records %>%
  filter(str_detect(lava_records$bulletin_and_latest_reports, "buried"))


lava_records_summary = alldata %>%
  filter(str_detect(alldata$summary, "lava"))
lava_impact_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "impact"))
lava_damage_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "damage"))
lava_destroy_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "destroy"))
lava_flow_destroy_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "lava flow destroy"))
lava_inundated_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "inundated"))
lava_buried_records_summary = lava_records %>%
  filter(str_detect(lava_records$summary, "buried"))


#bind them together
all_records <- rbind(lava_impact_records, lava_damage_records, lava_destroy_records, lava_flow_destroy_records, lava_inundated_records, lava_buried_records)
all_records_summary <- rbind(lava_impact_records_summary, lava_damage_records_summary, lava_destroy_records_summary, lava_flow_destroy_records_summary, lava_inundated_records_summary, lava_buried_records_summary)
total = rbind(all_records, all_records_summary)

#unique
all_records_filtered <- total %>%
  distinct()

#iceland
iceland <- all_records_filtered %>%
filter(all_records_filtered$country =="Iceland")
table = write.xlsx(iceland, "iceland.xlsx")
install.packages("xlsx")
write.csv(iceland,"iceland.csv", row.names = FALSE)

#export
final = data.frame(volcano = all_records_filtered$name, record = all_records_filtered$bulletin_and_latest_reports)
write.csv(final,"list_impacts.csv", row.names = FALSE)

table = write.table(final, "impacts.txt")

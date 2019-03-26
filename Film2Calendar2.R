rm(list = ls())
library(tidyverse)
library(rvest)
library(lubridate)
Year <- year(now())
Month <- month(now())
url <- str_c("https://www.cfa.org.cn/tabid/562/Default.aspx?p=1&time=", Year,",", Month, "#fs")
webpage <- read_html(url)
day <- webpage %>%
  html_nodes(".fa_date") %>%
  html_text() %>%
  as.numeric()

############ Subject ######################
Subject <- webpage %>%
  html_nodes(".yp") %>%
  html_text()  %>%
  str_trim() %>%
  as.character()

########## Date&Time processing ########
Start_Time <- webpage %>%
  html_nodes(".fysj") %>%
  html_text() %>%
  str_trim() 
long <- webpage %>%
  html_nodes(".sc") %>%
  html_text() %>%
  str_extract_all("[0-9]+[0-9]") 
long[48] <- 83
long <- long %>%
  unlist() %>%
  as.numeric() %>%
  duration(units = "minutes")
########## Description ##################
Language <-  webpage %>%
  html_nodes(".yz") %>%
  html_text() %>%
  str_trim()
Caption <- webpage %>%
  html_nodes(".zm") %>%
  html_text() %>%
  str_trim()
Price <- webpage %>%
  html_nodes(".pj") %>%
  html_text() %>%
  str_trim()
Ticket <- webpage %>%
  html_nodes(".gp") %>%
  html_text() %>%
  str_trim()
Description <- str_c(Language, Caption, Price, Ticket,
       sep = "\n")
Location <- webpage %>%
  html_nodes(".zt") %>%
  html_text() %>%
  str_trim()
Location <- str_c("中国电影资料馆", Location,
      sep = ",")
Start_Date <- str_c(Year,Month,day,
                    sep = "-") 

position <- which(Subject == "影片")
position[length(position) + 1] <- length(Start_Time)
start <- ""
for(i in (1:(length(position)-1))){
  for(j in (position[i]:length(Start_Time))){
    if(j <= position[i + 1]){
      start[j] <- str_c(Start_Date[i], Start_Time[j],
                            sep = " ") 
    }
  } 
}
  
start <- start %>%
  ymd_hm()
end <- start + long
########## Create a csv file #######################
Film <- data.frame(Subject, 
           start,
           end, 
           Location,
           Description)%>%
  as_tibble() %>%
  filter(Subject != "影片") %>%
  mutate(Start_Date = date(start),
         Start_Time = strftime(start, format = "%H:%M:%S", tz = "UTC"),
         End_Date = date(end),
         End_Time = strftime(end, format = "%H:%M:%S", tz = "UTC")) %>%
  select(-start,
         -end) 
names(Film) <-  names(Film) %>%
  str_replace_all("_", " ")
write.csv(Film,
          "C:/Users/dhmia/Desktop/小西天影片排片.csv")



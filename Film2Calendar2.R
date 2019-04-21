# Declaration block -------------------------------
# Copyright statement comment: This project is licensed under 
#                              the MIT License.
# Author: ggplot-2 
# File description comment, 
# - Purpose: Convert web info to google calendar accepted format
# - input: a web pape
# - output a csv

# library package ---------------------------------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readxl)
library(rvest) # Web spider


# Read HTML ----------------------------------------  
Year <- year(now())
Month <- month(now())
url <- str_c("https://www.cfa.org.cn/tabid/562/Default.aspx?p=1&time=", 
             Year,",", Month, "#fs")
webpage <- read_html(url)

# Resolve neat data --------------------------------
Subject <- webpage %>%
  html_nodes(".yp") %>%
  html_text()  %>%
  str_trim() %>%
  as.character()

day <- webpage %>%
  html_nodes(".fa_date") %>%
  html_text() %>%
  as.numeric()

Start_Date <- str_c(Year,Month,day,
                    sep = "-") 

Start_Time <- webpage %>%
  html_nodes(".fysj") %>%
  html_text() %>%
  str_trim() 
long <- webpage %>%
  html_nodes(".sc") %>%
  html_text() %>%
  str_extract_all("[0-9]+[0-9]") 
# long[48] <- 83
long <- long %>%
  unlist() %>%
  as.numeric() %>%
  duration(units = "minutes")

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

position <- which(Subject == "影片")
position[length(position) + 1] <- length(Start_Time)
start <- ""
for(i in (1:(length(position)-1))){
  for(j in (position[i]:length(Start_Time))){
    if(j <= position[i + 1]) {
      start[j] <- str_c(Start_Date[i], Start_Time[j],
                            sep = " ") 
    }
  } 
}
  
start <- start %>%
  ymd_hm()
end <- start + long
# Create a csv file -----------------------------------------
Film <- data.frame(Subject, 
           start,
           end, 
           Location,
           Description) %>%
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

file_name <- str_c("./Movie information/小西天影片排片", 
                   Year, "年",
                   Month,"月",
                   ".csv",
                   sep = "")
write.csv(Film, file_name)

# How to Vlookup douban movie score and combine to 
# calendar info 
# douban_movie <- read_excel("./Data/豆瓣电影.xlsx") %>% 
#   rename(Subject = 电影名,
#          Score = 评分) %>% 
#   select(Subject, Score) 
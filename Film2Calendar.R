  ################# This function is aimed to generate a google-calendar-adapted ################# 
  ################# csv for exploring the film news ################# 
  rm(list = ls())
  ################# Load Packages #################
  library(rvest)
  library(tidyverse)
  
  ################# Load WebPage #################
  film <- read_html("https://www.cfa.org.cn/tabid/562/Default.aspx")
  
  ################# Film Date and start time #################
  Year <- 2018
  Month <- film %>%
    html_nodes(".fa_month") %>%
    html_text() %>%
    str_replace_all("月", "")
  Day <- film %>%
    html_nodes(".fa_date") %>%
    html_text()
  Start_Date <- str_c(Day,"/", Month, "/", Year)
  
  Start_time <- film %>%
    html_nodes(".fysj") %>%
    html_text() %>%
    str_trim()
  ################# Film Name & Location #################
  Subject <- film %>%
    html_nodes(".yp") %>%
    html_text() %>%
    str_trim()
  
  Location <- film %>%
    html_nodes(".zt") %>%
    html_text() %>%
    str_trim() 
  ################# Description #################
  Language <- film %>%
    html_nodes(".yz") %>%
    html_text() %>%
    str_trim()
  Caption <- film %>%
    html_nodes(".zm") %>%
    html_text() %>%
    str_trim()
  Price <- film %>%
    html_nodes(".pj") %>%
    html_text() %>%
    str_trim()
  Goupiao <- film %>%
    html_nodes(".gp") %>%
    html_text() %>%
    str_trim()
  Description <- str_c(Language, "\n", 
                       Caption, "\n",
                       Goupiao, "\n",
                       Price)
  ################# Output the calendar file #################
  Xiaoxitian <- data.frame(Subject, Start_time, Location, Description) %>%
    as.tbl()
  Xiaoxitian
  Xiaoxitian$Start_Date <- ""
  position <- which(Xiaoxitian$Subject == "影片")
  for(i in 1:nrow(Xiaoxitian)){
    for(j in 1:length(position)){
      if(i >= position[j]){
        Xiaoxitian$Start_Date[i] <-  Start_Date[j]
      }
    }
  }
  
  Xiaoxitian <- Xiaoxitian %>%
    mutate(End_Date = Start_Date) %>%
    distinct(Subject, .keep_all = TRUE) %>%
    filter(Subject != "影片") 
  getMovieinfo <- function(subject){
    library(tidyverse)
    library(RCurl)
    library(rjson)
    source("utils.R")
    
    subject_td <- subject %>%
      str_replace_all("[^\u4e00-\u9fa5]", "")
    
    url <- str_c("https://api.douban.com/v2/movie/search?q={", subject_td , "}")
    f <-getURL(url)
    
   reslist <-  fromJSON(f)
    movieid <- reslist$subjects[[1]]$id
    
    
    u=paste0("https://api.douban.com/v2/movie/",movieid)
    Sys.sleep(runif(1, 10,15))
    p=getURL(u,ssl.verifypeer = FALSE)
    
    
    # p<-.refreshURL(u,ssl.verifypeer = FALSE)
    reslist <- fromJSON(p)
    title<-reslist[["title"]]
    author<-unlist(reslist[["author"]]);names(author)<-NULL
    rating<-unlist(reslist[["rating"]])[2] %>%
      as.double()
    summary<-reslist[["summary"]]
    
    library(plyr)
    tags<-reslist[["tags"]] %>%
      ldply( data.frame) %>%
      select(name) 
    tags <- tags$name %>%
      str_c(collapse = ", ")
    tags <- str_c("标签：", tags)
    
    return(  str_c(subject, "\n",
                   "豆瓣评分：", rating, "\n",
                   "简介：",summary,"\n",
                   tags, "\n"))
    Sys.sleep(runif(1, 10,15))
    
  }
  
  # for(i in 1:nrow(Xiaoxitian)){
  #   print(str_c("i = ", i))
  #   Xiaoxitian$Description[i] <- str_c(getMovieinfo(Xiaoxitian$Subject[i]), 
  #                                      Xiaoxitian$Description[i])
  #   print(Xiaoxitian$Description[i])
  #   
  # }
  
  Douban <- ""
  for(i in 1:20){
  # for(i in 1:nrow(Xiaoxitian)){
    print(str_c("i = ", i))
    Douban[i] <- getMovieinfo(Xiaoxitian$Subject[i])
    print(Douban[i])
  }
  Douban[is.na(Douban )] <- ""
  temp <- str_c(Douban, Xiaoxitian$Description)
  Xiaoxitian$Description <- temp
  # getMovieinfo(Xiaoxitian$Subject[1])
  # str_c(getMovieinfo(Xiaoxitian$Subject[1]),
  #       Xiaoxitian$Description[1])
  write.csv(Xiaoxitian,
            "C:/Users/Administrator/Desktop/小西天影片排片.csv")
  

#################################
# Analysis of WaPo Video Data   #
# Coded by: Salomon Villatoro   #
# 4/30/2021                     #
#################################

  # Loading libraries
  library(tidyverse)
  library(readxl)
  library(janitor)
  library(lubridate)
  library(hrbrthemes)
  library(ggthemes)
  library(gghighlight)
  # Reading in important data
  # Summary of site visits
  summary <-read_excel("simulation.xlsx", sheet = 1, 
                       col_names = FALSE) %>% 
    slice(-1) %>%
    row_to_names(row_number = 1) %>% 
    mutate(Month = as.Date(as.integer(Month), origin = '1900-01-01')) %>% 
    pivot_longer(!Month, names_to = "visits", values_to = "num")
  
  ggplot(summary, aes(x = Month, y = as.integer(num), group = visits))+
    geom_line(color = "#006400", size = 1.25) +
    scale_x_date(date_labels = "%b",
                 limits = c(as.Date("2017-01-03"),NA),
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,NA),
                       labels = scales::comma)+
    ylab ("Visits")+
    xlab ("Month (2017)")+
    theme_ipsum()+ 
    guides(color=guide_legend(title="Source"))+
    facet_grid(facets = 'visits') +
    theme_ipsum()
  
  ggsave("facet_visits.png")
  # comparing video starts from 
  comparison<- read_excel("simulation.xlsx", sheet = 2, 
                          col_names = FALSE) %>% 
    slice(-1) %>%
    row_to_names(row_number = 1) %>% 
    mutate(Month = as.Date(as.integer(Month), origin = '1900-01-01')) %>% 
    select(-c(2,3)) %>% 
    rename("WaPo Total" = Total) %>% 
    pivot_longer(!Month,names_to = "source", values_to = "vstarts") %>% 
    group_by(source, Month, vstarts) %>%
    summarise()
  
  ggplot(comparison, mapping = aes(x = Month, y = as.integer(vstarts), group = `source`)) +
    geom_line(aes(color = source),size = 1.25) +
    scale_x_date(date_labels = "%b",
                 limits = c(as.Date("2017-01-03"),NA),
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,NA),
                       labels = scales::comma)+
    ylab ("Video Starts") +
    xlab ("Month (2017)")+
    theme_ipsum()+ 
    guides(color=guide_legend(title="Source")) +
    gghighlight(source %in% c("Facebook", "YouTube", "WaPo Total"))
  ggsave("source_plot.png")
  
  # looking at views over time based on video starts types
  start_type<- read_excel("simulation.xlsx", sheet = 11, 
    col_names = FALSE) %>% 
    row_to_names(row_number = 1) %>% 
    mutate(Month = as.Date(as.integer(Month), origin = '1900-01-01')) %>% 
    pivot_longer(!Month,names_to = "type", values_to = "vstarts") %>% 
    group_by(type, Month, vstarts) %>%
    summarise()
  
  ggplot(start_type, mapping = aes(x = Month, y = as.integer(vstarts), group = `type`)) +
    geom_line(aes(color = type),size = 1.25) +
    scale_x_date(date_labels = "%b",
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,12000000),
                       labels = scales::comma)+
    ylab ("Video Starts") +
    xlab ("Month (2017)") +
    theme_ipsum() + 
    guides(color=guide_legend(title="Type of Video Start"))
  ggsave("video_start_type.png")
  
  # looking at views over time based on video referrers
  refer<- read_excel("simulation.xlsx", sheet = 9, 
                          col_names = FALSE) %>% 
    slice(-c(1,13)) %>%
    row_to_names(row_number = 1) %>% clean_names() %>% 
    mutate(month = as.Date(as.integer(month), origin = '1900-01-01')) %>% 
    mutate(google_percent  = (as.integer(google_video_starts)/as.integer(google_referrals))*100) %>%
    mutate(fb_percent  = (as.integer(fb_video_starts)/as.integer(fb_referrals))*100)

  ggplot(refer) +
    geom_line(aes(x = month, y = google_percent, color = "Google"), size = 1.25) +
    geom_line(aes(x = month, y = fb_percent, color = "Facebook"), size = 1.25) +
    scale_x_date(date_labels = "%b %d",
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,NA))+
    ylab ("Monthly video starts as percent of referrals") +
    xlab ("Month (2017)")+
    theme_ipsum()
  ggsave("fb_google.png")
  
  # looking at views over time based on post sections
  section<- read_excel("simulation.xlsx", sheet = 5, 
                     col_names = FALSE) %>% 
    slice(-1) %>% 
    row_to_names(row_number = 1) %>%
    pivot_longer(!`Site Section`,names_to = "month", values_to = "vstarts") %>%
    mutate(month = as.Date(as.integer(month), origin = '1900-01-01')) %>% 
    mutate(vstarts = as.integer(vstarts))
  section_total <- section %>% filter(`Site Section` == "Total")
  section <- section %>% left_join(section_total, by = "month") %>% 
    filter(`Site Section.x` != "Total") %>%  
    mutate(percent = (vstarts.x/vstarts.y)*100)
    
  
  ggplot(section, mapping = aes(x = month, y = vstarts.x, group = `Site Section.x`)) +
    geom_line(aes(color = `Site Section.x`), size = 1)+
    geom_point(aes(color = `Site Section.x`), size = 1) +
    scale_x_date(date_labels = "%b",
                 limits = (c(as.Date("2017-01-01"), NA)),
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,NA),
                       labels = scales::comma)+
    ylab ("Monthly video starts by Post section") +
    xlab ("Month (2017)")+
    guides(color=guide_legend(title="Site Section")) +
    theme_ipsum() + 
    gghighlight(`Site Section.x` %in% c("wp - posttv", "wp - local", "wp - homepage"))
  ggsave("sections.png")
  
  #plotting all video starts by month
  ggplot(section_total, mapping = aes(x = month, y = vstarts)) +
    geom_line(size = 1, color = "#006400")+
    scale_x_date(date_labels = "%b",
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,NA),
                       labels = scales::comma)+
    ylab ("Monthly Video Starts") +
    xlab ("Month (2017)")+
    theme_ipsum()
  ggsave("summarystarts.png")
  
  # looking at views over time based on post sections
  refer<- read_excel("simulation.xlsx", sheet = 9, 
                     col_names = FALSE) %>% 
    slice(-1) %>%
    row_to_names(row_number = 1) %>% clean_names() %>% 
    mutate(month = as.Date(as.integer(month), origin = '1900-01-01')) %>% 
    pivot_longer(!month,names_to = "referrer", values_to = "vstarts") %>% 
    group_by(referrer, month, vstarts) %>%
    summarise() %>% filter(is.na(month)!= TRUE) %>% mutate(vstarts = as.integer(vstarts))
  
  ggplot(refer, mapping = aes(x = month, y = vstarts, group = referrer)) +
    geom_line(size = 1)+
    geom_point(aes(color = referrer), size = 3) +
    scale_x_date(date_labels = "%b %d",
                 date_breaks = "1 months") +
    scale_y_continuous(limits = c(0,NA))+
    ylab ("Video Starts") +
    theme_ipsum()

  
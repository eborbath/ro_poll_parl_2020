library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# Final results parliamentary elections - chamber of deputies

url <- "https://en.wikipedia.org/wiki/2020_Romanian_legislative_election#Results"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
df <- html_table(tbls, fill = T)[[7]]
df <- df[, c(2, 4)]

colnames(df) <- df[2, ]  

results <- df %>% 
  mutate(Party = case_when(grepl("National Liberal Party", Party) ~ "PNL",
                           grepl("Social Democratic Party", Party) ~ "PSD",
                           grepl("USR-PLUS", Party) ~ "USR.PLUS",
                           grepl("People's Movement Party", Party) ~ "PMP",
                           grepl("Hungarians in Romania", Party) ~ "UDMR",
                           grepl("Alliance of Liberals and Democrats", Party) ~ "ALDE",
                           grepl("PRO Romania", Party) ~ "PRO",
                           grepl("Alliance for the Unity of Romanians", Party) ~ "AUR")) %>% 
  filter(Party %in% c("PNL", "PSD", "USR.PLUS", "PMP", "UDMR", "ALDE", "PRO", "AUR")) %>% 
  rename(vote=`%`) %>% 
  mutate(vote=as.numeric(str_remove(vote, "%"))) %>% 
  rename(parties = Party) %>% 
  mutate(type="Parl. Election Chamber of Deputies")



# County Councils results, from: https://prezenta.roaep.ro/locale27092020/romania-pv-final

url <- "https://en.wikipedia.org/wiki/2020_Romanian_local_elections#Results"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
df <- html_table(tbls, fill = T)[[5]]
df <- df[, c(2, 13)]

df <- df %>% 
  mutate(Party = case_when(grepl("National Liberal Party", Party) ~ "PNL",
                           grepl("Social Democratic Party", Party) ~ "PSD",
                           grepl("USR-PLUS", Party) ~ "USR.PLUS",
                           grepl("People's Movement Party", Party) ~ "PMP",
                           grepl("Hungarians in Romania", Party) ~ "UDMR",
                           grepl("Alliance of Liberals and Democrats", Party) ~ "ALDE",
                           grepl("PRO Romania", Party) ~ "PRO")) %>% 
  filter(Party %in% c("PNL", "PSD", "USR.PLUS", "PMP", "UDMR", "ALDE", "PRO")) %>% 
  rename(vote=`County Councilsseats (CJ)`) %>% 
  mutate(vote=as.numeric(str_remove(vote, "%"))) %>% 
  rename(parties = Party) %>% 
  mutate(type="Local Election County Councils")

results <- rbind(results, df)
rm(df)

## EP Elections, from Wikipedia

url <- "https://en.wikipedia.org/wiki/2019_European_Parliament_election_in_Romania#Results"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
df <- html_table(tbls, fill = T)[[9]]
df <- df[, c(2, 10)]
df <- df %>% 
  mutate(Party = case_when(grepl("National Liberal Party", Party) ~ "PNL",
                           Party=="Social Democratic Party(Partidul Social Democrat)" ~ "PSD",
                           grepl("USR-PLUS Alliance", Party) ~ "USR.PLUS",
                           grepl("People's Movement Party", Party) ~ "PMP",
                           grepl("Democratic Alliance of Hungarians in Romania", Party) ~ "UDMR",
                           grepl("Alliance of Liberals and Democrats", Party) ~ "ALDE",
                           grepl("PRO Romania", Party) ~ "PRO")) %>% 
  filter(Party %in% c("PNL", "PSD", "USR.PLUS", "PMP", "UDMR", "ALDE", "PRO")) %>% 
  rename(vote=`% of votes`) %>% 
  mutate(vote=as.numeric(str_remove(vote, "%"))) %>% 
  rename(parties = Party) %>% 
  mutate(type="EP Election")

results <- rbind(results, df)
rm(df)

# Presidential Elections, from Wikipedia

url <- "https://en.wikipedia.org/wiki/2019_Romanian_presidential_election#Results"
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
tbls[grep("Invalid/blank votes",tbls,ignore.case = T)]
df <- html_table(tbls[grep("Invalid/blank votes",tbls,ignore.case = T)],fill = T)[[1]]
df <- df[, c(3,5)]
df <- df %>% 
  mutate(Party = case_when(grepl("National Liberal Party", Party) ~ "PNL",
                           grepl("Social Democratic Party", Party) ~ "PSD",
                           grepl("USR-PLUS Alliance", Party) ~ "USR.PLUS",
                           grepl("People's Movement Party", Party) ~ "PMP",
                           grepl("Democratic Alliance of Hungarians in Romania", Party) ~ "UDMR",
                           grepl("PRO - ALDE", Party) ~ "PRO.ALDE")) %>% 
  filter(Party %in% c("PNL", "PSD", "USR.PLUS", "PMP", "UDMR", "PRO.ALDE")) %>% 
  rename(vote=`First round`) %>% 
  mutate(vote=as.numeric(str_remove(vote, "%"))) %>% 
  rename(parties = Party) %>% 
  mutate(type="Pres. Election Ist Round")

results <- rbind(results, df)

results <- results %>% 
  mutate(date=case_when(type=="Parl. Election Chamber of Deputies" ~ "2020-12-06",
                        type=="Local Election County Councils" ~ "2020-09-27",
                        type=="Local Election Mayors" ~ "2020-09-27",
                        type=="EP Election" ~ "2019-05-26",
                        type=="Pres. Election Ist Round" ~ "2019-11-10")) %>% 
  mutate(date=as.Date(date))

rm(df, tbls, webpage, url)
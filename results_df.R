library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

# County Councils results, from: https://prezenta.roaep.ro/locale27092020/romania-pv-final

local_res <- read.csv(paste0(path, "\\results_2020_local\\pv_final_cntry_cj.csv"))

colnames(local_res)[27] <- "PMP"
colnames(local_res)[42] <- "ALDE"
colnames(local_res)[30] <- "UDMR"
colnames(local_res)[39] <- "PNL"
colnames(local_res)[36] <- "PSD"
colnames(local_res)[54] <- "USR.PLUS"
colnames(local_res)[33] <- "PRO"

results <- local_res %>% 
  mutate(total=sum(c, na.rm = TRUE)) %>% 
  select(total, PNL, PSD, USR.PLUS, PMP, ALDE, PRO, UDMR) %>% 
  mutate(PNL=sum(PNL, na.rm=TRUE),
         PSD=sum(PSD, na.rm=TRUE),
         USR.PLUS=sum(USR.PLUS, na.rm=TRUE),
         PMP=sum(PMP, na.rm=TRUE),
         ALDE=sum(ALDE, na.rm=TRUE),
         PRO=sum(PRO, na.rm=TRUE),
         UDMR=sum(UDMR, na.rm=TRUE)) %>% 
  unique(.) %>% 
  mutate_at(vars(PNL, PSD, USR.PLUS, PMP, ALDE, PRO, UDMR), ~ .*100/total) %>%
  select(-total) %>% 
  pivot_longer(everything(), names_to = "parties", values_to="vote") %>% 
  mutate(type="Local Election County Councils")

# Mayor results, from: https://prezenta.roaep.ro/locale27092020/romania-pv-final

local_res <- read.csv(paste0(path, "\\results_2020_local\\pv_final_cntry_p.csv"))

colnames(local_res)[29] <- "PMP"
colnames(local_res)[31] <- "ALDE"
colnames(local_res)[41] <- "UDMR"
colnames(local_res)[23] <- "PNL"
colnames(local_res)[25] <- "PSD"
colnames(local_res)[27] <- "USR.PLUS"
colnames(local_res)[33] <- "PRO"

local_res <- local_res %>% 
  mutate(total=sum(c, na.rm = TRUE)) %>% 
  select(total, PNL, PSD, USR.PLUS, PMP, ALDE, PRO, UDMR) %>% 
  mutate(PNL=sum(PNL, na.rm=TRUE),
         PSD=sum(PSD, na.rm=TRUE),
         USR.PLUS=sum(USR.PLUS, na.rm=TRUE),
         PMP=sum(PMP, na.rm=TRUE),
         ALDE=sum(ALDE, na.rm=TRUE),
         PRO=sum(PRO, na.rm=TRUE),
         UDMR=sum(UDMR, na.rm=TRUE)) %>% 
  unique(.) %>% 
  mutate_at(vars(PNL, PSD, USR.PLUS, PMP, ALDE, PRO, UDMR), ~ .*100/total) %>%
  select(-total) %>% 
  pivot_longer(everything(), names_to = "parties", values_to="vote") %>% 
  mutate(type="Local Election Mayors")

results <- rbind(results, local_res)
rm(local_res)

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
  mutate(date=case_when(type=="Local Election County Councils" ~ "2020-09-27",
                        type=="Local Election Mayors" ~ "2020-09-27",
                        type=="EP Election" ~ "2019-05-26",
                        type=="Pres. Election Ist Round" ~ "2019-11-10")) %>% 
  mutate(date=as.Date(date))

rm(df, tbls, webpage, url)
rm(list = ls())

# To set up the data

Sys.setlocale("LC_TIME", "C")

library(readxl)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(lubridate)
library(stringr)
library(tidyr)
library(plotly)
library(zoo)
library(imputeTS)
library(xts)
library(broom)
library(htmlwidgets)


theme_set(theme_fivethirtyeight() +  
            theme(axis.title.x=element_blank(),
                  # axis.text.x = element_text(angle = 45, hjust = 1),
                  legend.title = element_blank(),
                  legend.position="bottom", 
                  legend.direction="horizontal",
                  legend.margin=margin(t = 0, unit='cm'),
                  legend.key.width = unit(1.5,"cm")))

image_type <- ".jpeg"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- paste0(getwd(), "//")

dat <- read_xlsx(paste0(path, "ro_polls_2020.xlsx"))
write.csv(dat, paste0(path, "ro_polls_2020.csv"))

dat <- read.csv(paste0(path, "ro_polls_2020.csv"))


dat <- dat %>%
  rename(`Polling company`=`ï..Polling.Firm`) %>% 
  mutate(`Polling company`=factor(`Polling company`),
         `Fieldwork.Start`=dmy(`Fieldwork.Start`),
         `Fieldwork.End`=dmy(`Fieldwork.End`)) %>% 
  filter(Scope=="National") %>% 
  mutate_at(vars(Precision:Other), ~ifelse(.=="Not Available", NA, .)) %>% 
  mutate_at(vars(Precision:Other), ~as.numeric(str_replace_all(., c("%" = "")))) %>% 
  rowwise %>%
  mutate(date=mean.Date(c(`Fieldwork.Start`, `Fieldwork.End`), na.rm = TRUE)) %>% 
  select(`Polling company`, date, PSD:Other) %>% 
  mutate_at(vars(USR, PLUS), ~ifelse(is.na(.), 0, .)) %>% 
  mutate(USR.PLUS=ifelse(is.na(USR.PLUS), USR+PLUS, USR.PLUS)) %>% 
  select(-USR, -PLUS) %>% 
  mutate(numdata=as.numeric(date)) %>% 
  arrange(date)

colnames(dat)[colnames(dat)=="USR.PLUS"] <- "USR-PLUS"

colnames(dat)

# To fill in the missings for the small parties for the plot

UDMR.imp <- with(dat, zoo(UDMR, order.by = date))
UDMR.imp <- na.ma(UDMR.imp, k = 4, weighting = "linear")

PMP.imp <- with(dat, zoo(PMP, order.by = date))
PMP.imp <- na.ma(PMP.imp, k = 4, weighting = "linear")

ALDE.imp <- with(dat, zoo(ALDE, order.by = date))
ALDE.imp <- na.ma(ALDE.imp, k = 4, weighting = "linear")

PRO.imp <- with(dat, zoo(PRO, order.by = date))
PRO.imp <- na.ma(PRO.imp, k = 4, weighting = "linear")

dat <- cbind(dat, as.data.frame(UDMR.imp)[1])
dat <- cbind(dat, as.data.frame(PMP.imp)[1])
dat <- cbind(dat, as.data.frame(ALDE.imp)[1])
dat <- cbind(dat, as.data.frame(PRO.imp)[1])

dat <- dat %>% 
mutate(imptd_UDMR=ifelse(is.na(UDMR)& !is.na(UDMR.imp), 1, 0),
       imptd_PMP=ifelse(is.na(PMP)& !is.na(PMP.imp), 1, 0),
       imptd_ALDE=ifelse(is.na(ALDE)& !is.na(ALDE.imp), 1, 0),
       imptd_PRO=ifelse(is.na(PRO)& !is.na(PRO.imp), 1, 0)) %>% 
select(-UDMR, -PMP, -ALDE, -PRO) %>% 
rename(UDMR=UDMR.imp,
       PMP=PMP.imp,
       ALDE=ALDE.imp,
       PRO=PRO.imp) %>% 
mutate(imptd_PRO=ifelse(date<=ymd("2018-05-02"), 1, imptd_PRO),
       PRO=ifelse(date<=ymd("2018-05-02"), 0, PRO))

long <- dat %>% 
  mutate(UDMR = ifelse(imptd_UDMR==1, NA, UDMR),
         PMP = ifelse(imptd_PMP==1, NA, PMP),
         ALDE = ifelse(imptd_ALDE==1, NA, ALDE),
         PRO = ifelse(imptd_PRO==1, NA, PRO)) %>% 
  select(-starts_with("imptd_"), -numdata) %>% 
  pivot_longer(cols=PSD:PRO, names_to="parties", values_to="percent") %>% 
  filter(parties!="Other") %>% 
  mutate(parties=ifelse(parties=="PRO", "Pro Romania", parties)) %>% 
  arrange(date) 

# Interactive plot

p <- plot_ly(data=dat, x=~date) %>% 
  add_markers(y = ~PNL, text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="#cc9900"), name="PNL") %>%
  add_lines(y = ~fitted(loess(PNL~numdata)),
            line = list(color = '#cc9900'),
            name = "PNL", showlegend = TRUE) %>%
  add_markers(y = ~PSD, 
              text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="#EC1C24"), name="PSD") %>% 
  add_lines(y = ~fitted(loess(PSD~numdata)),
            line = list(color = '#EC1C24'),
            name = "PSD", showlegend = TRUE, text = ~`Polling company`) %>% 
  add_markers(y = ~`USR-PLUS`, text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="#6843d1"), name="USR-PLUS") %>%
  add_lines(y = ~fitted(loess(`USR-PLUS`~numdata)),
            line = list(color = '#a593d8'),
            name = "USR-PLUS", showlegend = TRUE) %>%
  add_markers(y = ~PRO, text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="black"), name="Pro Romania") %>%
  add_lines(y = ~ifelse(fitted(loess(PRO~numdata))<0, 0, fitted(loess(PRO~numdata))),
            line = list(color = 'black'),
            name = "Pro Romania", showlegend = TRUE) %>%
  add_markers(y = ~PMP, text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="#007BC8"), name="PMP") %>%
  add_lines(y = ~fitted(loess(PMP~numdata)),
            line = list(color = '#007BC8'),
            name = "PMP", showlegend = TRUE) %>%
  add_markers(y = ~ALDE, text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="#D1439D"), name="ALDE") %>%
  add_lines(y = ~fitted(loess(ALDE~numdata)),
            line = list(color = '#D1439D'),
            name = "ALDE", showlegend = TRUE) %>%
  add_markers(y = ~UDMR, text = ~`Polling company`, showlegend = FALSE, opacity=0.3,
              marker=list(color="#2E8348"), name="UDMR") %>%
  add_lines(y = ~fitted(loess(UDMR~numdata)),
            line = list(color = '#2E8348'),
            name = "UDMR", showlegend = TRUE) %>%
  layout(legend = list(orientation = 'h', xanchor = 'left'),
         yaxis = list(title = "Parties' popularity in pp."),
         xaxis = list(title = ""))

saveWidget(p, "plotly_2020.html", selfcontained = F, libdir = "lib")

# autosize = F, width = 800, height = 600


## GGPLOT overall

p <- ggplot(long, aes(x=date, y=percent, color=parties)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_manual("",breaks=c("PNL", "PSD", "USR-PLUS", "Pro Romania", "PMP", "ALDE", "UDMR"),
                     values = c("#cc9900", "#EC1C24", "#6843d1", "black", "#007BC8","#D1439D", "#2E8348")) +
  ylab("Parties' popularity in pp.") + xlab("") +
  theme_bw() +
  scale_x_date(date_breaks = "4 month", date_labels =  "%b-%y") +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  guides(color=guide_legend(keywidth = 3, keyheight = 1)) +
  expand_limits(y = 0)

ggsave(plot=p,
       filename = "overall.png",
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)

## Potential winners since the presidential elections

to_plot <- long %>% 
  filter(date>ymd("2019-11-24")) %>% 
  filter(parties %in% c("PNL", "PSD", "USR-PLUS"))

p <- ggplot(to_plot, aes(x=date, y=percent, color=parties)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_manual("",breaks=c("PNL", "PSD", "USR-PLUS"),
                     values = c("#cc9900", "#EC1C24", "#6843d1")) +
  ylab("Parties' popularity in pp.") + xlab("") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b-%y") +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  guides(color=guide_legend(keywidth = 3, keyheight = 1)) +
  expand_limits(y = 0)

ggsave(plot=p,
       filename = "winners.png",
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)

## Small parties

to_plot <- long %>% 
  filter(date>ymd("2019-11-24")) %>% 
  filter(parties %in% c("Pro Romania", "PMP", "ALDE", "UDMR")) 

p <- ggplot(to_plot, aes(x=date, y=percent, color=parties)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method="loess", se=FALSE) +
  geom_hline(yintercept = 5, linetype="dotted") +
  scale_color_manual("",breaks=c("Pro Romania", "PMP", "ALDE", "UDMR"),
                     values = c("black", "#007BC8","#D1439D", "#2E8348")) +
  ylab("Parties' popularity in pp.") + xlab("") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b-%y") +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  guides(color=guide_legend(keywidth = 3, keyheight = 1)) +
  expand_limits(y = 0)

ggsave(plot=p,
       filename = "smaller.png",
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)

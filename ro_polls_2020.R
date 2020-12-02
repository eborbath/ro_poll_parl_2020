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
                  legend.title = element_blank(),
                  legend.position="bottom", 
                  legend.direction="horizontal",
                  legend.margin=margin(t = 0, unit='cm'),
                  legend.key.width = unit(1.5,"cm")))

image_type <- ".jpeg"
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- paste0(getwd(), "//")
website <- "c:\\Users\\borbath\\Documents\\GitHub\\eborbath.github.io\\_includes\\"

dat <- read.csv(paste0(path, "ro_polls_2020.csv"))


dat <- dat %>%
  rename(`Polling company`=`Polling.Firm`) %>% 
  mutate(`Polling company`=factor(`Polling company`),
         `Fieldwork.Start`=dmy(`Fieldwork.Start`),
         `Fieldwork.End`=dmy(`Fieldwork.End`)) %>% 
  filter(Scope=="National") %>% 
  mutate_at(vars(Precision:Other), ~ifelse(.=="Not Available", NA, .)) %>% 
  mutate_at(vars(Precision:Other), ~as.numeric(str_replace_all(., c("%" = "")))) %>% 
  rowwise %>%
  # mutate(date=mean.Date(c(`Fieldwork.Start`, `Fieldwork.End`), na.rm = TRUE)) %>% 
  mutate(date=`Fieldwork.End`, na.rm=TRUE) %>% 
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
UDMR.imp <- na_ma(UDMR.imp, k = 4, weighting = "exponential")

PMP.imp <- with(dat, zoo(PMP, order.by = date))
PMP.imp <- na_ma(PMP.imp, k = 4, weighting = "exponential")

ALDE.imp <- with(dat, zoo(ALDE, order.by = date))
ALDE.imp <- na_ma(ALDE.imp, k = 4, weighting = "exponential")

PRO.imp <- with(dat, zoo(PRO, order.by = date))
PRO.imp <- na_ma(PRO.imp, k = 4, weighting = "exponential")

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

##################################
## To get the electoral results ##
##################################

source(paste0(path, "results_df.R"))
results <- results %>% 
  mutate(vote=round(vote, digits = 2))
  

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
  add_markers(data=results[results$parties=="PNL",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#cc9900", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="PNL") %>% 
  add_markers(data=results[results$parties=="PSD",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#EC1C24", symbol='diamond-dot', size=11, 
                          line = list(color = "black", width = 1)), name="PSD") %>%
  add_markers(data=results[results$parties=="USR.PLUS",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#6843d1", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="USR-PLUS") %>%
  add_markers(data=results[results$parties=="PRO",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="black", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="Pro Romania") %>%
  add_markers(data=results[results$parties=="PMP",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#007BC8", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="PMP") %>%
  add_markers(data=results[results$parties=="UDMR",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#2E8348", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="UDMR") %>%
  add_markers(data=results[results$parties=="ALDE",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#D1439D", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="ALDE") %>%
  add_markers(data=results[results$parties=="PRO.ALDE",], y = ~ vote, x= ~ date, text = ~ type, showlegend = FALSE,
              marker=list(color="#D1439D", symbol='diamond-dot', size=11,
                          line = list(color = "black", width = 1)), name="ALDE-Pro Romania") %>%
  layout(legend = list(orientation = 'h', xanchor = 'left'),
         yaxis = list(title = "Parties' popularity in pp."),
         xaxis = list(title = ""))

saveWidget(p, "plotly_2020.html", selfcontained = F, libdir = "lib")

## GGPLOT overall

p <- ggplot(long, aes(x=date, y=percent, color=parties)) +
  geom_point(alpha = 1/4) +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_manual("",breaks=c("PNL", "PSD", "USR-PLUS", "Pro Romania", "PMP", "ALDE", "UDMR"),
                     values = c("#cc9900", "#EC1C24", "#6843d1", "black", "#007BC8","#D1439D", "#2E8348")) +
  geom_point(data=results[results$parties=="PNL",], aes(y=vote, x=date), 
             fill="#cc9900", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="PSD",], aes(y=vote, x=date), 
             fill="#EC1C24", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="USR.PLUS",], aes(y=vote, x=date), 
             fill="#6843d1", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="PRO",], aes(y=vote, x=date), 
             fill="black", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="PMP",], aes(y=vote, x=date), 
             fill="#007BC8", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="ALDE",], aes(y=vote, x=date), 
             fill="#D1439D", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="UDMR",], aes(y=vote, x=date), 
             fill="#2E8348", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="PRO.ALDE",], aes(y=vote, x=date), 
             fill="#D1439D", alpha=1, shape=22, color="black", size=3) +
  ylab("Parties' popularity in pp.") + xlab("") +
  theme_bw() +
  scale_x_date(date_breaks = "4 month", date_labels =  "%b-%y") +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  guides(color=guide_legend(keywidth = 3, keyheight = 1)) +
  expand_limits(y = 0)

ggsave(plot=p,
       filename = paste0("overall", image_type),
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)

to_plot <- long %>% 
  filter(date>ymd("2019-11-24"))
results <- results %>% 
  filter(date>ymd("2019-11-24"))

# distribution

to_plot <- to_plot %>% 
  mutate(parties=factor(parties, levels=c("PNL", "PSD", "USR-PLUS",
                        "Pro Romania", "PMP", "ALDE", "UDMR")))

grid <- with(to_plot, seq(min(percent, na.rm = TRUE)-sd(percent, na.rm = TRUE), 
                          max(percent, na.rm = TRUE)+sd(percent, na.rm = TRUE), length = 1000))
normaldens <- plyr::ddply(to_plot, "parties", function(df) {
  data.frame(
    percent = grid,
    density = dnorm(grid, mean(df$percent), sd(df$percent))
  )
})

p <- ggplot(to_plot, aes(x=percent, fill=parties)) +
  geom_density(alpha=1/3, color=NA) +
  geom_line(aes(y = density), data = normaldens) +
  facet_wrap(~parties, nrow=3) +
  scale_fill_manual("",
                    breaks=c("PNL", "PSD", "USR-PLUS", "Pro Romania", "PMP", "ALDE", "UDMR"),
                    values = c("#cc9900", "#EC1C24", "#6843d1", "black", "#007BC8","#D1439D", "#2E8348")) +
  xlim(0, 55)


ggsave(plot=p,
       filename = paste0("dis_overall", image_type),
       path=paste0(path, "static\\"),
       scale=1.1,
       width = 7, height = 6, dpi=400)
  

## Potential winners since the presidential elections

to_plot <- long %>% 
  filter(date>ymd("2019-11-24")) %>% 
  filter(parties %in% c("PNL", "PSD", "USR-PLUS"))

p <- ggplot(to_plot, aes(x=date, y=percent, color=parties)) +
  geom_point(alpha = 1/3) +
  geom_smooth(method="loess", se=FALSE) +
  scale_color_manual("",breaks=c("PNL", "PSD", "USR-PLUS"),
                     values = c("#cc9900", "#EC1C24", "#6843d1")) +
  geom_point(data=results[results$parties=="PNL",], aes(y=vote, x=date), 
             fill="#cc9900", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="PSD",], aes(y=vote, x=date), 
             fill="#EC1C24", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="USR.PLUS",], aes(y=vote, x=date), 
             fill="#6843d1", alpha=1, shape=22, color="black", size=3) +
  ylab("Parties' popularity in pp.") + xlab("") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b-%y") +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  guides(color=guide_legend(keywidth = 3, keyheight = 1)) +
  expand_limits(y = 0)

ggsave(plot=p,
       filename = paste0("winners", image_type),
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)


# distributions

p <- ggplot(to_plot, aes(x=percent, fill=parties)) +
  geom_density(alpha=1/3, color=NA) +
  scale_fill_manual("",breaks=c("USR-PLUS", "PSD", "PNL"),
                     values = c("#6843d1", "#EC1C24", "#cc9900")) +
  stat_function(fun = dnorm, args = list(mean = mean(to_plot$percent[to_plot$parties=="PSD"]), 
                                         sd = sd(to_plot$percent[to_plot$parties=="PSD"]))) +
  stat_function(fun = dnorm, args = list(mean = mean(to_plot$percent[to_plot$parties=="PNL"]), 
                                         sd = sd(to_plot$percent[to_plot$parties=="PNL"]))) +
  stat_function(fun = dnorm, args = list(mean = mean(to_plot$percent[to_plot$parties=="USR-PLUS"]), 
                                         sd = sd(to_plot$percent[to_plot$parties=="USR-PLUS"]))) +
  xlim(5, 55)
  
ggsave(plot=p,
       filename = paste0("dis_winners", image_type),
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)

grid <- with(to_plot, seq(min(percent, na.rm = TRUE)-sd(percent, na.rm = TRUE),
                          max(percent, na.rm = TRUE)+sd(percent, na.rm = TRUE), length = 1000))
normaldens <- plyr::ddply(to_plot, "parties", function(df) {
  data.frame(
    percent = grid,
    density = dnorm(grid, mean(df$percent), sd(df$percent))
  )
})

p <- ggplot(to_plot, aes(x=percent, fill=parties)) +
  geom_density(alpha=1/3, color=NA) +
  geom_line(aes(y = density), data = normaldens) +
  facet_wrap(~parties, nrow=2) +
  scale_fill_manual("",breaks=c("PNL","PSD", "USR-PLUS"),
                    values = c("#cc9900", "#EC1C24", "#6843d1")) +
  xlim(0, 55)

ggsave(plot=p,
       filename = paste0("dis_winners2", image_type),
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
  geom_point(data=results[results$parties=="PRO",], aes(y=vote, x=date), 
             fill="black", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="PMP",], aes(y=vote, x=date), 
             fill="#007BC8", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="ALDE",], aes(y=vote, x=date), 
             fill="#D1439D", alpha=1, shape=22, color="black", size=3) +
  geom_point(data=results[results$parties=="UDMR",], aes(y=vote, x=date), 
             fill="#2E8348", alpha=1, shape=22, color="black", size=3) +
  ylab("Parties' popularity in pp.") + xlab("") +
  theme_bw() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b-%y") +
  theme(legend.title=element_blank(), legend.position="bottom") + 
  guides(color=guide_legend(keywidth = 3, keyheight = 1)) +
  expand_limits(y = 0)

ggsave(plot=p,
       filename = paste0("smaller", image_type),
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)


# distributions

grid <- with(to_plot, seq(min(percent, na.rm = TRUE)-sd(percent, na.rm = TRUE),
                          max(percent, na.rm = TRUE)+sd(percent, na.rm = TRUE), length = 1000))
normaldens <- plyr::ddply(to_plot, "parties", function(df) {
  data.frame(
    percent = grid,
    density = dnorm(grid, mean(df$percent), sd(df$percent))
  )
})

p <- ggplot(to_plot, aes(x=percent, fill=parties)) +
  geom_density(alpha=1/3, color=NA) +
  geom_line(aes(y = density), data = normaldens) +
  facet_wrap(~parties) +
  scale_fill_manual("",breaks=c("Pro Romania", "PMP", "ALDE", "UDMR"),
                     values = c("black", "#007BC8","#D1439D", "#2E8348")) +
  xlim(0, 15)

ggsave(plot=p,
       filename = paste0("dis_smaller", image_type),
       path=paste0(path, "static\\"),
       width = 8, height = 5, dpi=400)

# table for the website

polls <- read.csv(paste0(path, "ro_polls_2020.csv"))
polls <- polls %>% 
  filter(Scope!="European") %>% 
  select(-Scope, -Sample.Size.Qualification, -Participation, -Precision) %>% 
  mutate_all(~ ifelse(.=="Not Available", "", .)) %>% 
  mutate(Commissioners = ifelse(is.na(Commissioners), " ", Commissioners)) %>% 
  rename(Firm=Polling.Firm,
         Start=Fieldwork.Start,
         End=Fieldwork.End,
         Sample=Sample.Size) %>% 
  select(where(~ !(all(is.na(.)) | all(. == ""))))

display <- head(polls)
sink(paste0(website, 'recent_polls.html'))
knitr::kable(display, format = "html")
sink()

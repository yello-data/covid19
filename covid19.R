library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(ggthemes)

#DOWNLOAD AND TIDY
cvid <- read_csv("https://covid19.isciii.es/resources/serie_historica_acumulados.csv")
cvid$Fecha <- dmy(cvid$Fecha) #lubridate date
names(cvid)[1] <- "ccaa"
names(cvid) <- tolower(names(cvid))
cvid <- cvid %>% mutate_at(3:7, funs(ifelse(is.na(.), 0, .))) %>%
  mutate_at(3:7, funs(as.numeric(.))) %>%
  filter(!is.na(fecha))
sum(is.na(cvid))

#CREATE POPULATION VARIABLE
library(readxl)
pop <- read_xlsx("2853.xlsx")
cvid <- cvid %>%
  left_join(pop)
cvid <- cvid %>%
  mutate(ccaa = str_replace(ccaa, "^AN$", "AND"),
         ccaa = str_replace(ccaa, "^AR$", "ARA"),
         ccaa = str_replace(ccaa, "^AS$", "AST"),
         ccaa = str_replace(ccaa, "^CB$", "CBR"),
         ccaa = str_replace(ccaa, "^CE$", "CEU"),
         ccaa = str_replace(ccaa, "^CL$", "CLE"),
         ccaa = str_replace(ccaa, "^CM$", "CLM"),
         ccaa = str_replace(ccaa, "^CN$", "CNR"),
         ccaa = str_replace(ccaa, "^CT$", "CAT"),
         ccaa = str_replace(ccaa, "^EX$", "EXT"),
         ccaa = str_replace(ccaa, "^GA$", "GAL"),
         ccaa = str_replace(ccaa, "^IB$", "BAL"),
         ccaa = str_replace(ccaa, "^MC$", "MUR"),
         ccaa = str_replace(ccaa, "^MD$", "MAD"),
         ccaa = str_replace(ccaa, "^ME$", "MEL"),
         ccaa = str_replace(ccaa, "^NC$", "NAV"),
         ccaa = str_replace(ccaa, "^PV$", "EUS"),
         ccaa = str_replace(ccaa, "^RI$", "RIJ"),
         ccaa = str_replace(ccaa, "^VC$", "VAL"))

#CREATE TABLE VARIABLES
cvid28 <- cvid %>%
  arrange(ccaa, fecha) %>%
  group_by(ccaa) %>%
  transmute(fecha, casos_cum = casos,
         casos = casos_cum - lag(casos_cum, default = 0),
         hosp_cum = hospitalizados,
         hosp = hosp_cum - lag(hosp_cum, default = 0),
         uci_cum = uci,
         uci = uci_cum - lag(uci_cum, default = 0),
         fall_cum = fallecidos,
         fall = fall_cum - lag(fall_cum, default = 0),
         casos_pop = casos / pop * 100000,
         fall_pop = fall / pop * 100000,
         casos_cum_pop = casos_cum / pop * 100000,
         fall_cum_pop = fall_cum / pop * 100000,
         rec_cum = recuperados,
         rec = rec_cum - lag(rec_cum, default = 0),
         rec_pop = rec / pop * 100000,
         rec_cum_pop = rec_cum / pop * 100000,
         pop)

names(cvid28) #variables
unique(cvid28$ccaa) #cases

#MOST AND LEAST CASES
most <- cvid28 %>%
  filter(fecha > Sys.Date() - 3) %>%
  group_by(ccaa) %>%
  mutate(c = uci / sum(uci)) %>%
  filter(fecha == max(fecha)) %>%
  ungroup() %>%
  top_n(6, c) %>%
  pull(ccaa)

least <- cvid28 %>%
  filter(fecha > Sys.Date() - 3) %>%
  group_by(ccaa) %>%
  mutate(c = uci / sum(uci)) %>%
  filter(fecha == max(fecha)) %>%
  ungroup() %>%
  top_n(-6, c) %>%
  pull(ccaa)

#TESTS
cvid28 %>%
  filter(ccaa %in% c("AND", "EUS"), #cases
         fecha > Sys.Date() - 25) %>% #starting date
  ggplot(aes(x = fecha, y = fall_cum_pop, col = ccaa)) + #change Y
  stat_smooth(aes(y = fall_cum_pop, x = fecha), #mean of all CCAA, change Y
              inherit.aes = FALSE, col = "grey70", lty = 4, se = FALSE,
              span = .5, alpha = .2) +
  geom_smooth(se = F, span = .25, alpha = 0.8) +
  geom_point(alpha = .3, size = 2)

cvid_gg +
  scale_color_pander() +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), limits = c(0, 55)) +
  xlim(c(Sys.Date() - 25, Sys.Date())) +
  labs(title = "Casos diaris a Espanya per cada 100.000 hab.",
       subtitle = paste("Dades:", max(cvid28$fecha)),
         col= NULL, caption = "Font = Ministerio de Sanidad") +
  theme_hc() +
  theme(text = element_text(family = "Optima", size = 11),
        plot.title = element_text(size=14, face = "bold",
                                  margin = margin(b = -40)),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "gray50", face = "bold",
                                     size = 10,
                                     vjust = -14.5),
        legend.position = c(0.25, 0.625),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(face = "bold", vjust = 4),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank()
)


## CASES/POP
  scale_color_wsj() +
  theme_wsj() +
  guides(color = guide_legend(nrow = 2)) +

#DEATHS/POP
cvid28 %>%
  filter(ccaa %in% c("MD", "CT", "PV", "RI", "CL", "VC"),
         fecha > "2020-03-05") %>%
  ggplot(aes(x = fecha, y = fall_pop, col = ccaa)) +
  stat_smooth(aes(y = fall_pop, x = fecha), 
              inherit.aes = FALSE, col = "grey70", lty = 4, se = FALSE,
              span = .5, alpha = .2) +
  geom_smooth(se = F, span = .3) +
  scale_color_wsj() +
  stat_summary(alpha = .3, size = .1) +
  labs(col = "Morts/dia per 100.000 hab.") +
  theme_wsj() +
  theme(title = element_text(size=15),
        legend.position = c(0.02, 0.96), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.title = element_text(face="bold"))

#HOSP
cvid28 %>%
  filter(ccaa %in% c("MD", "CT", "PV", "RI", "CL", "VC"),
         fecha > "2020-03-05") %>%
  ggplot(aes(x = fecha, y = hosp, col = ccaa)) +
  stat_smooth(aes(y = hosp, x = fecha), 
              inherit.aes = FALSE, col = "grey70", lty = 4, se = FALSE,
              span = .5, alpha = .2) +
  geom_smooth(se = F, span = .3) +
  scale_color_wsj() +
  stat_summary(alpha = .3, size = .1) +
  labs(col = "Hospitalitzacions/dia") +
  theme_wsj() +
  theme(title = element_text(size=15),
        legend.position = c(0.02, 0.90), 
        legend.justification = c(0, 1),
        legend.title = element_text(face="bold"))

#UCI
cvid28 %>%
  filter(ccaa %in% c("MD", "CT", "PV", "RI", "CL", "VC"),
         fecha > "2020-03-05") %>%
  ggplot(aes(x = fecha, y = uci, col = ccaa)) +
  stat_smooth(aes(y = uci, x = fecha), 
              inherit.aes = FALSE, col = "grey70", lty = 4, se = FALSE,
              span = .5, alpha = .2) +
  geom_smooth(se = F, span = .3) +
  scale_color_wsj() +
  stat_summary(alpha = .3, size = .1) +
  labs(col = "UCI/dia") +
  theme_wsj() +
  theme(legend.position = c(0.02, 0.94), 
        legend.justification = c(0, 1),
        legend.title = element_text(face="bold", size = 22))


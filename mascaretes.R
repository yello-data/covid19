masc <- read_csv2("masc.csv")

diff <- cvid %>%
  filter(fecha == Sys.Date() - 7) %>%
  left_join(masc) %>%
  transmute(ccaa, casos = casos / sum(casos) * 100,
            hosp = hospitalizados / sum(hospitalizados) * 100,
            uci = uci / sum(uci) * 100,
            fall = fallecidos / sum(fallecidos) * 100,
            pop = pop / sum(pop) * 100,
            masc = masc / sum(masc) * 100,
            diffc = masc - casos,
            diffm = masc - fall,
            diffh = masc - hosp,
            diffp = masc - pop,
            diffu = masc - uci)

#GRÀFICS
thm <- theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_blank(),
        plot.subtitle = element_text(color = "gray50", face = "bold"),
        plot.title = element_text(face = "bold"))
  
#NUMERO DE CASOS
diff %>% 
  ggplot(aes(x = reorder(ccaa, diffc), y = diffc)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 8, col = "aquamarine4") + 
  geom_text(aes(label = ccaa), size = 2.6, col = "white") +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10)) +
  labs(title = "Repartiment de mascaretes / n.casos",
       caption = "Font = Ministerio de Sanidad") + thm

#POBLACIÓ
diff %>% 
  ggplot(aes(x = reorder(ccaa, diffp), y = diffp)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 8, col = "cornflowerblue") + 
  geom_text(aes(label = ccaa), size = 2.6, col = "white") +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10)) +
  labs(title = "Repartiment de mascaretes / població",
       subtitle = paste("Dades casos de", Sys.Date() - 7),
       caption = "Font = Ministerio de Sanidad") + thm

#MORTS
diff %>% 
  ggplot(aes(x = reorder(ccaa, diffm), y = diffm)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 8, col = "steelblue4") + 
  geom_text(aes(label = ccaa), size = 2.6, col = "white") +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10)) +
  labs(title = "Repartiment de mascaretes / morts",
       subtitle = paste("Dades casos de", Sys.Date() - 7),
       caption = "Font = Ministerio de Sanidad") + thm

#HOSPITALITZACIONS
diff %>% 
  ggplot(aes(x = reorder(ccaa, diffh), y = diffh)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 8, col = "indianred3") + 
  geom_text(aes(label = ccaa), size = 2.6, col = "white") +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10)) +
  labs(title = "Repartiment de mascaretes / hospitalitzacions",
       subtitle = paste("Dades casos de", Sys.Date() - 7),
       caption = "Font = Ministerio de Sanidad") + thm

#UCI
diff %>% 
  ggplot(aes(x = reorder(ccaa, diffu), y = diffu)) +
  geom_hline(yintercept = 0) +
  geom_point(size = 8, col = "yellow4") + 
  geom_text(aes(label = ccaa), size = 2.6, col = "white") +
  scale_y_continuous(breaks = c(-10, -5, 0, 5, 10)) +
  labs(title = "Repartiment de mascaretes / UCI",
       subtitle = paste("Dades casos de", Sys.Date() - 7),
       caption = "Font = Ministerio de Sanidad") + thm

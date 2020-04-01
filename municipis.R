area27 <- read_csv("https://github.com/ibesora/covid-19-data/raw/master/2020-03-27-areesBasiques.csv")
area30 <- read_csv("https://github.com/ibesora/covid-19-data/raw/master/2020-03-30-areesBasiques.csv")

area1 <- area27 %>%
  rename(c27 = cases, v27 = value) %>%
  left_join(area30, by = c("id" = "id")) %>%
  transmute(id, name = name.x, c27, c30 = cases, v27, v30 = value,
            cases = c30 - c27, value = v30 - v27)

area1 %>%
  ggplot(aes(x = c27, y = cases)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method= "lm", se = F)

area1[which(area1$cases > 20 & area1$c27 < 10), ]
area1[which(area1$cases > 10 & area1$c27 > 90), ]

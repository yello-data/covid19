cvid28 %>%
  group_by(fecha) %>%
  summarize(esp = sum(casos)) %>%
  ggplot(aes(x = fecha, y = esp)) +
  geom_line() +
  xlim(c(Sys.Date() - 25, Sys.Date() - 1))

cvid28 %>%
  ggplot(aes(x = fecha, y = rec_cum, col = ccaa)) +
  geom_line()

cvid28 %>%
  ungroup() %>%
  transmute(ccaa, fecha, casos_pop, rec_pop, fall_pop, sol = rec_pop + fall_pop) %>%
  gather(var, val, c(casos_pop, rec_pop, fall_pop, sol)) %>%
  filter(ccaa == c("CAT")) %>%
  ggplot(aes(x = fecha, y = val, col = var)) +
  geom_smooth(se = FALSE) +
  geom_point(alpha = 0.5)
library(dplyr)
library(reshape2)

calculate_prop <- function(data, kp_col, var_col, file_name, by_age = TRUE, filter_stillbirth = FALSE) {
  tmp <- data %>%
    filter(!!sym(kp_col) == 1) %>%
    rowwise() %>%
    mutate(var = ifelse(any(c(uce1, uce2, uce3) == "Klebsiella pneumoniae"), 1, 0)) %>%
    if (by_age) group_by(age.cat) else identity() %>%
    count(var) %>%
    mutate(prop = sprintf('%d (%.0f%%)', n, n / sum(n) * 100)) %>%
    filter(var == 1)
  
  if (filter_stillbirth) tmp <- tmp %>% filter(age.cat != "Stillbirth")
  write.csv(tmp, file_name)
}

calculate_prop(dat.1, "kp", "var", "tmp.csv", by_age = FALSE, filter_stillbirth = TRUE)

dat_kp <- dat.1 %>%
  filter(kp == 1) %>%
  rowwise() %>%
  mutate(var = ifelse(any(c(uce1, uce2, uce3) == "Klebsiella pneumoniae"), 1, 0))

calculate_prop(dat_kp, "kp", "var", "tmp.csv", by_age = TRUE)
calculate_prop(dat_kp %>% filter(age.cat != "Stillbirth"), "kp", "var", "tmp.csv", by_age = FALSE)

dat_filtered <- dat.1 %>%
  rowwise() %>%
  mutate(across(c(ic, uc, m1, m2, m3, m4, m5), ~ ifelse(any(c(uce1, uce2, uce3) == "Klebsiella pneumoniae"), "", .))) %>%
  filter(any(c(uce1, uce2, uce3) == "Klebsiella pneumoniae"))

tmp.1 <- dat_filtered %>%
  group_by(age.cat) %>%
  count(Champsid) %>%
  mutate(other.causes = as.integer(n > 1)) %>%
  count(other.causes) %>%
  filter(other.causes == 0)

denom.age <- dat.1 %>% filter(kp == 1) %>% count(age.cat) %>% rename(total = n)
tmp.1 <- tmp.1 %>% left_join(denom.age, by = 'age.cat') %>%
  mutate(prop = sprintf('%d (%.0f%%)', n, n / total * 100))

tmp.1 <- dat_filtered %>%
  filter(age.cat != 'Stillbirth') %>%
  count(Champsid) %>%
  mutate(other.causes = as.integer(n > 1)) %>%
  count(other.causes) %>%
  filter(other.causes == 0)

denom.tot <- dat.1 %>% filter(age.cat != 'Stillbirth') %>% filter(kp == 1) %>% count() %>% pull()
tmp.1$total <- denom.tot
tmp.1 <- tmp.1 %>%
  mutate(prop = sprintf('%d (%.0f%%)', n, n / total * 100))

calculate_prop(dat_kp, "kp", "var", "tmp.csv", by_age = TRUE)
calculate_prop(dat_kp %>% filter(age.cat != "Stillbirth"), "kp", "var", "tmp.csv", by_age = FALSE)

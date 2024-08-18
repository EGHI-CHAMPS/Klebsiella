library(ggplot2)
library(dplyr)

create_plot <- function(data, site_name, file_name, title_text) {
  
  tmp <- data %>% 
    filter(Site.Name == site_name) %>%
    filter(age.cat != 'Stillbirth') %>%
    group_by(age.cat) %>%
    summarise(pos = sum(kp), n = n()) %>%
    mutate(neg = n - pos, prop = pos / n, type = "Age group") %>%
    pivot_longer(cols = c(pos, neg), names_to = "variable", values_to = "value")
  
  tmp.2 <- data %>%
    filter(Site.Name == site_name, age.cat != 'Stillbirth', Calc.Location != "other", Calc.Location != "") %>%
    group_by(Calc.Location) %>%
    summarise(pos = sum(kp), n = n()) %>%
    mutate(neg = n - pos, prop = pos / n, type = "Location of death") %>%
    rename(age.cat = Calc.Location) %>%
    pivot_longer(cols = c(pos, neg), names_to = "variable", values_to = "value") %>%
    mutate(age.cat = recode(age.cat, community = "Community", facility = "Health facility"))
  
  tmp <- bind_rows(tmp, tmp.2)
  tmp.2 <- tmp %>% select(-variable, -value) %>% distinct() %>%
    mutate(label = scales::percent(prop, accuracy = 1))
  
  tmp <- tmp %>%
    mutate(label.n = factor(paste0(age.cat, "(n=", n, ")"),
                            levels = paste0(age.cat, "(n=", n, ")")),
           variable = factor(variable, levels = c("neg", "pos")),
           col = paste(type, variable))
  
  tot <- max(tmp.2$n)
  
  ggplot() +
    geom_col(data = tmp, aes(x = label.n, y = value, fill = col), color = 'black') +
    geom_point(data = tmp.2, aes(x = label.n, y = prop * tot), size = 2) +
    geom_line(data = tmp.2, aes(x = label.n, y = prop * tot), group = 1) +
    facet_grid(~type, scales = "free", space = "free") +
    geom_text(data = tmp.2, aes(x = label.n, y = prop * tot, label = label), vjust = -1.1, size = 3.5) +
    labs(title = title_text, y = "Deaths", x = "Age group") +
    scale_fill_manual(values = rev(c('#31A354', '#C7E9C0', '#3182BD', '#C6DBEF'))) +
    scale_y_continuous(expand = expansion(mult = c(0, .02)),
                       sec.axis = sec_axis(~ . / tot, labels = scales::percent, name = "Percent of deaths with Kp in causal chain")) +
    theme_minimal() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = .5),
          strip.background = element_rect(fill = 'grey45', color = "black"),
          legend.position = "none",
          strip.text = element_text(size = 10.5, face = "bold", color = "white"),
          axis.title = element_text(face = 'bold', size = 11),
          axis.title.x = element_blank(),
          title = element_text(face = 'bold', size = 10.5),
          axis.text.x = element_markdown(size = 10, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10))
  
  ggsave(file_name, width = 5.75, height = 5.45, units = "in", dpi = 360)
}

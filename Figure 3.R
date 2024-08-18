library(ggplot2)
library(dplyr)

dat %>%
  ggplot() +
    geom_col(
      position = "fill",
      aes(y = reorder(microb, desc(microb)), x = value, fill = variable)
    ) +
    facet_wrap(~ fct_rev(nosocomial)) +
    geom_text(
      data = subset(tests, nosocomial == ">48 hours" | nosocomial == "\u226448 hours/community"),
      aes(y = reorder(microb, desc(microb)), label = tests, x = 1.015),
      position = position_dodge(width = 1),
      hjust = 0, size = 3.6, show.legend = FALSE
    ) +
    coord_cartesian(clip = "off", xlim = c(0, 1)) +
    scale_fill_manual(values = stepped(n = 20)[c(20, 19, 17)]) +
    scale_x_continuous(expand = c(0, 0), labels = scales::percent) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = "Proportion", y = "Antimicrobial") +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA),
      strip.background = element_rect(fill = 'grey45', color = "black"),
      strip.text = element_text(size = 11, face = "bold", color = "white"),
      panel.spacing = unit(2.75, "lines"),
      plot.margin = unit(c(1, 3, 1, 1), "lines"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(face = "bold", size = 10),
      legend.position = "bottom"
    ) +
    guides(fill = guide_legend(reverse = TRUE))
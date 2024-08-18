library(ggplot2)
library(dplyr)

dat %>%
  ggplot(aes(y = n, x = var, fill = forcats::fct_reorder(uc, n, sum))) +
  geom_col(color = "black", position = "fill") +
  facet_grid(~type, scales = "free", space = "free") +
  scale_fill_manual(name = "Cause of death", values = rev(stepped(n = 24))) +
  scale_y_continuous(labels = scales::percent, expand = c(0, 0)) +
  scale_x_discrete(expand = c(0, 0)) +
  labs(y = "Proportion", x = "Site") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = .5),
    strip.background = element_rect(fill = 'grey45', color = "black"),
    strip.text = element_text(size = 13, face = "bold", color = "white"),
    panel.spacing = unit(1.5, "lines"),
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
    legend.key.size = unit(.5, "cm")
  ) +
  guides(fill = guide_legend(ncol = 1))

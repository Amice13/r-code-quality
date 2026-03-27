library(here)
library(wacolors)
source(here("replication/00_setup_load.R"))

alphas = exp(seq(0, 2, length.out=8))
crossing(x = seq(0, 320, by=2),
         alpha = alphas) %>%
    mutate(y = exp(-abs(x/100)^alpha),
           hl = alpha == alphas[3]) %>%
    ggplot(aes(x, y, color=alpha, group=alpha, alpha=hl, size=hl)) +
    geom_line() +
    scale_y_continuous("Probability of inclusion", labels=percent) +
    scale_color_wa_c("forest_fire", trans="log", guide="none") +
    scale_alpha_manual(values=c(0.5, 1), guide="none") +
    scale_size_manual(values=c(0.5, 2), guide="none") +
    labs(x="Distance from home (yards)") +
    theme_minimal(base_family="Times", base_size=12)
ggsave(here("paper/figures/model_kernel.pdf"), width=6.5, height=2.5, dpi=600)

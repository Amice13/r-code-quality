# Get the proportions of different pathways in each risk score quartile for FULL sample
# Share for Outreach pathway in group missing risk score is mutated to 0.5 as the default size of marker

gen_pathway_share <- function(df){
  df %>%
  group_by(z_u_phat_f, pathway) %>%
  summarise(n=n()) %>%
  group_by(z_u_phat_f) %>%
  mutate(n_quar=sum(n), share_size=n/n_quar) %>%
  mutate(share_size=ifelse(z_u_phat_f=="Missing", 0.5, share_size)) %>% # changing size of marker for missing to "normal size"=0.5
  arrange(z_u_phat_f) %>%
  ungroup() %>%
  filter(pathway!="re")
}

gen_itt_plot <- function(df, outcome_col, outcome_label, ylims, font, axis_size, axis_label_size, title_size) {

  ymin = ylims[[outcome_col]][1]
  ymax = ylims[[outcome_col]][2]
  yskip = ylims[[outcome_col]][3]

  df <- df %>%
    filter(outcome == outcome_col & grepl("pathway_risk", subgroup)) %>%
    filter(!grepl("__re", subgroup)) %>%
    select(subgroup, beta_itt, stderr_itt) %>%
    mutate(pathway = ifelse(grepl("cr", subgroup), "Outreach", "Algorithm")) %>%
    mutate(z_u_phat_f = case_when(grepl("1q", subgroup) ~ "1",
                                  grepl("2q", subgroup) ~ "2",
                                  grepl("3q", subgroup) ~ "3",
                                  grepl("4q", subgroup) ~ "4",
                                  grepl("missing", subgroup) ~ "Missing"))


  ci_text = "95% confidence interval"

  itt_plot <-
    df %>%
    mutate(z_u_phat_f = factor(z_u_phat_f, levels=c("Missing", 1, 2, 3, 4)),
           pathway = factor(pathway, levels=c('Algorithm', 'Outreach'))) %>%
    ggplot(aes(x=z_u_phat_f, y=beta_itt, group=pathway)) +
    geom_point(aes(col = pathway, fill = pathway, shape = pathway),
               size=5,
               stroke=0,
               position = position_dodge(width=0.5),
               show.legend = T) +
    geom_errorbar(aes(x=z_u_phat_f, ymin=beta_itt-stderr_itt*1.96, ymax=beta_itt+stderr_itt*1.96, width=.1, col=ci_text),
                  linewidth=0.7,
                  position = position_dodge(width=0.5)) +
    scale_shape_manual(name = "", limits = c('Algorithm', 'Outreach', ci_text), values = c(config$shp, NA)) +
    scale_color_manual(name = "", limits = c('Algorithm', 'Outreach', ci_text), values = c(config$cols, "grey24")) +
    guides(colour = guide_legend(override.aes = list(linetype = c('blank', 'blank', "solid"),
                                                     shape = c(config$shp, NA))),
           fill = "none", shape = "none") +
    geom_hline(yintercept=0) +
    theme_classic() +
    labs(x = "Risk score quartile",
         y = "ITT estimate",
         title = outcome_label) +
    scale_x_discrete(expand = c(0, 0.03)) +
    scale_y_continuous(limits = c(ymin, ymax),
                       breaks=seq(ymin, ymax, yskip),
                       labels = scales::label_number(accuracy = 0.01)) +
    theme(plot.title=element_text(hjust=0.5),
          legend.title=element_blank(),
          legend.position="bottom",
          axis.text.x = element_text(size = axis_size),
          axis.title.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_size),
          axis.title.y = element_text(size = axis_label_size),
          legend.text = element_text(size = axis_size),
          title = element_text(size = title_size),
          text = element_text(family = font))

  return(itt_plot)

}

takeup_quartile <- function(df, font, axis_size, axis_label_size, title_size, subtitle_size, floating_text_size){
  df %>%
    mutate(z_u_phat_f = factor(z_u_phat_f,
                               levels=c("Missing", 1, 2, 3, 4))) %>%
    mutate(label_hour = paste(round(avg_hours, 0), "hrs")) %>%
    ggplot(aes(x=z_u_phat_f, y=takeup_rate, group=pathway)) +
    geom_line(aes(col = pathway), lwd = config$line_width) +
    geom_point(aes(col = pathway, size = share_size, shape = pathway, stroke = 1)) +
    geom_text(aes(label=label_hour), color="grey24", size=floating_text_size, vjust=-2, family=font) +
    scale_color_manual(values = config$cols) +
    scale_shape_manual(values = config$shp_no_fill) +
    scale_size_continuous(range = c(5, 10)) +
    scale_x_discrete(expand = c(0, 0.27)) +
    scale_y_continuous(limits = c(0, 1),
                       breaks=seq(0, 1, 0.2),
                       labels = scales::label_number(accuracy = 0.1)) +
    theme_classic() +
    labs(x = "Risk score quartile",
         y = "Take-up rate",
         title = "Take-up Rate",
         subtitle = "with average number of hours worked", size=subtitle_size) +
    guides(size = "none") +
    guides(colour = guide_legend(override.aes = list(size=6, stroke=1))) +
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5),
          legend.title=element_blank(),
          legend.position="bottom",
          legend.key.width = unit(2, "cm"),
          axis.text.x = element_text(size = axis_size),
          axis.title.x = element_text(size = axis_label_size),
          axis.text.y = element_text(size = axis_size),
          axis.title.y = element_text(size = axis_label_size),
          legend.text = element_text(size = axis_size),
          title = element_text(size = title_size),
          text = element_text(family = font))
}

##
## treatment variationa plot
##


require(tidyverse)

## load data
data(paglayan2019)

## prepare data
paglayan2019 <- paglayan2019 %>%
  filter(!(state %in% c("WI", "DC"))) %>%
  mutate(id_time = year - min(year) + 1,
         id_subject = as.numeric(as.factor(state)),
         log_expenditure = log(pupil_expenditure + 1),
         log_salary      = log(teacher_salary + 1)
       )

## create data for plot
treatment_variation <- paglayan2019 %>%
  select(state, year, treatment) %>%
  group_by(state) %>%
  mutate(treatment_sum = sum(treatment)) %>%
  arrange(desc(treatment_sum), state, year) %>%
  select(-treatment_sum) %>%
  pivot_wider(id_cols = "state", names_prefix = "year",
              names_from = year, values_from = treatment) %>%
  ungroup() %>%  column_to_rownames(var = "state") %>%
  data.matrix()


## save plot
pdf(file = "../results/figures/figureA6_treatment_variation.pdf", height = 6.5, width = 7)
par(mar = c(2, 2, 0.5, 0.5))
image(t(apply(treatment_variation, 2, rev)), col = c("lightgray", '#1E88A8'),
      xaxt = "n", yaxt = "n")
axis(1, at = seq(0, 1, length.out = ncol(treatment_variation)),
        labels = str_remove(colnames(treatment_variation), "year"),
        las = 2, cex.axis = 0.8, tick = FALSE, mgp = c(0, 0, 0.2))
axis(2, at = seq(0, 1, length.out = nrow(treatment_variation)),
        labels = rownames(treatment_variation),
        las = 2, cex.axis = 0.65, tick = FALSE, mgp = c(0, 0, 0.2))
diff_h <- min(diff(seq(0, 1, length.out = nrow(treatment_variation))))
diff_v <- min(diff(seq(0, 1, length.out = ncol(treatment_variation))))
abline(h = seq(0, 1, length.out = nrow(treatment_variation)) + diff_h/2, lty = 1, col = 'white', lwd = 0.3)
abline(v = seq(0, 1, length.out = ncol(treatment_variation)) + diff_v/ 2, lty = 1, col = 'white', lwd = 0.3)
legend("topright", legend = c("Treated", "Control"), fill = c('#1E88A8', 'lightgray'), bg = 'white', cex = 0.8)
box(lwd = 1.1)
dev.off()

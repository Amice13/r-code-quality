#############################
# MAIN SELF-DETERMINATION ANALYSES
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/analysis/analysis_all.R
#
#############################

# LOAD GLOBALS AND DATA ##
if(!exists("LOADED_GLOBALS")){
  source("scripts/analysis/analysis_globals.R")
}
load_data(reload = F)


# HELPER FUNCTIONS ##

# DATA ####

## Load
points.yrs.df <- readRDS(file.path("data/analysis_data/sdm_100.rds"))

# Globals

## Variables
treat <- c( "diff.capeth.tv" )
contr.vars <- c("log(cap.dist)","log(border.dist)",
                "log(pop +1)", "median.altitude" , "median.slope",
                "diff.river", "abramstate.hist", "watershed.diff", "elevmean")

## Survival outcomes
surv.outcomes <- c("sdm.onset", "tco.onset", "breaks.away")



# Main ####

## Stub
stub <- "sdmsurv_main"

## Estimate
model.ls <- unlist(lapply(surv.outcomes, function(o){
  f.vec <- paste0("Surv(yrs.since.start, yrs.since.end, ", o, ")", " ~  ", 
              paste(c(treat,contr.vars), collapse = "+"),
              c("", " + strata(cow.yrs)"),
              " + cluster(cluster.id)")
  lapply(f.vec, function(f) coxph(as.formula(f),points.yrs.df))
}), recursive = F)

## Save
main.model.ls <- model.ls
saveRDS(main.model.ls, file = file.path(tem_res.path, "mainmodel_sdmcox.rds"))

## Min / Max hazard ratios
hr.vec <- sapply(model.ls, function(m){
  exp(m$coefficients[treat])
})
print(hr.vec)

## Prepare Table
add.lines <- list(latex.addline("Events:", sapply(model.ls, function(m){m$nevent})),
                  latex.addline("Country-year strata:", rep(c("no", "yes"), length(surv.outcomes))),
                  latex.addline("Controls:", rep(c("yes", "yes"), length(surv.outcomes))))

## Notes
latex.notes <- function(width = .95){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} 
  Cox Proportional Hazard models. 
  The unit of analysis is the point-year between 1946 and 2012. 
  Standard errors clustered on state-segments.
  Full results with control variables are reported in Table \\ref{sdmsurv_main_full}.
  Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}.")
}

## Save Tex
fileConn <- file(file.path(tab.path, paste0(stub,".tex")))
writeLines(stargazer(model.ls,se = lapply(model.ls, function(m){diag(m$var)^.5}),
                     title="Ethnic boundaries and the onset of self-determination claims, conflict, and border change",
                     keep = treat,
                     multicolumn = F,# se = se,
                     column.labels = collab_w_ruler(c("Secessionist Claim", "Secessionist Civil War", "Secession"), 
                                                    # add.below = c("Secessionist", "Irredentist", "", ""),
                                                    column.separate = c(2,2,2),
                                                    trim = 16),
                     column.separate = c(2,2,2), column.sep.width ="-12pt",
                     dep.var.caption = "Cox Proportional Hazard Model",
                     dep.var.labels = rep("", length(model.ls)),
                     covariate.labels = "Non-coethnic capital",
                     font.size = "scriptsize",
                     notes.align = "c", label=stub, align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("res.dev","ser", "wald", "logrank", "lr"),
                     notes = latex.notes(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)

## Save full model to Tex

### Variable labels
var.labs <- c("Non-coethnic capital", "Pt: Dist. to capital (log)",
              "Pt: Dist. to border (log)", "Pt: Population (log)",
              "Pt: Altitude", "Pt: Ruggedness", 
              "Pt-C: River", "Pt-C: Deep state lag",
              "Pt-C: Watershed", "Pt-C: Elevation")

### Save table
fileConn <- file(file.path(tab.path, paste0(stub,"_full.tex")))
writeLines(stargazer(model.ls,se = lapply(model.ls, function(m){diag(m$var)^.5}),
                     title="Ethnic boundaries and the onset of self-determination processes: Full results",
                     # keep = treat,
                     multicolumn = F,# se = se,
                     column.labels = collab_w_ruler(c("Secessionist Claim", "Secessionist Civil War", "Secession"), 
                                                    column.separate = c(2,2,2),
                                                    trim = 13),
                     column.separate = c(2,2,2), column.sep.width ="-15pt",
                     dep.var.caption = "Cox Proportional Hazard Model",
                     dep.var.labels = rep("", length(model.ls)),
                     covariate.labels = var.labs,
                     font.size = "scriptsize",
                     notes.align = "c", label= paste0(stub, "_full"), align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("res.dev","ser", "wald", "logrank", "lr"),
                     notes = latex.notes(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


## Plot


### prepare all obs
survival.all.df <- data.frame(lapply(points.yrs.df[, all.vars(model.ls[[1]]$call$formula)], 
                                     median, na.rm = T))
survival.all.df <- rbind(survival.all.df, survival.all.df)
survival.all.df$yrs.since.start <- 5
survival.all.df$yrs.since.end <- 6
survival.all.df[,treat] <- rep(c(0, 1 ), 
                                        each = nrow(survival.all.df)/2)
survival.all.df$COWCODE <- 365


### Models
plot.model.ls <- lapply(c(1,3,5), function(i) model.ls[[i]])

### Fit
fit.ls <- lapply(seq_along(plot.model.ls), function(i){
  print(i)
  m <- plot.model.ls[[i]]
  ## Draw 1000 coefs
  set.seed(1)
  sim <- mvrnorm(n=1000,mu=coef(m),Sigma=m$var)
  sim <- rbind(m$coefficients,
               sim)
  
  ## Fit
  fits <- do.call(rbind, lapply(seq_len(nrow(sim)), function(j){
    m$coefficients <- sim[j,]
    survfit(m, newdata = survival.all.df)$surv
  }))
  
  ## Return dataframe
  res <- data.frame(model = i, 
             sim = rep(seq_len(nrow(sim)), 
                       each = nrow(fits)/nrow(sim)),
             time = seq_len(nrow(fits)/nrow(sim)), 
             type = rep(c("Coethnic", "Non-coethnic", "Difference"), each = nrow(fits)),
             value = c(1-fits[,1], 1-fits[,2], (1-fits[,1])- (1-fits[,2])),
             stringsAsFactors = F
             )
  
  ## Return
  res
})

### Combine
plot.df <- do.call(rbind, fit.ls)
plot.df$type <- factor(plot.df$type,
                       levels = c("Non-coethnic","Coethnic",  "Difference"),
                       ordered = T)
plot.df$model <- c("Secessionist claim", 
                   "Secessionist civil war", "Secession")[plot.df$model]
plot.df$model <- factor(plot.df$model,
                       levels = unique(plot.df$model),
                       ordered = T)

### Qantiles
q025 <- aggregate.data.frame(plot.df[, "value", drop = F],
                            plot.df[, c("model", "type", "time")],
                            quantile, probs = .025)
q975 <- aggregate.data.frame(plot.df[, "value", drop = F],
                            plot.df[, c("model", "type", "time")],
                            quantile, probs = .975)

print(plot.df[plot.df$time == 50 & plot.df$sim == 1, ])

### Plot
g <- ggplot(plot.df, aes(x = time, y = value, 
                         col = type, lty = type, 
                         group = interaction(type))) + 
  # geom_step(data = plot.df, size = .5, alpha = .1) + 
  geom_step(data = q025, size = .5, alpha = .5) +
  geom_step(data = q975, size = .5, alpha = .5) +
  geom_step(data = plot.df[plot.df$sim == 1,], size = 1) + 
  geom_hline(yintercept = 0, lty = 2, col = "grey") +
  facet_wrap( ~ model, nrow = 1) + 
  xlab("Years")+ ylab("Cumulative probability of outcome onset") + #ylim(c(0,1)) +
  scale_color_manual(values = inferno(n = 10)[c(1, 5, 8)]) +
  scale_y_continuous(breaks = seq(-.4, .4, by = .2)) +
  labs(lty = "", color = "") +
  # scale_color_viridis(option = "plasma", discrete = T) +
  theme_minimal() +
  theme(legend.position = "top",
        panel.spacing = unit(1, "lines")) +
  labs(col = "")
  


### Save
png(file.path(fig.path, "survival_main.png"), width = 6.5, height = 4, 
    unit = "in", res = 400)
print(g)
dev.off()


# Robustness check: Periods from 1946 only ####

## Stub
stub <- "sdmsurv_1946borders"

## Select periods
these.spells <- points.yrs.df$spell.id[points.yrs.df$year == 1946]

## Estimate
model.ls <- unlist(lapply(surv.outcomes, function(o){
  f.vec <- paste0("Surv(yrs.since.start, yrs.since.end, ", o, ")", " ~  ", 
                  paste(c(treat,contr.vars), collapse = "+"),
                  c("", " + strata(cow.yrs)"),
                  " + cluster(cluster.id)")
  lapply(f.vec, function(f) {
    coxph(as.formula(f),
          points.yrs.df[points.yrs.df$spell.id %in% these.spells,])
  })
}), recursive = F)

## Prepare Table
add.lines <- list(latex.addline("Events:", sapply(model.ls, function(m){m$nevent})),
                  latex.addline("Country-year strata:", rep(c("no", "yes"), length(surv.outcomes))),
                  latex.addline("Controls:", rep(c("yes", "yes"), length(surv.outcomes))))

## Notes
latex.notes <- function(width = .95){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} 
  Cox Proportional Hazard models. 
  The unit of analysis is the point-year between 1946 and 2012. 
  Standard errors clustered on state-segments.
  Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}")
}

## Save Tex
fileConn <- file(file.path(tab.path, paste0(stub,".tex")))
writeLines(stargazer(model.ls,se = lapply(model.ls, function(m){diag(m$var)^.5}),
                     title="Ethnic boundaries and self-determination: Within 1946 borders only",
                     keep = treat,
                     multicolumn = F,# se = se,
                     column.labels = collab_w_ruler(c("Secessionist Claim", "Secessionist Civil War", "Secession"), 
                                                    column.separate = c(2,2,2),
                                                    trim = 13),
                     column.separate = c(2,2,2), column.sep.width ="-10pt",
                     dep.var.caption = "Cox Proportional Hazard Model",
                     dep.var.labels = rep("", length(model.ls)),
                     covariate.labels = "Non-coethnic capital",
                     font.size = "scriptsize",
                     notes.align = "c", label=stub, align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("res.dev","ser", "wald", "logrank", "lr"),
                     notes = latex.notes(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)


# 1886 Ethnic Data ####

## Stub
stub <- "sdmsurv_1886eth"

## Estimate
model.ls <- unlist(lapply(surv.outcomes, function(o){
  f.vec <- paste0("Surv(yrs.since.start, yrs.since.end, ", o, ")", " ~  ", 
                  paste(c( "diff.capeth.1886",contr.vars), collapse = "+"),
                  c("", " + strata(cow.yrs)"),
                  " + cluster(cluster.id)")
  lapply(f.vec, function(f) coxph(as.formula(f),points.yrs.df))
}), recursive = F)


## Prepare Table
add.lines <- list(latex.addline("Events:", sapply(model.ls, function(m){m$nevent})),
                  latex.addline("Country-year strata:", rep(c("no", "yes"), length(surv.outcomes))),
                  latex.addline("Controls:", rep(c("yes", "yes"), length(surv.outcomes))))

## Notes
latex.notes <- function(width = .95){
  paste0("\\parbox[t]{",width,"\\textwidth}{\\textit{Notes:} 
  Cox Proportional Hazard models. 
  The unit of analysis is the point-year between 1946 and 2012. 
  Standard errors clustered on state-segments.
  Full results with control variables are reported in Table \\ref{sdmsurv_main_full}.
  Significance codes: $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01}.")
}

## Save Tex
fileConn <- file(file.path(tab.path, paste0(stub,".tex")))
writeLines(stargazer(model.ls,se = lapply(model.ls, function(m){diag(m$var)^.5}),
                     title="Ethnic boundaries and the onset of self-determination claims, conflict, and border change: Ethnicity data from before 1886",
                     keep = "diff.capeth.1886",
                     multicolumn = F,# se = se,
                     column.labels = collab_w_ruler(c("Secessionist Claim", "Secessionist Civil War", "Secession"), 
                                                    column.separate = c(2,2,2),
                                                    trim = 10),
                     column.separate = c(2,2,2), column.sep.width ="-7pt",
                     dep.var.caption = "Cox Proportional Hazard Model",
                     dep.var.labels = rep("", length(model.ls)),
                     covariate.labels = "Non-coethnic$_{1886}$ capital$_t$",
                     font.size = "scriptsize",
                     notes.align = "c", label=stub, align =T,
                     add.lines = add.lines,digits = 3, intercept.top = T,intercept.bottom = F,
                     omit.stat = c("res.dev","ser", "wald", "logrank", "lr"),
                     notes = latex.notes(.95), notes.label = "", notes.append = F), 
           fileConn)
close(fileConn)

########################################
# Plot Replication Code for
# "The Effect of Citizenship on the Long-Term Earnings of Marginalized Immigrants: 
# Quasi-Experimental Evidence from Switzerland"
# Science Advances (DOI: 10.1126/sciadv.aay1610)
# Jens Hainmueller, Dominik Hangartner, and Dalston Ward
# December 2019
########################################

# IMPORTANT: For exact replication, use R 3.5.2 
# (original code run on MacOS 10.14.6)

library(data.table) #version 1.12.6
library(scales) #version 1.0.0
library(ggplot2) #version 3.2.1
library(viridis) #version 0.5.1
library(grid) #version 3.5.2
library(gridExtra) #version 2.3
library(quantreg) #version 5.51

# Note: requires loading in the did and RDD datasets and then fitting regression models with code from Analysis.R

#######################################
# Functions
#######################################

# Workhorse function for making RDD plots using ggplot2.
RDD.gg <- function(
  plot.dat, # a dataset (typically RDD)
  DV, # character string of variable to plot
  DV_lab, # character string of label for DV.
  ind.points = F, # logical.  Plot points for individual observations?
  title = NULL, # character.
  salary.var = T, # logical.  If a salary variable, divide points by 1,000 for readability.
  ybreaks = NULL, # numeric.  Where should breaks on Y axis be?  Will also reshape Y axis.
  spanL = .8, # numeric.  For smoothing below the cutoff.
  spanR = .8, # numeric.  For smoothing above the cutoff.
  point_bin = 1, # numeric (1, 2, or 3). How many integrer values to bin together?
  facet_title = F #logical.  If true, uses faceting for title instead of GGtitle.
  ){
  if(salary.var){
    div <- 1000
  } else {
    div <- 1
  }

  plot.dat <- copy(plot.dat) #prevents unintended altering of original data (at a slight cost in processing time!)

  if(point_bin <= 2){
    plot.dat[ naturalized == F, new_x := point_bin*ceiling(prct_ja/point_bin) - (point_bin - 1)]
    plot.dat[ naturalized == T, new_x := point_bin*floor(prct_ja/point_bin) + (point_bin - 1)]
    mean.plots <- plot.dat[ new_x %in% min(prct_ja):max(prct_ja) ,.(avgDV = mean(get(DV), na.rm = T)/div), keyby = .(naturalized, new_x)]
  }

  if(point_bin == 3){
    plot.dat[ naturalized == F, new_x := point_bin*ceiling(prct_ja/point_bin) - 2]
    plot.dat[ naturalized == T, new_x := point_bin*floor(prct_ja/point_bin - 0.34) + 3]
    mean.plots <- plot.dat[ new_x %in% min(prct_ja):max(prct_ja) ,.(avgDV = mean(get(DV), na.rm = T)/div), keyby = .(naturalized, new_x)]
  }

  out <- ggplot(data = plot.dat, aes(x = prct_ja, y = as.numeric(get(DV))/div, color = naturalized == 1, fill = naturalized == 1)) +
    geom_vline(xintercept = 50, colour = "black", lty = 2) +
    geom_point(data = mean.plots, aes(x = new_x, y = avgDV, color = naturalized == 1), size = 2) +
    geom_smooth(data = plot.dat[naturalized == 0], aes(x = prct_ja, y = as.numeric(get(DV))/div), method = "loess", span = spanL, method.args = list(degree = 1), col = viridis(n=3)[1]) +
    geom_smooth(data = plot.dat[naturalized == 1], aes(x = prct_ja, y = as.numeric(get(DV))/div), method = "loess", span = spanR, method.args = list(degree = 1), col = viridis(n=3)[2]) +
    scale_color_manual(values = viridis(n = 3)[-3], name = "Naturalization Referendum Outcome", labels = c("Rejected", "Accepted")) +
    scale_fill_manual(values = viridis(n = 3)[-3], guide = F) +
    scale_x_continuous(expand = c(0.025, 0.025)) +
    guides(color = guide_legend(override.aes = list(fill = NA))) +
    labs(x = "% \"Yes\" Votes in Naturalization Referendum", y = DV_lab) +
    theme(panel.grid.minor = element_blank(), legend.background = element_rect(fill = "white", linetype = "solid", size = .5, color = "black"), legend.title.align = .5, legend.position = "bottom")

  if(!is.null(ybreaks)){
    out <- out + coord_cartesian(ylim = c(min(ybreaks), max(ybreaks)))
    if(salary.var){
      out <- out + scale_y_continuous(breaks = ybreaks, labels = paste0(ybreaks, ",000"))
    } else {
      out <- out + scale_y_continuous(breaks = ybreaks)
    }
  }

  if(ind.points){
    out <- out + geom_point(data = plot.dat[get(DV) < plot.dat[, quantile(get(DV), .9, na.rm = T)] & get(DV) > plot.dat[, quantile(get(DV), .1, na.rm = T)]], aes(x = prct_ja, y = as.numeric(get(DV))/div, color = naturalized == 1), alpha = .2, size = 1)
  }

  if(!is.null(title)){
    if(!facet_title){
    out <- out + ggtitle(title)
    } else {
      plot.dat[ , ft := title]
      out <- out + facet_grid( ~ ft)
    }
  }

  return(out)
}

# Suite of functions for transfering regression results onto an aesthetically pleasing plot.
plot_prep <- function(treat_var, model){
  data.frame(
    Beta = summary(model)$coef[treat_var, 1],
    Se = summary(model)$coef[treat_var, 2],
    df = df.residual(model),
    design = ifelse(class(model) == "felm", "DiD", "RD")
  )
}

coefplot.gg <- function(
  model.list, # list of models, already processed through plot_prep
  var_names = c("-5 to -1", "0 to 5", "6 to 10", "11 to 15", "0 to 10", "0 to 15"), # character.  Must have length equal the number of models per specification.
  title = NULL, # character.
  facet_title = F, # logical.  If true, title is plotted via faceting.
  DV_lab = NULL # character.  What goes on the y axis?
){
  
  dfplot = data.frame(
    modelcoef = c(rep(as.numeric(NA), times = length(model.list))),
    ylo90 = c(rep(as.numeric(NA), times = length(model.list))),
    yhi90 = c(rep(as.numeric(NA), times = length(model.list))),
    ylo95 = c(rep(as.numeric(NA), times = length(model.list))),
    yhi95 = c(rep(as.numeric(NA), times = length(model.list))),
    design = c(rep(NA, times = length(model.list)))
  )
  
  for(i in 1:length(model.list)){
    dfplot$modelcoef[i] = model.list[[i]]$Beta
    dfplot$ylo90[i] = model.list[[i]]$Beta - qt(.95, model.list[[i]]$df)*(model.list[[i]]$Se)
    dfplot$yhi90[i] = model.list[[i]]$Beta + qt(.95, model.list[[i]]$df)*(model.list[[i]]$Se)
    dfplot$ylo95[i] = model.list[[i]]$Beta - qt(.975, model.list[[i]]$df)*(model.list[[i]]$Se)
    dfplot$yhi95[i] = model.list[[i]]$Beta + qt(.975, model.list[[i]]$df)*(model.list[[i]]$Se)
    dfplot$design[i] = levels(model.list[[i]]$design)
  }
  
  cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  dfplot$design <- factor(dfplot$design, levels = c("RD", "DiD")) # make it plot RD estimates first.
  
  if(facet_title) dfplot$title <- title
  
  p = ggplot(dfplot, aes(x = rep(1:6, times = length(unique(dfplot$design))), y = modelcoef, ymin = ylo95, ymax = yhi95, group = design, color = design, shape = design)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_vline(xintercept = 4.5, lty = 1, color = "white", size = 4) +
    geom_vline(xintercept = 1.5, lty = 2, size = 1) +
    geom_linerange(position = position_dodge(width = .4)) +
    geom_linerange(aes(ymin = ylo90, ymax = yhi90), position = position_dodge(width = .4), size = 1.75) +
    geom_point(position = position_dodge(width = .4), size = 3.75, fill = "white") +
    scale_x_continuous(breaks = 1:6, labels = var_names) +
    scale_y_continuous(breaks = c(0, 5000, 10000, 15000), labels = c("0", "5,000", "10,000", "15,000")) +
    scale_color_manual(values = cbPalette[c(7,6,3)], name = "Specification") +
    scale_shape_manual(values = c(21, 17), name = "Specification") +
    xlab("Years Before/After Naturalization Referendum") +
    ylab(DV_lab)  +
    theme(panel.grid.minor = element_blank())
  
  if(!is.null(title)){
    if(!facet_title){
      p <- p + ggtitle(title)
    } else {
      p <- p + facet_grid( ~ title)
    }
  }
  
  return(p)
}

DID.gg <- function(
  data, # dataset.  Probably zasi or did
  DV, # character. Name of variable to plot
  DV_lab = NULL, # character.  Y axis label.
  ybreaks = NULL, # numeric.  Where break on y axis
  title = NULL, # character.
  facet_title = F # logical.  If false, title is done with facetting.
  ){

  data <- copy(data)
  if(facet_title) data[ , title := title]

  rp_means <- data[ ,mean(get(DV), na.rm = T), by = .(ref_time, naturalized)]

  out <- ggplot(data = data.frame(data), aes(x = ref_time, y = as.numeric(get(DV)), color = naturalized == 1, fill = naturalized == 1 )) +
    geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
    geom_smooth(method = "loess", span = .7) +
    geom_point(data = rp_means, aes(x = ref_time, y = V1, color = naturalized == 1)) +
    scale_color_manual(values =  viridis(n = 3)[-3], name = "Naturalization Referendum Outcome", labels = c("Rejected", "Accepted")) +
    scale_fill_manual(values =  viridis(n = 3)[-3], guide = F) +
    scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-5, 15), expand = c(0.025,0.025)) +
    labs(x = "Years Before/After Naturalization Referendum", y = DV_lab)

  if(!is.null(ybreaks)){
    out <- out + scale_y_continuous(breaks = ybreaks, labels = comma)
  }

  if(!is.null(title)){
    if(!facet_title){
      out <- out + ggtitle(title)
    } else {
      out <- out + facet_grid( ~ title)
    }
  }

  return(out)
}

# Function for putting it all together with nice legends.
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {

  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]

  print(legend)

  g2 <- ggplotGrob(plots[[4]] + theme(legend.position = position))$grobs
  legend2 <- g2[[which(sapply(g2, function(x) x$name) == "guide-box")]]

  print(legend2)

  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)

  lheight2 <- sum(legend2$height)
  lwidth2 <- sum(legend2$width)

  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  # legendl <- c(list(legend, legend2), ncol = 2, nrow = 1)

  legend <- arrangeGrob(legend, legend2, ncol = 2)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))

  grid.newpage()
  grid.draw(combined)

  # return gtable invisibly
  invisible(combined)

}

# Function to bootstrap the CI for the quantile
geom_quantile_CI <- function(
  data, # probably zasi or did
  DV, # character.  Name of variable to estimate an effect for.
  n_boots = 200, # integer.  Higher numbers are slower but more reliable
  level = .95, # double. Confidence level.
  boot_index, # character.  Variable on which to sample.
  lambda = 1, # scalar.  Smoothing parameter.  larger numbers make it smoother. 
  tau = .5, # double.  Quantile to estimate.
  ... # additional arguments (for ggplot)
  ){
  setDT(data, key = boot_index)
  
  b.sample <- replicate(n_boots, data[ .(sample(unique(get(boot_index)), replace = T)), 
                                       predict(rqss(get(DV) ~ qss(ref_time, lambda = lambda), tau = tau), newdata = data.frame(ref_time = seq(-5, 15, .01)))])
  
  CI <- apply(b.sample, 1, quantile, c((1 - level)/2, (1 - level)/2 + level))
  CI <- as.data.frame(t(CI))
  colnames(CI) <- c( "lower", "upper")
  
  return(geom_ribbon(data = CI, aes(x = seq(-5, 15, .01), ymin = lower, ymax = upper), inherit.aes = F, ...))
}

## Useful for conducting the balance tests. 
pval_fit <- function(DV, DV_print = NULL, data, cov.string = "~ naturalized*prct_ja_0"){
  
  output <- data.frame(var = NA, est = NA, se = NA, p = NA)
  
  f <- as.formula(paste(DV, cov.string, sep = ""))
  q <- lm(f, data = data)
  output[1,2:4] <- summary(q)$coefficients[2, c(1,2,4)]
  
  if(!is.null(DV_print)) output[,1] <- unlist(DV_print) else output[,1] <- DV
  
  return(output)
}
#######################################

#######################################
# Figure 1: Make the raw pieces that go into figure 1, then put them together
#######################################

RDD_pre <- RDD.gg(RDD[prct_ja %in% 30:70], "sal2k_avg_pre_1to5", "Earnings (CHF)", ybreaks = seq(30, 55, 5), spanL = 1.5, spanR = 1, point_bin = 2, title = "A. Earnings before Naturalization Referendum", facet_title = T) +
  coord_cartesian(ylim = c(29, 56)) +
  theme(strip.text.x = element_text(size = 15, hjust = 0), legend.box.margin=margin(3,3,15,3), strip.background = element_rect(fill= "white"))

RDD_post <- RDD.gg(RDD[prct_ja %in% 30:70], "sal2k_avg_post_0to15", "Earnings (CHF)", ybreaks = seq(30,55,5), spanL = 1.3, spanR = 1.3, point_bin = 2, title = "B. Earnings after Naturalization Referendum", facet_title = T) +
  coord_cartesian(ylim = c(29, 56)) +
  theme(strip.text.x = element_text(size = 15, hjust = 0), strip.background = element_rect(fill= "white"))

DID <- DID.gg(data = did[prct_ja %in% 40:60], DV = "sal2k", title = "C. 20-Year Earnings Trend", facet_title = T, ybreaks = seq(45000, 60000, 5000), DV_lab = "Earnings (CHF)") +
  theme(panel.grid.minor = element_blank(), legend.justification = c(1,0), legend.position=c(.55,.15), legend.background = element_rect(fill = "white", linetype = "solid", size = .5, color = "black"), legend.title.align=0.5, strip.text.x = element_text(size = 15, hjust = 0), strip.background = element_rect(fill= "white"))

# Fit models first using Analysis_v2.R!
models <- mapply(
  plot_prep,
  c(rep("naturalized", 6), "post_naturalized_placebo", rep("post_naturalized", 5)),
  list(rd_placebo, rd_0to5,rd_6to10,rd_11to15, rd_0to10, rd_0to15,
    did_placebo, did_0to5, did_6to10, did_11to15, did_0to10, did_0to15),
  SIMPLIFY = F
  )

EST <- coefplot.gg(model.list = models, DV_lab = 'Effect of Nat. Referendum Success on Earnings (CHF)', title = "D. Effect of Naturalization Referendum Success", facet_title = T) +
  theme(legend.position = "right",
        legend.background = element_rect(fill = "white", linetype = "solid", size = .5, color = "black"),
        strip.text.x = element_text(size = 15, hjust = 0),
        strip.background = element_rect(fill= "white"),
        legend.box.margin=margin(3,3,15,3)) +
  xlab("Post-Referendum Years Used for Estimation") +
  coord_cartesian(clip = "off", ylim = c(-4200, 14900)) 
  # annotate(geom = "text", x = 2.5, y = -6600, label = "5-Year Post-Referendum Intervals")
# EST
grid_arrange_shared_legend(RDD_pre, RDD_post, DID, EST, nrow = 2, ncol = 2, position = "bottom")

#######################################

#######################################
# Figrue 2: Make the quantile regressions plot
#######################################
set.seed(8006)
quantileData <- copy(did[prct_ja %in% 40:60])

CI_T_25 <-  geom_quantile_CI(data = quantileData[naturalized == T], DV = "sal2k", n_boots = 1000, level = 0.95, boot_index = "personID", lambda = 1.5, alpha = .45, fill = "#21908CFF", tau = .25)
CI_F_25 <-   geom_quantile_CI(data = quantileData[naturalized == F], DV = "sal2k", n_boots = 1000, level = 0.95, boot_index = "personID", lambda = 1.5, alpha = .45, fill = "#440154FF", tau = .25)
CI_T_50 <-  geom_quantile_CI(data = quantileData[naturalized == T], DV = "sal2k", n_boots = 1000, level = 0.95, boot_index = "personID", lambda = 1.5, alpha = .45, fill = "#21908CFF", tau = .5)
CI_F_50 <-   geom_quantile_CI(data = quantileData[naturalized == F], DV = "sal2k", n_boots = 1000, level = 0.95, boot_index = "personID", lambda = 1.5, alpha = .45, fill = "#440154FF", tau = .5)
CI_T_75 <-  geom_quantile_CI(data = quantileData[naturalized == T], DV = "sal2k", n_boots = 1000, level = 0.95, boot_index = "personID", lambda = 1.5, alpha = .45, fill = "#21908CFF", tau = .75)
CI_F_75 <-   geom_quantile_CI(data = quantileData[naturalized == F], DV = "sal2k", n_boots = 1000, level = 0.95, boot_index = "personID", lambda = 1.5, alpha = .45, fill = "#440154FF", tau = .75)

rp_75q <- quantileData[ , quantile(sal2k, probs = .75, na.rm = T), by = .(ref_time, naturalized)]
rp_50q <- quantileData[ , quantile(sal2k, probs = .50, na.rm = T), by = .(ref_time, naturalized)]
rp_25q <- quantileData[ , quantile(sal2k, probs = .25, na.rm = T), by = .(ref_time, naturalized)]

quant_plot <- ggplot(data = data.frame(quantileData), aes(x = ref_time, y = as.numeric(sal2k), color = naturalized == 1, fill = naturalized == 1 )) +
  geom_vline(xintercept = 0, colour = gray(1/2), lty = 2) +
  CI_T_25 + CI_F_25 + CI_T_50 + CI_F_50 + CI_T_75 + CI_F_75 +
  geom_quantile(quantiles = c(.25, .5, .75), method = "rqss", lambda = 1.5) +
  geom_point(data = rp_25q, aes(x = ref_time, y = V1, color = naturalized == 1, shape = "25"), size = 2.5) +
  geom_point(data = rp_50q, aes(x = ref_time, y = V1, color = naturalized == 1, shape = "50"), size = 2.5) +
  geom_point(data = rp_75q, aes(x = ref_time, y = V1, color = naturalized == 1, shape = "75"), size = 2.5) +
  scale_color_manual(values = viridis(n = 3)[-3], name = "Naturalization Referendum Outcome", labels = c("Rejected", "Accepted")) +
  scale_fill_manual(values = viridis(n = 3)[-3], guide = F) +
  scale_shape_manual(values = 15:17, name = "Quantile", labels = c("25th", "50th", "75th")) +
  scale_x_continuous(breaks = seq(-15, 15, 5), limits = c(-5, 15), expand = c(0.025,0.025)) +
  scale_y_continuous(labels = comma) +
  theme(legend.position = "bottom", legend.title.align=0.5) +
  labs(x = "Years Before/After Naturalization Referendum", y = "Earnings (CHF)") +
  guides(color = guide_legend(override.aes = list(shape = NA, size = 3)), shape = guide_legend(override.aes = list(shape = 0:2, size = 2.5)))
quant_plot
#######################################


########################################
# Figure S2: Match Rate for archival data to ZAS
########################################

tax <- fread("<<OASI data>>", colClasses = c("nnaaff" = "character", "s_noaff11" = "character"))
ref <- fread("<<Referendum data>>")

tax_ind <- tax[!origin_grouped == "land_ch" , .(prct_ja = unique(prct_ja), naturalized = unique(naturalized)), by = personID]
tax_tab <- tax_ind[ , .N, keyby = .(prct_ja, naturalized)]
ref_tab <- ref[!origin_grouped == "land_ch" , .N, keyby = .(prozentjastimmen_gerundet, naturalized == 1)]
tax_tab[ , match_rate := N/ref_tab$N ]

RDD.gg(tax_tab[prct_ja %in% 30:70], DV = "match_rate", DV_lab = "Match Rate", ind.points = F, title = NULL, salary.var = F, ybreaks = NULL, spanL = 1, spanR = 1, point_bin = 2)  + coord_cartesian(ylim = c(0, 1)) + scale_y_continuous(expand = c(0, .025))


rm("tax", "ref", "tax_ind", "tax_tab", "ref_tab")
########################################


########################################
# Figure S3: Sorting diagram
########################################
ggplot() +
  geom_histogram(data = RDD[prct_ja >= 50], aes( x = prct_ja, fill = "#21908CFF"),binwidth = 1, position = "identity", color = "black", breaks = seq(49.5,70.5,1), alpha = 1) +
  geom_histogram(data = RDD[naturalized == 0], aes( x = prct_ja, fill = "#440154FF"), binwidth = 1,  position = "identity", color = "black", breaks = seq(29.5, 50.5, 1), alpha = 1) +
   xlab("% \"Yes\" Votes in Natualization Referendum") +
  ylab("Applicants") +
  scale_x_continuous(breaks = seq(25, 70, 5)) +
  scale_fill_manual(name = "Naturalization Referendum Outcome", values =  c("#21908CFF", "#440154FF"), labels = c("Accepted", "Rejected")) +
  guides(fill = guide_legend(reverse=T)) +
  theme(legend.position = "bottom")
########################################

########################################
# Figure S4: Balance test (6 by 8 figure)
########################################

test_vars <- c("Female" = "female",
               "Referendum Age: < 20" =  "ref_age < 20",
               "Referendum Age: 20-29" =  "ref_age %in% 20:29",
               "Referendum Age: 30-39" =  "ref_age %in% 30:39",
               "Referendum Age: 40-49" =  "ref_age %in% 40:49",
               "Referendum Age: 50-59" =  "ref_age %in% 50:59",
               "Referendum Age: 60 +" =  "ref_age >= 60",
               "Referendum Year < 1980" = "cabjahr < 1980",
               "Referendum Year: 1980-1984" = "cabjahr %in% 1980:1984",
               "Referendum Year: 1985-1989" = "cabjahr %in% 1985:1989",
               "Referendum Year: 1990-1994" = "cabjahr %in% 1990:1994",
               "Referendum Year: 1995-1999" = "cabjahr %in% 1995:1999",
               "Referendum Year: > 1999" = "cabjahr %in% 2000:2003",
               "Birth Year: 1920-1929" = "cgebj %in% 1920:1929",
               "Birth Year: 1930-1939" = "cgebj %in% 1930:1939",
               "Birth Year: 1940-1949" = "cgebj %in% 1940:1949",
               "Birth Year: 1950-1959" = "cgebj %in% 1950:1959",
               "Birth Year: 1960-1969" = "cgebj %in% 1960:1969",
               "Birth Year: 1970-1979" = "cgebj %in% 1970:1979",
               "Birth Year: 1980-1989" = "cgebj %in% 1980:1989",
               "Birth Year: 1990-1999" = "cgebj %in% 1990:1999",
               "Origin = Africa" =  "I(origin_grouped == \"land_africa\")",
               "Origin = Asia" = "I(origin_grouped == \"land_asia\")",
               "Origin = Western Europe" = "I(origin_grouped == \"land_eu_west\")",
               "Origin = Central & Eastern Europe" = "I(origin_grouped == \"land_mee\")",
               "Origin = Other" = "I(origin_grouped == \"land_other\")",
               "Origin = Turkey & (Former) Yugoslvaia" = "I(origin_grouped == \"land_see\")",
               "Origin = Southern Europen" = "I(origin_grouped == \"land_south\")",
               "Earnings (CHF), Ref. Year - 5" =  "sal2k_pre5",
               "Earnings (CHF), Ref. Year - 4" =  "sal2k_pre4",
               "Earnings (CHF), Ref. Year - 3" =  "sal2k_pre3",
               "Earnings (CHF), Ref. Year - 2" =  "sal2k_pre2",
               "Earnings (CHF), Ref. Year - 1" =  "sal2k_pre1",
               "Mean Unemployment, Ref. Year -1 to -5" = "unempMonths_avg_pre_1to5",
               "Disability Benefits, Ref. Year -1 to -5" = "disability_avg_pre_1to5")

balance_tests <- t(mapply(pval_fit, test_vars, names(test_vars), MoreArgs = list(data = RDD[prct_ja %in% 40:60]), USE.NAMES = F))
to_plot_data <- data.frame(var = unlist(balance_tests[,1]), est = unlist(balance_tests[,2]), se = unlist(balance_tests[,3]), p = unlist(balance_tests[,4]))
to_plot_data <- to_plot_data[order(to_plot_data$p), ]

ggplot(to_plot_data, aes(y =  nrow(to_plot_data):1, x = p)) +
  geom_point(size = 2) +
  geom_vline(xintercept = .05, lty = 2, color = "RED") +
  geom_vline(xintercept = .1, lty = 2, color = "BLUE") +
  scale_y_continuous( breaks = nrow(to_plot_data):1, labels = to_plot_data$var, limits = c(.5,nrow(to_plot_data) + .5), expand = expand_scale(0,0)) +
  ylab(NULL) +
  xlab(expression(paste(italic("p"), "-value", sep=""))) + 
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.y = element_line(size = .15, linetype = 2, color = "gray75")) +
  scale_x_continuous(expand = c(0,0), limits = c(-0.01, 1.01), breaks = c(0, .05, 0.1, 1))
########################################

#######################################
# Figure S5: Coefficient plot for the non-CPI adjusted models
#######################################

# Fit models first using Analysis_v2.R!
models_noCPI <- mapply(
  plot_prep,
  c(rep("naturalized", 6), "post_naturalized_placebo", rep("post_naturalized", 5)),
  list(rd_placebo_noCPI, rd_0to5_noCPI,rd_6to10_noCPI,rd_11to15_noCPI, rd_0to10_noCPI, rd_0to15_noCPI,
       did_placebo_noCPI, did_0to5_noCPI, did_6to10_noCPI, did_11to15_noCPI,did_0to10_noCPI, did_0to15_noCPI),
  SIMPLIFY = F
)

coefplot.gg(model.list = models_noCPI, DV_lab = 'Effect of Naturalization Referendum Success on Earnings (CHF)') +
  theme(legend.position = "bottom", legend.background = element_rect(fill = "white", linetype = "solid", size = .5, color = "black"), strip.text.x = element_text(size = 15, hjust = 0), strip.background = element_rect(fill= "white"), legend.box.margin=margin(3,3,15,3)) +
  xlab("Post-Referendum Years Used for Estimation")
########################################

#######################################
# Figures S6 and S7: BW robustness plots
#######################################

# For difference-in-difference specification
ggplot(data = BW_results_DD[BW_results_DD$BW >= 1,], aes(x = BW, y = Beta, ymin = Beta + qt(.025, df)*SE, ymax = Beta + qt(.975, df)*SE, color = BW == 10)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = Beta + qt(.05, df)*SE, ymax = Beta + qt(.95, df)*SE), size = 1.75) +
  geom_pointrange(shape = 21, fill = "white") +
  scale_color_manual(values = c("#0072B2", "#D55E00"), guide = F) +
  scale_y_continuous(labels = comma) +
  xlab("Distance from Referendum Success Threshold (in % \"Yes\" Votes)") +
  ylab("Effect of Naturalization Referendum Success on Earnings (CHF)")

# For regression discontinuity specification
ggplot(data = BW_results_RD[BW_results_RD$BW >= 1,], aes(x = BW, y = Beta, ymin = Beta + qt(.025, df)*SE, ymax = Beta + qt(.975, df)*SE, color = BW == 10)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_linerange(aes(ymin = Beta + qt(.05, df)*SE, ymax = Beta + qt(.95, df)*SE), size = 1.75) +
  geom_pointrange(shape = 21, fill = "white") +
  scale_color_manual(values = c("#0072B2", "#D55E00"), guide = F) +
  scale_y_continuous(labels = comma) +
  xlab("Distance from Referendum Success Threshold (in % \"Yes\" Votes)") +
  ylab("Effect of Naturalization Referendum Success on Earnings (CHF)")

#######################################


########################################
# Figure S8: RDD plot for attrition over time
########################################
RDD.gg(plot.dat = RDD[prct_ja %in% 30:70], DV = "obs_freq_0to15", DV_lab = "Number of Observations, 0-15 Years after Naturalization Referendum", salary.var = F, point_bin = 2, spanL = 1.2, spanR = 1.2, ybreaks = seq(0, 15, 3))
########################################
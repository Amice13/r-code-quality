############################################################
## Effect-size mixed model from master CSV (base R + lmerTest)
## - Adds precipitation (mean & anomaly) as predictors
## - Random intercept for corename when identifiable
## - 50y bins recommended, but WIDTH switch still works
############################################################

filepath<-"/Users/juanbenavides/Library/CloudStorage/Dropbox/papers/peat 210 pb/stats2/Geographic/derived"

setwd(filepath)

library(readr); library(dplyr); library(tidyr); library(stringr)
library(ggplot2); library(lme4); library(broom.mixed); library(lmerTest)


# ---------------- CONFIG ----------------
infile   <- "AGE_DEPTH_with_Tanoms_Tabs_CAR_PAR_PRE.csv"
WIDTH    <- 50            # 25 or 50
RESP     <- "CAR"         # "PAR" or "CAR"
PSET     <- "anoms"       # "levels" (Tmean,Tmin) or "anoms" (Tmean_anom,Tmin_anom)
MAKE_PLOT <- TRUE
# ----------------------------------------

wmean <- function(x, w=NULL){ if (is.null(w)) return(mean(x, na.rm=TRUE))
  ok <- is.finite(x) & is.finite(w); if(!any(ok)) return(NA_real_)
  sw <- sum(w[ok], na.rm=TRUE); if(sw<=0) return(mean(x[ok], na.rm=TRUE))
  sum(x[ok]*w[ok], na.rm=TRUE)/sw }
zcol <- function(v){ s <- sd(v, na.rm=TRUE); if(!is.finite(s)||s==0) return(rep(NA_real_, length(v)))
  (v-mean(v,na.rm=TRUE))/s }
recode_disturbance <- function(v){
  x <- tolower(trimws(as.character(v)))
  out <- ifelse(is.na(x), NA,
         ifelse(grepl("conserv|intact|control|undist",x),"intact",
         ifelse(grepl("degrad|disturb|burn|graz|drain|ditch|alter|impact",x),"degraded",x)))
  factor(out, levels=c("intact","degraded")) }
make_bin_label <- function(year,width){ s <- floor(year/width)*width; paste0(s,"\u2013",s+width-1L) }

# ---------- Read + clip + build precip anoms ----------
stopifnot(file.exists(infile))
dat0 <- read.csv(infile, check.names=FALSE, stringsAsFactors=FALSE)

need <- c("corename","age_calAD_mean","Tmean_abs_C","Tmin_abs_C",
          "Tmean_anom_C","Tmin_anom_C","PAR_cm_yr","CAR_app_g_m2_yr",
          "dt_years","disturbance","pre_mm")
miss <- setdiff(need, names(dat0))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))

# clip to >= 1900 and compute core-level precip mean & anomalies
dat0 <- dat0[is.finite(dat0$age_calAD_mean) & dat0$age_calAD_mean >= 1900, , drop=FALSE]
core_pmean <- ave(dat0$pre_mm, dat0$corename, FUN=function(x) mean(x, na.rm=TRUE))
dat <- data.frame(
  corename        = as.character(dat0$corename),
  age_calAD_mean  = dat0$age_calAD_mean,
  Tmean_abs_C     = dat0$Tmean_abs_C,
  Tmin_abs_C      = dat0$Tmin_abs_C,
  Tmean_anom_C    = dat0$Tmean_anom_C,
  Tmin_anom_C     = dat0$Tmin_anom_C,
  PAR_cm_yr       = dat0$PAR_cm_yr,
  CAR_app_g_m2_yr = dat0$CAR_app_g_m2_yr,
  dt_years        = dat0$dt_years,
  disturbance     = recode_disturbance(dat0$disturbance),
  Pmean_mm        = dat0$pre_mm,
  Panom_mm        = dat0$pre_mm - core_pmean,
  stringsAsFactors=FALSE
)

# ---------- Aggregate to core × bin (time-weighted) ----------
aggregate_width <- function(df, width){
  df$bin <- make_bin_label(df$age_calAD_mean, width)
  g <- interaction(df$corename, df$disturbance, df$bin, drop=TRUE)
  idx <- split(seq_len(nrow(df)), g)
  out <- lapply(idx, function(ii){
    ss <- df[ii, , drop=FALSE]
    data.frame(
      corename    = ss$corename[1],
      disturbance = ss$disturbance[1],
      bin         = ss$bin[1],
      PAR         = wmean(ss$PAR_cm_yr,       ss$dt_years),
      CAR         = wmean(ss$CAR_app_g_m2_yr, ss$dt_years),
      Tmean       = wmean(ss$Tmean_abs_C,     ss$dt_years),
      Tmin        = wmean(ss$Tmin_abs_C,      ss$dt_years),
      Tmean_anom  = wmean(ss$Tmean_anom_C,    ss$dt_years),
      Tmin_anom   = wmean(ss$Tmin_anom_C,     ss$dt_years),
      Pmean       = wmean(ss$Pmean_mm,        ss$dt_years),
      Panom       = wmean(ss$Panom_mm,        ss$dt_years),
      years       = sum(ss$dt_years, na.rm=TRUE),
      stringsAsFactors=FALSE)
  })
  do.call(rbind, out)
}
agg25 <- aggregate_width(dat, 25)
agg50 <- aggregate_width(dat, 50)
d <- if (WIDTH==25) agg25 else agg50
d <- d[!is.na(d$corename), , drop=FALSE]

# ---------- Build modeling frame ----------
y <- if (RESP=="PAR") d$PAR else d$CAR
if (PSET=="levels"){ x1<-d$Tmean; x1n<-"z_Tmean"; x2<-d$Tmin; x2n<-"z_Tmin"
} else { x1<-d$Tmean_anom; x1n<-"z_Tmean_anom"; x2<-d$Tmin_anom; x2n<-"z_Tmin_anom" }

dd <- data.frame(
  corename    = d$corename,
  disturbance = droplevels(d$disturbance),
  z_y         = zcol(y),
  stringsAsFactors=FALSE)
dd[[x1n]]    <- zcol(x1)
dd[[x2n]]    <- zcol(x2)
dd[["z_Pmean"]] <- zcol(d$Pmean)
dd[["z_Panom"]] <- zcol(d$Panom)

dd <- dd[is.finite(dd$z_y), , drop=FALSE]
# keep predictors with variance
use_x1 <- is.finite(sd(dd[[x1n]],na.rm=TRUE))    && sd(dd[[x1n]],na.rm=TRUE)    > 0
use_x2 <- is.finite(sd(dd[[x2n]],na.rm=TRUE))    && sd(dd[[x2n]],na.rm=TRUE)    > 0
use_pm <- is.finite(sd(dd$z_Pmean,na.rm=TRUE))   && sd(dd$z_Pmean,na.rm=TRUE)   > 0
use_pa <- is.finite(sd(dd$z_Panom,na.rm=TRUE))   && sd(dd$z_Panom,na.rm=TRUE)   > 0
use_ds <- is.factor(dd$disturbance) && nlevels(dd$disturbance) >= 2
preds <- c(if(use_x1) x1n, if(use_x2) x2n, if(use_pm) "z_Pmean", if(use_pa) "z_Panom", if(use_ds) "disturbance")
if (!length(preds)) stop("No usable predictors.")
keep <- is.finite(dd$z_y); for (nm in preds) keep <- keep & is.finite(dd[[nm]]); dd <- dd[keep,,drop=FALSE]

# ---------- Fit mixed model (or lm) ----------
tab_core <- table(dd$corename)
use_re   <- length(tab_core) >= 2 && any(tab_core >= 2)
rhs <- paste(preds, collapse=" + ")

if (use_re) {
  fml <- as.formula(paste("z_y ~", rhs, "+ (1|corename)"))
  fit0 <- try(lmer(fml, data=dd, REML=TRUE), silent=TRUE)
  if (inherits(fit0,"try-error")) { fit <- lm(as.formula(paste("z_y ~", rhs)), data=dd); model<-"lm"
  } else { fit <- lmerTest::as_lmerModLmerTest(fit0); model<-"lmer" }
} else { fit <- lm(as.formula(paste("z_y ~", rhs)), data=dd); model<-"lm" }

# ---------- Effects table ----------
if (model=="lmer"){
  sm <- summary(fit); co <- sm$coefficients
  est<-co[-1,"Estimate"]; se<-co[-1,"Std. Error"]
  eff <- data.frame(term=rownames(co)[-1], estimate=est,
                    conf.low=est-1.96*se, conf.high=est+1.96*se,
                    df=co[-1,"df"], p.value=co[-1,"Pr(>|t|)"], row.names=NULL)
} else {
  co <- summary(fit)$coefficients
  est<-co[-1,"Estimate"]; se<-co[-1,"Std. Error"]
  eff <- data.frame(term=rownames(co)[-1], estimate=est,
                    conf.low=est-1.96*se, conf.high=est+1.96*se,
                    df=NA_real_, p.value=co[-1,"Pr(>|t|)"], row.names=NULL)
}

# ---------- Type III ANOVA ----------
options(contrasts = c("contr.sum","contr.poly"))
if (inherits(fit,"lmerModLmerTest")) {
  cat("\n=== Type III ANOVA (Satterthwaite) ===\n"); print(anova(fit, type=3, ddf="Satterthwaite"))
} else {
  if (!requireNamespace("car", quietly=TRUE)) install.packages("car")
  cat("\n=== Type III ANOVA (lm) ===\n"); print(car::Anova(fit, type=3))
}
cat("\n=== Fixed effects ===\n"); print(eff, row.names=FALSE)

# ---------- Forest plot ----------
lab <- c("z_Tmean"="Mean temp (z)","z_Tmin"="Min temp (z)",
         "z_Tmean_anom"="Mean temp anom (z)","z_Tmin_anom"="Min temp anom (z)",
         "z_Pmean"="Precip mean (z)","z_Panom"="Precip anomaly (z)")
dist_term <- grep("^disturbance", eff$term, value=TRUE); if (length(dist_term)) lab[dist_term[1]] <- "Disturbance (degraded vs intact)"
eff$variable <- factor(ifelse(eff$term %in% names(lab), lab[eff$term], eff$term),
                       levels = rev(ifelse(eff$term %in% names(lab), lab[eff$term], eff$term)))
tag <- paste0(RESP, "_", WIDTH, "y_", PSET, "_withP")
p <- ggplot(eff, aes(estimate, variable)) +
  geom_vline(xintercept=0, linetype="dashed") +
  geom_errorbarh(aes(xmin=conf.low, xmax=conf.high), height=0) +
  geom_point(size=2) +
  labs(x="Standardized effect (β on z-response)", y=NULL,
       subtitle=paste0(RESP," — ",WIDTH,"y — ",PSET," — ", if (inherits(fit,"lmerModLmerTest")) "lmer" else "lm")) +
  theme_classic()
if (MAKE_PLOT) { print(p); ggsave(paste0("fx_forest_", tag, ".png"), p, width=7, height=4, dpi=300) }

# ---------- Diagnostics ----------
png(filename=paste0("diag_", tag, ".png"), width=1100, height=900, res=130)
op <- par(mfrow=c(2,2), mar=c(4,4,2,1))
rf <- residuals(fit); ff <- fitted(fit)
plot(ff, rf, pch=16, cex=.8, xlab="Fitted", ylab="Residuals", main="Residuals vs Fitted"); abline(h=0,lty=2,col="grey40")
qqnorm(rf, pch=16, cex=.7, main="Normal Q–Q (residuals)"); qqline(rf, col="grey40", lty=2)
hist(rf, breaks=30, main="Residuals histogram", xlab="Residual")
if (inherits(fit,"lmerModLmerTest") || inherits(fit,"lmerMod")) {
  re <- ranef(fit)$corename[,"(Intercept)"]; ord <- order(re)
  plot(re[ord], type="p", pch=16, xlab="Cores (sorted)", ylab="Random intercept", main="Core random intercepts"); abline(h=0,lty=2,col="grey40")
} else {
  plot(ff, sqrt(abs(rf)), pch=16, cex=.8, xlab="Fitted", ylab="√|Residuals|", main="Scale–Location (lm)")
}
par(op); dev.off()

# ---------- R² (Nakagawa) ----------
if (requireNamespace("performance", quietly = TRUE)) {
  r2 <- performance::r2(fit); print(r2)
} else {
  cat("\nTip: install.packages('performance') for marginal/conditional R².\n")
}

##+=============================================
##+=============================================
##+=============================================
#plots

## =========================
## CONFIG
## =========================
infile <- "AGE_DEPTH_with_Tanoms_Tabs_CAR_PAR_PRE.csv"  # has pre_mm
WIDTH  <- 50                                            # 50-year bins
save_png <- "panel_PAR_50y_temp_precip.png"

## =========================
## LIBS (no dplyr)
## =========================
if (!requireNamespace("ggplot2",  quietly=TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly=TRUE)) install.packages("gridExtra")
library(ggplot2); library(gridExtra)

## =========================
## HELPERS (base R)
## =========================
wmean <- function(x, w=NULL){
  if (is.null(w)) return(mean(x, na.rm=TRUE))
  ok <- is.finite(x) & is.finite(w); if (!any(ok)) return(NA_real_)
  sw <- sum(w[ok], na.rm=TRUE); if (sw <= 0) return(mean(x[ok], na.rm=TRUE))
  sum(x[ok]*w[ok], na.rm=TRUE) / sw
}
recode_disturbance <- function(v){
  x <- tolower(trimws(as.character(v)))
  out <- ifelse(is.na(x), NA,
         ifelse(grepl("conserv|intact|control|undist",x), "intact",
         ifelse(grepl("degrad|disturb|burn|graz|drain|ditch|alter|impact",x), "degraded", x)))
  factor(out, levels=c("intact","degraded"))
}
bin_label <- function(year, width){ s <- floor(year/width)*width; paste0(s, "\u2013", s+width-1L) }

## =========================
## READ + PREP (base R)
## =========================
stopifnot(file.exists(infile))
dat0 <- read.csv(infile, check.names=FALSE, stringsAsFactors=FALSE)

need <- c("corename","age_calAD_mean","Tmean_abs_C","Tmin_abs_C",
          "Tmean_anom_C","Tmin_anom_C","PAR_cm_yr","dt_years",
          "disturbance","pre_mm")
miss <- setdiff(need, names(dat0))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))

# clip to >=1900
dat0 <- dat0[is.finite(dat0$age_calAD_mean) & dat0$age_calAD_mean >= 1900, , drop=FALSE]

# precip anomaly per core (relative to core mean ≥1900)
core_pmean <- ave(dat0$pre_mm, dat0$corename, FUN=function(x) mean(x, na.rm=TRUE))
dat0$Panom_mm <- dat0$pre_mm - core_pmean

# disturbance recode + 50y bin
dat0$disturbance <- recode_disturbance(dat0$disturbance)
dat0$bin <- bin_label(dat0$age_calAD_mean, WIDTH)

## =========================
## AGGREGATE core × bin (base R)
## =========================
g <- interaction(dat0$corename, dat0$disturbance, dat0$bin, drop=TRUE)
idx <- split(seq_len(nrow(dat0)), g)

agg <- do.call(rbind, lapply(idx, function(ii){
  ss <- dat0[ii, , drop=FALSE]
  data.frame(
    corename    = ss$corename[1],
    disturbance = ss$disturbance[1],
    bin         = ss$bin[1],
    PAR         = wmean(ss$PAR_cm_yr,       ss$dt_years),
    Tmean       = wmean(ss$Tmean_abs_C,     ss$dt_years),
    Tmin        = wmean(ss$Tmin_abs_C,      ss$dt_years),
    Tmean_anom  = wmean(ss$Tmean_anom_C,    ss$dt_years),
    Tmin_anom   = wmean(ss$Tmin_anom_C,     ss$dt_years),
    Pmean       = wmean(ss$pre_mm,          ss$dt_years),
    Panom       = wmean(ss$Panom_mm,        ss$dt_years),
    n_layers    = length(ii),
    stringsAsFactors=FALSE
  )
}))
agg <- agg[is.finite(agg$PAR), , drop=FALSE]

## =========================
## PLOTTING
## =========================
okabe_ito <- c(intact="#0072B2", degraded="#D55E00")  # color-blind friendly

make_scatter <- function(df, xvar, xlab, show_legend = FALSE){
  keep <- is.finite(df[[xvar]]) & is.finite(df$PAR) & !is.na(df$disturbance)
  dd <- df[keep, , drop = FALSE]

  ggplot(dd, aes_string(x = xvar, y = "PAR",
                        color = "disturbance", size = "n_layers")) +
    geom_point(alpha = 0.9) +
    # keep ribbon out of the legend
    geom_smooth(method = "lm", se = TRUE, formula = y ~ x, show.legend = FALSE) +
    scale_color_manual(
      values = c(intact = "#0072B2", degraded = "#D55E00"),
      drop   = FALSE, name = NULL,
      guide  = guide_legend(override.aes = list(size = 2.2, alpha = 1))  # smaller color dots
    ) +
    scale_size_continuous(
      name = "Layers",
      range = c(1.5, 5),                                     # smaller points overall
      breaks = pretty(range(dd$n_layers, na.rm = TRUE), 3)   # 2–3 ticks
    ) +
    guides(fill = "none") +
    labs(x = xlab, y = "Peat accumulation (cm/yr)") +
    theme_classic() +
    theme(
      plot.title        = element_blank(),
      legend.position   = if (show_legend) c(.1, .5) else "none",
      legend.justification = c(0, 0),
      legend.text       = element_text(size = 7),             # smaller text
      legend.key.size   = unit(6, "pt"),                      # smaller keys
      legend.spacing    = unit(2, "pt"),
      legend.margin     = margin(1, 1, 1, 1),
      legend.background = element_rect(fill = scales::alpha("white", 0.55), color = NA),
      legend.key        = element_rect(fill = NA)
    )
}

p1 <- make_scatter(agg, "Tmean",      "Mean temperature (°C)")
p2 <- make_scatter(agg, "Tmean_anom", "Mean temperature anomaly (°C)",show_legend=TRUE)
p3 <- make_scatter(agg, "Tmin",       "Minimum temperature (°C)")
p4 <- make_scatter(agg, "Tmin_anom",  "Minimum temperature anomaly (°C)")
p5 <- make_scatter(agg, "Pmean",      "Precipitation (mm yr⁻¹)")
p6 <- make_scatter(agg, "Panom",      "Precipitation anomaly (mm yr⁻¹)")

# arrange in a 3x2 grid (no outer title)
grid <- gridExtra::grid.arrange(p1, p3, p5, p2, p4, p6, ncol=3)

# save
ggplot2::ggsave(save_png, grid, width=12, height=7.5, dpi=300)
cat("Saved panel to:", save_png, "\n")

#======================
#peat accum
## ====== CONFIG ======
infile   <- "AGE_DEPTH_with_Tanoms_Tabs_CAR_PAR_PRE.csv"
WIDTH    <- 50
out_png  <- "panel_CAR_50y_temp_precip.png"

## ====== LIBS (no dplyr) ======
if (!requireNamespace("ggplot2",  quietly=TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly=TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid",      quietly=TRUE)) install.packages("grid")
library(ggplot2); library(gridExtra); library(grid)

## ====== HELPERS (base R) ======
wmean <- function(x, w=NULL){
  if (is.null(w)) return(mean(x, na.rm=TRUE))
  ok <- is.finite(x) & is.finite(w); if (!any(ok)) return(NA_real_)
  sw <- sum(w[ok], na.rm=TRUE); if (sw <= 0) return(mean(x[ok], na.rm=TRUE))
  sum(x[ok]*w[ok], na.rm=TRUE)/sw
}
recode_disturbance <- function(v){
  x <- tolower(trimws(as.character(v)))
  out <- ifelse(is.na(x), NA,
         ifelse(grepl("conserv|intact|control|undist",x),"intact",
         ifelse(grepl("degrad|disturb|burn|graz|drain|ditch|alter|impact",x),"degraded",x)))
  factor(out, levels=c("intact","degraded"))
}
bin_label <- function(year, width){ s <- floor(year/width)*width; paste0(s, "\u2013", s+width-1L) }

## ====== READ + PREP (base R) ======
stopifnot(file.exists(infile))
dat0 <- read.csv(infile, check.names=FALSE, stringsAsFactors=FALSE)

need <- c("corename","age_calAD_mean","dt_years","disturbance","pre_mm",
          "Tmean_abs_C","Tmin_abs_C","Tmean_anom_C","Tmin_anom_C",
          "PAR_cm_yr","CAR_app_g_m2_yr")
miss <- setdiff(need, names(dat0))
if (length(miss)) stop("Missing columns: ", paste(miss, collapse=", "))

# clip to >=1900
dat0 <- dat0[is.finite(dat0$age_calAD_mean) & dat0$age_calAD_mean >= 1900, , drop=FALSE]

# precip anomaly per core (relative to core’s >=1900 mean)
core_pmean <- ave(dat0$pre_mm, dat0$corename, FUN=function(x) mean(x, na.rm=TRUE))
dat0$Panom_mm <- dat0$pre_mm - core_pmean

# disturbance + bin
dat0$disturbance <- recode_disturbance(dat0$disturbance)
dat0$bin <- bin_label(dat0$age_calAD_mean, WIDTH)

## ====== AGGREGATE core×bin (time-weighted) ======
g <- interaction(dat0$corename, dat0$disturbance, dat0$bin, drop=TRUE)
idx <- split(seq_len(nrow(dat0)), g)

agg <- do.call(rbind, lapply(idx, function(ii){
  ss <- dat0[ii, , drop=FALSE]
  data.frame(
    corename    = ss$corename[1],
    disturbance = ss$disturbance[1],
    bin         = ss$bin[1],
    CAR         = wmean(ss$CAR_app_g_m2_yr, ss$dt_years),
    Tmean       = wmean(ss$Tmean_abs_C,     ss$dt_years),
    Tmin        = wmean(ss$Tmin_abs_C,      ss$dt_years),
    Tmean_anom  = wmean(ss$Tmean_anom_C,    ss$dt_years),
    Tmin_anom   = wmean(ss$Tmin_anom_C,     ss$dt_years),
    Pmean       = wmean(ss$pre_mm,          ss$dt_years),
    Panom       = wmean(ss$Panom_mm,        ss$dt_years),
    n_layers    = length(ii),
    stringsAsFactors=FALSE
  )
}))
agg <- agg[is.finite(agg$CAR), , drop=FALSE]

## ====== PLOTTING ======
okabe_ito <- c(intact="#0072B2", degraded="#D55E00")  # color-blind friendly

make_scatter <- function(df, xvar, xlab, show_legend=FALSE){
  keep <- is.finite(df[[xvar]]) & is.finite(df$CAR) & !is.na(df$disturbance)
  dd <- df[keep, , drop=FALSE]

  ggplot(dd, aes_string(x=xvar, y="CAR",
                        color="disturbance", size="n_layers")) +
    geom_point(alpha=0.9) +
    geom_smooth(method="lm", se=TRUE, formula = y ~ x, show.legend = FALSE) +
    scale_color_manual(values = okabe_ito, drop=FALSE, name=NULL,
                       guide = guide_legend(override.aes = list(size = 2.2, alpha = 1))) +
    scale_size_continuous(name = "Layers", range = c(1.5, 5),
                          breaks = pretty(range(dd$n_layers, na.rm=TRUE), 3)) +
    guides(fill = "none") +
    labs(x = xlab, y = expression("Carbon accumulation (g m"^{-2}*" yr"^{-1}*")")) +
    theme_classic() +
    theme(
      plot.title        = element_blank(),
      legend.position   = if (show_legend) c(.1, .4) else "none",
      legend.justification = c(0, 0),
      legend.text       = element_text(size = 7),
      legend.key.size   = unit(6, "pt"),
      legend.spacing    = unit(2, "pt"),
      legend.margin     = margin(1, 1, 1, 1),
      legend.background = element_rect(fill = grDevices::adjustcolor("white", alpha.f = 0.55),
                                       color = NA),
      legend.key        = element_rect(fill = NA)
    )
}

p1 <- make_scatter(agg, "Tmean",      "Mean temperature (°C)")
p2 <- make_scatter(agg, "Tmean_anom", "Mean temperature anomaly (°C)",       show_legend=TRUE)
p3 <- make_scatter(agg, "Tmin",       "Minimum temperature (°C)")
p4 <- make_scatter(agg, "Tmin_anom",  "Minimum temperature anomaly (°C)")
p5 <- make_scatter(agg, "Pmean", expression(Precipitation~"(mm"~yr^{-1}*")"))
p6 <- make_scatter(agg, "Panom", expression(Precipitation~"anomaly (mm"~yr^{-1}*")"))


grid <- gridExtra::grid.arrange(p1, p3, p5, p2, p4, p6, ncol = 3)
ggsave(out_png, grid, width = 12, height = 7.5, dpi = 300)
cat("Saved:", out_png, "\n")


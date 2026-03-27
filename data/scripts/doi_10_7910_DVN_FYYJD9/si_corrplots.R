### Correlation plot helper functions
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut]
  )
}

p_format <- function(x, ndp = 3)
{
  out <-
    format(
      round(as.numeric(x), ndp),
      ns = ndp,
      scientific = F,
      just = "none"
    )
  ifelse(out == "0.05", "<0.05", out)
}

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
#http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
corstars <-
  function(x,
           method = c("pearson", "spearman"),
           removeTriangle = c("upper", "lower"),
           result = c("none", "html", "latex")) {
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix <- rcorr(x, type = method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value
    
    ## Define notions for significance levels; spacing is important.
    mystars <-
      ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(
      x
    )), R), 2))[, -1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep = ""), ncol = ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep = "")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep = "")
    
    ## remove upper triangle of correlation matrix
    if (removeTriangle[1] == "upper") {
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if (removeTriangle[1] == "lower") {
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew) - 1])
    if (result[1] == "none")
      return(Rnew)
    else{
      if (result[1] == "html")
        print(xtable(Rnew), type = "html")
      else
        print(xtable(Rnew), type = "latex")
    }
  }

#### ------- Lucid correlation matrix plot ------ ####
cor_matrix <- 
  lucid_survey %>%
  select(
    x_pid_7n,
    x_ideo_7n,
    vnat_1_n,
    vnat_2_n,
    vnat_3_n,
    vnat_4_n,
    vnat_5_n,
    vnat_binary_n,
    covid_vaxed_binary_n,
    altruism_willing_n,
    altruism_donate_n,
    recip_pos_gift_n,
    national_n,
    cosmop_n,
  ) %>%
  rename(vnat_binary = vnat_binary_n,
         vax_nat_1 = vnat_1_n,
         vax_nat_2 = vnat_2_n,
         vax_nat_3 = vnat_3_n,
         vax_nat_4 = vnat_4_n,
         vax_nat_5 = vnat_5_n,
         national = national_n,
         cosmop = cosmop_n,
         altruism_willing = altruism_willing_n,
         altruism_donate = altruism_donate_n,
         recip_gift = recip_pos_gift_n,
         x_pid_7 = x_pid_7n, 
         x_ideo_7 = x_ideo_7n,
         covid_vaxed_binary = covid_vaxed_binary_n
         ) %>% 
  na.omit() %>% 
  as.matrix() %>%
  rcorr() 

png(
  fil = "lucid_corrplot.png",
  height = 1500,
  width = 1500,
  res = 150
)

cor.plot <- 
  corrplot(
  cor_matrix$r,
  type = "upper",
  order = "FPC",
  number.cex = 0.25,
  insig = "blank",
  tl.col = "black",
  method ="color"
)
dev.off()

#### ------- NORC correlation matrix plot ------ ####
cor_matrix <- 
  norc_survey %>%
  select(
    x_vaxnat_n,
    x_patnat1_n,
    x_patnat2_n,
    x_altruism_n,
    x_pid_7n,
    x_ideo_7n
  ) %>%
  rename(partisanship = x_pid_7n,
         ideology = x_ideo_7n,
         altruism = x_altruism_n,
         vax_nationalism = x_vaxnat_n,
         nationalism = x_patnat1_n,
         patriotism = x_patnat2_n
  ) %>% 
  na.omit() %>% 
  as.matrix() %>%
  rcorr() 

png(
  file = "norc_corrplot.png",
  height = 1000,
  width = 1000,
  res = 150
)

cor.plot <- 
  corrplot(
    cor_matrix$r,
    type = "upper",
    order = "FPC",
    number.cex = 0.25,
    insig = "blank",
    tl.col = "black",
    method ="color"
  )

dev.off()



#0) Prep steps
    #a) load libraries
        library(data.table)
        library(fst)
        library(dplyr,  warn.conflicts = FALSE)
        library(Hmisc)
        library(lme4)
        library(survey)
        library(parameters)
        library(lmerTest)
        library(tidyr)
        library(ggpubr)
        library(cowplot)
        library(writexl)
        library(MASS)
        library(AER)
        library(multiwayvcov)
        library(lmtest)
        library(sandwich)
        library(ggthemes)
        library(metafolio)
        

    #b) make directories
        fig.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures"
        data.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Data"


#1) Read data
    df <- read_fst(file.path(data.dir, "p2p_covid_clean.fst"), as.data.table = T)
    summary(df$dv_worry_home_insecure)

    
#2) Select DVs
    dvs <- grep("dv", names(df), value = T)
    bin_vars <- names(df)[df[, lapply(.SD, function(x) length(unique(x)))] == 2]
    bin_dvs <- dvs[dvs %in% bin_vars]
    cat_dvs <- dvs[!dvs %in% bin_vars]
    bin_dvs <- "dv_fired"
    cat_dvs <- setdiff(cat_dvs, c("dv_worry_hosp_overwhelm", "Get_Health\nAdvice", "dv_worry_finances_ord", "dv_worry_food_insecure_ord", "dv_worry_home_insecure_ord"))
    cat_dvs <- cat_dvs[!grepl("likey", cat_dvs)]
    
#3) Omit missings for key variables
    # newvars <- c("pre_emp_full_time", "pre_emp_part_time", "pre_self_employed", "pre_food_insecure", "pre_home_insecure", "pre_child_number", "pre_occ_farming", "pre_occ_automotive", "pre_occ_construction", "pre_occ_manufacturing", "pre_occ_railroad", "pre_occ_forestry", "pre_occ_electrical", "pre_occ_beauty", "pre_occ_fire")
    keep_vars <- c(bin_dvs, cat_dvs, "pre_food_insecure", "pre_unemployed", "cv_age", "cv_sex_female", "iv_race_black", "iv_race_other", "iv_race_latino", "cv_educ_lths", "cv_educ_hs", "cv_educ_some_college")
    keeps <- !apply(df[, keep_vars, with = F], 1, function(x) any(is.na(x)))
    df <- df[keeps]

# df$zip.pop.dens_dum <- as.numeric(df$zip.pop.dens < 100)
# df$acs.pop.dens_dum <- as.numeric(df$acs_pop_dens < 100)
# df$acs_pop_dens <- log(1 + df$acs_pop_dens)
# df$zip.pop.dens <- log(1 + df$zip.pop.dens)
# df$zip.Urbanized_Area
# df$zip.Urban_Cluster
# df$zip.Rural
# df$Rural
# df$acs_rural

#4) Prep formulas
    f.m1 <- formula(tvar ~ 
                      cv_age + cv_sex_female + 
                      iv_race_black + iv_race_other + iv_race_latino +
                      cv_educ_lths + cv_educ_hs + cv_educ_some_college +
                      pre_unemployed + pre_food_insecure_dum + Rural
                    
    )
    # f.m2 <- update.formula(f.m1, ~ . + Rural)
    # f.m3 <- update.formula(f.m1, ~ . + zip.Rural)
    # f.m4 <- update.formula(f.m1, ~ . + zip.Rural + zip.Urban_Cluster)
    # f.m5 <- update.formula(f.m1, ~ . + acs_pop_dens)
    # f.m6 <- update.formula(f.m1, ~ . + zip.pop.dens)
    # f.m7 <- update.formula(f.m1, ~ . + acs.pop.dens_dum)
    # f.m8 <- update.formula(f.m1, ~ . + zip.pop.dens_dum)
    # f.m9 <- update.formula(f.m1, ~ . + pre_unemployed + pre_food_insecure_dum)
    l.forms <- list(f.m1)
    # l.forms <- list(f.m1, f.m2, f.m3, f.m4, f.m5, f.m6, f.m7, f.m8)

    
#5) Run models
    #a) Prep weighted dataset
        sdf <- as.data.frame(df)
        sdf$strata <- 1
        sdf <- rescale_weights(sdf, "strata", "weight_state")
    #b) Prep empty lists
        l.mod <- list()
        l.R2_insample <- list()
        l.R2_outsample <- list()
        l.chi2 <- list()
        l.mod2 <- list()
        l.R2_insample2 <- list()
        l.R2_outsample2 <- list()
        l.chi22 <- list() 
    #c) run bivariate models
        for(i in 1:length(bin_dvs)){
          sdf$tvar <- sdf[, bin_dvs[i]]
          t.mod <- list()
          t.R2_insample <- list()
          t.R2_outsample <- list()
          t.chi2 <- list()
          for(j in 1:length(l.forms)){
            mod <- glm(l.forms[[j]], data = sdf, weights = pweights_a, family = binomial)
            mod_null <- glm(tvar~1, data = sdf, weights = pweights_a, family = binomial)
            R2_insample = 1 - mod$deviance / mod$null.deviance # works for glm
            R2_outsample = 1- logLik(mod)/logLik(mod_null)
            chi2 <- pchisq(deviance(mod_null)-deviance(mod),
                           df.residual(mod_null)-df.residual(mod),
                           lower.tail=FALSE)
            vcov <- sandwich::vcovCL(mod, sdf$state_county_name)
            tmp <- coeftest(mod, vcov)
            
            t.mod[[j]] <- tmp
            t.R2_insample[[j]] <- R2_insample
            t.R2_outsample[[j]] <- R2_outsample
            t.chi2[[j]] <- chi2
          }
          df$state_county_name
          l.mod[[i]] <- t.mod
          l.R2_insample[[i]] <- t.R2_insample
          l.R2_outsample[[i]] <- t.R2_outsample
          l.chi2[[i]] <- t.chi2
          #warning results from non-integer weights. Not a real problem
        } 
    #d) run categorical models
        for(i in 1:length(cat_dvs)){
          sdf$tvar <- sdf[, cat_dvs[i]]
          t.mod <- list()
          t.R2_insample <- list()
          t.R2_outsample <- list()
          t.chi2 <- list()
          for(j in 1:length(l.forms)){
            mod <- polr(l.forms[[j]], data = sdf, weights = pweights_a, Hess = T)
            mod_null <- polr(tvar~1, data = sdf, weights = pweights_a, Hess = T)
            R2_insample = 1 - mod$deviance / mod$null.deviance # works for glm
            R2_outsample = 1- logLik(mod)/logLik(mod_null)
            chi2 <- pchisq(deviance(mod_null)-deviance(mod),
                           df.residual(mod_null)-df.residual(mod),
                           lower.tail=FALSE)
            
            vcov <- sandwich::vcovCL(mod, sdf$state_county_name)
            tmp <- coeftest(mod, vcov)
            t.mod[[j]] <- tmp
            t.R2_insample[[j]] <- R2_insample
            t.R2_outsample[[j]] <- R2_outsample
            t.chi2[[j]] <- chi2
          } 
          l.mod2[[i]] <- t.mod
          l.R2_insample2[[i]] <- t.R2_insample
          l.R2_outsample2[[i]] <- t.R2_outsample
          l.chi22[[i]] <- t.chi2
          #warning results from non-integer weights. Not a real problem
        }
    #e) append results
        l.mod <- c(l.mod, l.mod2)
        l.R2_insample <- c(l.R2_insample, l.R2_insample2)
        l.R2_outsample <- c(l.R2_outsample, l.R2_outsample2)
        l.chi2 <- c(l.chi2, l.chi22)
    
        
#6) Prettify models
    prstars<-function(x){
      ifelse(x<.001,"***",ifelse(x<.01,"**",ifelse(x<.05,"*","")))
    }
    pretty.mod.fun<-function(model){
      model.coef <- model[]
      model.format <- data.frame(model.coef)
      varnames <- row.names(model.format)
      model.format <- data.table(model.format)
      setnames(model.format, old = grep("^Pr", names(model.format), value = T), new = "Pr")
      setnames(model.format, old = grep("^Std", names(model.format), value = T), new = "se")
      model.format <- model.format[,.(
        var = varnames,
        coef = sapply(Estimate,function(x) sprintf("%.2f", x)),
        se = sapply(se,function(x) paste("(",sprintf("%.2f", x),")",sep="")),
        pval = prstars(Pr)
      ) ]
      return(model.format)
    }
    l.pr.mod <- lapply(l.mod, lapply, pretty.mod.fun)

    
#7) Append each dv to one table and slightly format
    for(i in 1:length(l.pr.mod)){
        tdf <- l.pr.mod[[i]]
        for(j in 1:length(tdf)){
            names(tdf[[j]])[2:ncol(tdf[[j]])] <- paste(names(tdf[[j]])[2:ncol(tdf[[j]])], j, sep = "")
        }
        tdf <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "var", sort = F), tdf)
        tdf[is.na(tdf)] <- ""
        l.pr.mod[[i]] <- tdf
    }

    
#8) Pull in correct var names and order
    t.order <- fread("C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Data/model var order.csv", header = F)
    names(t.order) <- c("Variable", "var")
    for(i in 1:length(l.pr.mod)){
      tdf <- l.pr.mod[[i]]
      tmp <- as.data.frame(tdf[1,][, lapply(.SD, function(x) "")])
      tdf <- rbind(tdf, tmp)
      tdf <- merge(t.order, tdf, by = "var", all = T, sort = F)
      tdf <- tdf[,lapply(.SD, function(x) ifelse(is.na(x), "", x))]
      crit1 <- tdf$var == ""
      crit2 <- !apply(tdf[, 3:ncol(tdf)], 1, function(x) all(x == ""))
      tdf <- tdf[crit1 | crit2]
      tdf$var <- NULL
      tdf <- tdf[!Variable %in% c("Marital Status", "County-Level", "W1 Employment", "W1 Home Insecure", "W1 Food Insecure", "W1 Occupaion") ]
      #pull in extra stats
          tmp <- as.data.frame(tdf[1,][, lapply(.SD, function(x) "")])
          tmp[1] <- "In Sample R2" 
          tmp[2] <- round(unlist(l.R2_insample[[i]]), 2)[1] 
          tdf <- rbind(tdf, tmp)
      
          tmp <- as.data.frame(tdf[1,][, lapply(.SD, function(x) "")])
          tmp[1] <- "Out Sample R2" 
          tmp[2] <- round(unlist(l.R2_outsample[[i]]), 2)[1] 
          tdf <- rbind(tdf, tmp)
          
          tmp <- as.data.frame(tdf[1,][, lapply(.SD, function(x) "")])
          tmp[1] <- "Chi2" 
          tmp[2] <- round(unlist(l.chi2[[i]]), 2)[1] 
          tdf <- rbind(tdf, tmp)
      l.pr.mod[[i]] <- tdf
    }

    
#9) Save results
    #a) get sheet names
        dv.index <- data.table(old = c(bin_dvs, cat_dvs), 
                               new = c("Fired",
                                       "Worried about Home Insecurity",
                                       "Worried about Food Insecurity",
                                       "Worried about Finances",
                                       "Likely to Ask Friends for Services",
                                       "Likely to Ask Friends for Loans",
                                       "Likely to Ask Friends for Food"
                                       )
        )
        clean_bin_dvs <- dv.index$new[match(bin_dvs, dv.index$old)]
        clean_cat_dvs <- dv.index$new[match(cat_dvs, dv.index$old)]
    #b) save
        names(l.pr.mod) <- c(clean_bin_dvs, clean_cat_dvs)[1:4]
        write_xlsx(l.pr.mod, turl <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/figures/raw_models.xlsx")
        # browseURL(turl)
    # return(pt2)

        
#10) Prep functions for graphing results
    #a) function for rounding based on base to round by
        mround  <-  function(x, base){
          base*round(x/base)
        }
    #b) function to set ggplot axis breaks based on input data
        five_breaks  <-  function(...){
            function(x){
              #get real limits
                  lim <- round(c(min(x)/1.1, max(x)/1.1)*100)/100
              #set limit to be simpler number if limits not equal
                  if(lim[1]!=lim[2]){
                    if(nchar(abs(lim[1]))<nchar(lim[2])){
                      lim[2] <- abs(lim[1])
                    }else{
                      lim[1] <- (-lim[2])
                    }
                  }
              #determine 5 breaks with one break at 0
                  br <- sort(c(lim, lim/2, 0))
                  br
              #if can't be broken into nice units, break into 7
                  # if(!br[4] %in% c(.01, .025, .05, .1, .25, .5, 1, 1.25, 2.5))
                  # br <- c(lim[1], lim[1]-((lim[1]/3)*1:6))
                  # round(br, 3)
            }
        }
    #c) function to label axis breaks with correct number of digits
        labsfun <- function(x){
            # if(nchar(x[2])>5 | nchar(x[1])>5){
              paste0(sprintf("%.1f", x))
            # }else if(nchar(x[2])>4 | nchar(x[1])>4){
            #   paste0(sprintf("%.2f", x))
            # }else{
            #   paste0(sprintf("%.1f", x))
            # }
          }


#11) Prep graph dataframe
    #a) convert to dataframe
        l.df <- copy(l.pr.mod)
        for(i in 1:length(l.df)){
          l.df[[i]]$dv <- names(l.df)[i]
        }
        df <- rbindlist(l.df)

    #b) create confidence and significance
        df[, est := as.numeric(coef1)]
        df[, CIupper := 1.96 * as.numeric(gsub("\\(|\\)", "", se1)) + est]
        df[, CIlower := 1.96 * -as.numeric(gsub("\\(|\\)", "", se1)) + est]
        df[, significant := ifelse(pval1 == "", "grey", "black")]

    #c) format variables
        df[, var := Variable]
        df[var == "Constant", var := "Threshold=1"]
        df[var == "County-Level", var := "Intercepts"]
        df <- df[!var %in% c("Intercepts","Threshold=1","Threshold=2","Threshold=3")]
        df[!var %in% c("Age", "Female","Race", "Education", "Rural", "Prior to COVID-19", "Intercepts"), var := paste("    ", var, sep = "")]
        df[, var := factor(var, levels = rev(unique(var)))]


    #d) format DVs
        df[grepl("Worried about", dv), dv := paste(dv, "Worry")]
        df[grepl("Likely to Ask", dv), dv := paste(dv, "Support")]
        df[, dv := gsub("Worried about |Likely to Ask Friends for | Insecurity", "", dv)]
        df[, dv := gsub("Services", "Service", dv)]
        df[, dv := gsub("Loans", "Loan", dv)]
        df[, dv := gsub("Services", "Service", dv)]
        df[, dv := gsub("Finances", "Finance", dv)]
        df[, dv := gsub("Support", "Help", dv)]

        df[dv == "Fired", dv := "Fired/\nUnemployed"]
        df[dv == "Home Worry", dv := "Housing\nInsecurity"]
        df[dv == "Food Worry", dv := "Food\nInsecurity"]
        df[dv == "Finance Worry", dv := "Financial\nInsecurity"]
        df[dv == "Service Help", dv := "Get Help -\nServices"]
        df[dv == "Loan Help", dv := "Get Help -\nFinances"]
        df[dv == "Food Help", dv := "Get Help -\nFood"]

        df[, dv := factor(dv, levels = c(as.character(unique(dv)[-1]), as.character(unique(dv)[1])))]


    #e) set limits for each y axis
        dft <- copy(df)
        dft[, y_min := -max(c(abs(CIupper), abs(CIlower)), na.rm = T), by = dv]
        dft$y_min <- ceiling(dft$y_min*22)/20
        dft$y_max <- (-dft$y_min)
        dft <- dft[!Variable %in% c("In Sample R2", "Out Sample R2", "Chi2", "W1 Employment", "W1 Home Insecure", "W1 Food Insecure", "W1 Occupation")]

        dft$dv
        dft$significant
        df.colors <- data.table(color = gg_color_hue(4), dv = unique(dft$dv))
        dft <- merge(df.colors, dft, by = "dv")
        dft$significant <- ifelse(dft$significant == "grey", "grey", dft$color)


#12) Plot
    p <- ggplot(dft, aes(y=est, x=var))+
      coord_flip(ylim = c(-2, 2))+
      geom_linerange(aes(ymin=CIlower, ymax=CIupper), size=.7, color=dft$significant) +
      facet_wrap(~dv, scales = "free_x", nrow = 3)+
      theme_minimal() +
      geom_hline(yintercept = 0)+
      geom_point(size=1.5, color=dft$significant) +
      labs(x="", y = "\n Parameter Estimate (log odds)")+
      # ggtitle("Figure 2. Parameter Estimates by Dependent Variable \n")+
      theme(panel.background = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            text=element_text(family="serif"),
            title = element_text(size=10, face="bold"),
            plot.title = element_text(hjust = .5),
            axis.title.x = element_text(size=11, face="bold"),
            strip.text.x = element_text(face = "bold", size = 10, margin = margin(0.05, 0.05, .2, 0.05, "cm")),
            axis.text.x = element_text(size=9.4),
            # axis.text.y = element_text(size=9),
            axis.text.y = element_text(size=9.5, hjust = 0, face = ifelse(rev(dft$var) %in% c("Age","Female","Race", "Education", "Prior to COVID-19", "Rural", "Intercepts"), "bold", "plain")),
            panel.spacing = unit(1.5, "lines"))+
      geom_blank(aes(y = y_min))+
      geom_blank(aes(y = y_max))+
      scale_y_continuous(labels = labsfun)

    f.name <- "C:/Users/admin/Desktop/Figure 2 - Parameter Estimates.png"
    ggsave(f.name, p, width = 8.5*.5, height = 12*.5, units = "in", dpi = 500)
    browseURL(f.name)


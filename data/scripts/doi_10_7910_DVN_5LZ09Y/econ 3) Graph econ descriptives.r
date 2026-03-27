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
        library(weights)
        library(metafolio)
        
    #b) make directories
        weighted.fig.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures/weighted_associations"
        unweighted.fig.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures/unweighted_associations"
        fig.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Figures"
        data.dir <- "C:/Users/admin/Desktop/IDrive-Sync/Analysis - Code/Postdoc/COVID/Data"
        
        
#1) Read data
    df <- fread(file.path(data.dir, "p2p_covid.csv"))
    
    
#2) group variables by type
    dvs <- grep("dv", names(df), value = T)
    bin_vars <- names(df)[df[, lapply(.SD, function(x) length(unique(x)))] == 2]
    bin_dvs <- dvs[dvs %in% bin_vars]
    cat_dvs <- dvs[!dvs %in% bin_vars]
    bin_dvs <- "dv_fired"
    cat_dvs <- cat_dvs[-1]
    
    #b) create dummy version of categorical vars
        newdummies <- paste(cat_dvs, "dum", sep = "")
        df[, (newdummies) := lapply(.SD, function(x) ifelse(x %in% c("Agree", "Strongly agree"), 1, 0)) ,.SDcol = cat_dvs] 
        bin_dvs <- c(bin_dvs, newdummies)
        bin_dvs <- setdiff(bin_dvs, "Get_Health\nAdvicedum")
        
        
#3) Rename variables
    #a) dependent variables and indices
        dv.index <- data.table(old = c(bin_dvs), 
                               new = c("Fired",
                                       "Worried about Home Insecurity",
                                       "Worried about Food Insecurity",
                                       "Worried about Finances",
                                       "Likely to Ask Friends for Services",
                                       "Likely to Ask Friends for Loans",
                                       "Likely to Ask Friends for Food"
                                       )
                                 )
        dvs <- dv.index$new
        setnames(df, old = dv.index$old, new = dv.index$new)
        bin_dvs <- dv.index$new[match(bin_dvs, dv.index$old)]
        cat_dvs <- dv.index$new[match(cat_dvs, dv.index$old)]
        
    #b) independent variables
        df[, race := ifelse(race == "white", "White",
                     ifelse(race == "black", "Black",
                     ifelse(race == "latino", "Latino", "Other"
        )))]
        df[, race := factor(race, levels = c("Black", "Latino", "Other", "White"))]
        setnames(df, "race", "Race")
        df[, Fired := Fired + dv_unable_find_work]
        df[Fired > 1, Fired := 1]
        df[, Education :=
             factor(
               ifelse(cv_educ_bachelors == 1, "Bachelors",
                      ifelse(cv_educ_some_college == 1, "Some College",
                             ifelse(cv_educ_hs == 1, "High School", "Less than HS"))),
               levels = c("Less than HS", "High School", "Some College", "Bachelors")
             )
           ]


#4) Prep and draw graphs
    #a) Race
        #i) data prep
            Variable = "Race"
            title = "Racial Disparities in Negative Economic Impact of COVID-19"
            title.size = 18
            tdf <- copy(df)
            tdf[, tvar := .SD, .SDcols = Variable]
            tdf <- tdf[, c(bin_dvs, "tvar", "weight_state"), with = F]
            
        #ii) get significance
            dvs <- names(tdf)[1:7]
            races <- as.character(unique(tdf$tvar)[2:4])
            dv.df <- expand.grid(dvs, races)
            names(dv.df) <- c("Variable", "tvar")
            dv.df$sig <- 0
            for(i in 1:nrow(dv.df)){
                dv.df$sig[i] <- wtd.t.test(x = unlist(tdf[tvar == dv.df$tvar[i], dv.df$Variable[i], with =F]),
                                  y = unlist(tdf[tvar == "White",  dv.df$Variable[i], with = F]), 
                                  weight = unlist(tdf[tvar == dv.df$tvar[i], "weight_state"]), 
                                  weighty = unlist(tdf[tvar == "White", "weight_state"]),
                                  samedata = F)$coefficients[3]
            }
            prstars<-function(x){
                ifelse(x<.001,"***",ifelse(x<.01,"**",ifelse(x<.05,"*","")))
            }
            dv.df$sig <- prstars(dv.df$sig)
            
        #iii) get means    
            tdf <- tdf[, lapply(.SD, function(x) wtd.mean(x, weight_state)), by = "tvar"]
            
        #iv) format for graph
            tdf <- data.table(gather(tdf, key = "Variable", value = "Percentage", -tvar))
            tdf <- tdf[Variable %in% bin_dvs]
            tdf <- merge(tdf, dv.df, all.x = T, by = c("tvar", "Variable"), sort = F)
            tdf[is.na(sig), sig := ""]
            tdf[grepl("Worried about", Variable), Variable := paste(Variable, "Worry")]
            tdf[grepl("Likely to Ask", Variable), Variable := paste(Variable, "Support")]
            tdf[, Variable := gsub("Worried about |Likely to Ask Friends for | Insecurity", "", Variable)]
            tdf[, Variable := gsub("Services", "Service", Variable)]
            tdf[, Variable := gsub("Loans", "Loan", Variable)]
            tdf[, Variable := gsub("Services", "Service", Variable)]
            tdf[, Variable := gsub("Finances", "Finance", Variable)]
            tdf[, Variable := gsub("Support", "Help", Variable)]
            tdf[Variable == "Fired", Variable := "Fired/\nUnemployed"]
            tdf[Variable == "Home Worry", Variable := "Housing\nInsecurity"]
            tdf[Variable == "Food Worry", Variable := "Food\nInsecurity"]
            tdf[Variable == "Finance Worry", Variable := "Financial\nInsecurity"]
            tdf[Variable == "Service Help", Variable := "Get Help -\nServices"]
            tdf[Variable == "Loan Help", Variable := "Get Help -\nFinances"]
            tdf[Variable == "Food Help", Variable := "Get Help -\nFood"]
            tdf[, Variable := factor(Variable, levels = c(as.character(unique(tdf$Variable)[-1]), as.character(unique(tdf$Variable)[1])))]
            
        #iv) print csv of numbers for Brea
            df.tmp <- copy(tdf)
            df.tmp$Variable <- gsub("\n", " ", df.tmp$Variable)
            setnames(df.tmp, "tvar", "Race")
            fwrite(df.tmp, file.path(fig.dir, "econ_race_stats.csv"))
            
        #v) get rid of get help vars    
            tdf <- tdf[!grepl("Get Help", tdf$Variable)]

        #vi) graph
            p1 <- ggplot(tdf, aes(x = tvar, y = Percentage, color = tvar)) + 
              coord_cartesian(ylim = c(0, .9)) +
              geom_linerange(
              aes(ymin = -.2, ymax = Percentage), 
              color = "lightgray", size = 2
            )+
              geom_point(size = 5)+
              geom_text(aes(label=sig), position=position_dodge(width=0.9), vjust=-0.5, size = 7)+
              facet_wrap(~Variable, scales = "free_x", ncol = 7) +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent_format(accuracy=1))+
              theme(
                panel.grid.major = element_line(colour = "grey85"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                text = element_text(family = "serif", size = 18),
                strip.text.x = element_text(size = 14, face = "bold"),
                axis.line = element_line(colour = "black"),
                legend.position="none",
                panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                plot.title = element_text(size=19, hjust =.5, margin = margin(b = 20)),
                axis.text.x = element_text(size=13.5, angle = 45, hjust = 1),
                plot.margin = unit(c(0,0,-1,0), "cm"),
                axis.text.y = element_text(size=13),
                axis.title.x= element_text(size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
                axis.title.y= element_text(size=20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
              labs(x = "", y = "") +
              scale_x_discrete(expand = c(.2, 0))
            
    #b) Education
        #i) data prep
            Variable = "Education"
            title = "Education Disparities in Negative Economic Impact of COVID-19"
            title.size = 18
            tdf <- copy(df)
            tdf[, tvar := .SD, .SDcols = Variable]
            # df <- df[cv_age < 65, ]
            tdf <- tdf[, c(bin_dvs, "tvar", "weight_state"), with = F]
            
        #ii) get significance
            dvs <- names(tdf)[1:7]
            races <- as.character(unique(tdf$tvar)[1:3])
            dv.df <- expand.grid(dvs, races)
            names(dv.df) <- c("Variable", "tvar")
            dv.df$sig <- 0
            for(i in 1:nrow(dv.df)){
                dv.df$sig[i] <- wtd.t.test(x = unlist(tdf[tvar == dv.df$tvar[i], dv.df$Variable[i], with =F]),
                                  y = unlist(tdf[tvar == "Bachelors",  dv.df$Variable[i], with = F]),
                                  weight = unlist(tdf[tvar == dv.df$tvar[i], "weight_state"]),
                                  weighty = unlist(tdf[tvar == "Bachelors", "weight_state"]),
                                  samedata = F)$coefficients[3]
            }
            prstars<-function(x){
                ifelse(x<.001,"***",ifelse(x<.01,"**",ifelse(x<.05,"*","")))
            }
            dv.df$sig <- prstars(dv.df$sig)
    
        #iii) get means
            tdf <- tdf[, lapply(.SD, function(x) wtd.mean(x, weight_state)), by = "tvar"]
            
        #iv) format for graph
            tdf <- data.table(gather(tdf, key = "Variable", value = "Percentage", -tvar))
            tdf <- tdf[Variable %in% bin_dvs]
            tdf <- merge(tdf, dv.df, all.x = T, by = c("tvar", "Variable"), sort = F)
            tdf[is.na(sig), sig := ""]
            tdf <- tdf[Variable %in% bin_dvs]
            tdf[grepl("Worried about", Variable), Variable := paste(Variable, "Worry")]
            tdf[grepl("Likely to Ask", Variable), Variable := paste(Variable, "Support")]
            tdf[, Variable := gsub("Worried about |Likely to Ask Friends for | Insecurity", "", Variable)]
            tdf[, Variable := gsub("Services", "Service", Variable)]
            tdf[, Variable := gsub("Loans", "Loan", Variable)]
            tdf[, Variable := gsub("Services", "Service", Variable)]
            tdf[, Variable := gsub("Finances", "Finance", Variable)]
            tdf[, Variable := gsub("Support", "Help", Variable)]
            tdf[Variable == "Fired", Variable := "Fired/\nUnemployed"]
            tdf[Variable == "Home Worry", Variable := "Housing\nInsecurity"]
            tdf[Variable == "Food Worry", Variable := "Food\nInsecurity"]
            tdf[Variable == "Finance Worry", Variable := "Financial\nInsecurity"]
            tdf[Variable == "Service Help", Variable := "Get Help -\nServices"]
            tdf[Variable == "Loan Help", Variable := "Get Help -\nFinances"]
            tdf[Variable == "Food Help", Variable := "Get Help -\nFood"]
            tdf[, Variable := factor(Variable, levels = c(as.character(unique(tdf$Variable)[-1]), as.character(unique(tdf$Variable)[1])))]
            
        #iv) write to csv    
            df.tmp <- copy(tdf)
            df.tmp$Variable <- gsub("\n", " ", df.tmp$Variable)
            setnames(df.tmp, "tvar", "Education")
            fwrite(df.tmp, file.path(fig.dir, "econ_education_stats.csv"))
            
        #v) get rid of get help vars    
            tdf <- tdf[!grepl("Get Help", tdf$Variable)]
            
        #vi) graph
            p2 <- ggplot(tdf, aes(x = tvar, y = Percentage, color = tvar)) +
              coord_cartesian(ylim = c(0, .9)) +
              geom_linerange(
              aes(ymin = -.2, ymax = Percentage),
              color = "lightgray", size = 2
            )+
              geom_point(size = 5)+
              geom_text(aes(label=sig), position=position_dodge(width=0.9), vjust=-0.5, size = 7)+
              facet_wrap(~Variable, scales = "free_x", ncol = 7) +
              theme_minimal() +
              scale_y_continuous(labels = scales::percent_format(accuracy=1))+
              theme(
                panel.grid.major = element_line(colour = "grey85"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor = element_blank(),
                text = element_text(family = "serif", size = 18),
                strip.text.x = element_text(size = 14, face = "bold"),
                axis.line = element_line(colour = "black"),
                legend.position="none",
                panel.spacing.x = unit(1, "lines"),
                panel.spacing.y = unit(1, "lines"),
                plot.title = element_text(size=19, hjust =.5, margin = margin(b = 20)),
                axis.text.x = element_text(size=13.5, angle = 45, hjust = 1),
                plot.margin = unit(c(0,0,-1,0), "cm"),
                axis.text.y = element_text(size=13),
                axis.title.x= element_text(size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
                axis.title.y= element_text(size=20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
              labs(x = "", y = "") +
              scale_x_discrete(expand = c(.2, 0))
        
    #c) Combine and save
        p <- plot_grid(p1, p2, ncol = 1, rel_heights = c(.5, .55))
        # t_title <- ggdraw() + draw_label("Racial and Educational Disparities in Economic Fallout from COVID-19", fontface='bold', fontfamily = 'serif', size = title.size)
        # p <- plot_grid(t_title, p, ncol=1, rel_heights=c(0.1, 1))
        ggsave(tdir <- file.path(fig.dir, paste(title, ".png", sep = "")), plot = p, height = 6*1.25/1.1, width = 6.5*1.25/1.1, units = "in")
        browseURL(tdir)


        
        
        
        
        
        
          
# #6) Create dichotomous graphs
#   #a) clean vars
#       df[, Sex := ifelse(sex == "male", "Male", "Female")]
#       df[, Education :=
#            factor(
#              ifelse(cv_educ_bachelors == 1, "Bachelors",
#                     ifelse(cv_educ_some_college == 1, "Some College",
#                            ifelse(cv_educ_hs == 1, "High School", "Less than HS"))),
#              levels = c("Less than HS", "High School", "Some College", "Bachelors")
#            )
#          ]
#       df[, Veteran := factor(ifelse(cv_veteran == 1, "Veteran", "Not Veteran"))]
#       df[, `Own Home` := factor(ifelse(cv_own_home == 1, "Own Home", "Rent"))]
#   #b) unweighted associations
#       fig.dir <- unweighted.fig.dir
#       f.draw.graph(Variable = "Race", weight = 'No', title = "Racial Disparities in Negative Economic Impact of COVID-19", exp = .2, row1.cap = .25, row2.cap = .6, row3.cap = .3)
#       f.draw.graph(Variable = "Sex", weight = 'No', title = "Sex Disparities in Negative Economic Impact of COVID-19", exp = .8, row1.cap = .15, row2.cap = .6)
#       f.draw.graph(Variable = "Education", weight = 'No', title = "Education Disparities in Negative Economic Impact of COVID-19", row1.cap = .2, row2.cap = .6, row3.cap = .2)
#       f.draw.graph(Variable = "Veteran", weight = 'No', title = "Veteran Disparities in Negative Economic Impact of COVID-19", exp = .8, row1.cap = .15, row2.cap = .5)
#       f.draw.graph(Variable = "Own Home", weight = 'No', title = "Home Owner Disparities in Negative Economic Impact of COVID-19", exp = .8, row1.cap = .15, row2.cap = .5)
#   #c) weighted asssociations  
#       fig.dir <- weighted.fig.dir
#       f.draw.graph(Variable = "Sex", weight = 'Yes', title = "Sex Disparities in Negative Economic Impact of COVID-19", exp = .8, row1.cap = .15, row2.cap = .6)
#       f.draw.graph(Variable = "Race", weight = 'Yes', title = "Racial Disparities in Negative Economic Impact of COVID-19", exp = .2, row1.cap = .25, row2.cap = .6, row3.cap = .3)
#       f.draw.graph(Variable = "Education", weight = 'Yes', title = "Education Disparities in Negative Economic Impact of COVID-19", row1.cap = .2, row2.cap = .6, row3.cap = .2)
#       f.draw.graph(Variable = "Veteran", weight = 'Yes', title = "Veteran Disparities in Negative Economic Impact of COVID-19", exp = .8, row1.cap = .15, row2.cap = .5)
#       f.draw.graph(Variable = "Own Home", weight = 'Yes', title = "Home Owner Disparities in Negative Economic Impact of COVID-19", exp = .8, row1.cap = .15, row2.cap = .5)
#   
# 
# 
# #7) Create graphs with multivariate predictors
#   #a) revise graph function
#       graph.fun <- function(p, var.expand, p.colors){
#         p + 
#           geom_smooth(aes(color = Variable), size = 2.5, se = F, span = .9) +
#           facet_wrap(~Variable, scales = "free_x", ncol = 4) +
#           theme_minimal() +
#           scale_y_continuous(labels = scales::percent_format(accuracy=1))+
#           # scale_x_continuous(limits = c(20, 65))+
#           # scale_y_continuous(labels = scales::percent)+
#           theme(
#             panel.grid.major = element_line(colour = "grey85"),
#             panel.grid.major.x = element_blank(),
#             panel.grid.minor = element_blank(),
#             text = element_text(family = "serif", size = 18),
#             strip.text.x = element_text(size = 14, face = "bold"),
#             axis.line = element_line(colour = "black"),
#             legend.position="none",
#             panel.spacing.x = unit(2, "lines"),
#             panel.spacing.y = unit(1, "lines"),
#             plot.title = element_text(size=17, hjust =.5, margin = margin(b = 20)),
#             axis.text.x = element_text(size=13.5, angle = 45, hjust = 1),
#             # axis.text.x = element_blank(),
#             plot.margin = unit(c(0,0,-1,0), "cm"),
#             axis.text.y = element_text(size=13),
#             axis.title.x= element_text(size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
#             axis.title.y= element_text(size=20, margin = margin(t = 0, r = 20, b = 0, l = 0)))+
#           # labs(x = "", y = "Percentage Agreed") +
#             labs(x = "", y = "") +
#             scale_color_manual(values = gg_color_hue(11)[p.colors])
#       }
#       
#   #c) unweighted asssociations  
#       fig.dir <- unweighted.fig.dir
#       f.draw.graph(Variable = "cv_age", weight = 'No', title = "Age Disparities in Negative Economic Impact of COVID-19", title.size = 18)
#       f.draw.graph(Variable = "acs_rural", weight = 'No', title = "Rural Disparities in Negative Economic Impact of COVID-19", title.size = 18, row1.cap = .15)
#       f.draw.graph(Variable = "acs_income_median", weight = 'No', title = "Income Disparities in Negative Economic Impact of COVID-19", row1.cap = .15)
#       
#   #c) weighted asssociations  
#       fig.dir <- weighted.fig.dir
#       f.draw.graph(Variable = "cv_age", weight = 'Yes', title = "Age Disparities in Negative Economic Impact of COVID-19", title.size = 18)
#       f.draw.graph(Variable = "acs_rural", weight = 'Yes', title = "Rural Disparities in Negative Economic Impact of COVID-19", title.size = 18, row1.cap = .15)
#       f.draw.graph(Variable = "acs_income_median", weight = 'Yes', title = "Income Disparities in Negative Economic Impact of COVID-19", row1.cap = .15)
#       
#       
# #7) Create graphs with multivariate outcomes
#     #a) create graphing functions
#         graph.fun <- function(p, var.expand){
#         p + 
#           geom_bar(position = position_stack(), stat = "identity", width = .7) +
#           geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 2.5) +
#           facet_wrap(~dv, scales = "free_x", ncol = 4) +
#           theme_minimal() +
#           scale_y_continuous(labels = function(x) paste(x, "%", sep = "")) +
#           # scale_y_continuous(labels = scales::percent)+
#           theme(
#             panel.grid.major = element_line(colour = "grey85"),
#             panel.grid.major.x = element_blank(),
#             panel.grid.minor = element_blank(),
#             text = element_text(family = "serif", size = 18),
#             strip.text.x = element_text(size = 14, face = "bold"),
#             axis.line = element_line(colour = "black"),
#             legend.position="bottom",
#             panel.spacing.x = unit(2, "lines"),
#             panel.spacing.y = unit(1, "lines"),
#             plot.title = element_text(size=19, hjust =.5, margin = margin(b = 20)),
#             axis.text.x = element_text(size=13.5, angle = 45, hjust = 1),
#             axis.text.y = element_text(size=13)#,
#             )+
#             labs(x = "", y = "", fill = "") +
#             scale_fill_manual(values = gg_color_hue(4)[c(1,4,3,2)])
#         }
#     
#         f.draw.graph <- function(Variable = "Race",
#                                  weight = 'No',
#                                  title = "Racial Disparities in Negative Economic Impact of COVID-19 Categorical",
#                                  exp = .2,
#                                  row1.cap = .3,
#                                  row2.cap = .5,
#                                  row3.cap = .2,
#                                  title.size = 18) {
#     
#           #a) subset to important variables
#               df[, tvar := .SD, .SDcols = Variable]
#               df <- df[cv_age < 65, ]
#               tdf <- df[, c(cat_dvs, "tvar", "weight_state"), with = F]
#           #b) tally responses by dv, iv, and level of agreement
#               l.df <- list()
#               for(i in 1:length(cat_dvs)) {
#                 if(weight == 'Yes'){
#                     l.df[[i]] <- tdf[, sum(weight_state), by = c("tvar", cat_dvs[i]) ]
#                 }else{
#                     l.df[[i]] <- tdf[, .N, by = c("tvar", cat_dvs[i]) ]
#                 }
#                 names(l.df[[i]]) <- c("tvar", "agreement", "count")
#                 l.df[[i]]$dv <- cat_dvs[i]
#               }
#               tdf <- rbindlist(l.df)
#               tdf <- tdf[tdf$agreement != ""]
#           #c) convert responses to percent format; agreement to correct order
#               tdf = tdf[, percent := count/sum(count) * 100, by = c("tvar", "dv")]
#               tdf$label = paste0(sprintf("%.0f", tdf$percent), "%")
#               tdf$Agreement <- factor(tdf$agreement, levels = c("Strongly agree", "Agree", "Disagree", "Strongly disagree"))
#               tdf[grepl("Worried about", dv), dv := paste(dv, "Worry")]
#               tdf[grepl("Likely to Ask", dv), dv := paste(dv, "Support")]
#               tdf[, dv := gsub("Worried about |Likely to Ask Friends for | Insecurity", "", dv)]
#               tdf[, dv := factor(dv, levels = unique(dv))]
#           #d) graph
#               p <- graph.fun(p = ggplot(tdf, aes(x = factor(tvar), y = percent, fill = Agreement)), var.expand = exp)
#               t_title <- ggdraw() + draw_label(title, fontface='bold', fontfamily = 'serif', size = title.size)
#               p <- plot_grid(t_title, p, ncol=1, rel_heights=c(0.1, 1))
#               ggsave(tdir <- file.path(fig.dir, paste(title, ".png", sep = "")), plot = p, height = 8, width = 11, units = "in")
#               browseURL(tdir)
#         }
#         
#     #b) unweighted asssociations  
#         fig.dir <- unweighted.fig.dir
#         f.draw.graph(Variable = "Race", weight = 'No', title = "Racial Disparities in Negative Economic Impact of COVID-19, Categorical DVs")
#         f.draw.graph(Variable = "Sex", weight = 'No', title = "Sex Disparities in Negative Economic Impact of COVID-19, Categorical DVs", exp = .8, row1.cap = .15)
#         f.draw.graph(Variable = "Education", weight = 'No', title = "Education Disparities in Negative Economic Impact of COVID-19, Categorical DVs", row1.cap = .15, row3.cap = .15)
#         f.draw.graph(Variable = "age_group", weight = 'No', title = "Age Disparities in Negative Economic Impact of COVID-19, Categorical DVs")
#         
#     #c) weighted asssociations  
#         fig.dir <- weighted.fig.dir
#         f.draw.graph(Variable = "Race", weight = 'Yes', title = "Racial Disparities in Negative Economic Impact of COVID-19, Categorical DVs")
#         f.draw.graph(Variable = "Sex", weight = 'Yes', title = "Sex Disparities in Negative Economic Impact of COVID-19, Categorical DVs", exp = .8, row1.cap = .15)
#         f.draw.graph(Variable = "Education", weight = 'Yes', title = "Education Disparities in Negative Economic Impact of COVID-19, Categorical DVs", row1.cap = .2)
#         f.draw.graph(Variable = "age_group", weight = 'Yes', title = "Age Disparities in Negative Economic Impact of COVID-19, Categorical DVs")
#         
# 
#         
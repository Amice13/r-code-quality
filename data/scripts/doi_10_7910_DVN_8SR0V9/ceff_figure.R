ceff_figure <- function(data = full_panel, 
                        legendtitle = "Publications in:",
                        output,
                        limits = NULL,
                        dv = c("num_of_pub","Q12_num","Q1_num","top10_num"),
                        lb = c("All Journals", "Top 50%", "Top 25%", "Top 10%")){
    data <- data %>% 
        mutate(diff = year - returnyear,
               dumm_12 = as.numeric(treat*(year - returnyear == -12)),
               dumm_11 = as.numeric(treat*(year - returnyear == -11)),
               dumm_10 = as.numeric(treat*(year - returnyear == -10)),
               dumm_9 = as.numeric(treat*(year - returnyear == -9)),
               dumm_8 = as.numeric(treat*(year - returnyear == -8)),
               dumm_7 = as.numeric(treat*(year - returnyear == -7)),
               dumm_6 = as.numeric(treat*(year - returnyear == -6)),
               dumm_5 = as.numeric(treat*(year - returnyear == -5)),
               dumm_4 = as.numeric(treat*(year - returnyear == -4)),
               dumm_3 = as.numeric(treat*(year - returnyear == -3)),
               dumm_2 = as.numeric(treat*(year - returnyear == -2)),
               dumm_1 = as.numeric(treat*(year - returnyear == -1)),
               dumm_0 = as.numeric(treat*(year - returnyear == 0)),
               dumm1 = as.numeric(treat*(year - returnyear == 1)),
               dumm2 = as.numeric(treat*(year - returnyear == 2)),
               dumm3 = as.numeric(treat*(year - returnyear == 3)),
               dumm4 = as.numeric(treat*(year - returnyear == 4)),
               dumm5 = as.numeric(treat*(year - returnyear == 5)),
               dumm6 = as.numeric(treat*(year - returnyear == 6)),
               dumm7 = as.numeric(treat*(year - returnyear == 7)),
               dumm8 = as.numeric(treat*(year - returnyear == 8)),
               dumm9 = as.numeric(treat*(year - returnyear == 9)))
    figure_df <- data.frame() 
    for (y in dv) {
        f <- as.formula(paste0(y,"~dumm_12+dumm_11+dumm_10+dumm_9+dumm_8+dumm_7+
                               dumm_6 + dumm_5 + dumm_4 + dumm_3 + dumm_2 + dumm_0+
                               dumm1 + dumm2 + dumm3 + dumm4 + dumm5 + dumm6 + dumm7 +
                               dumm8+ dumm9 + 
                               factor(year)+factor(uniqueID)+factor(diff)"))
        
        mod <- glm(f,
                   data = data,
                   family = poisson())
        coef <- coeftest(mod, vcov = vcovCL(mod, type = "HC0", 
                                            cluster = ~uniqueID))
        
        df_tmp <- data.frame(coef[8:18,1:2]) %>% 
            rename(se = Std..Error) %>% 
            mutate(ymin = Estimate - 1.961*se,
                   ymax = Estimate + 1.961*se,
                   year = c(-4,-3,-2,0,1,2,3,4,5,6,7)) %>%
            mutate(type = y)
        figure_df <- rbind(figure_df, df_tmp)
    }
    
    figure_df_adj <- figure_df %>% 
        mutate(year = if_else(type == dv[1], year - 0.225, year),
               year = if_else(type == dv[2], year - 0.075, year),
               year = if_else(type == dv[3], year + 0.075, year),
               year = if_else(type == dv[4], year + 0.225, year),
               type = if_else(type == dv[1],lb[1], type),
               type = if_else(type == dv[2],lb[2], type),
               type = if_else(type == dv[3],lb[3], type),
               type = if_else(type == dv[4],lb[4], type),
               type = factor(type,levels = lb)) 
    
    p <- ggplot(data = figure_df_adj, 
                aes(x = year, y = Estimate,color = type)) +
        geom_point(aes(shape = type),size = 1.2) +
        scale_shape_manual(values = c(16,17,15,18)) +
        geom_segment(aes(x = year, xend = year, 
                         y = ymin, yend = ymax,
                         color = type),size = 1) +
        geom_hline(yintercept = 0, lwd = 0.2) +
        geom_vline(xintercept = -1, lwd = 0.2) +
        scale_x_continuous(breaks = seq(-7,10)) + 
        theme_classic2() +
        ylab("Coefficients") + 
        xlab("Years to/After Return") +
        scale_color_manual(values = c("#BB0021FF","#3B4992FF",
                                      "#008280FF","#808180FF")) +
        theme(legend.position = "top") + 
        labs(color = legendtitle,shape = legendtitle) #+
    if(length(limits) > 0){
        p <- p + scale_y_continuous(breaks = seq(limits[1],limits[2]),
                                    limits = limits)
    }
    ggsave(p, filename = paste0(output,".png"),width = 8, height = 3.8)
    return(p)
}

ceff_figure_c2 <- function(data = full_panel, 
                           legendtitle = "Publications in:",
                           output,
                           limits = NULL,
                           dv = c("num_of_pub","Q12_num",
                                  "Q1_num",
                                  "top90_num"),
                           lb = c("All Journals",
                                  "Top 50%", "Top 25%", "Top 10%" )){
    data <- data %>% 
        mutate(diff = year - returnyear,
               dumm_12 = as.numeric(treat*(year - returnyear == -12)),
               dumm_11 = as.numeric(treat*(year - returnyear == -11)),
               dumm_10 = as.numeric(treat*(year - returnyear == -10)),
               dumm_9 = as.numeric(treat*(year - returnyear == -9)),
               dumm_8 = as.numeric(treat*(year - returnyear == -8)),
               dumm_7 = as.numeric(treat*(year - returnyear == -7)),
               dumm_6 = as.numeric(treat*(year - returnyear == -6)),
               dumm_5 = as.numeric(treat*(year - returnyear == -5)),
               dumm_4 = as.numeric(treat*(year - returnyear == -4)),
               dumm_3 = as.numeric(treat*(year - returnyear == -3)),
               dumm_2 = as.numeric(treat*(year - returnyear == -2)),
               dumm_1 = as.numeric(treat*(year - returnyear == -1)),
               dumm_0 = as.numeric(treat*(year - returnyear == 0)),
               dumm1 = as.numeric(treat*(year - returnyear == 1)),
               dumm2 = as.numeric(treat*(year - returnyear == 2)),
               dumm3 = as.numeric(treat*(year - returnyear == 3)),
               dumm4 = as.numeric(treat*(year - returnyear == 4)),
               dumm5 = as.numeric(treat*(year - returnyear == 5)),
               dumm6 = as.numeric(treat*(year - returnyear == 6)),
               dumm7 = as.numeric(treat*(year - returnyear == 7)),
               dumm8 = as.numeric(treat*(year - returnyear == 8)),
               dumm9 = as.numeric(treat*(year - returnyear == 9)))
    figure_df <- data.frame() 
    for (y in dv) {
        f <- as.formula(paste0(y,"~dumm_12+dumm_11+dumm_10+dumm_9+dumm_8+dumm_7+
                               dumm_6 + dumm_5 + dumm_4 + dumm_3 + dumm_2 + dumm_0+
                               dumm1 + dumm2 + dumm3 + dumm4 + dumm5 + dumm6 + dumm7 +
                               dumm8+ dumm9 + log(teamsize+1) + log(grant+1) +
                               factor(year)+factor(uniqueID)+factor(diff)"))
        mod <- glm(f,
                   data = data,
                   family = poisson())
        coef <- coeftest(mod, vcov = vcovCL(mod, type = "HC0", cluster = ~uniqueID))
        
        df_tmp <- data.frame(coef[8:18,1:2]) %>% 
            rename(se = Std..Error) %>% 
            mutate(ymin = Estimate - 1.961*se,
                   ymax = Estimate + 1.961*se,
                   year = c(-4,-3,-2,0,1,2,3,4,5,6,7)) %>%
            mutate(type = y)
        figure_df <- rbind(figure_df, df_tmp)
    }
    
    figure_df_adj <- figure_df %>% 
        mutate(year = if_else(type == dv[1], year - 0.225, year),
               year = if_else(type == dv[2], year - 0.075, year),
               year = if_else(type == dv[3], year + 0.075, year),
               year = if_else(type == dv[4], year + 0.225, year),
               type = if_else(type == dv[1],lb[1], type),
               type = if_else(type == dv[2],lb[2], type),
               type = if_else(type == dv[3],lb[3], type),
               type = if_else(type == dv[4],lb[4], type),
               type = factor(type,levels = lb)) 
    
    p <- ggplot(data = figure_df_adj, 
                aes(x = year, y = Estimate,color = type)) +
        geom_point(aes(shape = type),size = 1.2) +
        scale_shape_manual(values = c(16,17,15,18)) +
        geom_segment(aes(x = year, xend = year, 
                         y = ymin, yend = ymax,
                         color = type),size = 1) +
        geom_hline(yintercept = 0, lwd = 0.2) +
        geom_vline(xintercept = -1, lwd = 0.2) +
        scale_x_continuous(breaks = seq(-7,10)) + 
        theme_classic2() +
        ylab("Coefficients") + 
        xlab("Years to/After Return") +
        scale_color_manual(values = c("#BB0021FF","#3B4992FF",
                                      "#008280FF","#808180FF")) +
        theme(legend.position = "top") + 
        labs(color = legendtitle,shape = legendtitle)
    if(length(limits) > 0){
        p <- p + scale_y_continuous(breaks = seq(limits[1],limits[2]),
                                    limits = limits)
    }
    ggsave(p, filename = paste0(output,".png"),width = 8, height = 3.8)
    return(p)
}

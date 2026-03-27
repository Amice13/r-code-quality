reg <- function(y="y", data , control = NULL,
                model = "poisson"){
    covs <- c("treat_post","factor(year)","factor(uniqueID)","post",control)
    f <- as.formula(paste(y,"~", paste(covs, collapse = "+")))
    if (model == "poisson") {
        model <- glm(f,family = poisson(),data = data)
    }else if (model == "linear") {
        model <- lm(f,data = data)
    }
    return(model)
}

outreg <- function(data,name, control=NULL, 
                   model = "poisson", 
                   type = "pubs",
                   sig=0.1){
    if (model == "poisson") {
        mod1.1 <- reg("num_of_pub",data,control,model)
        mod1.2 <- reg("Q12_num",data,control,model)
        mod1.3 <- reg("Q1_num",data,control,model)
        mod1.4 <- reg("top10_num",data,control,model)
        
        mod2.1 <- reg("FA_pub",data,control,model)
        mod2.2 <- reg("FA_Q12pub",data,control,model)
        mod2.3 <- reg("FA_Q1pub",data,control,model)
        mod2.4 <- reg("FA_top10pub",data,control,model)
        
        mod3.1 <- reg("LA_pub",data,control,model)
        mod3.2 <- reg("LA_Q12pub",data,control,model)
        mod3.3 <- reg("LA_Q1pub",data,control,model)
        mod3.4 <- reg("LA_top10pub",data,control,model)
        
    }else{
        mod1.1 <- reg("log(num_of_pub+1)",data,control,model)
        mod1.2 <- reg("log(Q12_num+1)",data,control,model)
        mod1.3 <- reg("log(Q1_num+1)",data,control,model)
        mod1.4 <- reg("log(top10_num+1)",data,control,model)
        
        mod2.1 <- reg("log(FA_pub+1)",data,control,model)
        mod2.2 <- reg("log(FA_Q12pub+1)",data,control,model)
        mod2.3 <- reg("log(FA_Q1pub+1)",data,control,model)
        mod2.4 <- reg("log(FA_top10pub+1)",data,control,model)
        
        mod3.1 <- reg("log(LA_pub+1)",data,control,model)
        mod3.2 <- reg("log(LA_Q12pub+1)",data,control,model)
        mod3.3 <- reg("log(LA_Q1pub+1)",data,control,model)
        mod3.4 <- reg("log(LA_top10pub+1)",data,control,model)
        }
    
    rs1.1 <- sqrt(diag(vcovCL(mod1.1, cluster = ~uniqueID,type = "HC0")))
    rs1.2 <- sqrt(diag(vcovCL(mod1.2, cluster = ~uniqueID,type = "HC0")))
    rs1.3 <- sqrt(diag(vcovCL(mod1.3, cluster = ~uniqueID,type = "HC0")))
    rs1.4 <- sqrt(diag(vcovCL(mod1.4, cluster = ~uniqueID,type = "HC0")))
    
    rs2.1 <- sqrt(diag(vcovCL(mod2.1, cluster = ~uniqueID,type = "HC0")))
    rs2.2 <- sqrt(diag(vcovCL(mod2.2, cluster = ~uniqueID,type = "HC0")))
    rs2.3 <- sqrt(diag(vcovCL(mod2.3, cluster = ~uniqueID,type = "HC0")))
    rs2.4 <- sqrt(diag(vcovCL(mod2.4, cluster = ~uniqueID,type = "HC0")))
    
    rs3.1 <- sqrt(diag(vcovCL(mod3.1, cluster = ~uniqueID,type = "HC0")))
    rs3.2 <- sqrt(diag(vcovCL(mod3.2, cluster = ~uniqueID,type = "HC0")))
    rs3.3 <- sqrt(diag(vcovCL(mod3.3, cluster = ~uniqueID,type = "HC0")))
    rs3.4 <- sqrt(diag(vcovCL(mod3.4, cluster = ~uniqueID,type = "HC0")))
    
    coef <- c(mod1.1$coefficients[2],mod1.2$coefficients[2],mod1.3$coefficients[2],mod1.4$coefficients[2],
              mod2.1$coefficients[2],mod2.2$coefficients[2],mod2.3$coefficients[2],mod2.4$coefficients[2],
              mod3.1$coefficients[2],mod3.2$coefficients[2],mod3.3$coefficients[2],mod3.4$coefficients[2])
    
    irr <- format(exp(coef),digits = 3)
    names(irr) <- NULL
    
    covlab <- str_replace_all(c('YTTP * After Return',
                                control),
                              pattern = "log\\(grant_1\\+1\\)|log\\(grant_3\\+1\\)",
                              replacement = "logGrant")
    
    covlab <- str_replace_all(covlab,
                              pattern = "grant_1|grant_3",
                              replacement = "Grant")
    
    covlab <- str_replace_all(covlab,
                              pattern = "age",
                              replacement = "Age")
    
    covlab <- str_replace_all(covlab,
                              pattern = "teamsize",
                              replacement = "Team")
    
    covlab <- str_replace_all(covlab,
                              pattern = "treat_post_Alumni",
                              replacement = "YTTP * After Return * Alumni")
    
    if(sig == 0.05){
        stargazer(mod1.1,mod1.2,mod1.3,mod1.4,
                  mod2.1,mod2.2,mod2.3,mod2.4,
                  mod3.1,mod3.2,mod3.3,mod3.4,
                  dep.var.caption  = "",
                  dep.var.labels = c("Total Publications",
                                     "Top 50%", 
                                     "Top 25%",
                                     "Top 10%",
                                     "Total Publications",
                                     "Top 50%", 
                                     "Top 25%",
                                     "Top 10%",
                                     "Total Publications",
                                     "Top 50%", 
                                     "Top 25%",
                                     "Top 10%"),
                  star.cutoffs = c(0.05, 0.01, 0.001),
                  covariate.labels = covlab, 
                  order = c('treat_post'),
                  omit = c("year","uniqueID","Constant","Alumniyear","Alumniid"),
                  add.lines = list(c("IRR", irr),
                                   c("Year FE", "Y", "Y","Y", "Y", "Y", "Y","Y", "Y",
                                     "Y", "Y","Y", "Y"),
                                   c("Scientist FE", "Y", "Y","Y", "Y", "Y", "Y","Y", "Y",
                                     "Y", "Y","Y", "Y")),
                  type = "html", 
                  align = T,
                  no.space = T,
                  se = list(rs1.1,rs1.2,rs1.3,rs1.4,
                            rs2.1,rs2.2,rs2.3,rs2.4,
                            rs3.1,rs3.2,rs3.3,rs3.4),
                  omit.stat = c("rsq","aic","ser"),
                  notes = "Robust standard errors clustered by scientist in parentheses; *p<0.05; **p<0.01; ***p<0.001.",
                  notes.append = F,
                  notes.align = "l",
                  table.layout = "=!c-d#-ta-s=n",
                  df = FALSE,
                  out = paste0(name,'.html'))
    }else{
        stargazer(mod1.1,mod1.2,mod1.3,mod1.4,
                  mod2.1,mod2.2,mod2.3,mod2.4,
                  mod3.1,mod3.2,mod3.3,mod3.4,
                  dep.var.caption  = "",
                  dep.var.labels = c("Total Publications",
                                     "Top 50%", 
                                     "Top 25%",
                                     "Top 10%",
                                     "Total Publications",
                                     "Top 50%", 
                                     "Top 25%",
                                     "Top 10%",
                                     "Total Publications",
                                     "Top 50%", 
                                     "Top 25%",
                                     "Top 10%"),
                  covariate.labels = covlab, 
                  order = c('treat_post'),
                  omit = c("year","^post","uniqueID","Constant"),
                  add.lines = list(c("IRR", irr),
                                   c("Year FE", "Y", "Y","Y", "Y", "Y", "Y","Y", "Y",
                                     "Y", "Y","Y", "Y"),
                                   c("Scientist FE", "Y", "Y","Y", "Y", "Y", "Y","Y", "Y",
                                     "Y", "Y","Y", "Y")),
                  type = "html", 
                  align = T,
                  no.space = T,
                  se = list(rs1.1,rs1.2,rs1.3,rs1.4,
                            rs2.1,rs2.2,rs2.3,rs2.4,
                            rs3.1,rs3.2,rs3.3,rs3.4),
                  omit.stat = c("rsq","aic","ser"),
                  notes = "Robust standard errors clustered by scientist in parentheses; *p<0.1; **p<0.05; ***p<0.01.",
                  notes.append = F,
                  notes.align = "l",
                  table.layout = "=!c-d#-ta-s=n",
                  df = FALSE,
                  out = paste0(name,'.html'))
    }
}

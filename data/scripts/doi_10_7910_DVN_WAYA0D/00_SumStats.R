## Replication code for 'Locked Out of College: ...'
## Jacob R. Brown and Hanno Hilbig (hhilbig@g.harvard.edu)
## Nov 18, 2020
##
## This file: Figure A3, Figure B1, Table B2

## Set WD

rm(list = ls())
setwd("")

## Packages
## This function auto-installs packages if missing, and then loads them

check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE,
                     repos = "https://cran.rstudio.com")
  sapply(pkg, require, character.only = TRUE)
}

pkgList <- c("tidyverse", "gridExtra")
check.packages(pkgList)

#### Read data ####

df <- read.csv('Data_Main.csv',
               stringsAsFactors = F)

## Relevel advocate treatment, base category should be no advocate

df <- df %>% 
  mutate(treat_advocate = factor(treat_advocate,
                                 levels = c("No Advocate",
                                            "White Advocate",
                                            "Black Advocate")))

#### Figure A3 : Covariate balance ####

## Define treatment variables

treat_vars <- c('treat_felon', 'treat_black', 'treat_advocate')

## Define covariates

cov_list <- c('covar_public_private', 'covar_inst_size',
              'covar_type')

## Covariates to numeric

for (j in cov_list) {
  df[, j] <- as.numeric(as.factor(df[, j]))
}

## Function to calucalte balance

get_bal <- function(treatvar, cov_list) {
  
  out_temp <- lapply(cov_list, function(cv) {
    f <- as.formula(paste0(cv, ' ~', treatvar))
    
    ## Model
    m <- lm(f, data = df)
    
    ## Get coefs
    
    if (!treatvar == 'treat_advocate') {
      
      coef_list <- summary(m)$coefficients[2, 1]
      lower_list <- coef_list - 1.96*summary(m)$coefficients[2, 2]
      upper_list <- coef_list + 1.96*summary(m)$coefficients[2, 2]
      
      tv = treatvar
      
    } else {
      coef_list <- summary(m)$coefficients[2:3, 1]
      lower_list <- coef_list - 1.96*summary(m)$coefficients[2:3, 2]
      upper_list <- coef_list + 1.96*summary(m)$coefficients[2:3, 2]
      
      tv = c('treat_advocate_white', 'treat_advocate_black')
      
    }
    
    ## 
    
    data.frame(cov = cv, lower = lower_list, upper = upper_list,
               coef = coef_list, tv = tv, stringsAsFactors = F)
  })
  
  do.call('rbind', out_temp)
}

## Apply across treatment vars

out_list <- lapply(treat_vars, function(tv) {
  get_bal(treatvar = tv, cov_list = cov_list)
})

## Convert to DF and prep for plotting

out_df <- do.call('rbind', out_list)
out_df$cov <- factor(out_df$cov)
levels(out_df$cov) <- c('<5k Students\n(vs. > 5k)', 'Public Institution\n(vs. Private)', 
                        '2-year College\n(vs. 4-year)')

out_df$tv <- factor(out_df$tv)
levels(out_df$tv) <- c('Treatment: Black Advocate', 'Treatment: White Advocate',
                       'Treatment: Black', 'Treatment: Criminal Record')
out_df$tv <- factor(out_df$tv, 
                    levels =  c('Treatment: Black Advocate', 'Treatment: White Advocate',
                                'Treatment: Black', 'Treatment: Criminal Record')[4:1])

## Plot

p1 <- ggplot(data = out_df, aes(x = cov, y = coef)) +
  geom_hline(yintercept = 0, linetype = 'dotted', color = 'grey60') + 
  geom_errorbar(aes(ymax = upper, ymin = lower), color = 'black',
                width = 0) +
  geom_point(shape = 21, fill = 'white', size = 3) + xlab('') +
  coord_flip() + ylab('Difference in Means') +
  theme_bw() + ylim(c(-0.1, 0.1)) +
  # scale_y_continuous(breaks = seq(-0.05, 0.05,)) +
  facet_wrap(~ tv) + 
  theme(panel.spacing = unit(1.5, "lines"))
p1

#### Figure B1 : Distribution of school characteristics ####

## Logical vector of non-missings ob 

ss <- !is.na(df$outcome_response)

## Get school characteristics frequencies

t1 <- table(df[ss, 'covar_type'])[2:1]
t2 <- table(df[ss, 'covar_public_private'])
t3 <- table(df[ss, "covar_inst_size_raw"])

## Prep plotting

values <- c(t2, t1, t3)
labs <- c(names(t2), names(t1), names(t3))
plot_df <- data.frame(freq = values, var = c('Private', 'Public', 
                                             'Two-year', 'Four-year',
                                             '< 1,000',
                                             '1,000 - 4,999',
                                             '5,000 - 9,999',
                                             '10,000 - 19,999',
                                             '> 20,000'))
plot_df$facet <- rep(c(1,1,2,2,3,3,3,3,3), each = 1)
plot_df$var <- factor(plot_df$var, levels = plot_df$var[seq(1,9)])

## Plot this 

p1 <- ggplot(data = plot_df, aes(var, freq, group = 1))  +
  geom_bar(stat = 'identity', fill = 'grey90', color = 'black',
           width=0.4, position = position_dodge(width=0.5),
           size = 0.3) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  xlab('') + ylab('') + facet_wrap(~ facet, scale = 'free_x') +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank())
p1

#### Table B2: Response rates by name ####

## Get response rates

resp_rate <- df %>% group_by(name_person) %>% 
  summarise(m_response = mean(outcome_response, na.rm = T),
            n = length(outcome_response),
            m_felon = mean(treat_felon),
            m_black = mean(treat_black),
            m_adv = mean(treat_reference),
            m_adv_black = mean(treat_ref_black))

## Rounding

for (j in 2:7) resp_rate[, j] <- round(resp_rate[, j], 3)
for (j in 4:7) resp_rate[, j] <- round(resp_rate[, j], 3) * 100

## Rename columns

colnames(resp_rate) <- c('Name', 'Response rate', 
                         'Total emails', 'Pct. Ex-Felon',
                         'Pct. Black', 'Pct. Have an advocate', 
                         'Pct. Have a black advocate')

## Return table

stargazer::stargazer(resp_rate, summary = F, style = 'ajps',
                     out.header = F, 
                     rownames = F,
                     title = 'Response rates and treatment distribution by name',
                     font.size = 'small')

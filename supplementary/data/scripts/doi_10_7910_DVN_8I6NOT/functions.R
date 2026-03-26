################################## #
# The Psychology of Online Political Hostility  #
# 
# This script hosts all the custom functions used in this project
# 
#
# 2019.07.25.
# code by XXXXXXXXXXX
################################## #


# transforms vars to z-score and returns simple numeric vector (instead of matrix)
myscale <- function(x){
        as.numeric(scale(x))
}

# function to recode anes pid variable
pid <- function(xd, xr, xi){
        require(dplyr)
        x <- case_when(xd == 1 ~ 1,
                       xd == 2 ~ 2,
                       xi == 2 ~ 3,
                       xi == 3 ~ 4,
                       xi == 1 ~ 5,
                       xr == 2 ~ 6,
                       xr == 1 ~ 7)
        x
}

# flips reverse coded items 
flip <- function(x){
        minimum <- min(x, na.rm = T)
        maximum <- max(x, na.rm = T)
        
        (((x - maximum) * -1 ) + minimum)
}

# function to rescale to 0-1
#       b/c most vars are on 1-7 scale the default takes the max value to be 7
zero.one <- function(x, min = 1, max = 7){ 
        # max <- max(x, na.rm = T)
        # min <- min(x, na.rm = T)
        # x <- na_if(x, 8)
        ((x - min) / (max-min))
}

# function to center around the mean and divide by 2sd
twoSD <- function(x){
        sd <- sd(x, na.rm =T)
        mean <- mean(x, na.rm = T)
        (x - mean)/(2*sd)
}

# create indices. recode 8 as NA and then get average of all vars.
row.means <- function(x){
        x[x == 8] <- NA
        x <- rowMeans(x, na.rm = T)
        x
}

# standard error function
se <- function(x){
        xnn <- x[!is.na(x)]  # x with no na
        (sd(xnn)/sqrt(length(xnn)))
}


# custom function to draw predicted value plots 
predicted.plot <- function(fit, key, country){
        
        # define a range of values for IV of interest for which to predict 
        keystring <- seq(round(min(fit$model[key]), 2), 
                         round(max(fit$model[key]), 2), 
                         0.05)
        
        # remind the model which IV is not in the focus at the moment
        other <- ifelse(key == "sdrt", "ders", "sdrt")
        
        # create data frames with covariates set to 0
        # the two countries have slightly different covariates 
        if(country == "us"){
                newdat <- data.frame(
                        female = rep(mean(df.us$female), length(keystring)),
                        age = rep(mean(df.us$age), length(keystring)), 
                        highered = rep(mean(df.us$highered), length(keystring)), 
                        income = rep(mean(df.us$income, na.rm = T), length(keystring)), 
                        pid = rep(mean(df.us$pid), length(keystring)), 
                        white = rep(mean(df.us$white), length(keystring)))
                
                newdat[, other] <-  mean(df.us[, other], na.rm = T) 
                
        } else if(country == "dk"){
                newdat <- data.frame(
                        female = rep(mean(df.dk$female), length(keystring)),
                        age = rep(mean(df.dk$age), length(keystring)), 
                        highered = rep(mean(df.dk$highered), length(keystring)), 
                        income = rep(mean(df.dk$income, na.rm = T), length(keystring)), 
                        pid = rep("Red_block", length(keystring))
                ) 
                newdat[, other] <-  mean(df.dk[, other], na.rm = T)
        } else if(country == "us3"){
                newdat <- data.frame(
                        female = rep(mean(df.us3.part$female), length(keystring)),
                        age = rep(mean(df.us3.part$age, na.rm = T), length(keystring)), 
                        highered = rep(mean(df.us3.part$highered), length(keystring)), 
                        income = rep(mean(df.us3.part$income, na.rm = T), length(keystring)), 
                        pid = rep(mean(df.us3.part$pid), length(keystring)), 
                        white = rep(mean(df.us3.part$white), length(keystring)))
                
                newdat[, other] <-  mean(df.us3.part[, other], na.rm = T) 
                
        } else   {stop("Unrecognized country")}
        
        newdat[, key] <- keystring # add key IV to DF
        # newdat[, other] <- mean() # add other IV to DF and set to 0
        
        # check which environment is used in the model
        # split the first variable (dv) in fitted model object and check
        #       what is after the dash
        env <- str_split(names(fit$model[1]), fixed("_"))[[1]][2]
        
        # browser()
        # predict values and 95% confidence interval
        pred <- cbind(newdat,
                      predict(fit, newdata = newdat, interval = "confidence"),
                      env = env, country = country) %>% 
                dplyr::select(-other)%>% 
                mutate(country = fct_recode(country, Denmark = "dk", USA = "us", USA21 = "us3"), 
                       env = fct_recode(env, Offline = "offline", Online = "online"))
        
        pred
        
}



# function to export stats I wanna report
t.sum <- function(x,y, xname = "X", yname = "Y",
                  name = "t-test", paired = FALSE){
        t <- t.test(x, y, paired = paired)
        
        if(paired == FALSE){ 
                sum <- data.frame(
                        name = name, 
                        M.x = t$estimate[1],
                        SD.x = sd(x, na.rm =T), 
                        M.y = t$estimate[2], 
                        SD.y = sd(y, na.rm = T), 
                        diff = diff(t$estimate), 
                        t = t$statistic*-1, 
                        t.df = round(t$parameter, 0),
                        p = t$p.value,
                        row.names = "")
        } else {
                x.nona <- x[!is.na(x) & !is.na(y)]
                y.nona <- y[!is.na(x) & !is.na(y)]
                
                sum <- data.frame(
                        name = name, 
                        M.x = mean(x.nona),
                        SD.x = sd(x.nona),
                        M.y = mean(y.nona),
                        SD.y = sd(y.nona),
                        diff = t$estimate, 
                        t = t$statistic, 
                        t.df = round(t$parameter, 0),
                        p = t$p.value,
                        row.names = "")        
                }
        
        names(sum) <- gsub("x", xname, names(sum))
        names(sum) <- gsub("y", yname, names(sum))
        sum
}


# another wrapper for standard and equivalence t-tests
tost.sum <- function(x,y, xname = "X", yname = "Y",
                     name = "Equivalence test"){
        require(TOSTER)
        require(tidyverse)
        
        x.nona <- x[!is.na(x) & !is.na(y)]
        y.nona <- y[!is.na(x) & !is.na(y)]
        stopifnot(near(length(x.nona), length(y.nona)))
        
        bounds <- powerTOSTpaired(alpha = 0.05,
                                  statistical_power = 0.8,
                                  N = length(x.nona))
        
        t.results <- t.test(x.nona, y.nona, paired = T)
        t.results
        tost.results <- TOSTER::TOSTpaired(
                n = length(x.nona),
                m1 = mean(x.nona), m2 = mean(y.nona),
                sd1 = sd(x.nona), sd2 = sd(y.nona),
                r12 = cor(x.nona, y.nona),
                low_eqbound_dz = -1, high_eqbound_dz = bounds[2],
                plot = F, verbose = F)
        
        sum <- data.frame(
                name = name,
                M.x = mean(x.nona),
                SD.x = sd(x.nona),
                M.y = mean(y.nona),
                SD.y = sd(y.nona),
                
                diff = tost.results$diff,
                t = t.results$statistic,
                p = t.results$p.value,
                
                TOST.bound = tost.results$high_eqbound,
                TOST.t = tost.results$TOST_t2,
                TOST.p = tost.results$TOST_p2,
                
                N = length(x.nona),
                row.names = "")
        
        names(sum) <- gsub("x", xname, names(sum))
        names(sum) <- gsub("y", yname, names(sum))
        sum
        
}


# export raw alpha
get.alpha <- function(df, x){
        # name <- deparse(substitute(x[1]))
        # name <- str_split(name, "\\\"")[[1]][2]
        # name <- str_sub(name, 1, str_length(name)-1)
        
        psych::alpha(df[, c(x)], warnings = F)$total$raw_alpha
        
}



# function to calculate mean and standard deviation
mean.sd <- function(x){
        avg <-round(mean(x, na.rm = T), 2)
        sd <- round(sd(x, na.rm = T), 2)
        paste(avg, " (", sd, ")", sep = '')
}

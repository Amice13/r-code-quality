## ----Setup, include=FALSE, results="hide", warning=FALSE----------------------
require(knitr)
require(rmarkdown)
require(ggplot2)
require(showtext)
require(modelsummary)
require(kableExtra)
font_add(family = "Source Sans Pro",
         "SourceSansPro-Regular.otf")
showtext_auto()

opts_chunk$set(dev = c("png","pdf"),
               fig.lp = "fig:",
               cache = FALSE,
               echo = FALSE,
               results = "hide", ## not 'markup' or 'asis'
               message = FALSE,
               warning = TRUE,
               dpi = 96)



## ----loadlibs, echo = FALSE, message = FALSE----------------------------------
library(rio)
library(tidyverse)


## ----regtab1, results = "asis"------------------------------------------------
dat <- rio::import("sj-xlsx-1-pol-10.1177_02633957221086879.xlsx",
                   skip = 1,
                   na = c("-", "NA")) |>
    subset(!is.na(`Unique ID`))

m1 <- lm(Attendance ~ Time_Study + Time_Employment + Time_Care +
                   Time_Volunteering + Time_Medical + Time_Sport +
                   Time_Societies + Time_Commute + Time_Under_Studying +
                   Absent_Caring + Absent_Employment + Absent_Health +
                   Absent_Transport + Absent_Interest + Absent_Online,
         data = dat)

m2 <- lm(Mark ~ Time_Study + Time_Employment + Time_Care +
                   Time_Volunteering + Time_Medical + Time_Sport +
                   Time_Societies + Time_Commute + Time_Under_Studying +
                   Absent_Caring + Absent_Employment + Absent_Health +
                   Absent_Transport + Absent_Interest + Absent_Online +
                   Attendance + VLE_Login + VLE_Activity,
         data = dat)



modelsummary(list("Attendance" = m1, "Attainment" = m2),
             gof_omit = 'DF|Deviance|RMSE|F|Log|AIC|BIC',
             title = "Replication of Strong (2002), table 2. Ordinary least squares regression of attendance and attainment. Figures in square brackets are 95 percent confidence intervals. Coefficients discussed in the text are marked in bold. ",
             statistic = "conf.int",
             output = "kableExtra") %>%
    row_spec(c(5:6, 23:24), color = 'red', bold = TRUE)


## ----regtab2, results = "asis"------------------------------------------------
m3 <- lm(Attendance ~ Time_Study + Time_Employment + Time_Care +
                   Time_Volunteering + Time_Medical + Time_Sport +
                   Time_Societies + Time_Commute,
         data = dat)

m4 <- lm(Mark ~ Time_Study + Time_Employment + Time_Care +
                   Time_Volunteering + Time_Medical + Time_Sport +
                   Time_Societies + Time_Commute,
         data = dat)

modelsummary(list("Attendance" = m3, "Attainment" = m4),
             gof_omit = 'DF|Deviance|RMSE|F|Log|AIC|BIC',
             title = "Analyses of attendance and attainment without post-treatment variables.  Ordinary least squares regression of attendance and attainment. Coefficients discussed in the text are marked in bold. ",
             statistic = "conf.int",
             output = "kableExtra") %>%
    row_spec(c(5:6), color = 'red', bold = TRUE)



## ----senseplot, fig = TRUE, fig.cap = "Contour plot showing effect on attendance of hours in employment (contour lines) as a function of different levels of confounding", fig.width = 7, fig.height = 6----
library(sensemakr)
m.sensitivity <- sensemakr(model = m3,
                           benchmark_covariates = "Time_Study",
                           treatment = "Time_Employment",
                           kd = c(1, 3, 5),
                           ky = c(1, 3, 5), 
                           q = 1,
                           alpha = 0.05, 
                           reduce = TRUE)
plot(m.sensitivity, xlab = "Partial R² of X-factor with hours in employment",
     ylab = "Partial R² of X-factor with attendance",
     label.bump.y = -0.005,
     label.bump.x = 0.04)



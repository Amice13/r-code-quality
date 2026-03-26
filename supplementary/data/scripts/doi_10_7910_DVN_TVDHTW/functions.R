library(tidyverse)
library(readxl)
library(pander)
library(scales)
options(stringsAsFactors=FALSE)

########################################
## Prepare functions for income tax. ##
########################################

income_tax <- function(income) {
    x <- taxes[taxes$filing_as == 'married', ]
    diff <- income - x$lower_bound
    diff[diff < 0] <- 0

    above <- !is.na(x$upper_bound) & (income > x$upper_bound)
    diff[above] <- (x$upper_bound - x$lower_bound)[above]

    total <- sum(x$marginal_rate * diff)
    return(total)
}

tax_bill <- function(x) sapply(x, income_tax)

########################################
## Prepare simulation function. ##
########################################

simulate <- function(proportions = c(population = 0.2, costs = 0.8), raw = NULL) {

    ########################################
    ## Focus on households with ESI only. ##
    ########################################
    keep_esi_only <- function(data) {
        keep <- as.logical(data$has_employment_based_insurance)
        data$has_employment_based_insurance <- NULL
        return(data[keep, ])
    }

    #################################################################################
    ## This function is used to split the households into healthy and sick groups. ##
    #################################################################################
    split_population <- function(data) {
        to_merge <- data.frame(
            high_expenditure = c(TRUE, FALSE),
            proportion_of_population = c(proportions["population"], 1 - proportions["population"]),
            proportion_of_costs = c(proportions["costs"], 1 - proportions["costs"]),
            id_col = 1
        )
        data <- data %>%
            mutate(id_col = 1) %>%
            full_join(to_merge, by = "id_col") %>%
            select(-id_col)

        # NOTE: households is originally measured in 1000s of households
        data$households <- data$proportion_of_population * 1000 * data$households

        ## Calculate expenditures by group and return new data frame.
        data %>%
            group_by(income_group) %>%
            mutate(total_pool = sum(households * (employee_contribution + employer_contribution))) %>%
            ungroup() %>%
            mutate(household_cost = proportion_of_costs * total_pool / households) %>%
            filter(households > 0)
    }


    compute_holdback <- function(data) {
        ## If everyone is well we don't need to hold anything back
        if (proportions["population"] == 1 & proportions["costs"] == 1) {
            return(0)
        }

        ## pre: make sure employer and employee contributions are the
        ## same for all rows
        stopifnot(length(unique(data$employer_contribution)) == 1)
        stopifnot(length(unique(data$employee_contribution)) == 1)

        employer <- data$employer_contribution %>% head(1)
        employee <- data$employee_contribution %>% head(1)

        cost <- proportions["costs"]
        prop <- proportions["population"]

        ## price <- employer + employee
        cost_sick <- cost * price / prop
        cost_well <- (1 - cost) * price / (1 - prop)
        holdback <- prop * (cost_sick - cost_well)

        return(holdback)
    }

    #############################################
    ## Perform all the necessary calculations. ##
    #############################################
    if (is.null(raw)) {
        raw <- input_data
    }

    DT <- as_tibble(raw) %>%
        keep_esi_only()

    DT <- split_population(DT)

    DT$holdback <- compute_holdback(DT)

    DT <- DT %>%
        mutate(
            transfer = employer_contribution - holdback,
            income_increase = transfer / (mean_income + transfer / 2),
            price_increase = transfer / (employee_contribution + transfer / 2),
            ## Note: hi_price_pre and hi_price_post are actually
            ## quantities of health insurance purchased..
            hi_price_pre = employer_contribution + employee_contribution,
            hi_price_post = (hi_price_pre) * (1 + income_increase * income_elasticity + price_increase * price_elasticity),
            ## force households to purchase at least the holdback.
            hi_price_post = ifelse(hi_price_post < holdback, holdback, hi_price_post),
            new_taxable_income = hi_price_pre - hi_price_post,
            ## Note: the govt_transfer column is the payroll taxes collected from
            ## employee and employer
            govt_transfer = 2 * input_payroll_taxes * new_taxable_income,
            mean_income_pre=mean_income,
            mean_income_post=mean_income + new_taxable_income
        )
    DT <- as.data.frame(DT)

    income_0 <- DT$mean_income
    tax_0 <- tax_bill(income_0)

    income_1 <- income_0 + DT$new_taxable_income
    tax_1 <- tax_bill(income_1)

    DT$new_tax_revenue <- (tax_1 - tax_0) * DT$households

    after_tax_total_0 <- (income_0 - tax_0) * DT$households
    after_tax_total_1 <- (income_1 - tax_1) * DT$households

    DT$income_increase2 <- ((income_1 - tax_1) - (income_0 - tax_0)) / (income_0 - tax_0)

    DT$total_after_tax_income_change <- after_tax_total_1 - after_tax_total_0

    ## Add columns for summary stats
    DT$income_tax_pre <- tax_0
    DT$income_tax_post <- tax_1
    DT$payroll_tax_pre <- 2 * input_payroll_taxes * income_0
    DT$payroll_tax_post <- 2 * input_payroll_taxes * income_1
    DT$after_tax_income_pre <- income_0 - tax_0 - input_payroll_taxes * income_0
    DT$after_tax_income_post <- income_1 - tax_1 - input_payroll_taxes * income_1

    dollars <- sum(DT$new_tax_revenue) / 1e9

    pct_after_tax_increase <- DT %>%
        mutate(
            take_home_0 = mean_income_pre - tax_bill(mean_income_pre),
            take_home_1 = mean_income_post - tax_bill(mean_income_post)
        ) %>%
        summarise(
            pct = (sum(households * take_home_1) - sum(households * take_home_0)) / sum(households * take_home_0)
        ) %>%
        pull(pct)

    result <- list(
        input = raw,
        proportions = proportions,
        output = DT,
        'New after-tax household income (billions): $' = round(sum(DT$total_after_tax_income_change) / 1e9),
        'New US tax revenues (billions): $' = round(dollars),
        pct_after_tax_increase = pct_after_tax_increase
    )

    return(result)
}

########################################
## Prepare income table function. ##
########################################

income_table <- function(simulation) {
    data2 <- simulation$output

    ids <- c("income_group", "high_expenditure")
    stopifnot(nrow(unique(data2 %>% select(!!!ids))) == nrow(data2))

    stats <- data2 %>%
        mutate(
            row = row_number()
        ) %>%
        group_by(income_group) %>%
        summarise(
            row = max(row) / 2,
            average_0 = sum(households * after_tax_income_pre) / sum(households),
            average_1 = sum(households * after_tax_income_post) / sum(households),
            total_0 = sum(households * after_tax_income_pre) / 1e9,
            total_1 = sum(households * after_tax_income_post) / 1e9
        ) %>%
        ungroup() %>%
        arrange(row) %>%
        select(-row) %>%
        mutate(
            average_pct_chg = (average_1 - average_0) / average_0,
            total_chg = total_1 - total_0,
            pct_of_total_chg = total_chg / sum(total_chg)
        )

    totals <- data2 %>%
        summarise(
            average_0 = sum(households * after_tax_income_pre) / sum(households),
            average_1 = sum(households * after_tax_income_post) / sum(households),
            total_0 = sum(households * after_tax_income_pre) / 1e9,
            total_1 = sum(households * after_tax_income_post) / 1e9
        ) %>%
        mutate(
            average_pct_chg = (average_1 - average_0) / average_0,
            total_chg = total_1 - total_0,
            pct_of_total_chg = total_chg / sum(total_chg)
        ) %>%
        mutate(income_group = "All Incomes")

    stats <- bind_rows(stats, totals)

    # format cells
    cols <- c(
        "Income Group" = "income_group",
        "Average, Before" = "average_0",
        "Average, After" = "average_1",
        "Average, % Change" = "average_pct_chg",
        "Total, Before" = "total_0",
        "Total, After" = "total_1",
        "Total, Change" = "total_chg",
        "% of Total Change" = "pct_of_total_chg"
    )
    stats <- stats %>%
        mutate(
            average_0 = dollar(average_0),
            average_1 = dollar(average_1),
            total_0 = dollar(total_0),
            total_1 = dollar(total_1),
            total_chg = dollar(total_chg),
            average_pct_chg =  scales::percent(average_pct_chg),
            pct_of_total_chg = scales::percent(pct_of_total_chg)
        ) %>%
        select(!!!cols)

    concentration <- paste(scales::percent(simulation$proportions), collapse = "/")
    price_elasticity <- ifelse(all(simulation$output$price_elasticity == old_input$price_elasticity), as.character(old_input$price_elasticity), "Baseline")
    caption <- paste0("After-Tax Income. Totals reported in billions of dollars. Cost Concentration = ", concentration, ", Price Elasticity = ", price_elasticity)

    pander(stats, split.tables = Inf, caption = caption)
}

########################################
## Prepare summary table function. ##
########################################
summarytable <- function(proportions, caption) {
    
    n3 <- simulate(proportions = proportions)
    
    o3 <- simulate(proportions = proportions, raw = old_input)
    
    a <- n3$output
    a$price_elasticity <- "Baseline"
    b <- o3$output
    b$price_elasticity <- as.character(old_input$price_elasticity)
    raw_data <- bind_rows(a, b)
    
    ## HACK: to estimate reduction in medical care expenses
    raw_data$med_expenses_pre <- input_medicalcareexpenses * raw_data$hi_price_pre
    raw_data$med_expenses_post <- input_medicalcareexpenses * raw_data$hi_price_post
    
    cols <- c(
        "price_elasticity",
        "households",
        "hi_price_pre",
        "hi_price_post",
        "after_tax_income_pre",
        "after_tax_income_post",
        "income_tax_pre",
        "income_tax_post",
        "payroll_tax_pre",
        "payroll_tax_post",
        "med_expenses_pre",
        "med_expenses_post"
    )
    
    averages <- raw_data %>%
        select(!!!cols) %>%
        gather(key, value, 3:12) %>%
        group_by(price_elasticity, key) %>%
        summarise(average = sum(households * value) / sum(households)) %>%
        mutate(key = sub("after_tax", "aftertax", key)) %>%
        separate(key, into = c("k1", "k2", "time")) %>%
        mutate(key = paste(k1, k2, sep = "_")) %>%
        select(-k1, -k2) %>%
        spread(key = time, value = average) %>%
        mutate(change = (post - pre) / pre) %>%
        mutate(Level = "Household")
    
    totals <- raw_data %>%
        select(!!!cols) %>%
        gather(key, value, 3:12) %>%
        group_by(price_elasticity, key) %>%
        summarise(total = sum(households * value)) %>%
        mutate(key = sub("after_tax", "aftertax", key)) %>%
        separate(key, into = c("k1", "k2", "time")) %>%
        mutate(key = paste(k1, k2, sep = "_")) %>%
        select(-k1, -k2) %>%
        spread(key = time, value = total) %>%
        mutate(change = (post - pre) / pre) %>%
        mutate(Level = "Total")
    
    combined <- bind_rows(averages, totals)
    
    labels <- read_csv(trimws('
key , label
aftertax_income , After-Tax Income
hi_price        , Health Insurance Premiums
income_tax      , Income Tax
payroll_tax     , Payroll Tax
med_expenses    , Medical Care Expenses
'))
    
    to_select <- c(
        "Price Elasticity" = "price_elasticity",
        "Level" = "Level",
        "Variable" = "label",
        "Before" = "pre",
        "After" = "post",
        "Change" = "change"
    )
    
    combined %>%
        left_join(labels, by = "key") %>%
        arrange(desc(price_elasticity), Level, label) %>%
        mutate(pre = dollar(pre), post = dollar(post), change = percent(change, accuracy = .1)) %>%
        select(!!!to_select) %>%
        pandoc.table(split.table = Inf, caption = caption)
    
}


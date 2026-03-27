
build_bca_appendix_table <- function(list_of_bca_tables) {

  list_of_bca_tables %>%
    reduce(left_join) %>%
    mutate(violence_type = str_to_title(str_remove(violence_type, "s$")),
           across(ends_with("rate"), ~ round(.x, 2)),
           across(ends_with("cost"), ~ round(.x, 0)),
           across(ends_with("cost"), ~ prettyNum(.x, big.mark = ",", preserve.width = "none"))) %>%
    select(-c(starts_with("reporting_rate_"))) %>%
    rename(crime_type = violence_type,
           wtp_cost = wtp_estimate_x_cj_x_offender_productivity_cost,
           reporting_rate = average_reporting_rate) %>%
    relocate(bottom_up_cost, .before = wtp_cost)

}

print_bca_appendix_table <- function(list_of_bca_tables) {

  list_of_bca_tables %>%
    build_bca_appendix_table() %>%
    kable(booktabs=T, longtable=T, linesep = "",
          col.names = c("Crime Type",
                        "Victim Costs", "Legal System Costs", "Offender Productivity Costs",
                        "Total Bottom-Up Costs", "Willingness-to-Pay Costs",
                        "Estimated Clearance Rate", "Estimated Reporting Rate"),
          align=c("l", rep("c", 7))) %>%
    kable_styling(font_size = 11) %>%
    add_header_above(c(" " = 1, "Bottom-Up Components" = 3), font_size = "11") %>%
    column_spec(1, width="9.5em", latex_valign = 'm') %>%
    column_spec(2:5, width="5.7em", latex_valign = 'm') %>%
    column_spec(6, width="8.3em", latex_valign = 'm') %>%
    column_spec(7:8, width="5em", latex_valign = 'm')

}

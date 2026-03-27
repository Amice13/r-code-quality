

# Function to programmatically barcharts:

plot.one.variable <- function(.data, x_var, fill_var = NULL, pos="identity", orientation="vertical", leg.pos = "bottom", title = "My bar chart", subtitle = " ", x.title = " ", y.title = " ", show_bar_count=FALSE){
  
  fill_var <- enquo(fill_var)
  
  if (dplyr::is_grouped_df(.data)) {
    return(dplyr::do(.data, plot.one.variable(.)))
  }
  
  p <-
    if(is.null( {{fill_var}} )){
      .data %>% 
        ggplot(aes({{x_var}})) 
    } else {
      .data %>% 
        ggplot(aes({{x_var}}, fill = !!fill_var))
    }
  
  plot <- p +
    geom_bar(position=pos)+
    general_graphs_options + 
    theme(legend.position=leg.pos) +
    labs(title=title,
         subtitle=subtitle,
         x = x.title,
         y = y.title)
  
  if(orientation == "horizontal"){
    plot <- plot + coord_flip()
  }
  if(show_bar_count){
    plot <- plot + geom_text(stat = "count", aes(label= after_stat(count), vjust=-0.2)) 
  }
  return(plot)
}

# Function for confidence intervals plots:

ci_plot <- function(.data, var_1, var_2, mean_var, title = "My ci plot", subtitle = ""){
  
  var_1 <- enquo(var_1)
  var_2 <- enquo(var_2)
  mean_var <- enquo(mean_var)
  
  .data %>% 
    group_by(!!var_1, !!var_2) %>%
    summarise(
      mean_value = mean(!!mean_var),
      sd_value = sd(!!mean_var),
      n = n(),
      se = sd_value / sqrt(n), # standard error
      ci_lower = mean_value - qt(1 - 0.05 / 2, n - 1) * se, # 95% CI lower bound
      ci_upper = mean_value + qt(1 - 0.05 / 2, n - 1) * se  # 95% CI upper bound
    ) %>% 
    ggplot(aes(x = !!var_1, y = mean_value, shape = !!var_2)) +
    geom_point(size=2) +
    facet_wrap(vars(!!var_2)) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1 ) +
    general_graphs_options + 
    theme(strip.placement = "outside") +
    labs(title=title,
         subtitle = subtitle,
         x="",
         y="") 
}

# Load the necessary library
library(ggplot2)

# Define the function
plot_bars <- function(data, x_var, fill_var = NULL) {
  # Check if fill_var is provided
  if (is.null(fill_var)) {
    # Create the ggplot object for the single variable bar chart
    p <- ggplot(data, aes_string(x = x_var)) +
      geom_bar() +
      labs(title = paste("Bar Chart of", x_var),
           x = x_var, y = "Count") +
      theme_minimal()
  } else {
    # Create the ggplot object for the stacked bar chart
    p <- ggplot(data, aes_string(x = x_var, fill = fill_var)) +
      geom_bar(position = "stack") +
      labs(title = paste("Stacked Bar Chart of", x_var, "filled by", fill_var),
           x = x_var, y = "Count") +
      theme_minimal()
  }
  
  # Return the plot
  return(p)
}

# Example usage
# Create a sample data frame
df <- data.frame(
  category1 = sample(letters[1:5], 100, replace = TRUE),
  category2 = sample(letters[6:10], 100, replace = TRUE)
)

# Plot single variable bar chart
single_var_plot <- plot_bars(df, "category1")
print(single_var_plot)

# Plot stacked bar chart
stacked_bar_plot <- plot_bars(df, "category1", "category2")
print(stacked_bar_plot)

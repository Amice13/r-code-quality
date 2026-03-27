#######################################################################################################
# Create contact matrix (use the socialmixr package)
#######################################################################################################

rm(list = ls())
source("s-base_packages.R")
library(socialmixr)
library(pomp)
library(cowplot) #to centre legend

# Get POLYMOD data and create contact matrix -------------------------------------------------------
# Take POLYMOD matrix from the UK
age_limits <- seq(0, 70, by = 5) # Age limits for age categories
Ntot <- 1e7

data(polymod)
survey_countries(polymod)
#> Using POLYMOD social contact data. To cite this in a publication, use the 'cite' function
#> [1] "Italy"          "Germany"        "Luxembourg"     "Netherlands"   
#> [5] "Poland"         "United Kingdom" "Finland"        "Belgium"

CM_uk <- bake(file = "_data/contact_matrix_UK_symm2.rds", 
               expr = {
                 contact_matrix(polymod, 
                                countries = "United Kingdom",  
                                age.limits = age_limits,
                                survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                                symmetric = T)
               })
stopifnot(isSymmetric(object = CM_all$matrix, check.attributes = F))
CM <- CM_uk$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_uk_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_uk_long$country <- "United Kingdom"

CM_italy <- bake(file = "_data/contact_matrix_italy_symm2.rds", 
              expr = {
                contact_matrix(polymod, 
                               countries = "Italy",  
                               age.limits = age_limits,
                               survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                               symmetric = T)
              })
CM <- CM_italy$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_italy_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_italy_long$country <- "Italy"

CM_lux <- bake(file = "_data/contact_matrix_lux_symm2.rds", 
                 expr = {
                   contact_matrix(polymod, 
                                  countries = "Luxembourg",  
                                  age.limits = age_limits,
                                  survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                                  symmetric = T)
                 })
CM <- CM_lux$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_lux_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_lux_long$country <- "Luxembourg"

CM_nl <- bake(file = "_data/contact_matrix_nl_symm2.rds", 
               expr = {
                 contact_matrix(polymod, 
                                countries = "Netherlands",  
                                age.limits = age_limits,
                                survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                                symmetric = T)
               })
CM <- CM_nl$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_nl_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_nl_long$country <- "Netherlands"

CM_pol <- bake(file = "_data/contact_matrix_pol_symm2.rds", 
              expr = {
                contact_matrix(polymod, 
                               countries = "Poland",  
                               age.limits = age_limits,
                               survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                               symmetric = T)
              })
CM <- CM_pol$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_pol_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_pol_long$country <- "Poland"

CM_fin <- bake(file = "_data/contact_matrix_fin_symm2.rds", 
               expr = {
                 contact_matrix(polymod, 
                                countries = "Finland",  
                                age.limits = age_limits,
                                survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                                symmetric = T)
               })
CM <- CM_fin$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_fin_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_fin_long$country <- "Finland"

CM_be <- bake(file = "_data/contact_matrix_be_symm2.rds", 
               expr = {
                 contact_matrix(polymod, 
                                countries = "Finland",  
                                age.limits = age_limits,
                                survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                                symmetric = T)
               })
CM <- CM_be$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_be_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_be_long$country <- "Belgium"

# Get contact matrices from other studies
# Apparently the German survey is wrong, let's replace Germany by France for example 
france_survey <- get_survey("https://doi.org/10.5281/zenodo.1157918")
saveRDS(france_survey, "france.rds")
CM_france <- bake(file = "_data/contact_matrix_france2.rds", 
               expr = {
                 contact_matrix(france_survey,
                                age.limits = age_limits,
                                survey.pop = data.frame(lower.age.limit = age_limits, population = rep(Ntot / 15, 15)),
                                symmetric = T)
               })
CM <- CM_france$matrix
rownames(CM) <- colnames(CM)
n_ages <- nrow(CM)
CM_france_long <- CM %>% melt(varnames = c("age1", "age2"), value.name = "contacts")
CM_france_long$country <- "France"

# Merge all the contact matrices per country
CM_all <- rbind(CM_uk_long, CM_italy_long, CM_lux_long, CM_nl_long, CM_pol_long, CM_fin_long, CM_be_long, CM_france_long)
head(CM_all)

# Plot matrix; x-axis: contact age, y-axis: reporter age
# Useful information at: https://rud.is/b/2016/02/14/making-faceted-heatmaps-with-ggplot2/
theme_set(theme_classic(base_size = 12) + theme(panel.grid.minor = element_blank()))
pl_contact <- ggplot(data = CM_all, mapping = aes(x = age2, y = age1, fill = contacts)) + 
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "red") + #The French maximum is slightly higher than that in the Polymod study, but that is okay for the sensitivity analysis
  #theme(legend.position = "top") + 
  labs(x = "Contact age", y = "Reporter age", fill = "Daily \ncontacts") +
  facet_wrap(~country, ncol=2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(legend.title.align=0.5) +
  theme(strip.text = element_text(size=10, margin = margin(.1, 0, .1, 0, "cm")), strip.background = element_blank())
pl_contact_legend <- ggdraw(align_legend(pl_contact)) #see function below
print(pl_contact_legend )
ggsave(filename = "_figsms4/FigS11_contact_matrices.pdf",  plot=pl_contact_legend, width = 8, height = 9)

# Function to align legend in centre
align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]
  
  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")
  
  # there can be multiple guides within one legend box  
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]
    
    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2
    
    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }
  
  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}










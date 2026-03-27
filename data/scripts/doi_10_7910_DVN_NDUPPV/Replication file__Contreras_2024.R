
#-------------------------------------------------------------------------------
# Article: Outsourcing Machines: How Programmatic Parties Include Clientelistic Strategies 
# Author: Gonzalo Contreras Aguirre
# Journal: Comparative Political Studies
# Version: 21-05-24

# Load the packages required to reproduce the results presented in the article: 

# Packages (and versions)
library(ggplot2) # v. 3.5.0
library(dplyr) # v. 1.1.4
library(haven) # v. 2.5.4
library(psych) # v. 2.4.3
library(rnaturalearth) # v. 1.0.1

# Data from Expert Surveys in 18 Latin American Countries, 2018-2019. PREPPS Latam V2:
'Wiesehomeier, Nina; Singer, Matthew; Ruth-Lovell, Saskia, 2021, "Political Representation, 
# Executives, and Political Parties Survey: Data from Expert Surveys in 18 Latin American Countries, 
# 2018-2019. PREPPS Latam V2", https://doi.org/10.7910/DVN/JLOYIJ, Harvard Dataverse, V1, UNF:6:tVgxOYC2L9vl8fAFMFjNkA== [fileUNF]'

# Data available on Harvard Dataverse: https://dataverse.harvard.edu/file.xhtml?fileId=4372420&version=1.0&toolType=PREVIEW

setwd() # Set directory
PREPPS_Latam_V2 <- read_dta("PREPPS Latam V2.dta") # Import dataset from the file's directory

# Describe dataset: 
names(PREPPS_Latam_V2)
View(PREPPS_Latam_V2)
dim(PREPPS_Latam_V2)
head(PREPPS_Latam_V2)

# Subset the original dataframe keeping variables of interest
Prog <- PREPPS_Latam_V2 %>%
  select(country, partyname_es, dimensionid, score_pos, sd_pos, n_pos, score_imp,  sd_imp, n_imp) %>%
  filter(dimensionid == 51) %>%
  filter(n_pos > 4) # I followed Wiesehomeier et al. (2021) suggestion to exclude all the observations (parties)
                    # with less than 4 valid ratings. In doing so, El Salvador, Honduras, Panama, Nicaragua and
                    # Guatemala were dropped.
  
# Check subset
View(Prog)
dim(Prog)
names(Prog)
table(Prog$country)



### FIGURE A.1. (Supplementary Material): Box plot Programmatic linkages scores in Latin American countries

P <- ggplot(Prog, aes(x = reorder(country, -score_pos), y = score_pos)) # ordered from higher to lower mean

P + 
  geom_boxplot(outlier.shape = NA) + # excluiding outliers
  stat_summary(fun.y="mean") + # Great. You included the mean with a black dot inside the box.
  labs(x = "Country",
       y = "Programmatic score",
       title = "Programmatic linkages score by countries",
       caption = "Data: PREPPS Latina America. Wiesehomeier, Nina, Matthew Singer, and Saskia Ruth-Lovell (2021)") +
       theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
             plot.caption = element_text(hjust = 0)) 
  

# If you want to add data points, include geom_jitter in the chunk above. See this link: https://r-graph-gallery.com/89-box-and-scatter-plot-with-ggplot2.html
geom_jitter(width = 0.1) 


# Use DescribeBy to get a summary by country
describeBy(Prog$score_pos, group = Prog$country, mat = TRUE) # mat argument to obtain the output in a matrix. 

# To obtain the mean by country. For example, Chile and Uruguay.
Prog %>%
  filter(country %in% c("Chile", "Uruguay")) %>%
  group_by(country) %>%
  summarize(mean_score_pos = mean(score_pos, na.rm = TRUE))


### FIGURE 2 (Main document): Programmatic scores map in Latin America:

# Get the spatial data worldwide from "rnaturalearth" package 
worldwide <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

# For more information about "ne_countries" function from "rnaturalearth" package:
'https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html'

# Explore spatial dataset
View(worldwide)
list(worldwide$sovereignt)
View(countries110) # this provides the number of countries (177)

table(worldwide$region_wb) # Number of countries by region (includes Latin America & Caribbean)

# Get the spatial data for South America
south_america <- ne_countries(
  continent = "South America",
  returnclass = "sf"
)

# Get the spatial data for Central America
central_america <- ne_countries(
  country = c("Belize", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panama"),
  returnclass = "sf"
)

# Combine the spatial data for South America and Central America
south_central_america <- rbind(south_america, central_america)

# Plot the map of South and Central America
plot(south_central_america)

View(south_central_america)
list(south_central_america$sovereignt)

# Create a ne vaeiable with "programmatic score" from PREPPS:
values <- c(2.80, 3.33, NA, 3.35, 2.80, 2.73, 2.64, 3.02, 2.88, NA, NA, 2.85, 2.93, 2.82, NA, 2.88, NA, NA, NA, NA, NA)

# The map:
map_plot <- ggplot() +
  geom_sf(data = south_central_america, aes(fill = values)) +
  theme_minimal() +
  labs(fill = "Programmatic score")

# Map as reported in the main document:
map_plot <- ggplot() +
  geom_sf(data = south_central_america, aes(fill = values), show.legend = TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient(name = "Programmatic score"
  ) +
  geom_sf(data = south_central_america[south_central_america$name == "Chile", ], fill = NA, color = "red", size = 1) + # To paint Chile
  labs(title = "Programmatic linkages score in Latin American countries",
       caption = "Data: PREPPS Latina America. Wiesehomeier, Nina, Matthew Singer, and Saskia Ruth-Lovell (2021)")
   

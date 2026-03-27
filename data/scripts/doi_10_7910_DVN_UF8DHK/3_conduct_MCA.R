install.packages("Factominer",
                "factoextra",
                "tidyverse",
                "ggplot2",
                "readxl",
                "psych")

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggplot2)
library(readxl)
library(psych)

sessionInfo()
# Load Data ---------------------------------------------------------------
root <- "~/"
path <- paste0(root,"mca_ready_data/")

setwd(path)
df <- read_excel("fragments_architecture_combined.xlsx")

# select data -------------------------------------------------------------
df %>%
  colnames()

df <- df %>%
  mutate(
    region_code = case_when(region_code == 0 ~ 0,
                            region_code == 1 ~ 1,
                            region_code == 2 ~ 2,
                            region_code == 3 ~ 3,
                            region_code >= 4 ~ 4)
           )

mca_df <- df %>%
  select("language",
         "genre",
         "prose",
         "verse",
         "material",
         "medium",
         "region",
         "monks_monastary",
         "gods_buddha",
         "demons_spirits",
         "heaven",
         "hell",
         "royalty",
         "ministers_public_servants","military",
         "settlement",
         "housing",
         "landscapes",
         "travel",
         "economy",
         "signum",
         "householder",
         "jewels_methaphors",     
         "worldly_goods",
         "region_code",
         "prim_trans",
         "idno") %>%
  na.omit()


# add variable for the (grouped) #observations ----------------------------
region_grouped <- mca_df %>%
  group_by(region) %>%
  count()

region_grouped <- region_grouped %>%
  mutate(no_sources = case_when(n <= 5 ~ "<=5",
                                n >= 5 & n<= 10 ~ "6-10",
                                n > 10 & n<= 100 ~ "11-100",
                                n > 100 ~ ">100")) %>%
  select(region,no_sources)

mca_df <- left_join(mca_df,
          region_grouped, 
          by="region")

mca_df %>% 
  select(material) %>%
  unique()

prim_trans <- mca_df %>%
  select(prim_trans, idno)

mca_df <- mca_df %>%
  select(-prim_trans, -idno)

# get descritptives for mca -----------------------------------------------
describe(mca_df)
table(as.integer(mca_df$region_code))


for(name in mca_df %>%
    colnames){
  print(name)
  mca_df[[name]] <- factor(mca_df[[name]])
}

# Conduct MCA -------------------------------------------------------------
res <- MCA(mca_df, 
           quali.sup = c(7,25,26),
           ncp=3, 
           graph = FALSE)
summary(res)

## get variables with high contribution on axes
variable_names <- rownames(res$var$contrib)
contributions <- as_tibble(res$var$contrib)
contributions <- contributions %>%
  mutate("variables" = variable_names, .before = `Dim 1`)

number_of_variables <- ncol(mca_df)-1

## get the variables with contributions greater than mean
dim1_variables <- contributions %>%
  filter(`Dim 1` > 1/number_of_variables)
dim2_variables <- contributions %>%
  filter(`Dim 2` > 1/number_of_variables)
dim3_variables <- contributions %>%
  filter(`Dim 3` > 1/number_of_variables)

## get the texts with the highest contributions on the dimensions
text_contribs <- as_tibble(res$ind$contrib)
text_contribs <- cbind(text_contribs, prim_trans)

text_cordinates <- as_tibble(res$ind$coord)
text_cordinates <- cbind(text_cordinates, prim_trans)

dim1_contribs <- text_contribs %>%
  arrange(desc(`Dim 1`)) %>%
  head(n=10)

dim2_contribs <- text_contribs %>%
  arrange(desc(`Dim 2`)) %>%
  head(n=10)

dim3_contribs_upper <- text_cordinates %>%
  arrange(desc(`Dim 3`)) %>%
  head(n=10)

dim3_contribs_lower <- text_cordinates %>%
  arrange(`Dim 3`) %>%
  head(n=10)

## get a screeplot
scree <- fviz_screeplot(res)
scree <- scree + theme_bw()
scree

## create biplots
dim_1_2 <- fviz_mca_var(res, 
                        repel = T,
                        labelsize = 3) +
        theme(
        axis.title = element_text(size = 7.5),
        axis.text = element_text(size = 7.5))


dim_1_3 <- fviz_mca_var(res, 
                        repel = T,
                        axes = c(1,3),
                        labelsize = 3) +
  theme(
    axis.title = element_text(size = 7.5),
    axis.text = element_text(size = 7.5))

dim_2_3 <- fviz_mca_var(res, 
                        repel = T,
                        axes = c(2,3),
                        labelsize = 3) +
  theme(
    axis.title = element_text(size = 7.5),
    axis.text = element_text(size = 7.5))

## set colour theme for plots
dim_1_2 <- dim_1_2 + 
  theme_bw()
dim_1_3 <- dim_1_3 + 
  theme_bw()
dim_2_3 <- dim_2_3 + 
  theme_bw()


# Save graphics -----------------------------------------------------------
ggsave(plot = scree, 
       filename = "screeplot_with_region_code.png",
       width = 18, 
       height = 16,
       units = "cm",
       bg='white',
       dpi = 600)

ggsave(plot = dim_1_2, 
       filename = "dimensions1_2_with_region_code.png",
       width = 18, 
       height = 16,
       units = "cm",
       bg='white',
       dpi = 600)

ggsave(plot = dim_1_3, 
       filename = "dimensions1_3_with_region_code.png",
       width = 18, 
       height = 16,
       units = "cm",
       bg='white',
       dpi = 600)

ggsave(plot = dim_2_3, 
       filename = "dimensions2_3_with_region_code.png",
       width = 18, 
       height = 16,
       units = "cm",
       bg='white',
       dpi = 600)


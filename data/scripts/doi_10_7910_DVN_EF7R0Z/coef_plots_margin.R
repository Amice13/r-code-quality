
# Produces Figure 4 and appendix Figure 3
# Requires:
#   - full.Rdata
#   - country_aggregated.Rdata
# Produces:
#   - coef_plot_margin.pdf
#   - coef_plot_margin_appendix.pdf


# If not installed already
# install.packages("dplyr","dotwhisker","ggplot2","broom","lfe")

# please set your working directory to the /code folder in the parent directory which also contains the /data and /figures folders
# setwd()

library(dplyr)
require(dotwhisker)
library(ggplot2)
require(broom)
library(lfe)

#### FULL ####
# Paper Plots
load("../data/output/full.Rdata")
load("../data/output/country_aggregated.Rdata")


#### Margin ####

to_include = c("margin.norm","Polity_class","margin.norm:Polity_class","pr","margin.norm:pr")
full_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|FID|0|un + year, data=full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "All")
full_m1_pr <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class + margin.norm*pr|FID|0|un + year, data=full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")

full_Nat <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|un|0|un + year, data=country_full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "All")
full_Nat_pr <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class + margin.norm*pr|un|0|un + year, data=country_full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")

#### TROPICAL ####
print("Tropical")
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","tropical mountain system","Subtropical humid forest","Tropical shrubland")
full <- full %>% filter(GEZ_TERM %in% tropical_sub)

load("../data/output/country_aggregated_tropical.Rdata")

country_full <- country_full_tropical

#### Margin ####
tropical_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|FID|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_m1_pr <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class + margin.norm*pr|FID|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")

tropical_Nat <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|un|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_Nat_pr <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class + margin.norm*pr|un|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")



#### NONTROPICAL ####
load("../data/output/full.Rdata")


full <- full %>% filter(!(GEZ_TERM %in% tropical_sub))

load("../data/output/country_aggregated_nontropical.Rdata")
country_full<-country_full_nontropical

#### Margin ####
nontropical_m1 <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|FID|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_m1_pr <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class + margin.norm*pr|FID|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")

nontropical_Nat <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class|un|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_Nat_pr <- felm(forest.diff ~ forest.l + PCGDP.l + PCGDP.change.l + Pop.growth.l + margin.norm*Polity_class + margin.norm*pr|un|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")

data<-rbind(full_m1,full_m1_pr,full_Nat,full_Nat_pr,
      tropical_m1,tropical_m1_pr,tropical_Nat,tropical_Nat_pr,nontropical_m1,nontropical_m1_pr,nontropical_Nat,nontropical_Nat_pr)
save(data,
     file = "../data/output/margin_models_results.Rdata")

#### PRODUCES FIGURE 6 ####

# function for a smaller legend
addSmallLegend <- function(myPlot, pointSize = 0.6, textSize = 6, spaceLegend = 0.05) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_blank(),
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

load("../data/output/margin_models_results.Rdata")

all_models<-data %>% 
  mutate(coef = term) %>%
  mutate(model = system) %>%
  mutate(term = paste(term,level,sep = ", ")) %>%
  filter(coef %in% c("margin.norm")) %>%
  filter(forest == "full") %>% arrange(desc(model))
all_models$model <- factor(all_models$model, 
                           levels = c("All","Majoritarian"), 
                           labels = c("All","Majoritarian"))

plot <- dwplot(na.omit(all_models),vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(aes(shape = model)),
               whisker_args = list(aes(linetype = model))) %>% 
  relabel_predictors(c("margin.norm, cell" = "Competitiveness, Cell",                       
                       "margin.norm, country" = "Competitiveness, National")) + 
  # facet_grid(.~ forest, drop=TRUE, space = "free")  +
  theme_classic() + xlab("pp forest change per election year, 1 point decrease in margin") + ylab("") +
  scale_colour_grey(start = .1, end = .1, # if start and end same value, use same colour for all models 
                    name = "Model", 
                    breaks = c(0, 1),
                    labels = c("Majoritarian", "All")) + 
  # ggtitle("Relationship between democratic transitions and forest cover change") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.02, 0.01),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="transparent"),
        legend.title.align = .5,
        legend.title = element_blank())
plot
plot<-addSmallLegend(plot)
ggsave(file="../figures/coef_plot_margin.pdf",plot = plot,device = "pdf",width = 7,height = 2,units = "in")


#### PRODUCES APPENDIX FIGURE 3 ####
all_models<-data %>%
  mutate(coef = term) %>%
  mutate(model = system) %>%
  mutate(term = paste(term,level,sep = ", ")) %>%
  filter(coef %in% c("margin.norm"))

plot <- dwplot(na.omit(all_models),vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(aes(shape = model)),
               whisker_args = list(aes(linetype = model))) %>%
  relabel_predictors(c("margin.norm, cell" = "Competitiveness, Cell",
                       "margin.norm, country" = "Competitiveness, National")) +
  facet_grid(.~ forest, drop=TRUE, space = "free")  +
  theme_bw() + xlab("pp forest change per election year, 1 point decrease in margin") + ylab("") +
  scale_colour_grey(start = .1, end = .1, # if start and end same value, use same colour for all models 
                    name = "Model", 
                    breaks = c(0, 1),
                    labels = c("Majoritarian", "All")) + 
  # ggtitle("Relationship between democratic transitions and forest cover change") +
  theme(plot.title = element_text(face="bold"),
        legend.position = c(0.02, 0.01),
        legend.justification = c(0, 0),
        legend.background = element_rect(colour="grey80"),
        legend.title.align = .5)
plot <-addSmallLegend(plot)

ggsave(file="../figures/coef_plot_margin_appendix.pdf",plot = plot,device = "pdf",width = 7,height = 3,units = "in")


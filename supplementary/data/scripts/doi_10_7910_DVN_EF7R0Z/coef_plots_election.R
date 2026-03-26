# Paper Plots
# Produces Figure 3 and appendix Figure 2
# Requires:
#     - full.Rdata
#     - country_aggregated.Rdata
# Produces:
#     - coef_plot_election.pdf
#     - coef_plot_election_appendix.pdf

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


load("../data/output/full.Rdata")

load("../data/output/country_aggregated.Rdata")

# terms to include
to_include = c("election_DPI","close80","close90","election_DPI:pr","close80:pr","close90:pr")



#### Election ####
print("election")
full_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + year|0|un + year, data=full)%>% 
    tidy() %>%filter(term %in% to_include) %>%  mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "All")
full_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + year|0|un + year, data=full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "All")
full_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + year|0|un + year, data=full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "All")
full_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "All")
full_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "All")
full_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)%>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "All")

full_m1_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class + election_DPI*pr|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")
full_m2_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class + close80*pr|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")
full_m3_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class + close90*pr|FID + year|0|un + year, data=full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "cell") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")
full_Nat_m1_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class + election_DPI*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")
full_Nat_m2_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class + close80*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")
full_Nat_m3_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class + close90*pr|un + year|0|un + year, data=country_full) %>% 
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "country") %>% mutate(forest = "full") %>% mutate(system = "Majoritarian")

#### TROPICAL ####
print("Tropical")
tropical_sub=c("Tropical rainforest","Subtropical mountain system","Subtropical dry forest","Tropical dry forest","Tropical moist deciduous forest","tropical mountain system","Subtropical humid forest","Tropical shrubland")
full <- full %>% filter(GEZ_TERM %in% tropical_sub)

load("../data/output/country_aggregated_tropical.Rdata")
country_full <- country_full_tropical

#### Election ####
print("election")
tropical_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + year|0|un + year, data=full)%>%
    tidy() %>%filter(term %in% to_include) %>%  mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + year|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + year|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "All")
tropical_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "All")

tropical_m1_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class + election_DPI*pr|FID + year|0|un + year, data=full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")
tropical_m2_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class + close80*pr|FID + year|0|un + year, data=full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")
tropical_m3_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class + close90*pr|FID + year|0|un + year, data=full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "cell") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")
tropical_Nat_m1_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class + election_DPI*pr|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")
tropical_Nat_m2_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class + close80*pr|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")
tropical_Nat_m3_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class + close90*pr|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "country") %>% mutate(forest = "tropical") %>% mutate(system = "Majoritarian")


#### NONTROPICAL ####

load("../data/output/full.Rdata")
full <- full %>% filter(!(GEZ_TERM %in% tropical_sub))

load("../data/output/country_aggregated_nontropical.Rdata")
country_full<-country_full_nontropical

#### Election ####
print("election")
nontropical_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|FID + year|0|un + year, data=full)%>%
    tidy() %>%filter(term %in% to_include) %>%  mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|FID + year|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|FID + year|0|un + year, data=full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_Nat_m1 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class|un + year|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_Nat_m2 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class|un + year|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "All")
nontropical_Nat_m3 <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class|un + year|0|un + year, data=country_full)%>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "All")

nontropical_m1_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class + election_DPI*pr|FID + year|0|un + year, data=full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")
nontropical_m2_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class + close80*pr|FID + year|0|un + year, data=full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")
nontropical_m3_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class + close90*pr|FID + year|0|un + year, data=full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "cell") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")
nontropical_Nat_m1_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + election_DPI + Polity_class + election_DPI:Polity_class + election_DPI*pr|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "All") %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")
nontropical_Nat_m2_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close80 + Polity_class + close80:Polity_class + close80*pr|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "10pt") %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")
nontropical_Nat_m3_pr <- felm(forest.diff ~ forest.l +PCGDP.l + PCGDP.change.l + Pop.growth.l + close90 + Polity_class + close90:Polity_class + close90*pr|un + year|0|un + year, data=country_full) %>%
    tidy() %>% filter(term %in% to_include) %>% mutate(model = "5pt") %>% mutate(level = "country") %>% mutate(forest = "nontropical") %>% mutate(system = "Majoritarian")


data<-rbind(full_m1,full_m2,full_m3,full_Nat_m1,full_Nat_m2,full_Nat_m3,full_m1_pr,full_m2_pr,full_m3_pr,full_Nat_m1_pr,full_Nat_m2_pr,full_Nat_m3_pr,
            tropical_m1,tropical_m2,tropical_m3,tropical_Nat_m1,tropical_Nat_m2,tropical_Nat_m3,tropical_m1_pr,tropical_m2_pr,tropical_m3_pr,tropical_Nat_m1_pr,tropical_Nat_m2_pr,tropical_Nat_m3_pr,
            nontropical_m1,nontropical_m2,nontropical_m3,nontropical_Nat_m1,nontropical_Nat_m2,nontropical_Nat_m3,nontropical_m1_pr,nontropical_m2_pr,nontropical_m3_pr,nontropical_Nat_m1_pr,nontropical_Nat_m2_pr,nontropical_Nat_m3_pr)

save(data,
     file="../data/output/election_model_results.Rdata")


#### PRODUCES FIGURE 4 ####

load("../data/output/election_model_results.Rdata")
all_models <- data %>% 
    mutate(coef = term) %>%
    mutate(term = paste(term,level,system,sep = ", ")) %>%
    filter(coef %in% c("election_DPI","close80","close90")) %>%
    filter(forest == "full")

two_brackets <- list(c("All", "Election year, Cell", "Margin<10, Country"), 
                     c("Majoritarian", "Election year, Cell ", "Margin<10, Country "))

plot <- {dwplot(na.omit(all_models),vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
                # dot_args = list(aes(shape = model)),
                # whisker_args = list(aes(linetype = model)), dodge_size = 0.2
) %>% 
        relabel_predictors(c("election_DPI, cell, All" = "Election year, Cell",                       
                             "close80, cell, All" = "Margin<20, Cell", 
                             "close90, cell, All" = "Margin<10, Cell",
                             "election_DPI, country, All" = "Election year, Country", 
                             "close80, country, All" = "Margin<20, Country",                       
                             "close90, country, All" = "Margin<10, Country",
                             "election_DPI, cell, Majoritarian" = "Election year, Cell ",                       
                             "close80, cell, Majoritarian" = "Margin<20, Cell ", 
                             "close90, cell, Majoritarian" = "Margin<10, Cell ",
                             "election_DPI, country, Majoritarian" = "Election year, Country ", 
                             "close80, country, Majoritarian" = "Margin<20, Country ",                       
                             "close90, country, Majoritarian" = "Margin<10, Country ")) + 
        # facet_grid(.~ system , drop=TRUE, space = "free")  +
        theme_classic() + xlab("percentage points change in forest in election years") + ylab("") +
        xlim(c(-4,1.5)) +
        scale_colour_grey(start = .1, end = .1) + 
        # ggtitle("Relationship between democratic transitions and forest cover change") +
        theme(plot.title = element_text(face="bold"),
              legend.position = "none",
              legend.justification = c(0, 0),
              legend.background = element_rect(colour="transparent"),
              legend.title.align = .5,
              legend.title = element_blank(),
              plot.margin=grid::unit(c(0,0,0,0), "mm"))} %>% add_brackets(two_brackets)

ggsave(file="../figures/coef_plot_elections.pdf",plot = plot,device = "pdf",width = 7,height = 4,units = "in")


#### PRODUCES APPENDIX FIGURE 2 ####

all_models <- data %>% 
    mutate(coef = term) %>%
    mutate(term = paste(term,level,sep = ", ")) %>%
    filter(coef %in% c("election_DPI","close80","close90"))

plot <- dwplot(na.omit(all_models),vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2),
               dot_args = list(aes(shape = model)),
               whisker_args = list(aes(linetype = model))) %>%
    relabel_predictors(c("election_DPI, cell" = "Election, Cell",
                         "close80, cell" = "Margin<20, Cell",
                         "close90, cell" = "Margin<10, Cell",
                         "election_DPI, country" = "Election, Country",
                         "close80, country" = "Margin<20, Country",
                         "close90, country" = "Margin<10, Country")) +
    facet_grid(.~ system + forest, drop=TRUE, space = "free")  +
    theme_bw() + xlab("Coefficient Estimate") + ylab("") +
    scale_colour_grey(start = .1, end = .1) + 
    # ggtitle("Relationship between democratic transitions and forest cover change") +
    theme(plot.title = element_text(face="bold"),
          legend.position = "none")

ggsave(file="../figures/coef_plot_elections_appendix.pdf",plot = plot,device = "pdf",width = 9,height = 3,units = "in")

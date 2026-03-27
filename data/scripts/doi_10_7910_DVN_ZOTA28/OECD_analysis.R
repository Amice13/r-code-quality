################################################################################

## Packages
# To install and open the R packages that you need for this code. 
need <- c('tidyverse','glue','rdrobust', 'stargazer', 'broom', 'lubridate', 'hrbrthemes',
          'ggplot2', 'readxl','png','grid', 'gridExtra', 'cowplot','ggthemes','haven',
          'extrafont')
have <- need %in% rownames(installed.packages()) 
if(any(!have)) install.packages(need[!have]) 
invisible(lapply(need, library, character.only=T)) 

# To set up the working directory. 
script_folder = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(glue('{script_folder}'))
rm(list = ls())

# Change path to wherever data folder with oecd data is located
path <- "X:/xxx/xxx"
setwd(path)
setwd("data/oecd")
getwd()

################################################################################

file_list <- list.files(path=getwd())

# Load data set 1
oecd.inf.mortality <- read_csv(file_list[1], show_col_types = FALSE) %>%
              filter(VAR=="MATIINFA" | VAR=="MATIINTW") %>%
              group_by(Country, Year) %>%
              mutate(flag = max(row_number())) %>%
              filter(!(flag==2 & VAR=="MATIINTW"))


# Load data set 2
SIDS.campaigns <- read_excel(file_list[2])
                  
# Adjustments
names(SIDS.campaigns) <- c("Country", "campaign.year","source")
SIDS.campaigns$campaign.year <- as.numeric(SIDS.campaigns$campaign.year)

# Merges infant mortality and campaign data for main data set
mortality.analysis <- merge(SIDS.campaigns, oecd.inf.mortality, by="Country")
mortality.analysis$event <- mortality.analysis$Year - mortality.analysis$campaign.year
mortality.analysis$inf.mor <- mortality.analysis$Value*10
mortality.analysis <- mortality.analysis %>% 
                      filter(event>-8 & event<9) 
                      
c.list <- unique(mortality.analysis$Country)

# Specifies function for RD
implied.rd <- function(x){
  c.name <- c.list[x]
  dft <- mortality.analysis %>% 
    mutate(flag=as.numeric(event==0 | event==1)) %>%
    filter(Country==c.name & flag==0) %>%
    mutate(trend = case_when(event<0 ~ event, TRUE ~ event-1), 
           treat = as.numeric(event>0), 
           trend.right = trend*treat)
    
mo1 <- lm(data=dft, formula = inf.mor ~ trend + trend.right + treat)    
coef.treat <- mo1$coefficients["treat"]
inf.mean <- mean(dft$inf.mor[dft$event<0])
rel.change <- coef.treat/inf.mean

out1 <- as.data.frame(cbind(c.name,coef.treat, inf.mean, rel.change))
out1 <- out1 %>% mutate(coef.treat = round(as.numeric(coef.treat),2), 
                        inf.mean = round(as.numeric(inf.mean),2),
                        rel.change = round(as.numeric(rel.change)*100,2))

return(out1)  
}

# Estimates RD for each country
out1 <- lapply(1:length(c.list), implied.rd) 
out1 <- do.call(rbind.data.frame, out1)
out1 <- out1 %>% arrange(rel.change) %>% 
        mutate(c.name = as.factor(c.name)) 

out1$c.name <- reorder(out1$c.name, -out1$rel.change)

# Merges RD estimates on main data set
mortality.analysis <- merge(mortality.analysis,out1,by.x = "Country",by.y = "c.name")
mortality.analysis$Country <- reorder(mortality.analysis$Country, mortality.analysis$rel.change)

# Produce output
# Graph - part 1
g1 <- mortality.analysis %>% 
  mutate(event = Year - campaign.year) %>%
  ggplot() +
  geom_rect(aes(xmin = campaign.year-0.5, xmax = campaign.year+1.5, 
                ymin = -Inf, ymax = Inf),
            fill = "grey", alpha = 0.5) + 
  geom_line(aes(x=Year,y=inf.mor)) +
  geom_point(aes(x=Year,y=inf.mor)) +
  geom_smooth(data=
                mortality.analysis[mortality.analysis$event<0,], 
              aes(x=Year,y=inf.mor), colour="red",se=FALSE, size=.75,
              method = "lm", formula =  y ~ poly(x,1)) + 
  geom_smooth(data=
                mortality.analysis[mortality.analysis$event>1,], 
              aes(x=Year,y=inf.mor), colour="blue",se=FALSE, size=.75,
              method = "lm", formula =  y ~ poly(x,1)) + 
  facet_wrap(~Country, scales="free", ncol=3) + 
  theme_bw() +
  theme(text=element_text(family = "Georgia")) +
  ylab("Infant mortality (per 10K live births)") + 
  xlab("") 

# Graph - part 2
g2 <- ggplot(out1, aes(x=c.name, y=rel.change)) +
  geom_segment( aes(x=c.name, xend=c.name, y=0, yend=rel.change), color="grey", size=0.5) +
  geom_point( color="black", size=3, alpha=0.6, fill="black") +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()) + ylab("Estimate for \nrelative change in \ninfant mortality (%)") + 
    xlab("") + 
  theme(text=element_text(family = "Georgia")) 

# Graphs - combined
g3 <- grid.arrange(g1, g2, widths=c(0.65, 0.35), nrow=1)


################################################################################
# Output
# Create output folder in main directory folder specified in "path"
output <- "output"
setwd(path)
getwd()
setwd(output)
getwd()

# Save figure to output folder
ggsave('sids.comparative.png', g3,  width = 11.5, height=11.5,  dpi = 1200)

# Save table on estimates in tex-format to output folder
t.name = paste("oecd_inf_mortality",".","tex",sep="")

write.table(out1, file = paste(t.name,sep="/"), 
            quote=FALSE, sep="&", eol = "\\\\",
            col.names = FALSE, 
            row.names = FALSE) 

# end

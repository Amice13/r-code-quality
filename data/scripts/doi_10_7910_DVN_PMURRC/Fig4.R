# ----------------------------------------------------------------------
# Fig4.R
# (Main Figures from PurpleAir CA Indoor/Outdoor Analysis)
# ----------------------------------------------------------------------

# Convert CA map
ca.sf = st_as_sf(ca)

# Load monitor-specific coefficients
hte = read_csv("Results/Data/coeffs_by_monitor.csv")

# Load ACS housing stock data by census block group 
# (pre-downloaded and included here for ease)
hh = readRDS("Data/ACS_Houshold_Characteristics.rds")

# ----------------------------------------------------------------------
# Figure 1A,B - Maps of Individual Indoor-Outdoor coefficients
# ----------------------------------------------------------------------

betas = left_join(hte,hh)
  coordinates(betas) = ~lon+lat
  betas$Coefficient = betas$coef_0p3
  crs(betas) = crs(hh)
betas.sf = st_as_sf(betas)
  
mp1 = ggplot() + theme_minimal(base_size=14) +
  geom_sf(data=ca.sf,col="black",fill="transparent") + 
  geom_sf(data=hh,aes(fill=sharepost1960),col="transparent") +  scale_fill_gradient(low="grey30",high="grey90") +
  geom_sf(data=betas.sf,aes(col=Coefficient),pch=16,cex=1) + scale_colour_viridis_b() + 
  theme(legend.position="right") + guides(fill=guide_legend("Share Post 1960"))

pdf(width=8,height=6,file="Results/Figures/Fig4A_HTE_ACS_2.pdf")
  print(mp1)
dev.off()

pdf(width=6,height=6,file="Results/Figures/Fig4B_HTE_ACS_BAY_2.pdf")
  print(mp1 + ylim(c(37.6,38)) + xlim(c(-122.6,-122)) + theme(legend.position="none"))
dev.off()

# ----------------------------------------------------------------------
# Figure 1C - Relationship with Housing Age
# ----------------------------------------------------------------------

# Relationship w Housing Age
df = hte
summary(lm(coef_0p3~built_after_1960,data=df))

mp2 = ggplot(data=df[df$built_after_1960>0,],aes(y=coef_0p3,x=built_after_1960)) + theme_minimal(base_size=14) +
  geom_point(pch=16,col="red",alpha=0.8) + geom_smooth(method="lm",col="black") + 
  xlab("Share of Structures Built Post-1960") + ylab("Coefficient")

pdf(width=4,height=3,file="Results/Figures/Fig4C_BldgAge_Relationship.pdf")
  print(mp2)
dev.off()

# Create plots
{
## SCORES density
# grf + demog
p1 = ggdensity(data=aux_data,
               x='tau_grf_d',fill = "lightgray", alpha=0.2, add = "mean")
# grf
p2 = ggdensity(data=aux_data,
               x='tau_grf', fill = "lightgray", alpha=0.2, add = "mean")
# beat
p3 = ggdensity(data=aux_data,
               x='tau_beat', fill = "lightgray", alpha=0.2, add = "mean")

## SCORES by Z
# grf + demog
p1a = ggdensity(data=aux_data,
                x='tau_grf_d', color='Protected', fill='Protected', alpha=0.2, add = "mean")
# grf
p2a = ggdensity(data=aux_data,
                x='tau_grf', color='Protected', fill='Protected', alpha=0.2, add = "mean")
# beat
p3a = ggdensity(data=aux_data,
                x='tau_beat', color='Protected', fill='Protected', alpha=0.2, add = "mean")

## SCORES vs. X
# grf + demog
p1b = ggplot(aux_data, aes(x=X1, y=tau_grf_d)) +
  geom_point()+
  geom_smooth()
p1c = ggplot(aux_data, aes(x=X2, y=tau_grf_d)) +
  geom_point()+
  geom_smooth()
p1d = ggplot(aux_data, aes(x=X3, y=tau_grf_d)) +
  geom_point()+
  geom_smooth()
p1e = ggplot(aux_data, aes(x=X4, y=tau_grf_d)) +
  geom_point()+
  geom_smooth()

# grf
p2b = ggplot(aux_data, aes(x=X1, y=tau_grf)) +
  geom_point()+
  geom_smooth()
geom_smooth()
p2c = ggplot(aux_data, aes(x=X2, y=tau_grf)) +
  geom_point()+
  geom_smooth()
p2d = ggplot(aux_data, aes(x=X3, y=tau_grf)) +
  geom_point()+
  geom_smooth()
p2e = ggplot(aux_data, aes(x=X4, y=tau_grf)) +
  geom_point()+
  geom_smooth()

# BEAT
p3b = ggplot(aux_data, aes(x=X1, y=tau_beat)) +
  geom_point()+
  geom_smooth()
p3c = ggplot(aux_data, aes(x=X2, y=tau_beat)) +
  geom_point()+
  geom_smooth()
p3d = ggplot(aux_data, aes(x=X3, y=tau_beat)) +
  geom_point()+
  geom_smooth()
p3e = ggplot(aux_data, aes(x=X4, y=tau_beat)) +
  geom_point()+
  geom_smooth()
} # end of creating plots
  
# Config. plots
base_font_size = 14
label_font_size = 14
if(Sys.info()['sysname']=="Windows"){windowsFonts(Times=windowsFont("Times New Roman"))}
plot_theme =  theme(
  text=element_text(family="Times", face="plain", size=base_font_size),
  axis.title=element_text(family="Times", face="plain",size=label_font_size),
  panel.background = element_rect(fill = "white", colour = "gray",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major.y = element_line(color = "grey80",size = 0.1),
  panel.grid.minor.y = element_line(color = "grey80",size = 0.1))
theme_set(plot_theme)
    

## Combining plots
my_xlim = c(-2.5,2.5)
my_ylim = c(-2.5,2.5)

p_example_grf_fd = ggarrange(p1+ylab("")+xlim(1.2*my_xlim)+xlab("(a) Predicted CATE"),
                                p1b+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(b) X1"),
                                p1c+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(c) X2 (corr. Z1)"),
                                p1d+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(d) X3"),
                                p1e+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(e) X4"),
                                p1a+ylab("")+xlim(1.2*my_xlim)+xlab("(f) Target score"),
                                ncol = 6,nrow = 1)

p_example_grf_np = ggarrange(p2+ylab("")+xlim(1.2*my_xlim)+xlab("(a) Predicted CATE"),
                          p2b+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(b) X1"),
                          p2c+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(c) X2 (corr. Z1)"),
                          p2d+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(d) X3"),
                          p2e+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(e) X4"),
                          p2a+ylab("")+xlim(1.2*my_xlim)+xlab("(f) Target score"),
                          ncol = 6,nrow = 1)

p_example_beat = ggarrange(p3+ylab("")+xlim(1.2*my_xlim)+xlab("(a) Predicted CBT"),
                           p3b+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(b) X1"),
                           p3c+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(c) X2 (corr. Z1)"),
                           p3d+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(d) X3"),
                           p3e+ylab("")+xlim(my_xlim)+ylim(my_ylim)+xlab("(e) X4"),
                           p3a+ylab("")+xlim(1.2*my_xlim)+xlab("(f) Target score"),
                           ncol = 6,nrow = 1)


print(p_example_grf_fd)
print(p_example_grf_np)
print(p_example_beat)

rm(list=ls(pattern="^p1"))
rm(list=ls(pattern="^p2"))
rm(list=ls(pattern="^p3"))
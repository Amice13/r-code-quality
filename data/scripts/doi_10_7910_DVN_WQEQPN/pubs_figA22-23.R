
fig.path <- "results/"
fig1 <- paste0(fig.path,"pubs_figA22a.pdf")
fig2 <- paste0(fig.path, "pubs_figA22b.pdf")
fig3 <- paste0(fig.path, "pubs_figA23a.pdf")
fig4 <- paste0(fig.path, "pubs_figA23b.pdf")


colors <- wes_palette("IsleofDogs1", 5)
plot1 <- ggplot(pubs, aes(x=pops.23,y=totalpubs.23) ) +
        geom_point( col = "black" , size =1.3)  +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2),
    size = 1, se = TRUE, aes(colour = "Quadratic fit")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.3, size = 1, se = TRUE, aes(colour = "Loess, span =0.3")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.5, size = 1, se = TRUE, aes(colour = "Loess, span =0.5")) +
    stat_smooth(method = "loess", formula = y ~
   x, span = 0.8, size = 1, se = TRUE, aes(colour = "Loess, span =0.8")) +
   scale_colour_manual(name="Smoothing", values=colors[1:4]) +
   labs(x = "Ethnic Population in 1923", y = "Total Publications",
        title="Publications and Population size in Rowling and Wilson (1923)") +
   theme_minimal(base_size=14) +
   theme(axis.text.y = element_text(size=14),legend.position = "bottom") +
   ggsave(fig1 , height = 7 , width = 10)


plot2 <- ggplot(pubsss, aes(x=pops.23,y=totalpubs.23) ) +
        geom_point( col = "black" , size =1.3)  +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2),
    size = 1, se = TRUE, aes(colour = "Quadratic fit")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.3, size = 1, se = TRUE, aes(colour = "Loess, span =0.3")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.5, size = 1, se = TRUE, aes(colour = "Loess, span =0.5")) +
    stat_smooth(method = "loess", formula = y ~
   x, span = 0.8, size = 1, se = TRUE, aes(colour = "Loess, span =0.8")) +
   scale_colour_manual(name="Smoothing", values=colors[1:4]) +
   labs(x = "Ethnic Population in 1923", y = "Total Publications",
        title="Publications and Population size in Rowling and Wilson (1923)",
        subtitle = "Zoom to smaller populations") +
   theme_minimal(base_size=14) +
   theme(axis.text.y = element_text(size=14),legend.position = "bottom") +
    ggsave(fig2 , height = 7 , width = 10)

colors <- wes_palette("IsleofDogs1", 5)
plot3 <- ggplot(pubs, aes(y=pubpc,x=pops.23) ) +
        geom_point( col = "black" , size =1.3)  +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2),
    size = 1, se = TRUE, aes(colour = "Quadratic fit")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.3, size = 1, se = TRUE, aes(colour = "Loess, span =0.3")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.5, size = 1, se = TRUE, aes(colour = "Loess, span =0.5")) +
    stat_smooth(method = "loess", formula = y ~
   x, span = 0.8, size = 1, se = TRUE, aes(colour = "Loess, span =0.8")) +
   scale_colour_manual(name="Smoothing", values=colors[1:4]) +
   labs(x = "Ethnic Population in 1923", y = "Publications per capita",
        title="Publications per capita and Population size in Rowling and Wilson (1923)") +
   theme_minimal(base_size=14) +
   theme(axis.text.y = element_text(size=14),legend.position = "bottom") +
   ggsave(fig3 , height = 7 , width = 10)


pubsss <- subset(pubs, pops.23 <2e05)
plot4 <- ggplot(pubsss, aes(y=pubpc,x=pops.23) ) +
        geom_point( col = "black" , size =1.3)  +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2),
    size = 1, se = TRUE, aes(colour = "Quadratic fit")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.3, size = 1, se = TRUE, aes(colour = "Loess, span =0.3")) +
     stat_smooth(method = "loess", formula = y ~
    x, span = 0.5, size = 1, se = TRUE, aes(colour = "Loess, span =0.5")) +
    stat_smooth(method = "loess", formula = y ~
   x, span = 0.8, size = 1, se = TRUE, aes(colour = "Loess, span =0.8")) +
   scale_colour_manual(name="Smoothing", values=colors[1:4]) +
   labs(x = "Ethnic Population in 1923", y = "Publications per capita",
        title="Publications per capita and Population size in Rowling and Wilson (1923)",
        subtitle = "Zoom to smaller populations") +
   theme_minimal(base_size=14) +
   theme(axis.text.y = element_text(size=14),legend.position = "bottom") +
    ggsave(fig4 , height = 7 , width = 10)

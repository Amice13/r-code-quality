
#set working directory
#setwd("")

gpA <- read.csv("Identity_graph_HMPVA_Ggene.csv", sep = ",", header = TRUE)
head(gpA)

gpB <- read.csv("Identity_graph_HMPVB_Ggene.csv", sep = ",", header = TRUE)
head(gpB)
require(ggplot2)

## HMPV group A G gene - Figure 1:

ggplot(gpA, aes(x = Position, y = Identity, fill = diversity)) +
  geom_bar(stat="identity", width = 0.3) +
  xlab("\nAmino acid position") +
  ylab("Sequence Identity\n") +
  scale_fill_manual(values = c('red', 'black')) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))


ggplot(gpA, aes(x = Position, y = Identity)) +
  geom_area(alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  xlab("\nAmino acid position") +
  ylab("Sequence Identity\n") +
  theme(axis.title = element_text(face = "bold", size = 18))
  #theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)))


## HMPV group B G gene - Figure 1:

ggplot(gpB, aes(x = Position, y = Identity, fill = diversity)) +
  geom_bar(stat="identity", width = 0.3) +
  xlab("\nAmino acid position") +
  ylab("Sequence Identity\n") +
  scale_fill_manual(values = c('red', 'black')) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))


ggplot(gpB, aes(x = Position, y = Identity)) +
  geom_area(alpha = 0.4) +
  theme_classic() +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10)) +
  xlab("\nAmino acid position") +
  ylab("Sequence Identity\n") +
  theme(axis.title = element_text(face = "bold", size = 18))
#theme(axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 20, l = 0)))

## HMPV group A SH gene - Supp. Figure 2:

SHA <- read.csv("Identity_graph_HMPVA_SHgene.csv", sep = ",", header = TRUE)
head(SHA)

SHB <- read.csv("Identity_graph_HMPVB_SHgene.csv", sep = ",", header = TRUE)
head(SHB)

ggplot(SHA, aes(x = Position, y = Identity, fill = diversity)) +
  geom_bar(stat="identity", width = 0.5) +
  xlab("\nAmino acid position") +
  ylab("Sequence Identity\n") +
  scale_fill_manual(values = c('red', 'black')) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))

ggplot(SHB, aes(x = Position, y = Identity, fill = diversity)) +
  geom_bar(stat="identity", width = 0.5) +
  xlab("\nAmino acid position") +
  ylab("Sequence Identity\n") +
  scale_fill_manual(values = c('red', 'black')) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 12)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n=10))


print("Running fig6.R...")

# Setup
library(compactness)
library(ggalt)
library(ggplot2)
set.seed(02138)

shpname = "../data/allval3.shp" # this is a shp file with only the relevant districts; change this name to reflect the location of allval3.shp. R uses your default application for extracting compressed files. This depends on what your application generates
namecol = "name"
shp= read_shapefiles(shp = shpname, namecol = namecol)
load("../results/final_models.RData")
feat = generate_features(shp=shp)
feat = feat[,-2]
feat$name = as.character(feat$name)

preds_df = generate_predictions(feat, namecol = namecol, new.models = models)

dists = list.files("../data")
dists = dists[grepl(".jpg", dists)]
dists = gsub(".jpg", "", dists[1:20])
dists = data.frame(dists, 1:20)
dists2 = merge(dists, preds_df, by.x="dists", by.y="district")

###################################
##### Undergrads
###################################
setwd("../data")
f = list.files()
f = f[grepl("j[12345]{1}_L", f) | grepl("j[12345]{1}_E", f) |grepl("j[12345]{1}_R", f)  |grepl("j[12345]{1}_S", f)]

for(i in 1:length(f)){
  temp = read.csv(f[i])
  if(ncol(temp)==1){
    temp = read.csv(f[i], sep="\t")
    if(ncol(temp)==4){
      temp = temp[,c(2,4)]
    }else {
      temp = temp[,c(2,3)]
    }
  } else if(ncol(temp)==3){
    temp = temp[,c(2,3)]
  } else if(ncol(temp == 4)){
    temp = temp[,c(2,4)]
  }
  colnames(temp) = c("district", "rank")
  temp$rank = temp$rank* -1 + 22
  assign(f[i], temp)
}

ugrads1 = cbind(j2_E.csv[order(j2_E.csv$district),],
                j2_L.csv[order(j2_L.csv$district),],
                j2_R.csv[order(j2_R.csv$district),],
                j2_S.csv[order(j2_S.csv$district),])
ugrads1 = ugrads1[,c(1,2,4,6,8)]

cors = sapply(2:ncol(ugrads1), FUN=function(x) cor(ugrads1[,x], dists2$compactness))
hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

ugrads1 = data.frame(source = "ugrads", cors = abs(cors))


###################################
##### JDs
###################################
# NOTE: Make Sure You Unzip the Shiny Data.7z file in the replication archive
# before doing the steps below. The shiny_results directories are compressed
# to preserve space

dat = read.csv("../data/law_students.csv", sep="", header=F)[,c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40)]

out = list()
for(i in 1:nrow(dat)){
  a = dat[i,]
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out[[i]] = as.numeric(a$ranks)
}
out = do.call(cbind, out)
out = data.frame(out)

f = list.files("../data/Shiny Data/shiny_results", full.names = TRUE)
f = f[grepl("a5725a06ce880f6a1790bc991113502f", f)]
f = f[-length(f)]

out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)
out = cbind(out, out2)

cors = sapply(1:ncol(out), FUN=function(x) cor(out[,x], dists2$compactness))
mean(abs(cors))

jds = data.frame(source = "jds", cors = abs(cors))


###################################
##### MTurkers
###################################
f = list.files("../data/Shiny Data/shiny_results2", full.names = TRUE)
f = f[grepl(".RData", f)]


out = list()
starttime = list()
stoptime = list()
count = list()
for(i in 1:length(f)){
  load(f[i])
  temp = a
  a = as.numeric(a[[1]])
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out[[i]] = as.numeric(a$ranks)
  starttime[[i]] = temp[[4]]
  stoptime[[i]] = temp[[5]]
  count[[i]] = temp[[2]] + temp[[3]]
  
}
out = do.call(cbind, out)
out = data.frame(out)

cors = sapply(1:ncol(out), FUN=function(x) cor(out[,x], dists2$compactness))
#hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

count = unlist(count)
time = unlist(stoptime) - unlist(starttime)
cors2 = cors[unlist(count) > 25 & time > 1000]

mturkers = data.frame(source="mturk", cors = abs(cors2))


###################################
##### Undergrads part 2
###################################
setwd("../data/Shiny Data/shiny_results")


ugs = c("14acb23e9801cbc261ce5ea36a190ab0.RData", "0bf36d47247d6297bbc2b33a3fd58d88.RData", "16aa8ecebe5d67a2b605e43266d01c9d.RData",
        "73a7cef4316053d7c637f29bd596180b.RData", "6daf4e19e1ff0e67a6e48aa509da90dc.RData", "53a9e4588c7605af09f38ce0ff07069a.RData",
        "adc6b7144da80db8ce847caaf2c7bd52.RData", "1a7d50350f4435f9d90eac0efab0bf45.RData",
        "f0d5f4ba761c75ce0a2417c6d73034cf.RData")
ugs = ugs[ugs %in% list.files()]

f = list.files()
f = f[grepl("a552", f)]
f = c(f, ugs)
f = f[-4]

out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)

cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
mean(abs(cors))

ugrads2 = data.frame(source="ugrads", cors = abs(cors))


###################################
##### PhD Students
###################################
f = list.files()
f = f[grepl("688b47", f)]

out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)

cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
#hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

phds = data.frame(source="phds", cors = abs(cors))


###################################
##### Law School Professors
###################################

profs = c("1b7c971570862ee8d0a7efb8071652e4.RData", "3b181bb36518459cfc8bb5ca980f2dc4.RData", "4674a449ae0998f9e5f22aa508fbbf8a.RData",
          "48a5c46c23b6ed09586a36705467f802.RData", "59186b5e76037f5e98a6d070def5a3bd.RData", "6e4b2d2f475ec7da3da208e12439006b.RData",
          "ed5d11b9c5efeb5fd7b9f3989e542844.RData", "f9ed5d11b9c5efeb5fd7b9f3989e54284435d8245567377d5c2126181c977d9d.RData", "6e277925258d630326aed30504e2fc07.RData",
          "fa6fd1342611fc0200e8b47e36dfa6df.RData", "0ef26e679e8afcb519c82d1d35a907ed.RData", "91fa8e8b0f87bf70537e8295e4f1e897.RData",
          "ef46fdb0a2367275c1dca174f7cffe37.RData", "ed5d11b9c5efeb5fd7b9f3989e542844.RData", "6d483e2ff0112cd411b0a5f91bd553e9.RData")

f = profs[profs %in% list.files()]
out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)

cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

profs = data.frame(source="profs", cors = abs(cors))


###################################
##### Political Consultants
###################################
              
consultants = c("093db002891bcecf28da470c48833994.RData", "3b181bb36518459cfc8bb5ca980f2dc4.RData",
                "49b2ec0455323530ad154c15b8b2f580.RData", "7a0cdd5d75ec33606eaf3666bdbbfbdb.RData", "86b3229767ee46db971ce0e457713a11.RData")
f = consultants[consultants %in% list.files()]
out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)

cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
#hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

consultants = data.frame(source="consultants", cors = abs(cors))


###################################
##### Public Officials
###################################

pubs = c("c548ad667dbaa70e4b2c77fa60731255.RData", "7a243ab025092f099a9bf73ff931bb5a.RData", "508a8d6c6267a7f8ed373be84d4fd4b8.RData",
         "92afa8293a4ac54d8912066126044dd7.RData", "d9134eb4eb7b7a7a894b23b337ee289d.RData", "a263f4143811e508a16cfffc27b7712e.RData",
         "21a33ab30e72c3329919a3bcf3d19391.RData", "e9f8d63214077454026a25e5aa0e5c1e.RData", "9b605d3a8827335cf466a1d573d5ef6a.RData",
         "e9f8d63214077454026a25e5aa0e5c1e.RData", "49b2ec0455323530ad154c15b8b2f580.RData", "92e335bee1e6521631dcd0fe3d39447a.RData",
         "9ef0fae965a9a0275f4c1c59a92c3c22.RData", "b51187b6ca7f4a6bdb662ce85e38e80d.RData", "e6ec6017e4f99cec9895ea9f94f99542.RData",
         "52e400a9936d674e19bba23e937ac3ef.RData", "2a4f1a640e2ab4547a725b76214fc0fe.RData", "2f9f7f86b079c7863413d7502ab92445.RData",
         "3e835e45c92248211d4d58d892c2228a.RData", "aa39d96c440bb6c7a63d3d4019193730.RData", "85412b3a4d04053776fb17d869d13509.RData")


f = pubs[pubs %in% list.files()]
f = c(f, list.files()[grepl("6bc8a06c88982cbf0a83a7d428f4fc3d", list.files())])
f = c(f, list.files()[grepl("cb7085e4669fd87ddeff09aa01f47bf9", list.files())])



out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)

cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
#hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

pubs = data.frame(source="pubs", cors = abs(cors))



###################################
##### Judges
###################################

judges = c("82e860a100cdf4f2b5bb9f8f3c1e3736.RData", "6293ed573ad8e51fd472640220b3215a.RData", "342c168c18d3e7c0803afff637317814.RData",
           "39edde0f0ad67c5a70e28c927a029e15.RData", "859d9af345b1d2e262b2ed908909c7e4.RData", "4b5004b2f940302c04ba3473fbb77d62.RData",
           "d01196bb34e2790bc0a7d10d04599c3e.RData")

f = list.files()
f = f[grepl("59f6d03b2500ee467a8aa7937ea58941", f)]
f2 = judges[judges %in% list.files()]
f = c(f, f2)


out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)


cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

judges = data.frame(source="judges", cors = abs(cors))


###################################
##### Lawyers
###################################

lawyers = c("a85b450e89edaad831d015ea12d8b95d.RData", "7ba2c8aab3fccc2f0367c699c9f71064", "62d625b07e29b1498125396b329e7a3d.RData",
            "1fcb34832f1eead557112605016034b7.RData", "1ac63389ac1d68c3489c2d992467903f.RData", "16f07588b4ff5431851d801e6f885519.RData",
            "0a637c782de7529852083f7f30f0d1a6.RData", "r10.RData", "d9134eb4eb7b7a7a894b23b337ee289d.RData",
            "d9134eb4eb7b7a7a894b23b337ee289d.RData", "9b605d3a8827335cf466a1d573d5ef6a.RData", "a88ec46be9c0c59f75437a2356015374.RData")
f = lawyers[lawyers %in% list.files()]
out2 = list()
for(i in 1:length(f)){
  load(f[i])
  a = as.numeric(a)
  a = data.frame(a[1:20], ranks=1:20)
  a = a[order(a$a),]
  out2[[i]] = as.numeric(a$ranks)
}
out2 = do.call(cbind, out2)
out2 = data.frame(out2)

cors = sapply(1:ncol(out2), FUN=function(x) cor(out2[,x], dists2$compactness))
#hist(abs(cors), xlab="Correlation", main="Correlation to Predicted Ranks")
mean(abs(cors))

lawyers = data.frame(source="lawyers", cors = abs(cors))


####################################
##### Random data, for completeness
####################################
random = data.frame(source="random", cors = sapply(1:10000, FUN=function(x) cor(sample(1:20), dists2$compactness)))

                                                   
                                                   
###################################
##### Plotting
###################################

plotdf = rbind(random, jds, mturkers,  ugrads2, phds, judges, lawyers, consultants, pubs, profs)


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

p=ggplot(plotdf, aes(x=cors, fill=source)) +
  geom_bkde(alpha = 0.5, truncate=TRUE, color="black", bandwidth=.07,kernel="normal") + 
  xlab("Correlation to Predicted Ranks") + ylab("") +
  scale_fill_manual(values=c(NA, NA, NA, NA, "red", NA, NA, NA, "blue", NA))+
  #scale_x_continuous(limits = c(.5, 1), breaks=c(-1,-.75,-.5,-.25,0,0.25,0.5,0.75,1),
  scale_x_continuous(limits = c(-1, 1), breaks=c(-1,-.75,-.5,-.25,0,0.25,0.5,0.75,1),
                     labels = c("-1.0", "-0.75", "-0.5", "-0.25", "0", "0.25", "0.5", "0.75", "1.0")) +
  geom_segment(x=-1, y=0, xend=1, yend=0) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_blank(),
        #axis.line.x = element_line(color="black", size = .5),
        axis.line.x = element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x = element_text(size=24),
        axis.title.x = element_text(size=24),
        legend.position = "none",
        plot.margin=unit(c(0,25,5,0), "mm")) + 
  scale_y_continuous(limits = c(0,6), expand = c(0, 0)) +
  annotate("text", x=-0.35, y=1.3, label="Random", col="blue", size=5) +
  annotate("text", x=0.45, y=1.6, label="MTurkers", col="red", size=5) +
  #annotate("text", x=0.62, y=6.6, label="Experts:", col="black", size=7) +
  annotate("text", x=0.35, y=4.75, label="Judges", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=4.55, label="Public Officials", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=4.35, label="Lawyers", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=4.15, label="Consultants", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=3.95, label="Law Faculty", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=3.75, label="JD Students", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=3.55, label="PhD Students", col="black", size=5, hjust = 0) +
  annotate("text", x=0.35, y=3.35, label="Undergraduates", col="black", size=5, hjust = 0)
p
ggsave(filename = "../../../results/fig6.png", device="png", plot=p, width=10, height=7, units="in")                                     

setwd("../../../code")

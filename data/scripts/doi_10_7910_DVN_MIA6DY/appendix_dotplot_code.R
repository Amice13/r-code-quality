#Script to produce figures B1-B6

library(foreign)
library(lattice)

dat<-read.dta("dp_repV12.dta")





##Figure B1
dat2 <-dat[order(dat$lrecon_bam), ]
dat2$idlr<-reorder(dat2$id, dat2$lrecon_bam)

dat2a<-dat2[1:131,]

pdf("lrecon_bam_a.pdf", width=5, height=7)
dotplot(dat2a$idlr~dat2a$lrecon_bam,xlim=c(-2, 1.5),
        xlab=list(label = "Economic Left-Right: left parties", cex=1.1),
        main="Party Placements with 95% Credible Intervals",
        scales=list(cex=0.275),
        panel = function(x,y,subscripts,lower, upper){
          panel.segments(dat2a$lrecon_bamLB, y,dat2a$lrecon_bamUB, y, col="gray65", lty=1)
          panel.points(x,y, pch=16, col="black", cex=.7)
        })


dev.off()

#Figure B2
dat2b<-dat2[132:262,]


pdf("lrecon_bam_b.pdf", width=5, height=7)
dotplot(dat2b$idlr~dat2b$lrecon_bam,xlim=c(-2, 1.5),
        xlab=list(label = "Economic Left-Right: right parties", cex=1.1),
        main="Party Placements with 95% Credible Intervals",
        scales=list(cex=0.275),
        panel = function(x,y,subscripts,lower, upper){
          panel.segments(dat2b$lrecon_bamLB, y,dat2b$lrecon_bamUB, y, col="gray65", lty=1)
          panel.points(x,y, pch=16, col="black", cex=.7)
        })
dev.off()

#Figures B3-B4

dat3 <-dat[order(dat$galtan_bam), ]
dat3$id3<-reorder(dat3$id, dat3$galtan_bam)
#Figure B3
dat3a<-dat3[1:131,]

#
pdf("galtan_bam_aa.pdf", width=5, height=7)
dotplot(dat3a$id3~dat3a$galtan_bam,xlim=c(-2, 1.5),
	xlab=list(label = "Social Left-Right: left parties", cex=1.1),
	main="Party Placements with 95% Credible Intervals",
	scales=list(cex=0.275),
	panel = function(x,y,subscripts,lower, upper){
 	panel.segments(dat3a$galtan_bamLB, y, dat3a$galtan_bamUB, y, col="gray65", lty=1)
	panel.points(x,y, pch=16, col="black", cex=.7)
})
#
dev.off()

dat3b<-dat3[132:262,]

#
pdf("galtan_bam_b.pdf", width=5, height=7)
dotplot(dat3b$id3~dat3b$galtan_bam,xlim=c(-2, 1.5),
        xlab=list(label = "Social Left-Right: right parties", cex=1.1),
        main="Party Placements with 95% Credible Intervals",
        scales=list(cex=0.275),
        panel = function(x,y,subscripts,lower, upper){
          panel.segments(dat3b$galtan_bamLB, y, dat3b$galtan_bamUB, y, col="gray65", lty=1)
          panel.points(x,y, pch=16, col="black", cex=.7)
        })
#
dev.off()




#Figures B5-B6


dat4 <-dat[order(dat$eu_bam), ]
dat4$id4<-reorder(dat4$id, dat4$eu_bam)



#Figure B3
dat4a<-dat4[1:131,]

#
pdf("eu_bam_b.pdf", width=5, height=7)
dotplot(dat4a$id4~dat4a$eu_bam,xlim=c(-2, 1.5),
	xlab=list(label = "EU Position: Anti-EU parties", cex=1.1),
	main="Party Placements with 95% Credible Intervals",
	scales=list(cex=0.275),
	panel = function(x,y,subscripts,lower, upper){
 	panel.segments(dat4a$eu_bamLB, y, dat4a$eu_bamUB, y, col="gray65", lty=1)
	panel.points(x,y, pch=16, col="black", cex=.7)
})
#
dev.off()

dat4b<-dat4[132:262,]

#
pdf("eu_bam_a.pdf", width=5, height=7)
dotplot(dat4b$id4~dat4b$eu_bam,xlim=c(-2, 2.5),
        xlab=list(label = "EU Position: Pro-EU parties", cex=1.1),
        main="Party Placements with 95% Credible Intervals",
        scales=list(cex=0.275),
        panel = function(x,y,subscripts,lower, upper){
          panel.segments(dat4b$eu_bamLB, y, dat4b$eu_bamUB, y, col="gray65", lty=1)
          panel.points(x,y, pch=16, col="black", cex=.7)
        })
#
dev.off()

#
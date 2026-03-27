## Author: Kabir Khanna
## Updated: December 20, 2015

load("df.RData")
df <- df[df$SR.WHI == 1 | df$SR.BLA == 1 | df$SR.HIS == 1 | df$SR.ASI == 1 | df$SR.OTH == 1, ]

df$surname.anon <- factor(df$surname.anon)
df$County <- factor(df$County)

## All Races
p.name.all <- table(df$surname.anon) / sum(table(df$surname.anon))
p.county.all <- table(as.factor(df$County)) / sum(table(as.factor(df$County)))
p.obs.all <- table(df$surname.anon, as.factor(df$County)) / sum(table(df$surname.anon, as.factor(df$County)))

p.exp.all <- p.name.all %*% t(p.county.all)
resid.all <- c(p.obs.all - p.exp.all)
save(resid.all, file = "resid.all.RData")
print("All Races complete (no conditioning)")

## Condition on Whites
df.whi <- df[df$SR.WHI == 1, ]
p.name.whi <- table(df.whi$surname.anon) / sum(table(df.whi$surname.anon))
p.county.whi <- table(as.factor(df.whi$County)) / sum(table(as.factor(df.whi$County)))
p.obs.whi <- table(df.whi$surname.anon, as.factor(df.whi$County)) / sum(table(df.whi$surname.anon, as.factor(df.whi$County)))

p.exp.whi <- p.name.whi %*% t(p.county.whi)
resid.whi <- c(p.obs.whi - p.exp.all)
resid.whi.cond <- c(p.obs.whi - p.exp.whi)
save(resid.whi, file = "resid.whi.RData")
save(resid.whi.cond, file = "resid.whi.cond.RData")
print("Whites complete")

## Condition on Blacks
df.bla <- df[df$SR.BLA == 1, ]
p.name.bla <- table(df.bla$surname.anon) / sum(table(df.bla$surname.anon))
p.county.bla <- table(as.factor(df.bla$County)) / sum(table(as.factor(df.bla$County)))
p.obs.bla <- table(df.bla$surname.anon, as.factor(df.bla$County)) / sum(table(df.bla$surname.anon, as.factor(df.bla$County)))

p.exp.bla <- p.name.bla %*% t(p.county.bla)
resid.bla <- c(p.obs.bla - p.exp.all)
resid.bla.cond <- c(p.obs.bla - p.exp.bla)
save(resid.bla, file = "resid.bla.RData")
save(resid.bla.cond, file = "resid.bla.cond.RData")
print("Blacks complete")

## Condition on Latinos
df.his <- df[df$SR.HIS == 1, ]
p.name.his <- table(df.his$surname.anon) / sum(table(df.his$surname.anon))
p.county.his <- table(as.factor(df.his$County)) / sum(table(as.factor(df.his$County)))
p.obs.his <- table(df.his$surname.anon, as.factor(df.his$County)) / sum(table(df.his$surname.anon, as.factor(df.his$County)))

p.exp.his <- p.name.his %*% t(p.county.his)
resid.his <- c(p.obs.his - p.exp.all)
resid.his.cond <- c(p.obs.his - p.exp.his)
save(resid.his, file = "resid.his.RData")
save(resid.his.cond, file = "resid.his.cond.RData")
print("Latinos complete")

## Condition on Asians
df.asi <- df[df$SR.ASI == 1, ]
p.name.asi <- table(df.asi$surname.anon) / sum(table(df.asi$surname.anon))
p.county.asi <- table(as.factor(df.asi$County)) / sum(table(as.factor(df.asi$County)))
p.obs.asi <- table(df.asi$surname.anon, as.factor(df.asi$County)) / sum(table(df.asi$surname.anon, as.factor(df.asi$County)))

p.exp.asi <- p.name.asi %*% t(p.county.asi)
resid.asi <- c(p.obs.asi - p.exp.all)
resid.asi.cond <- c(p.obs.asi - p.exp.asi)
save(resid.asi, file = "resid.asi.RData")
save(resid.asi.cond, file = "resid.asi.cond.RData")
print("Asians complete")

## Condition on Others
df.oth <- df[df$SR.NAT == 1 | df$SR.OTH == 1, ]
p.name.oth <- table(df.oth$surname.anon) / sum(table(df.oth$surname.anon))
p.county.oth <- table(as.factor(df.oth$County)) / sum(table(as.factor(df.oth$County)))
p.obs.oth <- table(df.oth$surname.anon, as.factor(df.oth$County)) / sum(table(df.oth$surname.anon, as.factor(df.oth$County)))

p.exp.oth <- p.name.oth %*% t(p.county.oth)
resid.oth <- c(p.obs.oth - p.exp.all)
resid.oth.cond <- c(p.obs.oth - p.exp.oth)
save(resid.oth, file = "resid.oth.RData")
save(resid.oth.cond, file = "resid.oth.cond.RData")
print("Others complete")

resid.all.cond <- (resid.whi.cond * nrow(df.whi) + 
                     resid.bla.cond * nrow(df.bla) + 
                     resid.his.cond * nrow(df.his) + 
                     resid.asi.cond * nrow(df.asi) + 
                     resid.oth.cond * nrow(df.oth)) / nrow(df)
save(resid.all.cond, file = "resid.all.cond.RData")
print("All Races complete (conditioning)")

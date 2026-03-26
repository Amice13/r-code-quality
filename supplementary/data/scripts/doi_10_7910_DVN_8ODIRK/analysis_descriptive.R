############################
## Replication Material: Script for replicating descriptive statistics
## Paltra, Sältzer, Stecker
## Affective States - Cultural and Affective Polarization in a Multilevel-Multiparty System 
## 2025
## Political Behavior
############################

# Loading all required datasets ------------------------------------------------
load("wordscores_data.Rdata")
load("all_ap_measures.Rdata")

# Figure 1 Germany State Party Position across Time ----------------------------
pdf("wordscores_ge_cult.pdf")
attach(ws_final)
plot(econ_afd, soci_afd,
     ylim = c(0,30),
     xlim = c(0,30),
     pch = 19,
     col = c("blue"),
     xlab = "Economic Dimension",
     ylab = "Cultural Dimension",
     xaxt="n",
     yaxt="n")
points(econ_spd, soci_spd,
       ylim = c(0,30),
       xlim = c(0,30),
       pch = 19,
       col = c("red"),
       xlab = "Economic Dimension",
       ylab = "Cultural Dimension")
points(econ_cdu, soci_cdu,
       ylim = c(0,30),
       xlim = c(0,30),
       pch = 19,
       col = c("black"),
       xlab = "Economic Dimension",
       ylab = "Cultural Dimension")
points(econ_fdp, soci_fdp,
       ylim = c(0,30),
       xlim = c(0,30),
       pch = 19,
       col = c("yellow"),
       xlab = "Economic Dimension",
       ylab = "Cultural Dimension")
points(econ_pds, soci_pds,
       ylim = c(0,30),
       xlim = c(0,30),
       pch = 19,
       col = c("magenta"),
       xlab = "Economic Dimension",
       ylab = "Cultural Dimension")
points(econ_gru, soci_gru,
       ylim = c(0,30),
       xlim = c(0,30),
       pch = 19,
       col = c("green"),
       xlab = "Economic Dimension",
       ylab = "Cultural Dimension")
axis(1, at=c(seq(0, 30, by=5)), labels=c("Left", 5, 10, 15, 20, 25, "Right"))
axis(2, at=c(seq(0, 30, by=5)), labels=c("Left", 5, 10, 15, 20, 25,"Right"))
legend("bottomright", legend=c("AfD", "SPD", "CDU", "FDP", "The Left", "Greens"),
       col=c("blue", "red", "black", "yellow", "magenta", "green"),pch =19, cex=1)
dev.off()

# Figure C3 Affective Polarization with different measures in Germany ----------
pdf("ap_ge_final_all.pdf")
attach(api_comp_ltw)
plot(year, wap_spread, col ="blue",
     pch = 19,
     ylim = c(0, 6),
     ylab = "Affective Polarization",
     xlab = "Year")
points(year, wap_distance, col = "black", pch = 19)
points(year, API, col = "red", pch = 19)
text(year, wap_distance, labels=landyear, cex= 0.5)
legend("bottomright", inset=c(0, 0),
       legend=c("WAP(Spread) by Wagner", 
                "WAP(Distance) by Wagner", 
                "API by Reiljan"),
       col=c("blue", "black", "red"),pch =19, cex=1)
dev.off()
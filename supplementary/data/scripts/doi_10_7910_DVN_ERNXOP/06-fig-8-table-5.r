#####################################
### REGRESSIONS WITH LR ON DV
#####################################

    pdf(file="plots/regression_leftright_associations.pdf", width=8, height=3.5)
    # windows(8,3)
    par(mfrow=c(1,2),mar=c(4, 7, 1, 1), oma=c(0, 0,0, 0))
    fit <- lm(leftright ~ max.left.factor +
                male + age + education + income + pol.interest + east + factor.party.preference, data=d) #
    u <- coef(fit)[-1]
    low <- confint(fit)[,1][-1]
    high <- confint(fit)[,2][-1]
    y.axis <- c(length(u):1)
    plot(u, y.axis, type = "p", axes = F, xlab = "Left-right scale", ylab = "", pch = 20, cex = 1, xlim = c(-3,3), xaxs = "r", main="Associations with left (M1)", cex.main=.8)
    segments(low, y.axis, high, y.axis, lwd =  1)
    axis(1, at = seq(-3, 3, by=0.5), tick = T,cex.axis = 0.8, mgp = c(2,1.7,1))
    abline(v=0, lty=2)
    abline(v=c(-3,-2,-1,1,2,3), lty=2, col="gray")
    names <- names(coef(fit))[-1]
    names <- str_replace_all(names, "data2\\[, c\\(i\\)\\]", "")
    names <- str_replace_all(names, "factor.party.preference", "")
    names <- str_replace_all(names, "[.]", " ")
    names <- gsub("(^)([[:alpha:]])", "\\1\\U\\2", names, perl=TRUE)
    names[1:3] <- names.left[2:4]
    names <- str_replace(names, "Spd", "SPD")
    names <- str_replace(names, "Fdp", "FDP")
    names.left.below <- names
    axis(2, at = y.axis, label = names , las = 1, tick = T, ,mgp = c(2,.6,0), cex.axis = 0.7)
    # mtext(side=3,cex=0.9,line=1,"Left-right scale")
    # RIGHT ASSOCIATIONS
    fit2 <- lm(leftright ~  max.right.factor +
                 male + age + education + income + pol.interest + east + factor.party.preference, data=d) #
    u <- coef(fit2)[-1]
    low <- confint(fit2)[,1][-1]
    high <- confint(fit2)[,2][-1]
    y.axis <- c(length(u):1)
    plot(u, y.axis, type = "p", axes = F, xlab = "Left-right scale", ylab = "", pch = 20, cex = 1, xlim = c(-3,3), xaxs = "r", main="Associations with right (M2)", cex.main=.8)
    segments(low, y.axis, high, y.axis, lwd =  1)
    axis(1, at = seq(-3, 3, by=0.5), tick = T,cex.axis = 0.8, mgp = c(2,1.7,1))
    abline(v=0, lty=2)
    abline(v=c(-3,-2,-1,1,2,3), lty=2, col="gray")
    names <- names(coef(fit2))[-1]
    names <- str_replace_all(names, "data2\\[, c\\(i\\)\\]", "")
    names <- str_replace_all(names, "factor.party.preference", "")
    names <- str_replace_all(names, "[.]", " ")
    names <- gsub("(^)([[:alpha:]])", "\\1\\U\\2", names, perl=TRUE)
    names[1:3] <- names.right[2:4]
    names <- str_replace(names, "Spd", "SPD")
    names <- str_replace(names, "Fdp", "FDP")
    names.right.below <- names
    axis(2, at = y.axis, label = names , las = 1, tick = T, ,mgp = c(2,.6,0), cex.axis = 0.7)
    dev.off()
    stargazer(fit, fit2, type="latex", title="Regression Results",
              dep.var.labels=c("Left-right scale","Left-right scale"), covariate.labels=c(names.left.below[1:3], names.right.below),
               omit.stat=c("LL","ser","f"), ci=FALSE, digits=2, ci.level=0.95, single.row=FALSE, no.space=TRUE)

  # SUBSAMPLE TEST HOLDING ASSOCIATIONS FOR THE RESPECTIVE OTHER SCALE POINT CONSTANT
    # RIGHT TOPIC/ASSOCIATIONS CONSTANT
    # TABLE 6 IN APPENDIX
    fit1 <- lm(leftright ~ max.left.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.right==1,])
    fit2 <- lm(leftright ~ max.left.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.right==2,])
    fit3 <- lm(leftright ~ max.left.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.right==3,])
    fit4 <- lm(leftright ~ max.left.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.right==4,])
    #
    stargazer(fit1, fit2, fit3, fit4, type="latex", title="Regression Results", dep.var.labels=c("Left-right scale"), covariate.labels=names.left.below, omit.stat=c("LL","ser","f"), ci=FALSE, digits=2, ci.level=0.95, single.row=FALSE, column.labels = paste("Subsample", names.right, sep=" "), no.space=TRUE)
    # LEFT TOPIC/ASSOCIATIONS CONSTANT
    # TABLE 7 IN APPENDIX
    fit5 <- lm(leftright ~ max.right.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.left==1,])
    fit6 <- lm(leftright ~ max.right.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.left==2,])
    fit7 <- lm(leftright ~ max.right.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.left==3,])
    fit8 <- lm(leftright ~ max.right.factor + male + age + education + income + pol.interest + east + factor.party.preference, data=d[d$max.left==4,])
    #
    stargazer(fit5, fit6, fit7, fit8, type="latex", title="Regression Results", dep.var.labels=c("Left-right scale"), covariate.labels=names.right.below, omit.stat=c("LL","ser","f"), ci=FALSE, digits=2, ci.level=0.95, single.row=FALSE, column.labels = paste("Subsample", names.left, sep=" "), no.space=TRUE)



#  PART 3. If yes, are these associations systematically different for different groups of people?



  # EFFECT OF SOCIODEMOGRAPHICS ON TOPIC USAGE
    # LEFT
      ## regressions of topic proportions on demographic covariates
      fit1 <- summary(lm(Ltopic1*100 ~ education + age + male + east + income, data=d))
      fit2 <- summary(lm(Ltopic2*100 ~ education + age + male + east + income, data=d))
      fit3 <- summary(lm(Ltopic3*100 ~ education + age + male + east + income, data=d))
      fit4 <- summary(lm(Ltopic4*100 ~ education + age + male + east + income, data=d))
      ## saving the regression coefficients in data frame to plot them later
      df1 <- rbind(
        data.frame(responses="(L)", coef=coef(fit1)[-1,1], sd=coef(fit1)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.left[1],
        stringsAsFactors=F),
      data.frame(responses="(L)", coef=coef(fit2)[-1,1], sd=coef(fit2)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.left[2],
        stringsAsFactors=F),
      data.frame(responses="(L)", coef=coef(fit3)[-1,1], sd=coef(fit3)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.left[3],
        stringsAsFactors=F),
      data.frame(responses="(L)", coef=coef(fit4)[-1,1], sd=coef(fit4)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.left[4],
        stringsAsFactors=F))



    # RIGHT
      fit1 <- summary(lm(Rtopic1*100 ~ education + age + male + east + income, data=d))
      fit2 <- summary(lm(Rtopic2*100 ~ education + age + male + east + income, data=d))
      fit3 <- summary(lm(Rtopic3*100 ~ education + age + male + east + income, data=d))
      fit4 <- summary(lm(Rtopic4*100 ~ education + age + male + east + income, data=d))
      df2 <- rbind(
        data.frame(responses="(R)", coef=coef(fit1)[-1,1], sd=coef(fit1)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.right[1]),
        data.frame(responses="(R)", coef=coef(fit2)[-1,1], sd=coef(fit2)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.right[2]),
        data.frame(responses="(R)", coef=coef(fit3)[-1,1], sd=coef(fit3)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.right[3]),
        data.frame(responses="(R)", coef=coef(fit4)[-1,1], sd=coef(fit4)[-1,2],
        variable = c('Education (0-2)', 'Age (0-5)', 'Male (0-1)', 'East (0-1)',
          'Income (0-3)'), topic=names.right[4],
        stringsAsFactors=F))


      ### PLOT Socio-demographics and measurement values
      pd <- rbind(df1, df2)
      pd$topic <- factor(pd$topic, levels=c(names.left, names.right))
      p <- ggplot(pd, aes(y=coef, x=variable))
      pq <- p + coord_flip() +
      facet_wrap(~ topic, nrow=2) + #
      geom_linerange(aes(ymin=coef-2 *sd, ymax=coef+2*sd)) +
      geom_linerange(aes(ymin=coef-sd, ymax=coef+sd), size=1) +
      # changing y axis title and scale (note it's y and not bc of coord_flip)
      scale_y_continuous("Marginal Effect of Variable on Percent Topic Usage") +
      # adding line at 0
      geom_hline(yintercept=0,color="grey50") +
      theme_bw() + theme( axis.title.y=element_blank() )
      pq
      ggsave(pq, file="plots/topic-model-plot.pdf", height=4.25, width=8.5)




  # RELATIONSHIPS AND BIAS

    # EDUCATION-> IDEOLOGY
    fit1 <- lm(leftright ~ education, data=d[!is.na(d$max.left),])
    fit2 <- lm(leftright ~ education + max.left.factor, data=d[!is.na(d$max.left),])
    fit3 <- lm(leftright ~ education + max.left.factor + education*max.left.factor, data=d[!is.na(d$max.left),])
    fit4 <- lm(leftright ~ education, data=d[!is.na(d$max.right),])
    fit5 <- lm(leftright ~ education + max.right.factor, data=d[!is.na(d$max.right),])
    fit6 <- lm(leftright ~ education + max.right.factor + education*max.right.factor, data=d[!is.na(d$max.right),])
    stargazer(fit1, fit2, fit3, fit4, fit5, fit6, type="latex", title="Regression Results", dep.var.labels=c("Left-right scale"), covariate.labels = c("Education", names.left[2:4], paste("Education *", names.left[2:4]), names.right[2:4], paste("Education *", names.right[2:4])), omit.stat=c("LL","ser","f"), ci=FALSE, digits=2, ci.level=0.95, single.row=FALSE, no.space=TRUE)

    # Check whether it's a sample problem:     summary(lm(leftright ~ education, data=d[!is.na(d$max.right),]))
    # It's not...


#########################################
######## NETWORK v PUNDIT MODELS ########
#########################################


library(lmtest)
library(sandwich)


svp <- read.csv()

names(svp)


# Table A3.1 Appendix (Figure 2 main text)
ideo.mod = lmer(pun_ideo ~ treatment + as.factor(pundit) + (1|resp_id),
                      data=svp, REML=TRUE); summary(ideo.mod, corr = FALSE)



### NOTE ###
# Annotated so that first two letters represent the source (aka network)
# Last two letters represent pundit.
# co = control network (i.e. AP)
# i = in-party
# o = out-party
# Example: si.po = in-party network, out-party pundit

#Table A3.2 Bias Model Appendix (Figure 3 main text)
bias.mod = lmer(bias ~ coparty_treatment + as.factor(pundit)+ (1|resp_id),
                     data=svp, REML=TRUE); summary(bias.mod, corr = FALSE)

#Table A3.2 Interest Model Appendix (Figure 4 main text)
interest.mod = lmer(interest ~ coparty_treatment + as.factor(pundit)+ (1|resp_id),
                     data=svp, REML=TRUE); summary(interest.mod, corr = FALSE)

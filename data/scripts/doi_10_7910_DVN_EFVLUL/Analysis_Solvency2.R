################################################################################
## This script analyzes the effect of an in crease in hedging demand on
## cross-currency bases after implementation of Solvency II.
##

## Use monthly data
panel_s2 = read_fst('../CleanData/S2Panel_M.fst', as.data.table = TRUE)
panel_s2 = panel_s2[ccy != 'DKK']

##
panel_s2[ , i.S2 := as.numeric(ccy %in% c('GBP', 'EUR'))]
panel_s2[ , i.post := as.numeric(date >= as.yearmon(ymd('20160101')))]
panel_s2[ , i.S2.post := i.S2 * i.post]

init.date = as.yearmon(ymd('20140101'))
end.date  = as.yearmon(ymd('20180101'))

data.plt = copy(panel_s2[date >= init.date & date <= end.date])
data.plt[ , b.abs := abs(b.1y)]
data.plt[ , b.5y.abs := abs(b.5y)]
data.plt[ , debt_usd_gdp.abs := abs(debt_usd_gdp)]
data.plt[ , yield_spread.1y := fwd_prem.1y - (b.1y / 100)]
data.plt[ , yield_spread.5y := fwd_prem.5y - (b.5y / 100)]

mod.b = lm(b.abs ~ debt_usd_gdp.abs, data.plt[date < 2016], na.action = na.exclude)
mod.b.5y = lm(b.5y.abs ~ debt_usd_gdp.abs, data.plt[date < 2016], na.action = na.exclude)

data.plt[ , b.resid := b.abs - predict(mod.b, newdata=data.plt)]
data.plt[ , b.5y.resid := b.5y.abs - predict(mod.b.5y, newdata=data.plt)]

data.plt.collapsed = data.plt[ , .(b.resid = mean(b.resid),
                                   b.5y.resid = mean(b.5y.resid)),
                              by=.(i.S2, date)]

data.plt.wide =
    dcast(data.plt.collapsed, date ~ i.S2,
          value.var=c('b.resid', 'b.5y.resid'))
data.plt.wide[ , b.resid.diff := b.resid_1 - b.resid_0]
data.plt.wide[ , b.5y.resid.diff := b.5y.resid_1 - b.5y.resid_0]

plt.b.resid =
    ggplot(data.plt.wide, aes(y=b.resid.diff, x=date)) +
    geom_point() + geom_vline(xintercept = 2016) +
    theme_few() +
    ylab('basis points') + xlab('') +
    ggtitle('Currency basis diff (1Y)')

file_out = str_c('../Out/Figure_6_A.pdf')
ggsave(file_out, plot = plt.b.resid, width=4, height=4.2)

plt.b.5y.resid =
    ggplot(data.plt.wide, aes(y=b.5y.resid.diff, x=date)) +
    geom_point() + geom_vline(xintercept = 2016) +
    theme_few() +
    ylab('basis points') + xlab('') +
    ggtitle('Currency basis diff (5Y)')

file_out = str_c('../Out/Figure_6_B.pdf')
ggsave(file_out, plot = plt.b.5y.resid, width=4, height=4.2)


data.collapsed =
    data.plt[ ,
             .(b.abs=mean(b.abs), b.5y.abs=mean(b.5y.abs),
               i.S2=mean(i.S2),
               i.S2post = mean(i.S2 * i.post),
               debt_usd_gdp.abs = mean(debt_usd_gdp.abs),
               yield_spread.1y = mean(yield_spread.1y),
               yield_spread.5y = mean(yield_spread.5y),
               fwd_prem.1y = mean(fwd_prem.1y),
               date = mean(date)),
             by=.(ccy, i.post)]


mod.b.out =
    felm(b.abs ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.plt)

mod.b.xs.out =
    felm(b.abs ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.collapsed)

mod.b.5y.out =
    felm(b.5y.abs ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.plt)

mod.b.5y.xs.out =
    felm(b.5y.abs ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.collapsed)

texreg(list(mod.b.out, mod.b.5y.out, mod.b.xs.out, mod.b.5y.xs.out),
       file = '../Out/Table_8.tex',
       table=FALSE, booktabs = TRUE, use.packages=FALSE,
       float.pos = 'htp!', stars = c(0.01, 0.05, 0.10),
       reorder.coef = c(5, 4, 3, 2, 1),
       custom.header = list('Monthly Data'=1:2, 'Collapsed Data'=3:4),
       custom.model.names = c('1 Year', '5 Year',
                              '1 Year', '5 Year'),
       custom.coef.names =
           c('Constant', "|USD Imbalance|",
             'Post', "Solvency 2", "Solvency 2 $\\times$ Post"),
       custom.note='', include.groups=FALSE, include.adjrs=FALSE)


#### What is happening with yields?

mod.1y.out =
    felm(abs(yield_spread.1y) ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.plt)

mod.1y.xs.out =
    felm(abs(yield_spread.1y) ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.collapsed)

mod.5y.out =
    felm(abs(yield_spread.5y) ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.plt)

mod.5y.xs.out =
    felm(abs(yield_spread.5y) ~ debt_usd_gdp.abs + i.S2*i.post |
             0 | 0 | ccy + date,
         data=data.collapsed)

texreg(list(mod.1y.out, mod.5y.out, mod.1y.xs.out, mod.5y.xs.out),
       file = '../Out/Table_A9.tex',
       table=FALSE, booktabs = TRUE, use.packages=FALSE,
       float.pos = 'htp!', stars = c(0.01, 0.05, 0.10),
       reorder.coef = c(5, 4, 3, 2, 1),
       custom.header = list('Monthly Data'=1:2, 'Collapsed Data'=3:4),
       custom.model.names = c('1 Year', '5 Year',
                              '1 Year', '5 Year'),
       custom.coef.names =
           c('Constant', "|USD Imbalance|",
             'Post', "Solvency 2", "Solvency 2 $\\times$ Post"),
       custom.note='', include.groups=FALSE, include.adjrs=FALSE)

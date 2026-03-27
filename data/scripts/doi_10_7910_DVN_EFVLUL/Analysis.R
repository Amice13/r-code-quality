################################################################################
## Analysis.R
## This script conducts the analysis and produces figures for the paper
################################################################################


rm(list = ls())

source('Init.R')

## Load data
datin   = read_fst('../CleanData/MainPanel_M.fst', as.data.table = TRUE)
panel_d = read_fst('../CleanData/MainPanel_D.fst', as.data.table = TRUE)
panel_swap = read_fst('../CleanData/SwapLinePanel.fst', as.data.table = TRUE)

## Generate main and extended panels
panel = datin[g10 == TRUE]

## Generate fx volatility measures
panel[ , mean.fxvol := mean(fxvol), by = .(date)]
panel[ , mean.D.fxvol := mean(D.fxvol.1m), by = .(date)]

## Standardize volatility variables
panel[ , fxvol.std := scale(fxvol), by=.(ccy)]
panel[ , D.fxvol.1m.std := scale(D.fxvol.1m), by=.(ccy)]
panel[ , mean.D.fxvol.std := scale(mean.D.fxvol)]

panel.1y = panel[tenor == '1y']
panel.1y[ , eq_usd_gdp := niip_usd_gdp - debt_usd_gdp]

############################################################
## Figure 1

hedge_vol = as.data.table(fread('../CleanData/hedgeratio_cvix.csv'))
hedge_vol[ , date:=mdy(date)]

pdf('../Out/Figure_1.pdf', width=8, height=6)

dualplot(hedge_vol$date,
         hedge_vol$CVIX, hedge_vol$HedgeRatio,
         ylab1='Currency Volatility Index (CVIX)',
         ylab2='Hedge Ratio (Percent)', ylim1=c(0.05,.20), ylim2=c(30,90),
         col = c('tomato2', 'dodgerblue2'), lwd = c(2, 2))

dev.off()

############################################################
## Figure 2

## Cross-currency bases
plot_levels(panel.1y, 'b', 'debt_usd_gdp', dtrange=c(2000, 2021),
            exportexhibits = export)

## Option risk-reversals
plot_levels(panel.1y, 'rr', 'debt_usd_gdp', dtrange=c(2000, 2021),
            exportexhibits = export)


############################################################
## Figure 3 and Figure A2
plot_timeseries_month(panel.1y, 2001.75, 2022)


############################################################
## Figure 4
plot_levels(panel.1y, 'fwd_prem', 'debt_usd_gdp', dtrange=c(2000, 2021),
            exportexhibits = export)

plot_levels(panel.1y, 'rx', 'debt_usd_gdp', dtrange=c(2000, 2021),
            exportexhibits = export)

############################################################
## Figure 5
plot_levels(panel.1y, 'sprd', 'debt_usd_gdp', dtrange=c(2000, 2021),
            exportexhibits = TRUE)

############################################################
## Figure 6 and Tables 8, A9: Solvency II 

source('Analysis_Solvency2.R')


############################################################
## Figure 7
plot_swapline_covid(panel_swap, 'debt_usd_gdp', exportexhibits = export)


############################################################
## Table 1: Summary Statistics
summary_row = function(dat, label){
    ave = round(mean(dat, na.rm = TRUE), 2)
    sd  = round(sd(dat, na.rm = TRUE), 2)
    min = round(min(dat, na.rm = TRUE), 2)
    max = round(max(dat, na.rm = TRUE), 2)
    out = c(label, ave, sd, min, max)
    return(out)
}

tmp = panel.1y[ , .(b, abs.b = abs(b), sprd, rx, rr,
                    fxvol, D.fxvol.1m,
                    niip_usd_gdp, debt_usd_gdp, eq_usd_gdp)]
sum.table =
        rbind(summary_row(tmp$b, 'Cross-currency basis (bps)'),
          summary_row(tmp$abs.b, 'Absolute cross-currency basis (bps)'),
          summary_row(tmp$rx, 'Annualized currency excess returns (pct)'),
          summary_row(tmp$sprd, '5-yr minus 1-yr basis spread (bps)'),
          summary_row(tmp$rr, 'Risk-reversal (pct)'),
          summary_row(tmp$fxvol, 'FX Volatility (pct)'),
          summary_row(tmp$D.fxvol.1m, '$\\Delta$ FX Volatility (pct)'),
          summary_row(tmp$niip_usd_gdp, 'USD NFA / GDP'),
          summary_row(tmp$debt_usd_gdp, 'USD Net Debt Holdings / GDP'),
          summary_row(tmp$eq_usd_gdp, 'USD Net Equity Holdings / GDP'))

sum.table =
    kbl(sum.table,
        booktabs=TRUE, linesep = "", format='latex',
        digits=0, format.args = list(big.mark = ","),
        align = c('l', 'r', 'r', 'r', 'r'),
        col.names = NULL, table.envir = NULL, escape=FALSE) %>%
    add_header_above(c('', 'Mean', 'Std. Dev.', 'Min', 'Max'), bold=TRUE)

cat(sum.table, file='../Out/Table_1.tex')


############################################################
## Tables 2, 3, A3, A4, and A5
reg_measures(panel.1y)

############################################################
## Table 4
reg.b.self =
    felm(D.b.1m ~ debt_usd_gdp + D.fxvol.1m.std + debt_usd_gdp : D.fxvol.1m.std |
                ccy + date | 0 | ccy + date, panel.1y)
reg.b.global =
    felm(D.b.1m ~ debt_usd_gdp + debt_usd_gdp : mean.D.fxvol.std |
                ccy + date | 0 | ccy + date, panel.1y)
reg.rr.self =
    felm(D.rr.1m ~ debt_usd_gdp + D.fxvol.1m.std + debt_usd_gdp : D.fxvol.1m.std |
                ccy + date | 0 | ccy + date, panel.1y)
reg.rr.global =
    felm(D.rr.1m ~ debt_usd_gdp + debt_usd_gdp : mean.D.fxvol.std |
                ccy + date | 0 | ccy + date, panel.1y)

texreg(list(reg.b.self, reg.b.global, reg.rr.self, reg.rr.global),
       file = '../Out/Table_4.tex',
       use.packages=FALSE, table=FALSE, booktabs = TRUE,
       float.pos = 'htp!',
       stars = c(0.1, 0.05, 0.01),
       custom.header = list('Currency Basis (bps)'=1:2,
                            'Risk Reversals (pct)' = 3:4),
       custom.model.names = c('(1)', '(2)', '(3)', '(4)'),
       custom.coef.names =
           c('USD Imbalance', '$\\Delta$ FX Vol.',
             'USD Imbalance $\\times$ $\\Delta$ FX Vol.',
             'USD Imbalance $\\times$ $\\Delta$ Global FX Vol.'),
       reorder.coef = c(3, 4, 1, 2),
       omit.stat = c('adj.rsq', 'ser'), custom.note='',
       include.groups=FALSE, include.adjrs=FALSE)


############################################################
## Table 5
reg.fx.self =
    felm(D.s.1m ~ debt_usd_gdp * D.fxvol.1m.std |
                ccy + date | 0 | ccy + date, panel.1y)
reg.fx.global =
    felm(D.s.1m ~ debt_usd_gdp + debt_usd_gdp : mean.D.fxvol.std |
                ccy + date | 0 | ccy + date, panel.1y)

reg.fx.self.niip =
    felm(D.s.1m ~ niip_gdp * D.fxvol.1m.std |
                ccy + date | 0 | ccy + date, panel.1y)

reg.fx.global.niip =
    felm(D.s.1m ~ niip_gdp +  niip_gdp : mean.D.fxvol.std |
                ccy + date | 0 | ccy + date, panel.1y)

reg.fx.self.race =
    felm(D.s.1m ~ niip_gdp + debt_usd_gdp * D.fxvol.1m.std + niip_gdp : D.fxvol.1m.std |
                ccy + date | 0 | ccy + date, panel.1y)

reg.fx.global.race =
    felm(D.s.1m ~ debt_usd_gdp + niip_gdp +
             debt_usd_gdp : mean.D.fxvol.std +
             niip_gdp : mean.D.fxvol.std |
             ccy + date | 0 | ccy + date, panel.1y)

texreg(list(reg.fx.self, reg.fx.global,
            ## reg.fx.self.niip, reg.fx.global.niip,
            reg.fx.self.race, reg.fx.global.race),
       file = '../Out/Table_5.tex',
       use.packages=FALSE, table=FALSE, booktabs = TRUE,
       custom.coef.names =
            c('USD Imbalance', '$\\Delta$ FX Vol.',
              'USD Imbalance $\\times$ $\\Delta$ FX Vol.',
              'USD Imbalance $\\times$ $\\Delta$ Global FX Vol.',
              'NFA',
              'NFA$\\times$ $\\Delta$ FX Vol.',
              'NFA$\\times$ $\\Delta$ Global FX Vol.'),
       float.pos = 'htp!',
       omit.coef = '(Intercept)',,
       reorder.coef = c(3, 4, 6, 7, 1, 2, 5),
       custom.model.names = c('(1)', '(2)', '(3)', '(4)'),
       omit.stat = c('adj.rsq', 'ser'), custom.note='',
       stars = c(0.1, 0.05, 0.01),
       include.groups=FALSE, include.adjrs=FALSE)


############################################################
## Table 6 and Table A8: Forecasting Regressions 
forecast_regressions(panel.1y)

############################################################
## Table 7: Cross-currency Basis Term Structure
reg.avg.sprd =
    felm(sprd ~ debt_usd_gdp | 0 | 0 | ccy + date, panel.1y)
reg.avg.sprd.fe =
    felm(sprd ~ debt_usd_gdp | date | 0 | ccy + date, panel.1y)
reg.sprd.self =
    felm(D.sprd.1m ~ debt_usd_gdp + D.fxvol.1m.std + debt_usd_gdp : D.fxvol.1m.std |
                date | 0 | ccy + date, panel.1y)
reg.sprd.global =
    felm(D.sprd.1m ~ debt_usd_gdp + debt_usd_gdp : mean.D.fxvol.std |
                date | 0 | ccy + date, panel.1y)

texreg(list(reg.avg.sprd, reg.avg.sprd.fe, reg.sprd.self, reg.sprd.global),
       file = '../Out/Table_7.tex',
       use.packages=FALSE, table=FALSE, booktabs = TRUE,
       float.pos = 'htp!',
       custom.header = list('Level (pct)'=1:2, 'Changes (pct)' = 3:4),
       custom.model.names = c('(1)', '(2)', '(3)', '(4)'),
       omit.coef = '(Intercept)',
       custom.coef.names =
            c('USD Imbalance', '$\\Delta$ FX Vol.',
                'USD Imbalance $\\times$ $\\Delta$ FX Vol.',
                'USD Imbalance $\\times$ $\\Delta$ Global FX Vol.'),
       reorder.coef = c(1, 3, 4, 2),
       stars = c(0.1, 0.05, 0.01),
       omit.stat = c('adj.rsq', 'ser'), custom.note='',
       include.groups=FALSE, include.adjrs=FALSE,
       custom.gof.rows =
           list('Date F.E.' = c('', 'Y', 'Y', 'Y'),
                'Currency F.E.' = c('', '', 'Y', 'Y')))

############################################################
#### Table A2

setorder(panel.1y, ccy, date)

panel.1y[ , RX_1y := exp(ds / 100) - exp(fwd_prem / 100), by=.(ccy)]
panel.1y[ , dS_1m := shift(exp(s), -1) / exp(s), by=.(ccy)]
panel.1y[ , realized_dS_sd :=
                frollapply(exp(dS_1m), 12, FUN=sd, align = 'left') * sqrt(12),
         by=.(ccy)]

panel.1y[ , realized_dS_var :=realized_dS_sd ^ 2, by=.(ccy)]

us_1y = fredr(series_id = 'DGS1', frequency = 'm')
us_1y = as.data.table(us_1y)
us_1y[ , date := as.yearmon(date)]
us_1y[ , us_1y := value / 100 + 1]
panel_tmp = merge(panel.1y, us_1y[ , .(date, us_1y)],
                  by='date', all.x = TRUE) 

panel_tmp[ , RX_1y_scaled := RX_1y / (debt_usd_gdp * us_1y)]
panel_tmp[ , fxvol := fxvol / 100]
panel_tmp[ , fxvar := fxvol^2]

reg_1 = felm(RX_1y_scaled ~ fxvar | 0 | 0 | ccy + date, panel_tmp)

reg_2 = felm(RX_1y_scaled ~ fxvar | date | 0 | ccy + date, panel_tmp)

reg_3 = felm(RX_1y_scaled ~ fxvar | date + ccy | 0 | ccy + date, panel_tmp)

texreg(list(reg_1, reg_2, reg_3),
       use.packages=FALSE, table=FALSE, booktabs = TRUE,
       float.pos = 'htp!',
       stars = c(0.1, 0.05, 0.01),
       custom.model.names = c('(1)', '(2)', '(3)'),
       custom.coef.names =
           c('Constant', 'FX Vol. (Realized)'),
       omit.stat = c('adj.rsq', 'ser'), custom.note='',
       file = '../Out/Table_A2.tex',
       include.groups=FALSE, include.adjrs=FALSE)


############################################################
#### Table A6
reg.b.self =
    felm(D.b.1m ~ debt_usd_gdp + D.fxvol.1m.std + debt_usd_gdp : D.fxvol.1m.std |
                date | 0 | date, panel.1y)
reg.b.global =
    felm(D.b.1m ~ debt_usd_gdp + debt_usd_gdp : mean.D.fxvol.std |
                date | 0 | date, panel.1y)
reg.rr.self =
    felm(D.rr.1m ~ debt_usd_gdp + D.fxvol.1m.std + debt_usd_gdp : D.fxvol.1m.std |
                date | 0 | date, panel.1y)
reg.rr.global =
    felm(D.rr.1m ~ debt_usd_gdp + debt_usd_gdp : mean.D.fxvol.std |
                date | 0 | date, panel.1y)

texreg(list(reg.b.self, reg.b.global, reg.rr.self, reg.rr.global),
       file = '../Out/Table_A6.tex',
       use.packages=FALSE, table=FALSE, booktabs = TRUE,
       float.pos = 'htp!',
       stars = c(0.1, 0.05, 0.01),
       custom.header = list('Currency Basis (bps)'=1:2,
                            'Risk Reversals (pct)' = 3:4),
       custom.model.names = c('(1)', '(2)', '(3)', '(4)'),
       custom.coef.names =
           c('USD Imbalance', '$\\Delta$ FX Vol.',
             'USD Imbalance $\\times$ $\\Delta$ FX Vol.',
             'USD Imbalance $\\times$ $\\Delta$ Global FX Vol.'),
       reorder.coef = c(3, 4, 1, 2),
       omit.stat = c('adj.rsq', 'ser'), custom.note='',
       include.groups=FALSE, include.adjrs=FALSE)

############################################################
## Figure A3: Pre-crisis unconditional relationships

plot_levels(panel.1y, 'b', 'debt_usd_gdp', dtrange=c(2000, 2008),
            exportexhibits = export, label = '_pre07')

plot_levels(panel.1y, 'rr', 'debt_usd_gdp', dtrange=c(2000, 2008),
            exportexhibits = export, label = '_pre07')


############################################################
## Table A7

reg.fx.self.eq =
    felm(D.s.1m ~ eq_usd_gdp + D.fxvol.1m.std + eq_usd_gdp : D.fxvol.1m.std |
                ccy + date | 0 | ccy + date, panel.1y)
reg.fx.global.eq =
    felm(D.s.1m ~ eq_usd_gdp + eq_usd_gdp : mean.D.fxvol.std |
                ccy + date | 0 | ccy + date, panel.1y)

texreg(list(reg.fx.self.eq, reg.fx.global.eq),
       file = '../Out/Table_A7.tex',
       use.packages=FALSE, table=FALSE, booktabs = TRUE,
       float.pos = 'htp!',
       custom.header = list('Currency Returns (pct)' = 1:2),
       custom.model.names = c('(1)', '(2)'),
       omit.coef = '(Intercept)',
       custom.coef.names =
            c('USD Equity Imbalance', '$\\Delta$ FX Vol.',
                'USD Equity Imbalance $\\times$ $\\Delta$ FX Vol.',
                'USD Equity Imbalance $\\times$ $\\Delta$ Global FX Vol.'),
       reorder.coef = c(3, 4, 1, 2),
       stars = c(0.1, 0.05, 0.01),
       omit.stat = c('adj.rsq', 'ser'), custom.note='',
       include.groups=FALSE, include.adjrs=FALSE)


############################################################
## Figure A4

plot_timeseries_covid(panel_d, dtbegin_covid, dtend_covid)


############################################################
## Figure A5

## Load raw ccy data for term structure
xc    = read_fst('../CleanData/xccyextended.fst',as.data.table=T)   
xcf = xc[source == 'def']
xcf[tenor %like% 'y$',tenoryr:=as.numeric(str_extract(tenor,'\\d+'))]
xcf[tenor %like% 'm$',tenoryr:=as.numeric(str_extract(tenor,'\\d+'))/12]

plot_basis_term(xcf, 'covid')
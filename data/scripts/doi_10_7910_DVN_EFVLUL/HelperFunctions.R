################################################################################
## Contains helper functions to build exhibits.
################################################################################


plot_basis_term = function(panel, label,
                           exportexhibits = TRUE,
                           dates = c(ymd(20200213), ymd(20200313))){

    data_JPY = copy(panel)
    data_JPY = data_JPY[ccy == 'JPY' & date %in% dates,
                .(date=as.character(date), tenor=(tenoryr), b)]
    data_JPY[date == '2020-02-13', date := 'Feb 13, 2020']
    data_JPY[date == '2020-03-13', date := 'Mar 13, 2020']

    data_AUD = copy(panel)
    data_AUD = data_AUD[ccy == 'AUD' & date %in% dates,
                .(date=as.character(date), tenor=(tenoryr), b)]
    data_AUD[date == '2020-02-13', date := 'Feb 13, 2020']
    data_AUD[date == '2020-03-13', date := 'Mar 13, 2020']

    fig_JPY = ggplot(data_JPY, aes(x=tenor, y=b, color=date)) +
        geom_line() + theme_few() + theme(legend.title=element_blank()) +
        ggtitle('JPY') + ylab('Cross-currency basis (bps)') +
        xlab('Maturity (Years)')+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = 'right') +
        scale_color_brewer(palette="Dark2")

    fig_AUD = ggplot(data_AUD, aes(x=tenor, y=b, color=date)) +
        geom_line() + theme_few() + theme(legend.title=element_blank()) +
        ggtitle('AUD') + ylab('Cross-currency basis (bps)') +
        xlab('Maturity (Years)')+
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = 'right') +
        scale_color_brewer(palette="Dark2")

    fig_out = ggarrange(fig_AUD, fig_JPY,
                        common.legend = TRUE,
                        legend = 'bottom')

    if(exportexhibits){
        file_out = str_c('../Out/term_', label, '.pdf')
        ggsave(file_out, plot = fig_out, width=8, height=4)
    }
}


plot_levels = function(panel, var, measure,
                       dtrange = c(ymd(20000101), ymd(20200401)),
                       label = '', stat_loc = 'upper',
                       exportexhibits=FALSE){

    ## plot avg (unconditional) levels in a given drange versus measures
    data = copy(panel[date %between% dtrange])
    setnames(data, old = measure, new = 'measure')
    setnames(data, old = var, new = 'var')

    avg = data[ , .(measure = mean(measure, na.rm = T),
                    var = mean(var, na.rm = T)),
               .(country, ccy, region, bis, g10)]

    if(var == 'fx'){
        vlab = 'SPOT'
        line_color = 'green4'
        ylab = TeX('Appreciation $\\leftarrow$ Spot (pct) $\\rightarrow$ Depreciation')
    } else if(var == 'b'){
        vlab = 'CIP'
        line_color = 'blue4'
        ylab = TeX('Overvalue $\\leftarrow$ Forward (bps) $\\rightarrow$ Undervalue')
    } else if(var == 'sprd'){
        vlab = 'SPRD'
        line_color = 'yellow4'
        ylab = '5y - 1y Spread (bps)'
    } else if(var == 'rx'){
        vlab = 'RX'
        line_color = 'green4'
        ylab = 'FX Excess Returns (pct)'
    } else if(var == 'fwd_prem'){
        vlab = 'FP'
        line_color = 'aquamarine4'
        ylab = 'Forward Premium (pct)'
    } else if(var == 'rx_fwd'){
        vlab = 'RXFWD'
        line_color = 'orange4'
        ylab = 'Forward Excess Return (pct)'
    } else if(var == 'rr'){
        vlab = 'OPT'
        line_color = 'darkred'
        ylab = TeX('Put Expensive $\\leftarrow$ Risk-Reversal % $\\rightarrow$ Call Expensive')
    } else{
      vlab = var
      line_color = 'red'
      ylab = var
    }

    if(measure == 'niip_gdp'){
        xlab = 'NIIP (share of GDP)'
        flab = 'NIIP'
    } else if(measure == 'ca_gdp'){
        xlab = 'Current Account (share of GDP)'
        flab = 'CA'
    } else if(measure == 'debt_gdp'){
        xlab = 'Net Debt + FDI (share of GDP)'
        flab = 'Debt'
    } else if(measure == 'debt_usd_gdp'){
      xlab = 'USD Debt Holdings (share of GDP)'
      flab = 'DolDebt'
    } else if(measure == 'niip_usd_gdp'){
      xlab = 'USD NIIP (share of GDP)'
      flab = 'DolNIIP'
    } else if(measure == 'niip'){
      xlab = 'NIIP'
      flab = 'NIIPDollar'
    } else{
      xlab = measure
      flab = measure
    }

    reg_avg = lm(var ~ measure, avg)
    b_measure = round(reg_avg$coefficients[2], 2)
    rsq  = round(summary(reg_avg)$r.squared, 2)
    corr = round(cor(avg$measure, avg$var) , 2)
    stat_line = str_c('corr. = ', corr)

    x_loc = 0.8 * max(avg$measure) + 0.2 * min(avg$measure)
    y_loc = max(avg$var)
    if(stat_loc == 'lower'){
        y_loc = min(avg$var)
    }

    plot =
        ggplot(avg, aes(x=measure, y=var, label=ccy)) +
        geom_point() + geom_smooth(method='lm', se=F, color = line_color) +
        geom_text_repel() +
        theme(legend.position='bottom') + theme_few() +
        ylab(ylab) + xlab(xlab) +
        annotate('text', x = x_loc, y = y_loc, label = stat_line, size = 4.5)
    if (vlab=='OPT') {
      plot=plot+ theme(text = element_text(size=11))
    }

    if(exportexhibits){
        out_file = str_c('../Out/', vlab, 'Avg_', flab, label, '.pdf')
        ggsave(out_file, plot = plot, width=4, height = 4.2)
    } else {
        return(plot)
    }
}


plot_changes = function(panel, var, measure, date_range, label,
                        exportexhibits = TRUE, drop_NOK = FALSE,
                        stat_loc = 'upper'){

    ## plot change in a date range versus measures
    data = copy(panel)
    setnames(data, old = measure, new = 'measure')
    setnames(data, old = var, new = 'var')

    if(var == 'fx'){
        vlab = 'SPOT'
        line_color = 'green4'
        ylab = TeX('Appreciation $\\leftarrow$ Spot (pct) $\\rightarrow$ Depreciation')
    } else if(var == 'b'){
        vlab = 'CIP'
        line_color = 'blue4'
        ylab = TeX('Overvalue $\\leftarrow$ Forward (bps) $\\rightarrow$ Undervalue')
      } else if(var == 'rr'){
        vlab = 'OPT'
        line_color = 'darkred'
        ylab = TeX('Put Expensive $\\leftarrow$ Risk-Reversal (pct)  $\\rightarrow$ Call Expensive')
      } else{
        vlab = var
        line_color = 'red'
        ylab = var
      }

    if(measure == 'niip_gdp'){
        xlab = 'NIIP (share of GDP)'
        flab = 'NIIP'
    } else if(measure == 'ca_gdp'){
        xlab = 'Current Account (share of GDP)'
        flab = 'CA'
    } else if(measure == 'debt_gdp'){
        xlab = 'Net Debt + FDI (share of GDP)'
        flab = 'Debt'
    } else if(measure == 'debt_usd_gdp'){
        xlab = 'USD Debt Holdings (share of GDP)'
        flab = 'DolDebt'
    } else if(measure == 'niip_usd_gdp'){
        xlab = 'USD NIIP (share of GDP)'
        flab = 'DolNIIP'
    } else if(measure == 'eq_gdp'){
        xlab = 'Net Equity (share of GDP)'
        flab = 'Equity'
      } else if(measure == 'niip'){
        xlab = 'NIIP'
        flab = 'NIIPDollar'
      } else{
        xlab = measure
        flab = measure
      }

    data = merge(data[date %between% date_range & !is.na(var), .SD[.N], .(ccy)],
                 data[date %between% date_range & !is.na(var),
                      .(D.var = .SD[.N, var] - .SD[1, var]), .(ccy)],
                 by = 'ccy', all.x = T)


    x_loc = 0.8 * max(data$measure) + 0.2 * min(data$measure)
    y_loc = max(data$D.var)
    if(stat_loc == 'lower'){
        y_loc = min(data$D.var)
    }

    if(drop_NOK){

        entry_NOK = data[ccy == 'NOK']

        corr = round(cor(data[ccy != 'NOK']$measure, data[ccy != 'NOK']$D.var) , 2)
        stat_line = str_c('corr. = ', corr)
        y_loc = 0.8 * max(data$D.var) + 0.2 * min(data$D.var)

        plt = data[g10==T & ccy != 'NOK'] %>%
            ggplot(aes(x = measure, y = D.var, label=ccy)) +
            geom_point() + geom_smooth(method='lm', se=F, color = line_color) +
            geom_point(data = entry_NOK, color = 'gray30') +
            geom_text_repel(data = entry_NOK, color = 'gray30', label = 'NOK')

    } else {

        corr = round(cor(data$measure, data$D.var) , 2)
        stat_line = str_c('corr. = ', corr)

        plt = data[g10==T] %>%
            ggplot(aes(x=measure, y=D.var, label=ccy)) +
            geom_point() + geom_smooth(method='lm', se=F, color = line_color)
    }

    plt = plt +
        geom_text_repel() +
        annotate('text', x = x_loc, y = y_loc, label = stat_line, size = 4.5) +
        theme(legend.position='bottom') + theme_few() +
        ylab(ylab) + xlab(xlab)

    if(exportexhibits){
        out_file = str_c('../Out/', vlab,'Chg_', label, '_', flab, '.pdf')
        ggsave(out_file,
               plot = plt, width=4, height = 4.2)
    } else {
        return(plt)
    }
}

plot_swapline_covid = function(panel, measure,
                               exportexhibits = TRUE){

    perm_lines = c('JPN', 'EUR', 'GBR', 'CAN', 'CHE')

    data = copy(panel)
    setnames(data, old = measure, new = 'measure')

    if(measure == 'niip_gdp'){
        xlab = 'NIIP (share of GDP)'
        flab = 'NIIP'
    } else if(measure == 'ca_gdp'){
        xlab = 'Current Account (share of GDP)'
        flab = 'CA'
    } else if(measure == 'debt_gdp'){
        xlab = 'Net Debt + FDI (share of GDP)'
        flab = 'Debt'
    } else if(measure == 'debt_usd_gdp'){
        xlab = 'USD Debt Holdings (share of GDP)'
        flab = 'DolDebt'
    } else if(measure == 'niip_usd_gdp'){
        xlab = 'USD NIIP (share of GDP)'
        flab = 'DolNIIP'
    } else if(measure == 'eq_gdp'){
        xlab = 'Net Equity (share of GDP)'
        flab = 'Equity'
    } else{
        xlab = measure
        flab = measure
    }

    ## Stick correlation statistic in upper right corner
    corr = round(cor(data$measure, data$amt_gdp) , 2)
    stat_line = str_c('corr. = ', corr)
    x_loc = 0.8 * max(data$measure) + 0.2 * min(data$measure)
    y_loc = max(data$amt_gdp)

    plot =
        data %>%
        ggplot(aes(x=measure, y=amt_gdp, label=iso3)) +
        geom_point() + geom_smooth(method='lm', se = FALSE, color = 'red4') +
        annotate('text', x = x_loc, y = y_loc, label = stat_line, size = 4.5) +
        geom_hline(yintercept = 0, color = 'grey') +
        geom_text_repel() +
        theme(legend.position='none') + theme_few() +
        ylab('Maximum Draws (share of GDP)') + xlab(xlab)

    if(exportexhibits){
        out_file = str_c('../Out/Figure_7.pdf')
        ggsave(out_file, plot, width=4, height=4.2)
    } else {
        return(plot)
    }
}


reg_fxvol = function(dt, measure='debt_gdp'){

    if(measure == 'niip_gdp'){
        xlab = 'NIIP (share of GDP)'
        measure_lab = 'NIIP / GDP'
        cross_lab = 'NIIP / GDP \\times \\Delta FX Vol'
        flab = 'NIIP'
    } else if(measure == 'ca_gdp'){
        xlab = 'Current Account (share of GDP)'
        measure_lab = 'CA / GDP'
        cross_lab = 'CA / GDP \\times \\Delta FX Vol'
        flab = 'CA'
    } else if(measure == 'debt_gdp'){
        xlab = 'Net Debt + FDI (share of GDP)'
        measure_lab = 'Debt + FDI / GDP'
        cross_lab = 'Debt + FDI / GDP \\times \\Delta FX Vol'
        flab = 'Debt'
    } else if(measure == 'debt_usd_gdp'){
        xlab = 'USD Debt Holdings (share of GDP)'
        measure_lab = 'USD Debt Holdings / GDP'
        cross_lab = 'USD Debt Holdings / GDP \\times \\Delta FX Vol'
        flab = 'DolDebt'
    } else if(measure == 'niip_usd_gdp'){
        xlab = 'USD NIIP (share of GDP)'
        measure_lab = 'USD NIIP / GDP'
        cross_lab = 'USD NIIP / GDP \\times \\Delta FX Vol'
        flab = 'DolNIIP'
    }

    setnames(dt, old = measure, new = 'measure')

    reg_b  = felm(D.b ~ D.fxvol * measure | date | 0 | ccy, dt)
    reg_fx = felm(D.fx ~ D.fxvol * measure | date | 0 | ccy, dt)
    reg_sprd = felm(D.sprd ~ D.fxvol * measure | date | 0 | ccy, dt)
    reg_rr = felm(D.rr ~ D.fxvol * measure | date | 0 | ccy, dt)

    reg_out = str_c('../Out/reg_fxvol_', measure,'.tex')
    reg_label = str_c('tab:fxvol_', measure)
    list(reg_b, reg_fx, reg_sprd, reg_rr) %>%
        stargazermod(out = reg_out, title = 'Effect of FX Vol.',
                     label = '', model.numbers = FALSE,
                     dep.var.labels = c('$\\Delta b$', '$\\Delta$fx',
                                        '$\\Delta$spread', '$\\Delta$risk-reversal'),
                     covariate.labels = c('\\Delta FX Vol', measure_lab, cross_lab),
                     dep.var.caption = '', omit.stat = c('adj.rsq', 'ser', 'f'))

    reg_out = str_c('../Out/reg_fxvol_', measure,'_presentation.tex')
    reg_label = str_c('tab:fxvol_', measure)
    list(reg_b, reg_fx) %>%
        stargazermod(out = reg_out, title = 'Effect of FX Vol.',
                     label = '', model.numbers = FALSE,
                     dep.var.labels = c('$\\Delta b$', '$\\Delta$fx',
                                        '$\\Delta$spread', '$\\Delta$risk-reversal'),
                     covariate.labels = c('\\Delta FX Vol', measure_lab, cross_lab),
                     dep.var.caption = '', omit.stat = c('adj.rsq', 'ser', 'f'))
}


summary_row = function(data){

    var_mean = mean(data$var, na.rm = T) %>% round(2)
    var_std  = sd(data$var, na.rm = T) %>% round(2)
    var_min  = min(data$var, na.rm = T) %>% round(2)
    var_max  = max(data$var, na.rm = T) %>% round(2)

    return(c(var_mean, var_std, var_min, var_max))
}

summary_statistics = function(panel){

    data = copy(panel)

    output =
        rbind(summary_row(data[ , .(ccy, date, var = b)]),
              summary_row(data[ , .(ccy, date, var = abs(b))]),
              summary_row(data[ , .(ccy, date, var = rx12m)]),
              summary_row(data[ , .(ccy, date, var = sprd)]),
              summary_row(data[ , .(ccy, date, var = niip_gdp)]),
              summary_row(data[ , .(ccy, date, var = debt_gdp)]),
              summary_row(data[ , .(ccy, date, var = eq_gdp)]),
              summary_row(data[ , .(ccy, date, var = ca_gdp)]))

    rownames(output) = c('Cross-currency basis (bps)',
                         'Absolute cross-currency basis (bps)',
                         'Annualized currency excess returns (pct)',
                         '5-year minus 1-year basis spread (bps)',
                         'NIIP / GDP', 'Net Debt + FDI / GDP',
                         'Net Equity / GDP', 'Current Account / GDP')

    colnames(output) = c('Mean', 'Std. Dev.', 'Min', 'Max')

    output = xtable(output,include.rownames = T, include.colnames = T)
    align(output) = c('lrrrr')
    output %>%
        print.xtable( ., type = 'latex', floating = FALSE,
                     file = '../Out/summary_statistics.tex',
                     hline.after = c(-1, -1, 0, nrow(output), nrow(output)))
}

forecast_regressions = function(panel){

    dat=copy(panel)

    ## Generate month var
    setorder(dat, ccy, date)

    dat[ , month := month(as.yearmon(date))]

    dat[ , F1D.fx := (shift(s, n=1, type='lead')-s) * 100, ccy]
    dat[ , F3D.fx := (shift(s, n=3, type='lead')-s) * 100, ccy]
    dat[ , F6D.fx := (shift(s, n=6, type='lead')-s) * 100, ccy]
    dat[ , F9D.fx := (shift(s, n=9, type='lead')-s) * 100, ccy]
    dat[ , F12D.fx := (shift(s, n=12, type='lead')-s) * 100, ccy]
    dat[ , F18D.fx := (shift(s, n=18, type='lead')-s) * 100, ccy]
    dat[ , F24D.fx := (shift(s, n=24, type='lead')-s) * 100, ccy]

    #### Newey West Standard Errors
    dat.plm = pdata.frame(dat, index=c('ccy', 'date'))
    plm.F3D = plm(F3D.fx ~ fxvol.std * debt_usd_gdp, dat.plm, model='within')
    plm.F6D = plm(F6D.fx ~ fxvol.std * debt_usd_gdp, dat.plm, model='within')
    plm.F12D = plm(F12D.fx ~ fxvol.std * debt_usd_gdp, dat.plm, model='within')

    ct.F3D = coeftest(plm.F3D, vcov=vcovNW(plm.F3D, type='HC1', maxlag=6))
    ct.F6D = coeftest(plm.F6D, vcov=vcovNW(plm.F6D, type='HC1', maxlag=9))
    ct.F12D = coeftest(plm.F12D, vcov=vcovNW(plm.F12D, type='HC1', maxlag=18))

    table.se = list(ct.F3D[ , 2], ct.F6D[ , 2], ct.F12D[ , 2])
    table.pval = list(ct.F3D[ , 4], ct.F6D[ , 4], ct.F12D[ , 4])

    texreg(list(plm.F3D, plm.F6D, plm.F12D),
           file = '../Out/Table_6.tex',
           table=FALSE, booktabs = TRUE, use.packages=FALSE,
           float.pos = 'htp!', stars=c(0.01, 0.05, 0.1),
           custom.model.names = c('3 Month', '6 Month', '12 Month'),
           custom.coef.names = c('FX Vol.', 'USD Imbalance',
                                 'USD Imbalance $\\times$ FX Vol'),
           reorder.coef = c(3, 1, 2),
           override.se = table.se,
           override.pvalues = table.pval,
           custom.note='', include.groups=FALSE, include.adjrs=FALSE)


    #### Non-overlapping observations
    texreg(list(
        felm(F3D.fx ~ fxvol.std * debt_usd_gdp | date | 0 |  ccy + date,
             dat.plm %>% filter(month %in% c(1, 4, 7, 10))),
        felm(F6D.fx ~ fxvol.std * debt_usd_gdp | date | 0 |  ccy + date,
             dat.plm %>% filter(month %in% c(1, 7))),
        felm(F12D.fx ~ fxvol.std * debt_usd_gdp | date | 0 | ccy + date,
             dat.plm %>% filter(month %in% c(1)))),
        file = '../Out/Table_A8.tex',
        table=FALSE, booktabs = TRUE, use.packages=FALSE,
        float.pos = 'htp!', stars=c(0.01, 0.05, 0.1),
        custom.model.names = c('3 Month', '6 Month', '12 Month'),
        custom.coef.names = c('FX Vol.', 'USD Imbalance',
                                'USD Imbalance $\\times$ FX Vol'),
        reorder.coef = c(3, 1, 2),
        override.se = table.se,
        override.pvalues = table.pval,
        custom.note='', include.groups=FALSE, include.adjrs=FALSE)
}

dualplot <- function (x1, y1, y2, x2 = x1, col = c("#C54E6D", "#009380"),
    lwd = c(1, 1), colgrid = NULL, mar = c(3, 6, 3, 6) + 0.1,
    ylab1 = paste(substitute(y1), collapse = ""), ylab2 = paste(substitute(y2),
        collapse = ""), nxbreaks = 5, yleg1 = paste(gsub("\n$",
        "", ylab1), "(left axis)"), yleg2 = paste(ylab2, "(right axis)"),
    ylim1 = NULL, ylim2 = NULL, ylim.ref = NULL, xlab = "", main = NULL,
    legx = "topleft", legy = NULL, silent = FALSE, bty = "n",
    ...)
{
    ylab1 <- as.character(ylab1)
    ylab2 <- as.character(ylab2)
    y1 <- as.numeric(y1)
    y2 <- as.numeric(y2)
    if (is.null(ylim.ref)) {
        if (length(y1) == length(y2)) {
            ylim.ref <- c(1, 1)
        }
        else {
            if (min(x1) > min(x2)) {
                ylim.ref <- c(1, which(abs(x2 - min(x1)) == min(abs(x2 -
                  min(x1)))))
            }
            else {
                ylim.ref <- c(which(abs(x1 - min(x2)) == min(abs(x1 -
                  min(x2)))), 1)
            }
        }
    }
    oldpar <- par(mar = mar)
    xbreaks <- round(seq(from = min(c(x1, x2)), to = max(c(x1,
        x2)), length.out = nxbreaks))
    if (is.null(ylim1) & is.null(ylim2)) {
        if (min(c(y1, y2), na.rm = TRUE) < 0) {
            message("With negative values ylim1 or ylim2 need to be chosen by a method other than treating both series visually as though they are indexed. Defaulting to mean value +/- 3 times the standard deviations.")
            ylim1 <- c(-3, 3) * sd(y1, na.rm = TRUE) + mean(y1,
                na.rm = TRUE)
            ylim2 <- c(-3, 3) * sd(y2, na.rm = TRUE) + mean(y2,
                na.rm = TRUE)
        }
        if (ylim.ref[1] > length(y1)) {
            stop("ylim.ref[1] must be a number shorter than the length of the first series.")
        }
        if (ylim.ref[2] > length(y2)) {
            stop("ylim.ref[2] must be a number shorter than the length of the second series.")
        }
        if (!silent)
            message("The two series will be presented visually as though they had been converted to indexes.")
        ind1 <- as.numeric(y1)/y1[ylim.ref[1]]
        ind2 <- as.numeric(y2)/y2[ylim.ref[2]]
        indlimits <- range(c(ind1, ind2), na.rm = TRUE)
        ylim1 = indlimits * y1[ylim.ref[1]]
        ylim2 = indlimits * y2[ylim.ref[2]]
    }
    else {
        if (!silent)
            warning("You've chosen to set at least one of the vertical axes limits manually.  Up to you, but it is often better to leave it to the defaults.")
    }

    plot(x1, y1, type = "l", axes = FALSE, lwd = lwd[1], xlab = xlab,
        ylab = "", col = col[1], main = main, xlim = range(xbreaks),
        ylim = ylim1)
    if (!is.null(colgrid)) {
        grid(lty = 1, nx = NA, ny = NULL, col = colgrid)
        abline(v = xbreaks, col = colgrid)
    }

    axis(2, col = col[1], col.axis = col[1], las = 1)
    mtext(paste0("\n", ylab1, "\n"), side = 2, col = col[1],
        line = 1.5)
    par(new = TRUE)
    plot(x2, y2, xlab = "", ylab = "", axes = FALSE, type = "l",
        lwd = lwd[2], col = col[2], xlim = range(xbreaks), ylim = ylim2)
    mtext(paste0("\n", ylab2, "\n"), side = 4, col = col[2],
        line = 4.5)
    axis(4, col = col[2], col.axis = col[2], las = 1)
    axis(1, at = xbreaks, labels = format(xbreaks, '%b %Y'))
    legend(x = legx, y = legy, legend = c(yleg1, yleg2), text.col = col,
        lty = c(1, 1), lwd = lwd, col = col, bty = bty, ...)
    par(oldpar)
}

plot_fx_turnover = function(exportexhibits = T){

    bis = fread('../CleanData/BIS_Turnover_over_time.csv') %>%
        melt(id.var='.id') %>%
        dcast(variable~.id,value.var="value",fill=0)

    bis[,year:=year(ymd(variable))][,variable:=NULL]
    names(bis) <- tolower(names(bis))
    bis[is.na(currencyswap),currencyswap:=0]
    bis[is.na(currencyswapnonfin),currencyswapnonfin:=0]
    bis[is.na(totalfxoptions),totalfxoptions:=0]
    bis[,tot:=spot+forward+fxswap+currencyswap]
    bis[,spotfrac:=spot/tot]
    bis[,fwdswapfrac:=(tot-spot)/tot]
    bis[,totnonfin:=spotnonfin+forwardnonfin+fxswapnonfin+currencyswapnonfin]
    bis[,spotfracnonfin:=spotnonfin/totnonfin]
    bis[,fwdswapfracnonfin:=(totnonfin-spotnonfin)/totnonfin]

    plot = bis[ year>1990, .(year, Spot=spot/1e6,
                             `Forward and Swap`=(tot-spot)/1e6)] %>%
        melt(id.var='year') %>%
        ggplot(aes(x=year, y=value, fill=variable)) +
        geom_bar(position="dodge", stat="identity") +
        theme_few() +
        ylab('Daily Average Turnover (US$Trillion)') + xlab('') +
        theme(legend.title=element_blank(), legend.position='bottom')

    ggsave('../Out/bisturnover_time.pdf', plot, width=6, height=4)

}

reg_measures = function(panel){


    panel_full = copy(panel)

    panel_full = panel[g10 == TRUE]
    panel_full[ , eq_usd_gdp := niip_usd_gdp - debt_usd_gdp]

    ## Cross-currency bases (2000 - 2021)
    reg_full =
        list(panel_full %>% felm(b ~ niip_usd_gdp |0|0|ccy,data=.),
             panel_full %>% felm(b ~ niip_usd_gdp |date|0|ccy,data=.),
             panel_full %>% felm(b ~ debt_usd_gdp |0|0|ccy,data=.),
             panel_full %>% felm(b ~ debt_usd_gdp |date|0|ccy,data=.),
             panel_full %>% felm(b ~ eq_usd_gdp |0|0|ccy,data=.),
             panel_full %>% felm(b ~ eq_usd_gdp |date|0|ccy,data=.))

    reg_full_fxvol =
        list(panel_full %>% felm(b ~ niip_usd_gdp + fxvol |0|0|ccy,data=.),
             panel_full %>% felm(b ~ niip_usd_gdp + fxvol |date|0|ccy,data=.),
             panel_full %>% felm(b ~ debt_usd_gdp + fxvol |0|0|ccy,data=.),
             panel_full %>% felm(b ~ debt_usd_gdp + fxvol |date|0|ccy,data=.),
             panel_full %>% felm(b ~ eq_usd_gdp + fxvol |0|0|ccy,data=.),
             panel_full %>% felm(b ~ eq_usd_gdp + fxvol |date|0|ccy,data=.))

    reg_full %>%
        texreg(file='../Out/Table_2.tex',
               table=FALSE, booktabs = TRUE,
               use.packages=FALSE,
               custom.title='Imbalances and Cross-Currency Bases',
               custom.model.names = c('(1)', '(2)', '(3)', '(4)', '(5)', '(6)'),
               custom.coef.names=c('constant', 'USD NIIP / GDP',
                                   'USD Debt Holdings / GDP',
                                   'USD Equity Holdings / GDP'),
               omit.coef = '(Intercept)', omit.stat = c('adj.rsq', 'ser'),
               float.pos = 'htp!',
               custom.header = list('Cross-Currency Basis (bps)'=1:6),
               stars=c(0.01, 0.05, 0.1),
               custom.gof.rows =
                   list('Fixed Effects' = c('', 'Month', '', 'Month', '', 'Month')),
               custom.note='',
               include.groups=FALSE, include.adjrs=FALSE)

    reg_full_fxvol %>%
        texreg(file='../Out/Table_A4.tex',
               table=FALSE, booktabs = TRUE,
               use.packages=FALSE,
               custom.title='Imbalances and Cross-Currency Bases',
               custom.model.names = c('(1)', '(2)', '(3)', '(4)', '(5)', '(6)'),
               custom.coef.names=c('constant', 'USD NIIP / GDP', 'FX Vol.',
                                   'USD Debt Holdings / GDP',
                                   'USD Equity Holdings / GDP'),
               reorder.coef = c(1, 3, 4, 2),
               omit.coef = '(Intercept)', omit.stat = c('adj.rsq', 'ser'),
               float.pos = 'htp!',
               custom.header = list('Cross-Currency Basis (bps)'=1:6),
               stars=c(0.01, 0.05, 0.1),
               custom.gof.rows =
                   list('Fixed Effects' = c('', 'Month', '', 'Month', '', 'Month')),
               custom.note='',
               include.groups=FALSE, include.adjrs=FALSE)

    ## Risk Reversals (2005 - 2021)
    reg_full =
        list(panel_full %>% felm(rr ~ niip_usd_gdp |0|0|ccy,data=.),
             panel_full %>% felm(rr ~ niip_usd_gdp |date|0|ccy,data=.),
             panel_full %>% felm(rr ~ debt_usd_gdp |0|0|ccy,data=.),
             panel_full %>% felm(rr ~ debt_usd_gdp |date|0|ccy,data=.),
             panel_full %>% felm(rr ~ eq_usd_gdp |0|0|ccy,data=.),
             panel_full %>% felm(rr ~ eq_usd_gdp |date|0|ccy,data=.))

    reg_full_fxvol =
        list(panel_full %>% felm(rr ~ niip_usd_gdp + fxvol |0|0|ccy,data=.),
             panel_full %>% felm(rr ~ niip_usd_gdp + fxvol |date|0|ccy,data=.),
             panel_full %>% felm(rr ~ debt_usd_gdp + fxvol |0|0|ccy,data=.),
             panel_full %>% felm(rr ~ debt_usd_gdp + fxvol |date|0|ccy,data=.),
             panel_full %>% felm(rr ~ eq_usd_gdp + fxvol |0|0|ccy,data=.),
             panel_full %>% felm(rr ~ eq_usd_gdp + fxvol |date|0|ccy,data=.))

    reg_full %>%
        texreg(file='../Out/Table_3.tex',
               table=FALSE, booktabs = TRUE,
               use.packages=FALSE,
               custom.title='Imbalances and Risk Reversals',
               custom.model.names = c('(1)', '(2)', '(3)', '(4)', '(5)', '(6)'),
               custom.coef.names=c('constant', 'USD NIIP / GDP',
                                   'USD Debt Holdings / GDP',
                                   'USD Equity Holdings / GDP'),
               omit.coef = '(Intercept)', omit.stat = c('adj.rsq', 'ser'),
               float.pos = 'htp!',
               custom.header = list('Risk Reversals (pct)'=1:6),
               stars=c(0.01, 0.05, 0.1),
               custom.gof.rows =
                   list('Fixed Effects' = c('', 'Month', '', 'Month', '', 'Month')),
               custom.note='',
               include.groups=FALSE, include.adjrs=FALSE)

    reg_full_fxvol %>%
        texreg(file='../Out/Table_A5.tex',
               table=FALSE, booktabs = TRUE,
               use.packages=FALSE,
               custom.title='Imbalances, Risk Reversals and FX Volatility',
               custom.model.names = c('(1)', '(2)', '(3)', '(4)', '(5)', '(6)'),
               custom.coef.names=c('constant', 'USD NIIP / GDP', 'FX Vol.',
                                   'USD Debt Holdings / GDP',
                                   'USD Equity Holdings / GDP'),
               reorder.coef = c(1, 3, 4, 2),
               omit.coef = '(Intercept)', omit.stat = c('adj.rsq', 'ser'),
               float.pos = 'htp!',
               custom.header = list('Risk Reversals (pct)'=1:6),
               stars=c(0.01, 0.05, 0.1),
               custom.gof.rows =
                   list('Fixed Effects' = c('', 'Month', '', 'Month', '', 'Month')),
               custom.note='',
               include.groups=FALSE, include.adjrs=FALSE)

    ## With time fixed effects
    reg_full_time =
        list(panel_full %>% felm(b ~ niip_usd_gdp |date + ccy|0|date+ccy,data=.),
             panel_full %>% felm(b ~ debt_usd_gdp |date + ccy|0|date+ccy,data=.),
             panel_full %>% felm(b ~ eq_usd_gdp |date + ccy|0|date+ccy,data=.),
             panel_full %>% felm(rr ~ niip_usd_gdp |date + ccy|0|date+ccy,data=.),
             panel_full %>% felm(rr ~ debt_usd_gdp |date + ccy|0|date+ccy,data=.),
             panel_full %>% felm(rr ~ eq_usd_gdp |date + ccy|0|date+ccy,data=.))

    reg_full_time %>%
        texreg(file='../Out/Table_A3.tex',
               table=FALSE, booktabs = TRUE,
               use.packages=FALSE,
               custom.title='Time and Currency Fixed Effects',
               custom.model.names = c('(1)', '(2)', '(3)', '(4)', '(5)', '(6)'),
               custom.header = list('Cross-currency Basis (bps)'=1:3, 'Risk Reversals (pct)'=4:6),
               custom.coef.names=c('USD NIIP / GDP',
                                   'USD Debt Holdings / GDP',
                                   'USD Equity Holdings / GDP'),
               omit.stat = c('adj.rsq', 'ser'),
               float.pos = 'htp!',
               stars=c(0.01, 0.05, 0.1),
               custom.note='',
               include.groups=FALSE, include.adjrs=FALSE)
}



plot_timeseries_month = function(panel, dtbegin, dtend){

    ccy_ord = c('NZD', 'AUD', 'GBP', 'EUR', 'SEK', 'CAD', 'JPY', 'CHF', 'NOK')

    ## CIP time series
    plot =
        panel[date > dtbegin & date < dtend] %>%
        mutate(label=if_else(date==max(date), as.character(ccy), NA_character_)) %>%
        ggplot(aes(x=date, y=b, color=ccy)) +
        geom_line() + geom_hline(yintercept = 0, color='grey') +
        geom_label_repel(aes(label = label, color = ccy),
                         nudge_y=.001, nudge_x=2, cex=3,
                         segment.alpha	=.2, alpha=.75) +
        theme_few() + scale_colour_discrete(breaks = ccy_ord) +
        ylab('Currency Basis (bps)') + xlab('') +
        guides(color=F) +
        annotate('text', dtbegin + 2, -40, label='Forward overvalued') +
        annotate('text', dtbegin + 2,  30, label='Forward undervalued')

    out_file = str_c('../Out/Figure_A2.pdf')
    ggsave(out_file, plot, width=8, height=6)


    ## Options time series
    plot =
        panel %>%
        filter(date >= 2005) %>%
        mutate(label=if_else(date==max(date), as.character(ccy), NA_character_)) %>%
        ggplot(aes(x=date,y=rr,color=ccy)) +
        geom_line() + geom_hline(yintercept=0) +
        theme_few() + theme(legend.title=element_blank()) +
        geom_label_repel(aes(label = label, color = ccy),
                         nudge_y=.001, nudge_x=2, cex=3,
                         segment.alpha	=.2, alpha=.75) +
        xlab('') + ylab('Risk-Reversal (Implied Vol.)')+
        guides(color=F) +
        annotate('text', 2005.75, -4, label='Put expensive')+
        annotate('text', 2005.75,  4, label='Call expensive')

   out_file = str_c('../Out/Figure_3.pdf')
   ggsave(out_file, plot, width=8,height=6)
}


plot_timeseries_covid = function(panel, dtbegin, dtend){

    offset = 7

    ccy_ord = c('NZD', 'AUD', 'GBP', 'EUR', 'SEK', 'CAD', 'JPY', 'CHF', 'NOK')

    ## CIP time series
    plot =
        panel[date > dtbegin & date < dtend] %>%
        mutate(label=if_else(date==max(date), as.character(ccy), NA_character_)) %>%
        ggplot(aes(x=date, y=b, color=ccy)) +
        geom_line() + geom_hline(yintercept = 0, color='grey') +
        geom_label_repel(aes(label = label, color = ccy),
                         nudge_y=.001, nudge_x=2, cex=3,
                         segment.alpha	=.2, alpha=.75) +
        theme_few() + scale_colour_discrete(breaks = ccy_ord) +
        ylab('Currency Basis (bps)') + xlab('') +
        guides(color=F) +
        annotate('text', dtbegin + days(offset), -40, label='Forward overvalued') +
        annotate('text', dtbegin + days(offset),  30, label='Forward undervalued')

    out_file = str_c('../Out/Figure_A4_A.pdf')
    ggsave(out_file, plot, width=8, height=4)

    ## Options time series
    plot =
        panel[date > dtbegin & date < dtend] %>%
        mutate(label=if_else(date==max(date), as.character(ccy), NA_character_)) %>%
        ggplot(aes(x=date,y=rr,color=ccy)) +
        geom_line() + geom_hline(yintercept=0) +
        theme_few() + theme(legend.title=element_blank()) +
        geom_label_repel(aes(label = label, color = ccy),
                         nudge_y=.001, nudge_x=.7, cex=3,
                         segment.alpha	=.2, alpha=.75) +
        xlab('') + ylab('Risk-Reversal (Implied Vol.)')+
        guides(color=F) +
        annotate('text', dtbegin + days(offset), -4, label='Put expensive')+
        annotate('text', dtbegin + days(offset),  4, label='Call expensive')

   out_file = str_c('../Out/Figure_A4_B.pdf')
   ggsave(out_file, plot, width=8,height=4)
}
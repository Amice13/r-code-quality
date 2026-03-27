rm(list=ls())
countries <- c("AT", "BE.W", "BE.Bru", "BE.Fl", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GB", "GR", "HR", "HU", "IE", "IS", "IT", "LI", "LT", "LU", "LV", "MT", "NL", "NO", "PL", "PT", "RO", "SE", "SI", "SK")
ds <- data.frame(expand.grid(country=countries, year=2005:2020))

# Article 21 reports were downloaded on May 6, 2021, from http://cdr.eionet.europa.eu/ReportekEngine/searchdataflow
ds$namingshaming <- NA							# Naming and shaming of non-compliant operators (Question 4.1) (1=Yes, 0=No)
ds$suspension <- NA								# Withdrawal of permit; suspension of the installation (Question 4.1/4.2) (1=Yes, 0=No)
ds$inspection <- NA								# Spot or routine checks or inspections by the administration (Question 4.1/6.6) (1 = Yes, 0=No)
ds$inspection.n <- NA							# Number of inspections of installations that were carried out through site visits by the competent authority
ds$appointment <- NA							# Competent authority has right to appoint a verifier to an installations (Question 6.1) (1=Yes, 0=No)
ds$supervision <- NA							# Competent authority or other agency supervises verifiers (including spot checks, training, QA/QC procedures)  (Question 6.1) (1=Yes, 0=No)
ds$emissionsfines.n.installations.lower <- NA	# Number of installation operators with excess emissions fines imposed (Question 12.3/11.4) - Lower bound.
ds$emissionsfines.n.installations.upper <- NA	# Number of installation operators with excess emissions fines imposed (Question 12.3/11.4) - Upper bound.
ds$emissionsfines.q.installations <- NA			# Total fines listed related to excess emissions penalties (Euros) (Question 12.2/11.3)
ds$otherfines.q.installations <- NA				# Total fines listed related to all penalties (Euros) (Question 12.2/11.3)

## NB: I've appended "installation" to a few variables because they are also sometimes available separately for aircrafts. Please make sure you are getting the answer to the question about installations, not the question about aircrafts.

# envelopes(2)/Report under Article 21 Directive 87/2003/EC 2005/Austria_Article_21_Report_Part_1_2006.doc
ds$appointment[ds$country=="AT" & ds$year==2005] <- 0
ds$supervision[ds$country=="AT" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="AT" & ds$year==2005] <- 1
ds$suspension[ds$country=="AT" & ds$year==2005] <- 1
ds$inspection[ds$country=="AT" & ds$year==2005] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2005] <- 0

# envelopes(2)/Report under Article 21 Directive 87/2003/EC 2006/Austria_Article_21_Report_Part_1_2007.pdf
ds$appointment[ds$country=="AT" & ds$year==2006] <- 0
ds$supervision[ds$country=="AT" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="AT" & ds$year==2006] <- 1
ds$suspension[ds$country=="AT" & ds$year==2006] <- 1
ds$inspection[ds$country=="AT" & ds$year==2006] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2006] <- 0

# envelopes(2)/Report under Article 21 Directive 87/2003/EC for 2007/Austria_Article_21_Report_Part_1_2008_.pdf
ds$appointment[ds$country=="AT" & ds$year==2007] <- 0
ds$supervision[ds$country=="AT" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="AT" & ds$year==2007] <- 1
ds$suspension[ds$country=="AT" & ds$year==2007] <- 1
ds$inspection[ds$country=="AT" & ds$year==2007] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2007] <- 0

# envelopes(2)/Report under Article 21 Directive 87/2003/EC for 2008/Austria_Article_21_Report_Part_1_2009.doc
ds$appointment[ds$country=="AT" & ds$year==2008] <- 0
ds$supervision[ds$country=="AT" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="AT" & ds$year==2008] <- 1
ds$suspension[ds$country=="AT" & ds$year==2008] <- 1
ds$inspection[ds$country=="AT" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2008] <- "Maximum"
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2008] <- "To be calculated"
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2008] <- 0	# Proceedings ongoing.

# envelopes(2)/Report under Article 21 Directive 87/2003/EC for 2008/Austria_Article_21_Report_Part_1_2009.doc
ds$appointment[ds$country=="AT" & ds$year==2009] <- 0
ds$supervision[ds$country=="AT" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="AT" & ds$year==2009] <- 1
ds$suspension[ds$country=="AT" & ds$year==2009] <- 1
ds$inspection[ds$country=="AT" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2009] <- 0

# envelopes(1)/Report under Article 21 of Directive 2003/87 for 2012/Austria_Article_21_Report_for_2012 Part 1.pdf
ds$appointment[ds$country=="AT" & ds$year==2012] <- 0
ds$supervision[ds$country=="AT" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="AT" & ds$year==2012] <- 1
ds$suspension[ds$country=="AT" & ds$year==2012] <- 1
ds$inspection[ds$country=="AT" & ds$year==2012] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2012] <- 0

# envelopes(1)/AT Report under Article 21 of Directive 2003/87 for 2013/Art__21_Report_AT_for_2013.pdf
ds$namingshaming[ds$country=="AT" & ds$year==2013] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2013] <- 	1							# 4.2
ds$inspection[ds$country=="AT" & ds$year==2013] <- 	1							# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2013] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2013] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2013] <- 0				# 11.3

# envelopes(1)/AT Report under Article 21 of Directive 2003/87 for 2014/Art__21_Report_AT_Final_Public.pdf
ds$namingshaming[ds$country=="AT" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2014] <- 	1							# 4.2
ds$inspection[ds$country=="AT" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2014] <- 0							# 6.5 "There is no inspection only with regard to ETS issues. In regular inspections e.g. according to the IED) ETS issues might also be taken into account. However, there is no complete overview on this issue"
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2014] <- 0		# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2014] <- 0				# 11.3

# envelopes/AT Report under Article 21 of Directive 2003/87 for 2015/Art__21_Report_AT_2015_Public.pdf
ds$namingshaming[ds$country=="AT" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2015] <- 1								# 4.2
ds$inspection[ds$country=="AT" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2015] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2015] <- 0				# 11.3

# envelopes/AT Report under Article 21 of Directive 2003/87 for 2016/Art21Report_AT_public_2016.xml
ds$namingshaming[ds$country=="AT" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2016] <- 1								# 4.2
ds$inspection[ds$country=="AT" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2016] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2016] <- 0				# 11.3

# envelopes/AT Report under Article 21 of Directive 2003/87 for 2017 (Re-Submission)/Art__21_Report_AT_2017_public_01.08.2018.pdf
ds$namingshaming[ds$country=="AT" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2017] <- 1								# 4.2
ds$inspection[ds$country=="AT" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2017] <- 0							#6.5
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2017] <- 0				# 11.3

# envelopes/AT Report under Article 21 of Directive 2003/87 for 2018/Art__21_Report_AT_2019_public.pdf
ds$namingshaming[ds$country=="AT" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2018] <- 1								# 4.2
ds$inspection[ds$country=="AT" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2018] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2018] <- 0				# 11.3

# envelopes/AT Report under Article 21 of Directive 2003/87 for 2019/Art__21_Report_AT_2020_public.pdf
ds$namingshaming[ds$country=="AT" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="AT" & ds$year==2019] <- 1								# 4.2
ds$inspection[ds$country=="AT" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="AT" & ds$year==2019] <- 0							# 6.5 
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="AT" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Report article 21 of Directive 2003/87/EC/Report_on_the_implementation_of_directive_2003-87-EC.zip
ds$suspension[ds$country=="BE.W" & ds$year==2005] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2005] <- 0
ds$suspension[ds$country=="BE.Fl" & ds$year==2005] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2005] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2005] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="BE.W" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2005] <- 1
ds$supervision[ds$country=="BE.W" & ds$year==2005] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2005] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2005] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2005] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2005] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2005] <- 0

# envelopes(2)/2007 -  Report by Belgium under Article 21 of Directive 2003/87/EC/070712_Art_21_report_Belgium_reporting_year_2006_final.pdf
ds$namingshaming[ds$country=="BE.W" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2006] <- 1
ds$suspension[ds$country=="BE.W" & ds$year==2006] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2006] <- 0
ds$suspension[ds$country=="BE.Fl" & ds$year==2006] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2006] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2006] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2006] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2006] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2006] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2006] <- 0
ds$supervision[ds$country=="BE.W" & ds$year==2006] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2006] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2006] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2006] <- 0

# envelopes(2)/2008 - Report by Belgium under Article 21 of Directive 2003/87/EC/080714_BE-art_21_EU_ETS_reporting_on_period_2007_final.pdf
ds$namingshaming[ds$country=="BE.W" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2007] <- 1
ds$suspension[ds$country=="BE.W" & ds$year==2007] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2007] <- 0
ds$suspension[ds$country=="BE.Fl" & ds$year==2007] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2007] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2007] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2007] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2007] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2007] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2007] <- 0
ds$supervision[ds$country=="BE.W" & ds$year==2007] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2007] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2007] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2007] <- 0

# envelopes(1)/2009 - Report by Belgium under Article 21 of Directive 2003/87/EC/090804_art_21_EU_ETS_reporting_on_period_2008_-_BE_-_final.pdf
ds$suspension[ds$country=="BE.W" & ds$year==2008] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2008] <- 0
ds$suspension[ds$country=="BE.Fl" & ds$year==2008] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2008] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2008] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="BE.W" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2008] <- 1
ds$supervision[ds$country=="BE.W" & ds$year==2008] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2008] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2008] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2008] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2008] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2008] <- 0

# envelopes(1)/2010 - Report by Belgium under Article 21 of Directive 2003/87/EC/art_21_EU_ETS_reporting_on_period_2009_-_BE_-_final.pdf
ds$suspension[ds$country=="BE.W" & ds$year==2009] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$suspension[ds$country=="BE.Fl" & ds$year==2009] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2009] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="BE.W" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2009] <- 1
ds$supervision[ds$country=="BE.W" & ds$year==2009] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2009] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2009] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2009] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="BE.Bru" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2009] <- 0

# envelopes(1)/2011 - Report bu Belgium under Article 21 of Directive 2003/87/EC/art_21_EU_ETS_reporting_on_period_2010_-_BE_final.doc
ds$suspension[ds$country=="BE.W" & ds$year==2010] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2010] <- 0 
ds$suspension[ds$country=="BE.Fl" & ds$year==2010] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2010] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2010] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="BE.W" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2010] <- 1
ds$supervision[ds$country=="BE.W" & ds$year==2010] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2010] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2010] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2010] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2010] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2010] <- 0

# envelopes(1)/2012 - Report by Belgium under Article 21 of Directive 2003/87/EC/art_21_EU_ETS_reporting_on_period_2011_-_BE.doc
ds$suspension[ds$country=="BE.W" & ds$year==2011] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2011] <-  0
ds$suspension[ds$country=="BE.Fl" & ds$year==2011] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2011] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2011] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="BE.W" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2011] <- 1
ds$supervision[ds$country=="BE.W" & ds$year==2011] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2011] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2011] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2011] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2011] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2011] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2011] <- 1
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2011] <-  "To be calculated" # The one non-compliant installation is NESAR (BE000000000000309), which is bankrupt. All remaining allowances the account were surrendered by the appointed bailiff, but these were in-sufficient to cover all 2011 emissions. With the bankruptcy of the operator in mind, the compe-tent authority is considering further enforcement steps.
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2011] <- 0

# envelopes(1)/2012 - Report by Belgium under Article 21 of Directive 2003/87/EC/art_21_EU_ETS_reporting_on_period_2011_-_BE.doc
ds$suspension[ds$country=="BE.W" & ds$year==2012] <- 0
ds$suspension[ds$country=="BE.Bru" & ds$year==2012] <- 0 
ds$suspension[ds$country=="BE.Fl" & ds$year==2012] <- 1
ds$inspection[ds$country=="BE.W" & ds$year==2012] <- 1
ds$inspection[ds$country=="BE.Bru" & ds$year==2012] <- 0
ds$inspection[ds$country=="BE.Fl" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="BE.W" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2012] <- 1
ds$supervision[ds$country=="BE.W" & ds$year==2012] <- 1
ds$supervision[ds$country=="BE.Bru" & ds$year==2012] <- 1
ds$supervision[ds$country=="BE.Fl" & ds$year==2012] <- 1
ds$appointment[ds$country=="BE.W" & ds$year==2012] <- 1
ds$appointment[ds$country=="BE.Bru" & ds$year==2012] <- 0
ds$appointment[ds$country=="BE.Fl" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2012] <- 0 # The one non-compliant installation is NESAR (BE000000000000309), which is bankrupt. All remaining allowances the account were surrendered by the appointed bailiff, but these were in-sufficient to cover all 2011 emissions. With the bankruptcy of the operator in mind, the compe-tent authority is considering further enforcement steps.
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2012] <- 0

#Article21/envelopes(1)/2014 - Report by Belgium under Article 21 of Directive 2003/87/EC - corrected version/Article_21_questionnaire__v2.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2013] <- 	1					# S11_InstallationsComplianceMeasures
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2013] <- 	1						# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="BE.W" & ds$year==2013] <- 		1						# S04_NationalLawPermitUpdate 
ds$suspension[ds$country=="BE.Bru" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate 
ds$suspension[ds$country=="BE.Fl" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate 
ds$inspection[ds$country=="BE.W" & ds$year==2013] <- 	0						# S11_InstallationsComplianceMeasures
ds$inspection[ds$country=="BE.Bru" & ds$year==2013] <- 0								# S11_InstallationsComplianceMeasures
ds$inspection[ds$country=="BE.Fl" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="BE.W" & ds$year==2013] <- 		0				# S06_InstallationsVerificationReportChecksDetail1
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2013] <- 	0					# S06_InstallationsVerificationReportChecksDetail1
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2013] <- 	1					# S06_InstallationsVerificationReportChecksDetail1 - Inspection not specified for Flanders specifically. Inference based on Flanders being the only region that has performs spot checks and site visits.
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2013] <- 0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2013] <- 0	#S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="BE" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed

# envelopes(1)/2015 - Report by Belgium under Article 21 of Directive 2003/87/EC/be-eu-emt-envu6gexg.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2014] <- 1						# 11.1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2014] <- 0							# 11.1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2014] <- 	1						# 11.1
ds$suspension[ds$country=="BE.W" & ds$year==2014] <- 	1							# 4.2
ds$suspension[ds$country=="BE.Bru" & ds$year==2014] <- 	1							# 4.2
ds$suspension[ds$country=="BE.Fl" & ds$year==2014] <- 1								# 4.2
ds$inspection[ds$country=="BE.W" & ds$year==2014] <- 0							#  11.1
ds$inspection[ds$country=="BE.Bru" & ds$year==2014] <- 0								# 11.1
ds$inspection[ds$country=="BE.Fl" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="BE.W" & ds$year==2014] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2014] <-0 						#  6.5
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2014] <- 0						#  6.5
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2014] <- 0 	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2014] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2014] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2014] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="BE" & ds$year==2014] <- 0				# 11.3

# envelopes/2016 - Report by Belgium under Article 21 of Directive 2003/87/EC/be.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2015] <- 1							# 11.1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2015] <- 0							# 4.2
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="BE.W" & ds$year==2015] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Bru" & ds$year==2015] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Fl" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="BE.W" & ds$year==2015] <- 0								# 11.1
ds$inspection[ds$country=="BE.Bru" & ds$year==2015] <- 0								# 11.1
ds$inspection[ds$country=="BE.Fl" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="BE.W" & ds$year==2015] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2015] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2015] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2015] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="BE" & ds$year==2015] <- 0				# 11.3

# envelopes/2017 - Report by Belgium under Article 21 of Directive 2003/87/EC/be_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2016] <- 1							# 11.1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2016] <- 0							# 11.1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="BE.W" & ds$year==2016] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Bru" & ds$year==2016] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Fl" & ds$year==2016] <- 1								# 4.2
ds$inspection[ds$country=="BE.W" & ds$year==2016] <- 0								# 11.1
ds$inspection[ds$country=="BE.Bru" & ds$year==2016] <- 0								# 11.1
ds$inspection[ds$country=="BE.Fl" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="BE.W" & ds$year==2016] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2016] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Bru" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Bru" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2016] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Bru" & ds$year==2016] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2016] <- 0			# 11.3


# envelopes/2018 - Report by Belgium under Article 21 of Directive 2003/87/EC/be_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2017] <- 1							# 11.1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2017] <- 0							# 11.1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="BE.W" & ds$year==2017] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Bru" & ds$year==2017] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Fl" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="BE.W" & ds$year==2017] <- 0								# 11.1
ds$inspection[ds$country=="BE.Bru" & ds$year==2017] <- 0								# 11.1
ds$inspection[ds$country=="BE.Fl" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="BE.W" & ds$year==2017] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2017] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Bru" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Bru" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2017] <- 1 # 11.4 
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2017] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Bru" & ds$year==2017] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2017] <- "To be calculated"		# 11.3 IDs - 72 (Langerlo NV – Centrale Langerlo)
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2017] <- 0				# 11.3
ds$otherfines.q.installations[ds$country=="BE.Bru" & ds$year==2017] <- 0				# 11.3
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2017] <- 0				# 11.3

# envelopes/2019 - Report by Belgium under Article 21 of Directive 2003/87/EC/be_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2018] <- 1							# 11.1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2018] <- 0							# 11.1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="BE.W" & ds$year==2018] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Bru" & ds$year==2018] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Fl" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="BE.W" & ds$year==2018] <- 0								# 11.1
ds$inspection[ds$country=="BE.Bru" & ds$year==2018] <- 0								# 11.1
ds$inspection[ds$country=="BE.Fl" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="BE.W" & ds$year==2018] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2018] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Bru" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Bru" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2018] <- 0 # 11.4
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2018] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Bru" & ds$year==2018] <- 0	# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2018] <- 0				# 11.3
ds$otherfines.q.installations[ds$country=="BE.Bru" & ds$year==2018] <- 0				# 11.3
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2018] <- 0				# 11.3

# envelopes/2020 - Report by Belgium under Article 21 of Directive 2003/87/EC/be_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BE.W" & ds$year==2019] <- 1							# 11.1
ds$namingshaming[ds$country=="BE.Bru" & ds$year==2019] <- 0							# 11.1
ds$namingshaming[ds$country=="BE.Fl" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="BE.W" & ds$year==2019] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Bru" & ds$year==2019] <- 1								# 4.2 
ds$suspension[ds$country=="BE.Fl" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="BE.W" & ds$year==2019] <- 0								# 11,1
ds$inspection[ds$country=="BE.Bru" & ds$year==2019] <- 0								# 11.1
ds$inspection[ds$country=="BE.Fl" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="BE.W" & ds$year==2019] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Bru" & ds$year==2019] <- 0						# 6.5
ds$inspection.n[ds$country=="BE.Fl" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BE.W" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Bru" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.lower[ds$country=="BE.Fl" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.W" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Bru" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BE.Fl" & ds$year==2019] <- 0 # 11.4 
ds$emissionsfines.q.installations[ds$country=="BE.W" & ds$year==2019] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Bru" & ds$year==2019] <- 0			# 11.3
ds$emissionsfines.q.installations[ds$country=="BE.Fl" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="BE.W" & ds$year==2019] <- 0				# 11.3
ds$otherfines.q.installations[ds$country=="BE.Bru" & ds$year==2019] <- 0				# 11.3
ds$otherfines.q.installations[ds$country=="BE.Fl" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Reporting under Article 21(1) of the Directive 2003/87/EC/FinalhomeArt_21_Report_Annex_EN_WORD.doc
ds$appointment[ds$country=="BG" & ds$year==2006] <- 0
ds$supervision[ds$country=="BG" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="BG" & ds$year==2006] <- 0
ds$suspension[ds$country=="BG" & ds$year==2006] <- 1
ds$inspection[ds$country=="BG" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2006] <- 0

# envelopes(2)/Reporting under Article 21(1) of the Directive 2003/87/EC/Reporting_article_21_clear.doc.doc/
ds$appointment[ds$country=="BG" & ds$year==2007] <- 0
ds$supervision[ds$country=="BG" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="BG" & ds$year==2007] <- 0
ds$suspension[ds$country=="BG" & ds$year==2007] <- 1
ds$inspection[ds$country=="BG" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2007] <- 40

# envelopes(1)/Reporting under Article 21(1) of the Directive 2003/87/EC/BG_-_Reporting_article_21_for_2009_period.doc
ds$appointment[ds$country=="BG" & ds$year==2009] <- 1
ds$supervision[ds$country=="BG" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="BG" & ds$year==2009] <- 1
ds$suspension[ds$country=="BG" & ds$year==2009] <- 1
ds$inspection[ds$country=="BG" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2009] <- 40

# envelopes(1)/Reporting under Art. 21 of Directive 2003/87/EO for 2011/2011_final_art.21_BG.doc
ds$appointment[ds$country=="BG" & ds$year==2011] <- 1
ds$supervision[ds$country=="BG" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="BG" & ds$year==2011] <- 0
ds$suspension[ds$country=="BG" & ds$year==2011] <- 1
ds$inspection[ds$country=="BG" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2011] <- 0

# envelopes(1)/Reporting under Article 21 of the ETS Directive/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="BG" & ds$year==2013] <- 1							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="BG" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="BG" & ds$year==2013] <- 	1							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="BG" & ds$year==2013] <- 0							# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2013] <- 	0			# S11_InstallationsPenaltiesImposed

# envelopes/Reporting under Article 21(1) of Directive 2003/87/EC - Resubmission/bg-eu-emt-envu5blw_corrected28012016.xml
ds$namingshaming[ds$country=="BG" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="BG" & ds$year==2014] <- 1								# 4.2
ds$inspection[ds$country=="BG" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="BG" & ds$year==2014] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2014] <- 0				# 11.3

# envelopes/Reporting under Article 21(1) of Directive 2003/87/EC/article_21_2015_bg.xml
ds$namingshaming[ds$country=="BG" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="BG" & ds$year==2015] <- 1								# 4.2
ds$inspection[ds$country=="BG" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="BG" & ds$year==2015] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2015] <- 0				# 11.3

# envelopes/BG report_Directive 2003/87/EC Article 21_2016/bg_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BG" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="BG" & ds$year==2016] <- 1								# 4.2
ds$inspection[ds$country=="BG" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="BG" & ds$year==2016] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2016] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2016] <- "To be calculated"			# 11.3 IDs -  203535 (Tamara 2009 OOD)
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2016] <- 0				# 11.3

# envelopes/BG report_Directive 2003/87/EC Article 21_2017/bg_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BG" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="BG" & ds$year==2017] <- 1								# 4.2
ds$inspection[ds$country=="BG" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="BG" & ds$year==2017] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2017] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2017] <- 3	# 11.4
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2017] <- "To be calculated"			# 11.3 IDs - 49 (Tera Co Ltd), 153 (V&VGD Greenhouse Petrich Ltd), 201534 (Polysan)
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2017] <- 0				# 11.3

# envelopes/BG report_Directive 2003/87/EC Article 21_2018/bg_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BG" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="BG" & ds$year==2018] <- 1								# 4.2
ds$inspection[ds$country=="BG" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="BG" & ds$year==2018] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2018] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2018] <- "To be calculated"			# 11.3  IDs - 49 (Tera Co Ltd)
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2018] <- 0		 		# 11.3

# envelopes/BG report_Directive 2003/87/EC Article 21_2019/bg_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="BG" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="BG" & ds$year==2019] <- 1								# 4.2
ds$inspection[ds$country=="BG" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="BG" & ds$year==2019] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="BG" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="BG" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="BG" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="BG" & ds$year==2019] <- 0		 		# 11.3

# envelopes(2)/2008 Art. 21 report/2008_Questionnaire_CYETS_260608.pdf
ds$appointment[ds$country=="CY" & ds$year==2007] <- 1
ds$supervision[ds$country=="CY" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="CY" & ds$year==2007] <- 1
ds$suspension[ds$country=="CY" & ds$year==2007] <- 1
ds$inspection[ds$country=="CY" & ds$year==2007] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2007] <- 0

# Report for 2008 is not public.

# envelopes(1)/2010 Art. 21 Report (for 2009)/2010_Questionnaire_CYETS_220610.pdf
ds$suspension[ds$country=="CY" & ds$year==2009] <- 1
ds$inspection[ds$country=="CY" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="CY" & ds$year==2009] <- 1
ds$supervision[ds$country=="CY" & ds$year==2009] <- 1
ds$appointment[ds$country=="CY" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2009] <- 0

# envelopes(1)/2011 Art. 21 Report (for 2010)/2011_Questionnaire_CYETS_030611.pdf
ds$suspension[ds$country=="CY" & ds$year==2010] <- 1
ds$inspection[ds$country=="CY" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="CY" & ds$year==2010] <- 1
ds$supervision[ds$country=="CY" & ds$year==2010] <- 1
ds$appointment[ds$country=="CY" & ds$year==2010] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2010] <- 0

# envelopes(1)/2012 Art. 21 Report (for 2011)/2012_Questionnaire_CYETS_300612.pdf
ds$suspension[ds$country=="CY" & ds$year==2011] <- 1
ds$inspection[ds$country=="CY" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="CY" & ds$year==2011] <- 1
ds$supervision[ds$country=="CY" & ds$year==2011] <- 1
ds$appointment[ds$country=="CY" & ds$year==2011] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2011] <- 0

# Report for 2012 is not public.

# envelopes(1)/2015 Art.21 Report-corrected xml (for 2013)/cy-eu-emt-envu56yia-2013.xml
ds$namingshaming[ds$country=="CY" & ds$year==2013] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2013] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2013] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2013] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2013] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2013] <- 0				# 11.3

# envelopes(1)/2015 Art.21 Report-corrected xml (for 2014)/cy-eu-emt-envu56yia-2014.xml
ds$namingshaming[ds$country=="CY" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2014] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2014] <- 0				# 11.3

# envelopes/2016 Art.21 Report (for 2015)/cy.xml
ds$namingshaming[ds$country=="CY" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2015] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2015] <- 0				# 11.3

# envelopes/Article 21 report - Cyprus 2016/cy_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CY" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2016] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 report - Cyprus 2017/cy_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CY" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2017] <- 10						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 report - Cyprus 2018/cy_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CY" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2018] <- 19						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2018] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21Cyprus 2019/cy_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CY" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="CY" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="CY" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="CY" & ds$year==2019] <- 19						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CY" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CY" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="CY" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CY" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Czech Republic Article 21 Report - 2006/CZ_EU-ETS-Article21-Reporting.doc
ds$suspension[ds$country=="CZ" & ds$year==2006] <- 1
ds$inspection[ds$country=="CZ" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="CZ" & ds$year==2006] <- 0
ds$appointment[ds$country=="CZ" & ds$year==2006] <- 0
ds$supervision[ds$country=="CZ" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2006] <- 0

# envelopes(2)/Czech Republic Article 21 Report - 2007/Article21-Reporting_2007.doc
ds$suspension[ds$country=="CZ" & ds$year==2007] <- 1
ds$inspection[ds$country=="CZ" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="CZ" & ds$year==2007] <- 0
ds$appointment[ds$country=="CZ" & ds$year==2007] <- 0
ds$supervision[ds$country=="CZ" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2007] <- 0

# envelopes(2)/Czech Republic Article 21 Report - 2008/Article21-Reporting_2008.doc
ds$suspension[ds$country=="CZ" & ds$year==2008] <- 1
ds$inspection[ds$country=="CZ" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="CZ" & ds$year==2008] <- 0
ds$appointment[ds$country=="CZ" & ds$year==2008] <- 1
ds$supervision[ds$country=="CZ" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2008] <- 0

# envelopes(1)/Czech reporting Article 21 Report - 2009/Article21-Reporting_2009.doc
ds$suspension[ds$country=="CZ" & ds$year==2009] <- 1
ds$inspection[ds$country=="CZ" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="CZ" & ds$year==2009] <- 0
ds$appointment[ds$country=="CZ" & ds$year==2009] <- 1
ds$supervision[ds$country=="CZ" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2009] <- 0

# envelopes/Czech Republic Article 21 Report - 2013 resubmission/Article_21_questionnaire_CZ_2013_resubmitted.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2013] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2013] <- 1								# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2013] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2013] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2013] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2013] <- 2320				# 11.3 all no/yes

# envelopes/Czech Republic Article 21 Report - 2014 resubmission/Article_21_questionnaire_CZ_2014_resubmitted.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2014] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2014] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="CZ" & ds$year20142015] <-0 				# 11.3
  
# envelopes/Czech Republic Article 21 Report - 2015 resubmission/Article_21_questionnaire_CZ_2015_resubmitted.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2015] <- 3960				# 11.3 all no/yes

# envelopes/Czech Republic Article 21 Report - 2016/cz_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2016] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2016] <- 1000				# 11.3 no/yes

# envelopes/Czech Republic Article 21 Report - 2017 resubmission/cz_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2017] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2017] <- 1	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2017] <- 2	# 11.4
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2017] <- 1225169			# 11.3  no/yes
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2017] <- 1000				# 11.3 no/yes

# envelopes/Czech Republic Article 21 Report - 2018 resubmission/cz_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2018] <- 5						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2018] <- 1	# 11.4 IDs - Papírna APIS, s.r.o. (no id code), 160() 
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2018] <- 2	# 11.4 
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2018] <- 165096			#11.3 no/yes
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2018] <- 0				# 11.3

# envelopes/Czech Republic Article 21 Report - 2019/cz_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="CZ" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="CZ" & ds$year==2019] <- 	1							# 4.2 
ds$inspection[ds$country=="CZ" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="CZ" & ds$year==2019] <- 5						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2019] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2019] <- 	0			# 11.3 

# Report on 2005 not publicly available.
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2005] <- 5		# Based on 2007 report
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2005] <- 16		# Based on 2007 report
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2005] <- 3043560		# Based on 2007 report

# envelopes(1)/Artikel 21 Report Germany 2007/EU_ETS_German_Art_21_report_June_2008_final_15-07-08.pdf
ds$suspension[ds$country=="DE" & ds$year==2007] <- 0
ds$inspection[ds$country=="DE" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="DE" & ds$year==2007] <- 1
ds$appointment[ds$country=="DE" & ds$year==2007] <- 1
ds$supervision[ds$country=="DE" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2007] <- 0

# envelopes(2)/Artikel 21 Report Germany 2008/Art21-2008_EU_ETS_German_clean_30-07-09_final_10-08-09.pdf
ds$suspension[ds$country=="DE" & ds$year==2008] <- 0
ds$inspection[ds$country=="DE" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="DE" & ds$year==2008] <- 1
ds$appointment[ds$country=="DE" & ds$year==2008] <- 1
ds$supervision[ds$country=="DE" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2008] <- 7
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2008] <- 13
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2008] <- "To be calculated" # Operators named in the 7 finalised cases are: Ev. Waldkrankenhaus Spandau GmbH, Kartonfabrik Buchmann GmbH, ArcelorMittal Eisenhüttenstadt GmbH, Insolvenzverwalter Rolf Otto Neukirchen für die RKL Ruhr Kristall Glas AG, Schaefer Kalk GmbH & Co. KG, SWM Services GmbH, RAG Deutsche Steinkohle AG, Kesselanlage Victoria Lünen.
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2008] <- 1924840

# envelopes(1)/Artikel 21 Report Germany 2009/DE_Art21-2009_EU_ETS_German_clean_final-100910.pdf
ds$suspension[ds$country=="DE" & ds$year==2009] <- 0
ds$inspection[ds$country=="DE" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="DE" & ds$year==2009] <- 1
ds$appointment[ds$country=="DE" & ds$year==2009] <- 1
ds$supervision[ds$country=="DE" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2009] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2009] <- 5
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2009] <- 210320
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2009] <- 0

# envelopes(1)/Artikel 21 Report Germany 2010/DE_Art21-2010_EU_ETS_German_final-clean_pdf.pdf
ds$suspension[ds$country=="DE" & ds$year==2010] <- 0
ds$inspection[ds$country=="DE" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="DE" & ds$year==2010] <- 1
ds$appointment[ds$country=="DE" & ds$year==2010] <- 1
ds$supervision[ds$country=="DE" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2010] <- 4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2010] <- 7
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2010] <- 1429100
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2010] <- 0

# envelopes(1)/Article 21 Report Germany 2011/DE_Art21-2011_EU_ETS_German_-_clean_BMU_30-07-2012final.pdf
ds$suspension[ds$country=="DE" & ds$year==2011] <- 0
ds$inspection[ds$country=="DE" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="DE" & ds$year==2011] <- 1
ds$appointment[ds$country=="DE" & ds$year==2011] <- 1
ds$supervision[ds$country=="DE" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2011] <- 1	# Only one fine has been finalized: SCA Packaging Container board Deutschland GmbH
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2011] <- 20
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2011] <- 11056000 
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2011] <- 0

# envelopes(1)/Article 21 Report Germany 2012/131029_a_Art_21-report_DEU_2012_incl_Chpt_5_EN-version_clean.pdf
ds$suspension[ds$country=="DE" & ds$year==2012] <- 0
ds$inspection[ds$country=="DE" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="DE" & ds$year==2012] <- 1
ds$appointment[ds$country=="DE" & ds$year==2012] <- 1
ds$supervision[ds$country=="DE" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2012] <- 1	# Only one fine has been finalized: Salzgitter Flachstahl GmbH
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2012] <- 10
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2012] <- 1934000
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2012] <- 0

# envelopes(1)/Article 21 Report Germany 2013 (courtesy translation to English from Germany)/article21_questionaire_2013_english.xml
ds$namingshaming[ds$country=="DE" & ds$year==2013] <- 1							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="DE" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="DE" & ds$year==2013] <- 	1							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="DE" & ds$year==2013] <- 0						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed 
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2013] <-0 				# S11_InstallationsPenaltiesImposed

# envelopes(1)/Article 21 Report Germany 2014 (courtesy translation to English from Germany)/Article21_2015-english.xml
ds$namingshaming[ds$country=="DE" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="DE" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="DE" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="DE" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2014] <-1 	# 11.4
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2014] <-"To be calculated" 			# 11.3 IDs - 14260-0204 (Rath GmbH)
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2014] <- 	0			# 11.3

# envelopes/Article 21 Report Germany 2015/Article21_2015.xml
# envelopes/Article 21 Report Germany 2015 (courtesy translation to English from Germany)/Article21_2015_Translation.txt
#no file in second folder but corresponds to what is online
ds$namingshaming[ds$country=="DE" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="DE" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="DE" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="DE" & ds$year==2015] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2015] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2015] <- "To be calculated"			# 11.3 IDs - 14226-0037 (Fritz Winter Eisengießerei GmbH & Co KG)
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2015] <- 0				# 11.3

# envelopes/Article 21 Report Germany 2016 (courtesy translation to English from Germany)/Article21_2016-translation.xml
ds$namingshaming[ds$country=="DE" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="DE" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="DE" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="DE" & ds$year==2016] <-11						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2016] <- 0				# 11.3


# envelopes/Article 21 Report Germany 2017 (courtesy translation to English from Germany)/Article21_2017_translation.xml
ds$namingshaming[ds$country=="DE" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="DE" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="DE" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="DE" & ds$year==2017] <-9						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2017] <- 7878.5				# 11.3  no/yes

# envelopes/Article 21 Report Germany 2018 (courtesy translation to English from Germany)/Article21_2018_translation.xml
ds$namingshaming[ds$country=="DE" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="DE" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="DE" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="DE" & ds$year==2018] <-9						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2018] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2018] <- 2628.5				# 11.3 no/yes

# envelopes/Article 21 Report Germany 2019 (courtesy translation to English from Germany) - updated/Article21_2019-Translation_update
ds$namingshaming[ds$country=="DE" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="DE" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="DE" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="DE" & ds$year==2019] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DE" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DE" & ds$year==2019] <- 1	# 11.4 
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2019] <- "To be calculated"			# 11.3 IDs - 210665 (Holzheizkraftwerk Horn GmbH (2017))
ds$otherfines.q.installations[ds$country=="DE" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Emissions Trading Directive 2003/87/EC Art. 21/04072006_art_21_report_final.doc
ds$suspension[ds$country=="DK" & ds$year==2005] <- 1
ds$inspection[ds$country=="DK" & ds$year==2005] <- 0
ds$namingshaming[ds$country=="DK" & ds$year==2005] <- 1
ds$appointment[ds$country=="DK" & ds$year==2005] <- 0
ds$supervision[ds$country=="DK" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2005] <- 0

# envelopes(2)/Directive 2003/87/EC Article 21 - 2007/EC_Article_21_report_2007_-_final.doc
ds$suspension[ds$country=="DK" & ds$year==2006] <- 1
ds$inspection[ds$country=="DK" & ds$year==2006] <- 0
ds$namingshaming[ds$country=="DK" & ds$year==2006] <- 1
ds$appointment[ds$country=="DK" & ds$year==2006] <- 0
ds$supervision[ds$country=="DK" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2006] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2006] <- 1
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2006] <- "To be calculated"	# 2006: Lemvig Varmeværk
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2006] <- 0

# Based on 2009 report
# 2007: Due to a technical problem the surrendering for one operator – Sæby Varmevæk -was not validated by the system. The Danish Energy Agency has determined that the installation is in compliance. Further, the installation was in good faith and could not reasonably have foreseen the problem or corrected it.
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2007] <- 0

# Based on 2009 report
# 2008: All operators have surrendered sufficient allowances by 30 April 2009.
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2008] <- 0

# envelopes(1)/Article 21 report 2009/DK_Article_21_report_2009-_final.pdf
ds$suspension[ds$country=="DK" & ds$year==2009] <- 1
ds$inspection[ds$country=="DK" & ds$year==2009] <- 0
ds$namingshaming[ds$country=="DK" & ds$year==2009] <- 1
ds$appointment[ds$country=="DK" & ds$year==2009] <- 0
ds$supervision[ds$country=="DK" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2009] <- 0

# envelopes/Updated Article 21 report - 2013/dk-eu-emt-envu3s7ma-2013data.xml
ds$namingshaming[ds$country=="DK" & ds$year==2013] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2013] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2013] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2013] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2013] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2013] <-1 	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2013] <- "To be Calculated"			# 11.3 IDs - 401 (Regulerkraft Ikast ApS)
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2013] <- 0				# 11.3

#online database: /Article 21 report - 2014/dk-eu-emt-envu3s7ma.xml
ds$namingshaming[ds$country=="DK" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2014] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2014] <- 0				# 11.3

# envelopes/Article 21 DK-report - 2015/dk.xml
ds$namingshaming[ds$country=="DK" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2015] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2015] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2015] <- 0				# 11.3

# envelopes/Article 21 DK-report 2016/dk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="DK" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 report 2017 DK/dk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="DK" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 report 2018 DK/dk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="DK" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21 report 2019 DK/dk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="DK" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="DK" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="DK" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="DK" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2019] <- 2	# 11.4
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2019] <- 'To be calculated'			# 11.3 IDs - DK-222 (Guldborgsund Varme A/S), DK-223 (Guldborgsund Varme A/S)
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Emissions Trading Article 21 questionnaire EE/ESTONIA_Art_21_questionnaire_Part_1_and_2.doc/
ds$suspension[ds$country=="EE" & ds$year==2005] <- 0
ds$inspection[ds$country=="EE" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2005] <- 1
ds$appointment[ds$country=="EE" & ds$year==2005] <- 1
ds$supervision[ds$country=="EE" & ds$year==2005] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2005] <- 0

# envelopes(2)/Emissions Trading Article 21 questionnaire EE 2006/Art_21_Report.doc
ds$suspension[ds$country=="EE" & ds$year==2006] <- 0
ds$inspection[ds$country=="EE" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2006] <- 0
ds$appointment[ds$country=="EE" & ds$year==2006] <- 1
ds$supervision[ds$country=="EE" & ds$year==2006] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2006] <- 0

# envelopes(2)/Emissions Trading Article 21 questionnaire EE 2007/Art_21_Report.doc
ds$suspension[ds$country=="EE" & ds$year==2007] <- 0
ds$inspection[ds$country=="EE" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2007] <- 0
ds$appointment[ds$country=="EE" & ds$year==2007] <- 1
ds$supervision[ds$country=="EE" & ds$year==2007] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2007] <- 0

# envelopes(2)/Article 21 report 2008/Art_21_Report_2008__TC_.doc
ds$suspension[ds$country=="EE" & ds$year==2008] <- 0
ds$inspection[ds$country=="EE" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2008] <- 1
ds$appointment[ds$country=="EE" & ds$year==2008] <- 1
ds$supervision[ds$country=="EE" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2008] <- 154+58

# envelopes(1)/Article 21 report 2009/Art_21_Report_2009.odt
ds$suspension[ds$country=="EE" & ds$year==2009] <- 0
ds$inspection[ds$country=="EE" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2009] <- 1
ds$appointment[ds$country=="EE" & ds$year==2009] <- 1
ds$supervision[ds$country=="EE" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2009] <- 154+58

# envelopes(1)/Article 21 Report 2010/Art_21_Report_2010_EE.doc/
ds$suspension[ds$country=="EE" & ds$year==2010] <- 0
ds$inspection[ds$country=="EE" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2010] <- 1
ds$appointment[ds$country=="EE" & ds$year==2010] <- 1
ds$supervision[ds$country=="EE" & ds$year==2010] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2010] <- 0

# envelopes(1)/Article 21 report 2011/Art_21_Report_2011_EE.docx
ds$suspension[ds$country=="EE" & ds$year==2011] <- 0
ds$inspection[ds$country=="EE" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2011] <- 1
ds$appointment[ds$country=="EE" & ds$year==2011] <- 1
ds$supervision[ds$country=="EE" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2011] <- 0

# envelopes(1)/Article 21 report 2012/Art_21_Report_2012_EE.doc
ds$suspension[ds$country=="EE" & ds$year==2012] <- 0
ds$inspection[ds$country=="EE" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="EE" & ds$year==2012] <- 1
ds$appointment[ds$country=="EE" & ds$year==2012] <- 1
ds$supervision[ds$country=="EE" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2012] <- 0

# envelopes/Article 21 report 2014 resubmission_1/ee-eu-emt-envu5rm_g.xml
ds$namingshaming[ds$country=="EE" & ds$year==2013] <-1 							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="EE" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="EE" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="EE" & ds$year==2013] <- 		0				# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2013] <- 0	# S11_InstallationsPenaltiesImposed
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2013] <-0	# S11_InstallationsPenaltiesImposed
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2013] <-0 			# 11_InstallationsInfringementPenalties
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2013] <-0 				# 11_InstallationsInfringementPenalties

# envelopes/Article 21 report 2014 resubmission_1/Article_21_questionnaire_1.xml.xml
ds$namingshaming[ds$country=="EE" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="EE" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="EE" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="EE" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2014] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2014] <- 0				# 11.3

# envelopes/Article 21 Report 2015/ee.xml
ds$namingshaming[ds$country=="EE" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="EE" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="EE" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="EE" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2015] <- 0				# 11.3

# envelopes/Article 21 Report 2016/ee.xml
ds$namingshaming[ds$country=="EE" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="EE" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="EE" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="EE" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 Report 2017/ee_eu-ets-art21_report.xml
# envelopes/Article 21 Report 2017 resubmission/ee_eu-ets-art21_report.xml
#Both listed for the same year and have the same inputs. 
ds$namingshaming[ds$country=="EE" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="EE" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="EE" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="EE" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 Report 2018/ee_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="EE" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="EE" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="EE" & ds$year==2018] <- 1  						# 11.1
ds$inspection.n[ds$country=="EE" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2018] <- 0	# 11.4 t
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21 Report 2019/ee_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="EE" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="EE" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="EE" & ds$year==2019] <- 1  						# 11.1
ds$inspection.n[ds$country=="EE" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="EE" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Article 21 questionnaire/Informe_Art.21_-_ES_-_Julio2006.pdf
ds$suspension[ds$country=="ES" & ds$year==2005] <- 0
ds$inspection[ds$country=="ES" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2005] <- 1
ds$supervision[ds$country=="ES" & ds$year==2005] <- 1
ds$appointment[ds$country=="ES" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2005] <- "Maximum"
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2005] <- "To be calculated"
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2005] <- 0

# envelopes(2)/Report under Article 21 - Reporting year 2006/Informe_Art.21-2007-ES.pdf
ds$suspension[ds$country=="ES" & ds$year==2006] <- 0
ds$inspection[ds$country=="ES" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2006] <- 1
ds$supervision[ds$country=="ES" & ds$year==2006] <- 1
ds$appointment[ds$country=="ES" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2006] <- 2000

# envelopes(2)/Report under article 21 - Reporting year 2007/Informe_Art.21-2008-ES.pdf
ds$suspension[ds$country=="ES" & ds$year==2007] <- 0
ds$inspection[ds$country=="ES" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2007] <- 1
ds$supervision[ds$country=="ES" & ds$year==2007] <- 1
ds$appointment[ds$country=="ES" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2007] <- 0

# envelopes(2)/Report under article 21 - Reporting year 2008/Informe_Art.21-2009-ES.pdf
ds$suspension[ds$country=="ES" & ds$year==2008] <- 0
ds$inspection[ds$country=="ES" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2008] <- 1
ds$supervision[ds$country=="ES" & ds$year==2008] <- 1
ds$appointment[ds$country=="ES" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2008] <- 1000

# envelopes(1)/Report under article 21 - Reporting year 2009/Informe_Art_21-2010-ES-FINAL.pdf
ds$suspension[ds$country=="ES" & ds$year==2009] <- 0
ds$inspection[ds$country=="ES" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2009] <- 1
ds$supervision[ds$country=="ES" & ds$year==2009] <- 1
ds$appointment[ds$country=="ES" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2009] <- 3
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2009] <- 4	# 2011 report says another case has been initiated for 2009 failure to surrender allowances.
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2009] <- 383600	# Based on 2010 report
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2009] <- 10000*2 # En dos de los casos, los titulares tampoco hicieron entrega del informe de emisiones correspondiente al año 2009. The penalty for this type of violation is listed as euro 10,000.

# envelopes(1)/Report under article 21 - Reporting year 2010/Informe_Art_21-2011-ES-FINAL.pdf
ds$suspension[ds$country=="ES" & ds$year==2010] <- 0
ds$inspection[ds$country=="ES" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2010] <- 1 
ds$supervision[ds$country=="ES" & ds$year==2010] <- 1
ds$appointment[ds$country=="ES" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2010] <- 328700+3000+3000+3000

# envelopes(1)/Report under article 21 - Reporting year 2011/Informe_Art_21-2012-ES-FINAL.pdf
ds$suspension[ds$country=="ES" & ds$year==2011] <- 0
ds$inspection[ds$country=="ES" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2011] <- 1
ds$supervision[ds$country=="ES" & ds$year==2011] <- 1
ds$appointment[ds$country=="ES" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2011] <- 50001+328700+3120+3000+3000+3000+3000

# envelopes(1)/Report under article 21 - Reporting year 2012/Informe_Art_21-2013-ES-FINAL.pdf
ds$suspension[ds$country=="ES" & ds$year==2012] <- 0
ds$inspection[ds$country=="ES" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="ES" & ds$year==2012] <- 1
ds$supervision[ds$country=="ES" & ds$year==2012] <- 1
ds$appointment[ds$country=="ES" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2012] <- 25000+26002

# envelopes(1)/Report under article 21 - Reporting year 2013. 3rd Corrected version/Article_21_Questionnaire_v04.xml
ds$namingshaming[ds$country=="ES" & ds$year==2013] <-0 							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="ES" & ds$year==2013] <- 	1						# S04_NationalLawPermitUpdate - Los supuestos de extinción se recogen en el artículo 7 de la Ley 1/2005, de 9 de marzo.
ds$inspection[ds$country=="ES" & ds$year==2013] <-  1 						# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="ES" & ds$year==2013] <- 53						# S06_InstallationsVerificationReportChecksDetail1
#ds$appointment[ds$country=="ES" & ds$year==2013] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2013] <- NA							# 
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2013] <- 0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2013] <- 0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2013] <- 0			#S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2013] <- 25000			#S11_InstallationsPenaltiesImposed all no/yes

# envelopes/Report under article 21 - Reporting year 2014. 2nd Corrected version/es-eu-emt-envvqavba.xml
ds$namingshaming[ds$country=="ES" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="ES" & ds$year==2014] <- 1							# 4.2 
ds$inspection[ds$country=="ES" & ds$year==2014] <-  1 						# 11.1
ds$inspection.n[ds$country=="ES" & ds$year==2014] <- 36						# 6.5
#ds$appointment[ds$country=="ES" & ds$year==2014] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2014] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2014] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2014] <- 1 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2014] <- 	"To be calculated"		# 11.3 IDs - 891 (ANTIBIÓTICOS, S.A.)
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2014] <- 	30601			# 11.3 all no/yes

# envelopes/Report under article 21 - Reporting year 2015/Art_21_Y2015_ES.xml
ds$namingshaming[ds$country=="ES" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="ES" & ds$year==2015] <- 	1							# 4.2 
ds$inspection[ds$country=="ES" & ds$year==2015] <- 1  						# 11.1
ds$inspection.n[ds$country=="ES" & ds$year==2015] <- 	28					# 6.5
#ds$appointment[ds$country=="ES" & ds$year==2015] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2015] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2015] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2015] <- 37001				# 11.3 all remaining are no/yes

# envelopes/Report under article 21 - Reporting year 2016/es_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="ES" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="ES" & ds$year==2016] <- 	1							# 4.2 
ds$inspection[ds$country=="ES" & ds$year==2016] <-  1 						# 11.1
ds$inspection.n[ds$country=="ES" & ds$year==2016] <- 182						# 6.5
#ds$appointment[ds$country=="ES" & ds$year==2016] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2016] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2016] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2016] <- 	27501			# 11.3

# envelopes/Report under article 21 - Reporting year 2017/es_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="ES" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="ES" & ds$year==2017] <- 	1							# 4.2 
ds$inspection[ds$country=="ES" & ds$year==2017] <-   1						# 11.1
ds$inspection.n[ds$country=="ES" & ds$year==2017] <- 182						# 6.5
#ds$appointment[ds$country=="ES" & ds$year==2017] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2017] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2017] <- 0	# 11.4  
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2017] <- 451209				# 11.3 mixture of yes/no, no/yes

# envelopes/Report under article 21 - Reporting year 2018. Corrected version/es_eu-ets-art21_report_Y2018_Ed2019.xml
ds$namingshaming[ds$country=="ES" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="ES" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="ES" & ds$year==2018] <- 1  						# 11.1
ds$inspection.n[ds$country=="ES" & ds$year==2018] <- 16						# 6.5
#ds$appointment[ds$country=="ES" & ds$year==2018] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2018] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2018] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2018] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2018] <- 	0			# 11.3

# envelopes/Report under article 21 - Reporting year 2019/es_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="ES" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="ES" & ds$year==2019] <- 	1							# 4.2 
ds$inspection[ds$country=="ES" & ds$year==2019] <-  1 						# 11.1
ds$inspection.n[ds$country=="ES" & ds$year==2019] <- 14						# 6.5
#ds$appointment[ds$country=="ES" & ds$year==2019] <- NA							# 
#ds$supervision[ds$country=="ES" & ds$year==2019] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="ES" & ds$year==2019] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="ES" & ds$year==2019] <-1 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="ES" & ds$year==2019] <- "To be calculated"			# 11.3 IDs - 474 (DICEPA HIGIENE, S.L.)
ds$otherfines.q.installations[ds$country=="ES" & ds$year==2019] <- 	0			# 11.3 no/no for all 

# envelopes(2)/Directive 2003/87/EG Article 21 Questionnaire/Art_21_questionnaire_2006_Finland.doc
ds$suspension[ds$country=="FI" & ds$year==2005] <- 1
ds$inspection[ds$country=="FI" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="FI" & ds$year==2005] <- 1
ds$supervision[ds$country=="FI" & ds$year==2005] <- 1
ds$appointment[ds$country=="FI" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2005] <- 0

# envelopes(2)/Questionnaire 2007 Part1/Art_21_questionnaire_2007_Part1_Finland.doc
ds$suspension[ds$country=="FI" & ds$year==2006] <- 1
ds$inspection[ds$country=="FI" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="FI" & ds$year==2006] <- 1
ds$supervision[ds$country=="FI" & ds$year==2006] <- 1
ds$appointment[ds$country=="FI" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2006] <- 5	# Still pending. See 2008 report.
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2006] <- "To be calculated"	# Salon kaupunki, Loimaan Kaukolämpö Oy, Laitilan Lämpö Oy, Suomussalmen kunta, Kuusamon energia- ja vesiosuuskunta. See 2008 report.
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2006] <- 0

# envelopes(2)/Directive 2003/87/EC Article 21 Questionnaire 2007,Part 1/Art_21_questionnaire2008_Part1_Finland.doc
ds$suspension[ds$country=="FI" & ds$year==2007] <- 1
ds$inspection[ds$country=="FI" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="FI" & ds$year==2007] <- 1
ds$supervision[ds$country=="FI" & ds$year==2007] <- 1
ds$appointment[ds$country=="FI" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2007] <- 0

# envelopes(2)/Article 21 questionnaire2009_part1/Art_21_questionnaire2009_Part1_Finland.doc
ds$supervision[ds$country=="FI" & ds$year==2008] <- 1
ds$appointment[ds$country=="FI" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2006] <- 1	# Report refers to earlier year. Overwrite earlier report.
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2006] <- 4	# Report refers to earlier year. Overwrite earlier report.
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2007] <- 0	# Report refers to earlier year. Overwrite earlier report.
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2007] <- 0	# Report refers to earlier year. Overwrite earlier report.
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2006] <- "To be calculated" # 40 euro per tonne for Salon kaupunki, Loimaan Kaukolämpö Oy, Laitilan Lämpö Oy, and Suomussalmi Community. Only Suomussalmi Community has paid. The other three have appealed. Needs to be calculated based on data.
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2008] <- 0 

# envelopes(1)/Article 21 Questionnaire 2014/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="FI" & ds$year==2013] <- 1							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="FI" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate 
ds$inspection[ds$country=="FI" & ds$year==2013] <- 	1							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="FI" & ds$year==2013] <- 0						#S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2013] <-0 	# S11_OperatorPenalties 
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2013] <-2 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2013] <- "To be calculated" 		# S11_InstallationsPenaltiesImposed IDs - FI-442 (FnSteel Oy Ab), FI-659 (Rovaniemen koulutuskuntayhtymä)
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2013] <-0 				# S11_InstallationsPenaltiesImposed

# envelopes(1)/Article 21 Questionnaire 2015 Corrected/fi-eu-emt-envu47saw.xml
ds$namingshaming[ds$country=="FI" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="FI" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="FI" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="FI" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2014] <-0 	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2014] <-0 			# 11.3 
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2014] <-0 				# 11.3

# envelopes/Article 21 Questionnaire 2016/fi.xml
ds$namingshaming[ds$country=="FI" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="FI" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="FI" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="FI" & ds$year==2015] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2016] <- 0				# 11.3
  
# envelopes/Article 21 Questionnaire 2017/fi_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FI" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="FI" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="FI" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="FI" & ds$year==2016] <- 3						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 Questionnaire 2018/fi_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FI" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="FI" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="FI" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="FI" & ds$year==2017] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 Questionnaire 2019/fi_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FI" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="FI" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="FI" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="FI" & ds$year==2018] <- 3						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21 Questionnaire 2020/fi_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FI" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="FI" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="FI" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="FI" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FI" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/French report art. 21 - 2005/projet_2006_rapport_art.21_v2.doc
ds$suspension[ds$country=="FR" & ds$year==2005] <- 1
ds$inspection[ds$country=="FR" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="FR" & ds$year==2005] <- 1
ds$supervision[ds$country=="FR" & ds$year==2005] <- 0
ds$appointment[ds$country=="FR" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2005] <- 0

# envelopes(2)/French report art. 21 - 2006/2006_rapport_art21__english_.doc/
ds$suspension[ds$country=="FR" & ds$year==2006] <- 1
ds$inspection[ds$country=="FR" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="FR" & ds$year==2006] <- 1
ds$supervision[ds$country=="FR" & ds$year==2006] <- NA
ds$appointment[ds$country=="FR" & ds$year==2006] <- NA
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2006] <- 0

# envelopes(2)/French report art. 21 - 2007/2007_rapport_art21__english_.doc
ds$suspension[ds$country=="FR" & ds$year==2007] <- 1
ds$inspection[ds$country=="FR" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="FR" & ds$year==2007] <- 1
ds$supervision[ds$country=="FR" & ds$year==2007] <- NA
ds$appointment[ds$country=="FR" & ds$year==2007] <- NA
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2007] <- 0

# envelopes(2)/French report art. 21 - 2008/2008_art21_FR_english.doc/
ds$suspension[ds$country=="FR" & ds$year==2008] <- 1
ds$inspection[ds$country=="FR" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="FR" & ds$year==2008] <- 1
ds$supervision[ds$country=="FR" & ds$year==2008] <- 0
ds$appointment[ds$country=="FR" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2008] <- 0

# envelopes(1)/French report art. 21 - 2009/2009_art21_FR_english.doc
ds$suspension[ds$country=="FR" & ds$year==2009] <- 1
ds$inspection[ds$country=="FR" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="FR" & ds$year==2009] <- 1
ds$supervision[ds$country=="FR" & ds$year==2009] <- 0
ds$appointment[ds$country=="FR" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2009] <- 0

# envelopes(1)/French report art. 21 - 2013/fr-article21-2013data.xml
ds$namingshaming[ds$country=="FR" & ds$year==2013] <- 0							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2013] <- 	0							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2013] <- 	1							# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2013] <- 	20					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2013] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2013] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2013] <- 	0		# 11.3 
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2013] <- 	0			# 11.3 0 no/no for all fines with nonzero amounts

# envelopes(1)/French report art. 21 - 2014/fr-eu-emt-envu9u0ra.1.xml
ds$namingshaming[ds$country=="FR" & ds$year==2014] <- NA							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2014] <- 	0							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2014] <- 	NA							# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2014] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2014] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2014] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2014] <- 	0		# 11.3 
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2014] <- 		0		# 11.3

# envelopes/French report art. 21 - 2015/fr-correction.xml
ds$namingshaming[ds$country=="FR" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2015] <- 	1							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2015] <- 	6					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2015] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2015] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2015] <- 0				# 11.3

# envelopes/French report art. 21 - 2016/fr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FR" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2016] <- 	1							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2016] <- 	12					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2016] <- 0 	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2016] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2016] <- 0				# 11.3

# envelopes/FR_Art21_2017/fr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FR" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2017] <- 	1							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2017] <- 	23					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2017] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2017] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2017] <- 0				# 11.3

# envelopes/French report art21 2018/fr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FR" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2018] <- 	1							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2018] <- 	7					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2018] <- 0 	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2018] <- 0 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2018] <- 0				# 11.3

# envelopes/French report art21 2019/fr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="FR" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="FR" & ds$year==2019] <- 	1							# 4.2 
ds$inspection[ds$country=="FR" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="FR" & ds$year==2019] <- 	7					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="FR" & ds$year==2019] <- 0 	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="FR" & ds$year==2019] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="FR" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="FR" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Draft Commission Decision/Final_UK_Article_21_response.doc
ds$suspension[ds$country=="GB" & ds$year==2005] <- 1
ds$inspection[ds$country=="GB" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="GB" & ds$year==2005] <- 1
ds$supervision[ds$country=="GB" & ds$year==2005] <- 0
ds$appointment[ds$country=="GB" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] <- 4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2005] <- 4	# "We are still deciding what penalties to impose on operators who failed to surrender enough allowances." according to 2005 report, but 2 operators are given in 2006 report
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005] <- "To be calculated"	# Daniel Platt Limited, and Alphasteel Limited, Scandstick Limited, Mars UK Limited
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2005] <- NA	# "Fines: As determined by the courts"

# envelopes(1)/Article 21 Final Report/Art_21_Report_final_draft.doc
ds$suspension[ds$country=="GB" & ds$year==2006] <- 1
ds$inspection[ds$country=="GB" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="GB" & ds$year==2006] <- 1
ds$supervision[ds$country=="GB" & ds$year==2006] <- 0
ds$appointment[ds$country=="GB" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2006] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2006] <- 2
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2006] <- "To be calculated" # Scandstick Limited, and Mars UK Limited
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2006] <- 0

# envelopes(2)/UK 2008 Article 21 Report/UK_Article_21_2008_Report_Final.doc
# envelopes(2)/UK Article 21 2008 Report Final/UK_Article_21_2008_Report_Final.doc
ds$suspension[ds$country=="GB" & ds$year==2007] <- 1
ds$inspection[ds$country=="GB" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="GB" & ds$year==2007] <- 1
ds$supervision[ds$country=="GB" & ds$year==2007] <- 0
ds$appointment[ds$country=="GB" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2007] <- 0

# envelopes(1)/Article 21 report 2009/UK_2009_Article_21.doc
ds$suspension[ds$country=="GB" & ds$year==2008] <- 1	# No change
ds$inspection[ds$country=="GB" & ds$year==2008] <- 1	# No change
ds$namingshaming[ds$country=="GB" & ds$year==2008] <- 1	# No change
ds$supervision[ds$country=="GB" & ds$year==2008] <- 0
ds$appointment[ds$country=="GB" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- 2	# From 2012 report
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- "To be calculated"	# Two penalties totalling €1,000 to Ardagh Glass Ltd (GB-ETS-W-30015) relating to the 2008 and 2009 reporting years. Four penalties totalling €14,300 to Diageo Plc (GB-ETS-E-10052) relating to reporting years 2008 – 2011.
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2008] <- 0

# No public report for 2009. Information from 2012 report.
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- 2	# From 2012 report
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- "To be calculated"	# Two penalties totalling €1,000 to Ardagh Glass Ltd (GB-ETS-W-30015) relating to the 2008 and 2009 reporting years. Four penalties totalling €14,300 to Diageo Plc (GB-ETS-E-10052) relating to reporting years 2008 – 2011.

# envelopes(1)/UK Article 21 Report 2010/Article_21_UK_2010_-_Part_1.doc
ds$supervision[ds$country=="GB" & ds$year==2010] <- 0
ds$appointment[ds$country=="GB" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- 1
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- "To be calculated"	# From 2012 report: Four penalties totalling €14,300 to Diageo Plc (GB-ETS-E-10052) relating to reporting years 2008 – 2011.
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2010] <- 0

# envelopes(1)/UK Article 21 Report 2011/Art_21_questionnaire_2011_Submission_To_EU.doc
ds$supervision[ds$country=="GB" & ds$year==2011] <- 0
ds$appointment[ds$country=="GB" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- 13
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- 14	# From 2012 report
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- "To be calculated"	# 
# Astra Zeneca UK Ltd
# Sellafield Limited
# Esso Petroleum Company Ltd
# Murco Petroleum Ltd
# Skanska Rashleigh Weatherfoil Ltd
# York Hospital NHS Foundation Trust
# Whites Recycling Limited (in liquidation)
# FMC Biopolymer UK Limited
# Rohm & Haas (Scotland) Limited
# Allied Domecq Spirits & Wine Ltd
# Tennent Caledonian Breweries UK Limited
# Royal Group Of Hospitals
# AES Kilroot Power Station
# From 2012 report: Four penalties totalling €14,300 to Diageo Plc (GB-ETS-E-10052) relating to reporting years 2008 – 2011.
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2011] <- 0

# envelopes(1)/UK Article 21 report 2012/Article_21_report_2012_-_2013_-_06_-_26.doc
ds$supervision[ds$country=="GB" & ds$year==2012] <- 0
ds$appointment[ds$country=="GB" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2012] <- 0

# envelopes(1)/UK Article 21 Report 2013/UK_2013_Working_Draft.xml
ds$namingshaming[ds$country=="GB" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="GB" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="GB" & ds$year==2013] <- 	1							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="GB" & ds$year==2013] <- 	183					# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- 0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <-0 	#S11_InstallationsPenaltiesImposed 
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <-6179  #S11_InstallationsPenaltiesImposed - all no/yes

# envelopes(1)/UK Article 21 report 2014 - updated/gb-eu-emt-envu42pbg.xml
ds$namingshaming[ds$country=="GB" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="GB" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="GB" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="GB" & ds$year==2014] <- 12						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- 3	# 11.4
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- "To be calculated"	# 11.3 IDs - UK-S-IN-12363 (TOTAL E&P UK Limited), UK-S-IN-12334 (Calachem Limited), UK-E-IN-11568 (Sarval Ltd)
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- 24316 # 11.3 - all no/yes

# envelopes/UK Article 21 report for 2015/UK Article 21 report for 2015.xml
ds$namingshaming[ds$country=="GB" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="GB" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="GB" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="GB" & ds$year==2015] <- 32						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- 8	# 11.4
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- "To be calculated" 	# 11.3 IDs - UK-E-IN-11585 (Ministry of Defence - Brize Norton), UK-E-IN-11484 (Encirc Limited), UK-E-IN-11755 (Molson Coors Brewing Company (UK) Limited), UK-E-IN-11555 (SABIC UK Petrochemicals Limited), UK-E-IN-11668 (Cargill PLC), UK-E-IN-11817 (North Bristol NHS Trust), UK-E-IN-11804(Her Majestys Prison Service (HMPS)), UK-E-IN-11568 (Sarval Limited))
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- 146642.75 # 11.3 - all no/yes

# envelopes/EU ETS Article 21 report - 2016/gb_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GB" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="GB" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="GB" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="GB" & ds$year==2016] <- 32						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2016] <-6	# 11.4
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016] <- "To be calculated"	# 11.3
# UK-S-IN-12370 	Flotta Oil Terminal – Talisman Sinopec Energy UK Limited
# UK-S-IN-12399 	DSM Dalry
# 776 	Nexen Petroleum (UK) Limited - Buzzard
# 696 	BG International Limited - North Everest
# 61 	EnQuest Heather Limited -Thistle A
# 43 	Repsol - Claymore
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- 295456.61 # 11.3 - all no/yes

# envelopes/EU ETS Article 21 report - 2017/gb_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GB" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="GB" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="GB" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="GB" & ds$year==2017] <- 62						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2017] <- 8	# 11.4
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017] <- 3817	# 11.3 all no/yes - failure to surrender a permit 3817
# UK-S-IN-12370 	Repsol Sinopec Resources UK Limited
# UK-S-IN-12717 	Simec Lochaber Hydropower, Lochaber Smelter
# 39 	Marathon Oil UK Limited (Brae Bravo)
# 8 	Premier Oil UK Limited (Balmoral)
# 204463 	Premier Oil UK Limited (Voyaguer Spirit)
# 22 	Chevron North Sea Limited (Alba North Platform)
# UK-E-IN-11415 	ConocoPhillips Petroleum Co UK Ltd
# UK-E-IN-12722 	PUPL Realisation Ltd
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] <- 225965.52  # 11.3 all no/yes

# EU ETS Article 21 report for 2018 emissions/gb_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GB" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="GB" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="GB" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="GB" & ds$year==2018] <- 56						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2018] <- 11	# 11.4
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2018] <- "To be calculated"	# 11.3 UK-E-IN-11664 (Tata Steel UK Ltd), UK-E-IN-11678(Tata Steel UK Ltd)	,UK-E-IN-11705(Tata Steel UK Ltd), UK-E-IN-12603(Tata Steel UK Ltd), UK-E-IN-11788(British Airways PLC), UK-E-IN-11523 (CF Fertilisers UK Limited), UK-E-IN-12735 (Pauls Malt Limited), UK-E-IN-13331 (Growth Power Ltd (in Administration)), UK-E-IN-13426 (Imperium Centre Ltd),	UK-S-IN-12350 (Shell (UK) Limited), UK-S-IN-13049 (ENGIE FM Limited)		
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2018] <- 81972.01  # 11.3 - all good

# UK Article 21 report for 2019 emissions/gb_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GB" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="GB" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="GB" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="GB" & ds$year==2019] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2019] <- 7	# 11.4
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019] <- "To be calculated"	# 11.3 IDs - UK-E-IN-13310 (Uniper UK Limited), UK-E-IN-11691 (Rolls-Royce Plc), UK-E-IN-12586 (Rolls-Royce Plc), UK-E-IN-12795 (Total Lindsey Oil Refinery Ltd), UK-E-IN-13550 (Jones Lang Lasalle Ltd), UK-E-IN-12613 (British Ceramic Tile Ltd), UK-E-IN-13449 (Avara Avlon Pharma Services Ltd)
ds$otherfines.q.instasllations[ds$country=="GB" & ds$year==2019] <- 0  # 11.3

# envelopes(2)/Report under Article 21 Directive 87/2003/EC 2007/II-2007_GREEK_ANNUAL_REPORT_PURSUANT_TO_DIRECTIVE_200387EC_.6.08.pdf
ds$suspension[ds$country=="GR" & ds$year==2007] <- 1
ds$inspection[ds$country=="GR" & ds$year==2007] <- 0
ds$namingshaming[ds$country=="GR" & ds$year==2007] <- 1
ds$supervision[ds$country=="GR" & ds$year==2007] <- 0
ds$appointment[ds$country=="GR" & ds$year==2007] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2007] <- 0

# envelopes(2)/Report under Article 21 Directive 87/2003/EC 2008/Report_under_Article_21_Directive_87-2003-EC_2008.pdf
ds$suspension[ds$country=="GR" & ds$year==2008] <- 1	# "SAME, AS ANSWERED LAST YEAR"
ds$inspection[ds$country=="GR" & ds$year==2008] <- 0	# "SAME, AS ANSWERED LAST YEAR"
ds$namingshaming[ds$country=="GR" & ds$year==2008] <- 1	# "SAME, AS ANSWERED LAST YEAR"
ds$supervision[ds$country=="GR" & ds$year==2008] <- 0	# "SAME, AS ANSWERED LAST YEAR"
ds$appointment[ds$country=="GR" & ds$year==2008] <- 1	# "SAME, AS ANSWERED LAST YEAR"
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2008] <- 0

# envelopes(1)/Report under Article 21 Directive 87/2003/EC 2009/Report_under_Article_21_Directive_87-2003-EC_2009_.pdf
ds$suspension[ds$country=="GR" & ds$year==2009] <- 1	# "SAME, AS ANSWERED LAST YEAR"
ds$inspection[ds$country=="GR" & ds$year==2009] <- 0	# "SAME, AS ANSWERED LAST YEAR"
ds$namingshaming[ds$country=="GR" & ds$year==2009] <- 1	# "SAME, AS ANSWERED LAST YEAR"
ds$supervision[ds$country=="GR" & ds$year==2009] <- 0	# "No change has occurred in 2009"
ds$appointment[ds$country=="GR" & ds$year==2009] <- 1	# "No change has occurred in 2009"
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2009] <- 0

# envelopes(1)/Report under Article 21Directive 87/2003/EC 2010/Report_under_Article_21_Directive_87-2003-EC_2010.pdf
ds$suspension[ds$country=="GR" & ds$year==2010] <- 1	# "SAME, AS PREVIOUSLY ANSWERED"
ds$inspection[ds$country=="GR" & ds$year==2010] <- 0	# "SAME, AS PREVIOUSLY ANSWERED"
ds$namingshaming[ds$country=="GR" & ds$year==2010] <- 1	# "SAME, AS PREVIOUSLY ANSWERED"
ds$supervision[ds$country=="GR" & ds$year==2010] <- 0	# "No change has occurred in 2010"
ds$appointment[ds$country=="GR" & ds$year==2010] <- 1	# "No change has occurred in 2010"
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2010] <- 0

# envelopes(1)/Report under Article21 Greece 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="GR" & ds$year==2013] <-0 							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="GR" & ds$year==2013] <- 1								# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="GR" & ds$year==2013] <- 0								#S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="GR" & ds$year==2013] <- 0						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2013] <- 3	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2013] <- "To be Calculated"			# S11_InstallationsPenaltiesImposed IDs - CER-1 (CER VAS), CER-2 (CER KATS), CER-3 (CER CHR)
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed

#cant find greece 2014 folder but on webserver it is Report under /Article 21 Greece 2014/gr-eu-emt-envu5b8zg.xml
ds$namingshaming[ds$country=="GR" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="GR" & ds$year==2014] <- 1								# 4.2  
ds$inspection[ds$country=="GR" & ds$year==2014] <- 0								#11.1
ds$inspection.n[ds$country=="GR" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2014] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2014] <- 	8000			# 11.3 no/yes

# envelopes/Report under Article 21 Greece 2015/gr.xml
ds$namingshaming[ds$country=="GR" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="GR" & ds$year==2015] <- 1								# 4.2  
ds$inspection[ds$country=="GR" & ds$year==2015] <- 0								#11.1
ds$inspection.n[ds$country=="GR" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2015] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2015] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2015] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2015] <- 	0			# 11.3

# envelopes/Report under Article 21 Greece 2016/gr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GR" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="GR" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="GR" & ds$year==2016] <- 0								#11.1
ds$inspection.n[ds$country=="GR" & ds$year==2016] <- 8						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2016] <- 0				# 11.3

# envelopes/Report under Article 21 Greece 2017/gr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GR" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="GR" & ds$year==2017] <- 1								# 4.2  
ds$inspection[ds$country=="GR" & ds$year==2017] <- 0								#11.1
ds$inspection.n[ds$country=="GR" & ds$year==2017] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2017] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2017] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2017] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2017] <- 	0			# 11.3

# envelopes/Report under Article 21 Greece 2018/article21-questionnaire_1.xml
ds$namingshaming[ds$country=="GR" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="GR" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="GR" & ds$year==2018] <- 0								#11.1
ds$inspection.n[ds$country=="GR" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2018] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2018] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2018] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2018] <- 	0			# 11.3

# envelopes/Report under Article 21 Greece 2019/gr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="GR" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="GR" & ds$year==2019] <- 1								# 4.2  
ds$inspection[ds$country=="GR" & ds$year==2019] <- 0								#11.1
ds$inspection.n[ds$country=="GR" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="GR" & ds$year==2019] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="GR" & ds$year==2019] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="GR" & ds$year==2019] <-0 			# 11.3
ds$otherfines.q.installations[ds$country=="GR" & ds$year==2019] <- 	0			# 11.3

# envelopes/Croatia_Article 21 Questionnaire 2013 Corrected/hr-eu-emt-envu5voha.xml
ds$namingshaming[ds$country=="HR" & ds$year==2013] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2013] <- 1							# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2013] <- 1 								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2013] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2013] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2013] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2013] <-0 			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2013] <-0 	

# envelopes(1)/EU ETS Article 21 report/hr-eu-emt-envu5voha.xml
ds$namingshaming[ds$country=="HR" & ds$year==2014] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2014] <- 1							# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2014] <-1  								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2014] <-23 							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2014] <-0 			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2014] <- 0	    # 11.3

# envelopes/Reporting under Article 21(1) of Directive 2003/87/EC -report for 2015/hr.xml
ds$namingshaming[ds$country=="HR" & ds$year==2015] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2015] <- 30							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2015] <- 0		 		# 11.3

# envelopes/EU ETS Article 21 report 2016/hr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HR" & ds$year==2016] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2016] <- 19							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2016] <- 0		 		# 11.3

# envelopes/EU ETS Article 21 report 2017/hr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HR" & ds$year==2017] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2017] <- 44							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2017] <- 0		 		# 11.3

# envelopes/Croatia_Application of the Emissions Trading Directive 2003/87/EC(Article 21)_2018/hr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HR" & ds$year==2018] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2018] <- 9							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2018] <- 0		 		# 11.3

# envelopes/Croatia Application of the Emissions Trading Directive 2003/87/EC (Article 21) 2019/hr_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HR" & ds$year==2019] <- 1							# 11.1 
ds$suspension[ds$country=="HR" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="HR" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="HR" & ds$year==2019] <- 12							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HR" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HR" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HR" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="HR" & ds$year==2019] <- 0		 		# 11.3

# envelopes(2)/Art. 21 questionnaire/art21HUN_2006_part1.pdf
# ds$suspension[ds$country=="HU" & ds$year==2005] <- "No change", but no earlier report available.
# ds$inspection[ds$country=="HU" & ds$year==2005] <- "No change", but no earlier report available.
# ds$namingshaming[ds$country=="HU" & ds$year==2005] <- "No change", but no earlier report available.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2005] <- 0

# envelopes(2)/Questionnaire for reporting on the application of Directive 2003/87/EC /Art21_HU_2007.doc
ds$suspension[ds$country=="HU" & ds$year==2006] <- 0
ds$inspection[ds$country=="HU" & ds$year==2006] <- 0
ds$namingshaming[ds$country=="HU" & ds$year==2006] <- 0
ds$supervision[ds$country=="HU" & ds$year==2006] <- 1
ds$appointment[ds$country=="HU" & ds$year==2006] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2006] <- 160

# envelopes(2)/Art21_quest_HUN_2008/Art_21_2009_HU.doc
ds$suspension[ds$country=="HU" & ds$year==2008] <- 1
ds$inspection[ds$country=="HU" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="HU" & ds$year==2008] <- 0
# ds$supervision[ds$country=="HU" & ds$year==2008] <- "No change", but 2007 report not publicly available.
# ds$appointment[ds$country=="HU" & ds$year==2008] <- "No change", but 2007 report not publicly available.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2008] <- 35000+11000+2300

# envelopes(1)/Art21_HUN_2009/Article_21._report__Hungary__2010_Part_1-2.doc
ds$suspension[ds$country=="HU" & ds$year==2009] <- 1
ds$inspection[ds$country=="HU" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="HU" & ds$year==2008] <- 0
# ds$supervision[ds$country=="HU" & ds$year==2009] <- "No change", but 2007 report not publicly available.
# ds$appointment[ds$country=="HU" & ds$year==2009] <- "No change", but 2007 report not publicly available.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2009] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2009] <- 2
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2009] <- "To be calculated"	#	Nitrogénművek Zrt, and Globus Konzervipari Rt.
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2009] <- 1454+3999

# envelopes(1)/Art 21_HUN_2010/Article_21__report_Hungary_2011_Part_1-2_jav2.doc
ds$suspension[ds$country=="HU" & ds$year==2010] <- 1
ds$inspection[ds$country=="HU" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="HU" & ds$year==2010] <- 0
# ds$supervision[ds$country=="HU" & ds$year==2010] <- "No change", but 2007 report not publicly available.
# ds$appointment[ds$country=="HU" & ds$year==2010] <- "No change", but 2007 report not publicly available.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2010] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2010] <- 2
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2010] <- "To be calculated"	#	Nitrogénművek Zrt, and Globus Konzervipari Rt.
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2010] <- 1454+3999

# envelopes(1)/Art21_HUN_2011/Article_21__report_Hungary_2011_Part_1-2_0629_.doc
ds$suspension[ds$country=="HU" & ds$year==2011] <- 1
ds$inspection[ds$country=="HU" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="HU" & ds$year==2011] <- 0
# ds$supervision[ds$country=="HU" & ds$year==2011] <- "No change", but 2007 report not publicly available.
# ds$appointment[ds$country=="HU" & ds$year==2011] <- "No change", but 2007 report not publicly available.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2011] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2011] <- 1
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2011] <- "To be calculated"	# -	Higi Papírsoft Papírtermékeket Gyártó és Forgalmazó Zrt. 
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2011] <- 68000+111000

# envelopes(1)/Art21_HUN_2012/Article_21__report_Hungary_2012_Part_1-2_final.doc
ds$suspension[ds$country=="HU" & ds$year==2012] <- 1
ds$inspection[ds$country=="HU" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="HU" & ds$year==2012] <- 0
# ds$supervision[ds$country=="HU" & ds$year==2012] <- "No change", but 2007 report not publicly available.
# ds$appointment[ds$country=="HU" & ds$year==2012] <- "No change", but 2007 report not publicly available.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2012] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2012] <- 1
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2012] <- "To be calculated"	#	-	Higi Papírsoft Papírtermékeket Gyártó és Forgalmazó Zrt. 
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2012] <- 9200

# envelopes/ART_21_2013_HU_corrected version/hu-eu-emt-envu7fcdw_2013.xml
ds$namingshaming[ds$country=="HU" & ds$year==2013] <- 1	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2013] <- 1								# 4.2 - Permits can be withdrawn only if an installation is closed
ds$inspection[ds$country=="HU" & ds$year==2013] <- 1								# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2013] <- 2							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2013] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2013] <- 0				# 11.3

# envelopes(1)/ART_21_2014_HU_corrected version/hu-eu-emt-envu7fcdw.xml
ds$namingshaming[ds$country=="HU" & ds$year==2014] <-1 	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="HU" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2014] <- 44							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2014] <- 	61401			# 11.3 all no/yes

# envelopes/ART_21_2015_HU/hu.xml
ds$namingshaming[ds$country=="HU" & ds$year==2015] <- 1	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="HU" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2015] <- 47							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2015] <- 0				# 11.3

# envelopes/ART 21_2016_HU/hu.xml
ds$namingshaming[ds$country=="HU" & ds$year==2016] <- 1	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="HU" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2016] <- 42							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2016] <- 0				# 11.3

# envelopes/ART 21_2017_HU/hu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HU" & ds$year==2017] <- 1	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2017] <- 1								# 4.2
ds$inspection[ds$country=="HU" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2017] <- 37							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2017] <- 0				# 11.3

# envelopes/ART 21_2018_HU/hu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HU" & ds$year==2018] <- 1	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2018] <- 1								# 4.2
ds$inspection[ds$country=="HU" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2018] <- 26							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2018] <- 157494				#yes/no

# envelopes/ART 21_2019_HU/hu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="HU" & ds$year==2019] <- 1	   					# 11.1
ds$suspension[ds$country=="HU" & ds$year==2019] <- 1								# 4.2
ds$inspection[ds$country=="HU" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="HU" & ds$year==2019] <- 32							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="HU" & ds$year==2019] <- 157494				# 11.3 yes/no

# envelopes(2)/Article 21 Report Revised Format All 2005/Art_21_questionnaire_Ireland_RP_Part_1_All_2005.pdf
ds$suspension[ds$country=="IE" & ds$year==2005] <- 1
ds$inspection[ds$country=="IE" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="IE" & ds$year==2005] <- 1
ds$appointment[ds$country=="IE" & ds$year==2005] <- 0
ds$supervision[ds$country=="IE" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2005] <- 0

# envelopes(1)/ETS Implementation CY 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="IE" & ds$year==2013] <-0 							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="IE" & ds$year==2013] <- 1								# S04_NationalLawPermitUpdate 
ds$inspection[ds$country=="IE" & ds$year==2013] <- 1  						# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="IE" & ds$year==2013] <- 10						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2013] <-0 	# S11_OperatorPenalties 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed

# envelopes(1)/Ireland Art 21 Report CY 2014/ie-eu-emt-envu34roq.xml
ds$namingshaming[ds$country=="IE" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2014] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2014] <- 18						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2014] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2014] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2014] <-0       # 11.3

# envelopes/EU ETS Article 21 Questionnaire June 2016/ie.xml
ds$namingshaming[ds$country=="IE" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2015] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2015] <- 33						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2015] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2015] <- 0				# 11.3

# envelopes/Ireland EU ETS Article 21 CY 2016/ie_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IE" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2016] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2016] <- 12						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2016] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 Report- Ireland -2017/ie_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IE" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2017] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2017] <-12						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 Report Ireland for 2018 Emissions/ie_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IE" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2018] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2018] <- 11						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2018] <- 2	# 11.4 
  ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2018] <- "To be calculated"	# 11.3 IDs -  32 (St James' Hospital Board, Dublin), 210246 (Vodafone Group Services Ireland Limited)
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2018] <- 252271				# 11.3 201100 yes/yes. rest no/yes

# envelopes/Article 21 Report covering 2019 Emissions/ie_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IE" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2019] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2019] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2019] <- 0				# 11.3

# envelopes/Article 21 Report covering 2019 Emissions/ie_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IE" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="IE" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="IE" & ds$year==2019] <- 1  						# 11.1
ds$inspection.n[ds$country=="IE" & ds$year==2019] <- 2						# 6.5
#ds$appointment[ds$country=="IE" & ds$year==2019] <- NA							# 
#ds$supervision[ds$country=="IE" & ds$year==2019] <- NA 				# 
ds$emissionsfines.n.installations.lower[ds$country=="IE" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IE" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IE" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IE" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Article 21 reporting EU ETS Directive - Iceland/ETS_article_21_questionnaire_ISL.doc
ds$appointment[ds$country=="IS" & ds$year==2007] <- 0
ds$supervision[ds$country=="IS" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2007] <- 0

# envelopes(1)/Article 21 reporting EU ETS Directive - Iceland 2010/ETS_article_21_questionnaire_ISL_2010.doc
ds$appointment[ds$country=="IS" & ds$year==2008] <- 0
ds$supervision[ds$country=="IS" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2008] <- 0

# envelopes(1)/Article 21 Reporting EUETS Iceland 2013/Article_21_reporting_EUETS_Iceland_2013.pdf
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2012] <- 0

# envelopes(1)/Article 21 Reporting EU ETS Directive Iceland 2014/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="IS" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="IS" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="IS" & ds$year==2013] <- 	1							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="IS" & ds$year==2013] <-0 						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2013] <-0 			# S11_InstallationsPenaltiesImpose
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImpose

# envelopes(1)/Article 21 Reporting EU ETS Directive Iceland 2015- corrected/V2-is-eu-emt-envu5hsza.xml
ds$namingshaming[ds$country=="IS" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="IS" & ds$year==2014] <- 1								# 4.2
ds$inspection[ds$country=="IS" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="IS" & ds$year==2014] <-0 						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2014] <- 0				# 11.3

# envelopes/Article 21 Reporting EU ETS Directive Iceland 2016/is.xml
ds$namingshaming[ds$country=="IS" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="IS" & ds$year==2015] <- 1								# 4.2
ds$inspection[ds$country=="IS" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="IS" & ds$year==2015] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2015] <- 0				# 11.3

# envelopes/Article 21 Reporting EU ETS Directive Iceland 2017/is_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IS" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="IS" & ds$year==2016] <- 1								# 4.2
ds$inspection[ds$country=="IS" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="IS" & ds$year==2016] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2016] <- 0	# 11.4.
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2016] <- 0	#11.4
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 Reporting EU ETS Directive Iceland 2018/is_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IS" & ds$year==2017] <- 0							#11.1
ds$suspension[ds$country=="IS" & ds$year==2017] <- 1								# 4.2
ds$inspection[ds$country=="IS" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="IS" & ds$year==2017] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2017] <- 0	# 11.4.
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2017] <- 0				# 11.3

# envelopes/ Article 21 Reporting EU ETS Directive Iceland 2019/is_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IS" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="IS" & ds$year==2018] <- 1								# 4.2
ds$inspection[ds$country=="IS" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="IS" & ds$year==2018] <- 1							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21 Reporting EU ETS Directive Iceland 2020/is_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IS" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="IS" & ds$year==2019] <- 1								# 4.2
ds$inspection[ds$country=="IS" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="IS" & ds$year==2019] <- 2							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year==2019] <- 0			#11.3
ds$otherfines.q.installations[ds$country=="IS" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Italy Article 21 questionnaire 2005/Revised_Art_21_questionnaire_for_CCC_ITALY_final.doc
ds$suspension[ds$country=="IT" & ds$year==2005] <- 1
ds$inspection[ds$country=="IT" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="IT" & ds$year==2005] <- 0
ds$appointment[ds$country=="IT" & ds$year==2005] <- 1
ds$supervision[ds$country=="IT" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2005] <- 0

# envelopes(2)/Italy Article 21 questionnaire 2006/Art_21_Tables_Italy_2007.xls
ds$suspension[ds$country=="IT" & ds$year==2006] <- 1
ds$inspection[ds$country=="IT" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="IT" & ds$year==2006] <- 0
ds$appointment[ds$country=="IT" & ds$year==2006] <- 1
ds$supervision[ds$country=="IT" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2006] <- 25000

# envelopes(2)/Italy Article 21 questionnaire 2007 /Art_21_Questionnaire_for_CCC_Italy_2008_compiled.doc
ds$suspension[ds$country=="IT" & ds$year==2007] <- 1
ds$inspection[ds$country=="IT" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="IT" & ds$year==2007] <- 0
ds$appointment[ds$country=="IT" & ds$year==2007] <- 1
ds$supervision[ds$country=="IT" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2007] <- 50000+65000+4000 # + (exact amount to be determined by July 31st)

# envelopes(1)/Italy Article 21 questionnaire 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="IT" & ds$year==2013] <- 0							#S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="IT" & ds$year==2013] <- 	1							#S04_NationalLawPermitUpdate 
ds$inspection[ds$country=="IT" & ds$year==2013] <- 0 						# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="IT" & ds$year==2013] <- 0						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2013] <-14 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2013] <-14 	# S11_OperatorPenalties 
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2013] <- 5359601			# S11_InstallationsPenaltiesImposed all no/yes
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2013] <-2079908 				# S11_InstallationsPenaltiesImposed all no/yes

# envelopes(1)/Italy Article 21 questionnaire 2014/it-eu-emt-envu6l9jg.xml
ds$namingshaming[ds$country=="IT" & ds$year==2014] <-0 							# 11.1
ds$suspension[ds$country=="IT" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="IT" & ds$year==2014] <- 0  						# 11.1
ds$inspection.n[ds$country=="IT" & ds$year==2014] <- 0						# 6.5 
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2014] <-0 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2014] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2014] <- 19760900				# 11.3 all yes/yes

# envelopes/ITALY_art.21_2015/it.xml
ds$namingshaming[ds$country=="IT" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="IT" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="IT" & ds$year==2015] <- 0  						# 11.1
ds$inspection.n[ds$country=="IT" & ds$year==2015] <- 0						# 6.5 
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2015] <- 1	# 11.4 gives 0 (0) is the ID (operator). Likely not actual information. However, a fine is listed explicitly for 2015, so someone must have been fined
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2015] <- 1	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2015] <- 20040			# 11.3 all no/yes
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2015] <- 12327070				# 11.3  all no/yes

# envelopes/Italy Article 21 questionnaire 2016/it_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IT" & ds$year==2016] <- 0							# 11.1       
ds$suspension[ds$country=="IT" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="IT" & ds$year==2016] <- 0  						# 11.1
ds$inspection.n[ds$country=="IT" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2016] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2016] <- 1	# 11.4 listed in 2017 document
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2016] <- 1644500			# 11.3 all yes/yes
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2016] <- 218645.5				# 11.3 all yes/yes

# envelopes/Italy Article 21 questionnaire 2017/it_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IT" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="IT" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="IT" & ds$year==2017] <- 0  						# 11.1
ds$inspection.n[ds$country=="IT" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2017] <- 2	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2017] <- 5	# 11.4 five are in proceedings. two further cases were neither executed nor are their ongoing proceedings.
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2017] <- 7579100			# 11.3 
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2017] <- 21063928.84	# 11.3 not executed

# envelopes/Italy Article 21 questionnaire 2018/it_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IT" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="IT" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="IT" & ds$year==2018] <- 0  						# 11.1
ds$inspection.n[ds$country=="IT" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2018] <- 2	# 11.4 all imposed fines are still ongoing
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2018] <- 5	# 11.4 
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2018] <- 990200			# 11.3 all no/yes
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2018] <- 65055	# 11.3 all yes/yes

# envelopes/Italy Article 21 questionnaire 2019/it_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="IT" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="IT" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="IT" & ds$year==2019] <- 0  						# 11.1
ds$inspection.n[ds$country=="IT" & ds$year==2019] <- 0						# 6.5 not sure if not listed 
ds$emissionsfines.n.installations.lower[ds$country=="IT" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="IT" & ds$year==2019] <- 4	# 11.4
ds$emissionsfines.q.installations[ds$country=="IT" & ds$year==2019] <- 1117600	# 11.3 10000 no/yes. rest yes/yes
ds$otherfines.q.installations[ds$country=="IT" & ds$year==2019] <- 123998	# 11.3 all yes/yes

# envelopes(2)/LI Reporting Art 21 2008/LI_Questionnaire_Article_21__Final_Part_1.doc/
ds$suspension[ds$country=="LI" & ds$year==2008] <- 1
ds$inspection[ds$country=="LI" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="LI" & ds$year==2008] <- 1
ds$appointment[ds$country=="LI" & ds$year==2008] <- 1
ds$supervision[ds$country=="LI" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2008] <- 0

# envelopes(1)/LI_Reporting_Art_21_2009/2009_LI_Questionnaire_Article_21__Final_Part_1.doc
ds$suspension[ds$country=="LI" & ds$year==2009] <- 1
ds$inspection[ds$country=="LI" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="LI" & ds$year==2009] <- 1
ds$appointment[ds$country=="LI" & ds$year==2009] <- 1
ds$supervision[ds$country=="LI" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2009] <- 0

# envelopes(1)/LI Reporting Art 21 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="LI" & ds$year==2013] <-1 							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="LI" & ds$year==2013] <- 1								# S04_NationalLawPermitUpdate - not sure as EHG Art 7 only discusses issuance
ds$inspection[ds$country=="LI" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="LI" & ds$year==2013] <-1 						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed

# envelopes(1)/LI Reporting Art 21 2014/li-eu-emt-envu5f9mg.xml
ds$namingshaming[ds$country=="LI" & ds$year==2014] <-1 							# 11.1
ds$suspension[ds$country=="LI" & ds$year==2014] <- 1								# 4.2 - EHG Art 7 was amended in 2021 and now discusses issuance only.
ds$inspection[ds$country=="LI" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="LI" & ds$year==2014] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2014] <- 0				# 11.3

# envelopes/2003/87/LI Reporting Art 21 2015/li.xml
ds$namingshaming[ds$country=="LI" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="LI" & ds$year==2015] <- 1								# 4.2 - EHG Art 7 was amended in 2021 and now discusses issuance only.
ds$inspection[ds$country=="LI" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="LI" & ds$year==2015] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2015] <- 0				# 11.3

# envelopes/2003/87/LI Reporting Art 21 2016/li_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LI" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="LI" & ds$year==2016] <- 1								# 4.2 - EHG Art 7 was amended in 2021 and now discusses issuance only.
ds$inspection[ds$country=="LI" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="LI" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2016] <- 0				# 11.3

# envelopes/2003/87/LI Reporting Art 21 2017/li_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LI" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="LI" & ds$year==2017] <- 1								# 4.2 - EHG Art 7 was amended in 2021 and now discusses issuance only.
ds$inspection[ds$country=="LI" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="LI" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2017] <- 0				# 11.3

# envelopes/2003/87/LI Reporting Art 21 2018/li_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LI" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="LI" & ds$year==2018] <- 1								# 4.2 - EHG Art 7 was amended in 2021 and now discusses issuance only.
ds$inspection[ds$country=="LI" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="LI" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2018] <- 0				# 11.3

# envelopes/2003/87/LI Reporting Art. 21 2019/li_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LI" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="LI" & ds$year==2019] <- 1								# 4.2 - EHG Art 7 was amended in 2021 and now discusses issuance only.
ds$inspection[ds$country=="LI" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="LI" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LI" & ds$year==2019] <- 0				# 11.3

# envelopes(1)/Application of 2003/87/EC report - Lithuania 2013/Article_21_questionnaire.xml
ds$namingshaming[ds$country=="LT" & ds$year==2013] <- 1							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="LT" & ds$year==2013] <- 1								# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="LT" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="LT" & ds$year==2013] <- 74						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed

# envelopes(1)/EC Art. 21 report - Lithuania 2014/lt-eu-emt-envu4czrq.xml
ds$namingshaming[ds$country=="LT" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="LT" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="LT" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="LT" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2014] <- 0				# 11.3

# envelopes/2003/87/EC Art. 21 report - Lithuania 2015/lt.xml
ds$namingshaming[ds$country=="LT" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="LT" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="LT" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="LT" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2015] <- 0				# 11.3

# envelopes/2003/87/EC Art. 21 report - Lithuania 2016/lt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LT" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="LT" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="LT" & ds$year==2016] <- 	1							# 11.1
ds$inspection.n[ds$country=="LT" & ds$year==2016] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2016] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2016] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2016] <-	0		# 11.3
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2016] <-72 				# 11.3

# envelopes/2003/87/EC Art. 21 report - Lithuania 2017/lt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LT" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="LT" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="LT" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="LT" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2017] <- 	0			# 11.3

# envelopes/2003/87/EC Art. 21 report - Lithuania 2018/lt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LT" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="LT" & ds$year==2018] <- 	1							# 4.2 
ds$inspection[ds$country=="LT" & ds$year==2018] <- 	1							# 11.1
ds$inspection.n[ds$country=="LT" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2018] <- 0 	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2018] <- 0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2018] <-0			# 11.3
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2018] <- 0				# 11.3

# envelopes/2003/87/EC Art. 21 report - Lithuania 2019/lt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LT" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="LT" & ds$year==2019] <- 	1							# 4.2 
ds$inspection[ds$country=="LT" & ds$year==2019] <- 	1							# 11.1
ds$inspection.n[ds$country=="LT" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LT" & ds$year==2019] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LT" & ds$year==2019] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="LT" & ds$year==2019] <-	0		# 11.3
ds$otherfines.q.installations[ds$country=="LT" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Report 2005 - EU ETS Article 21/Article_21_Questionnaire_June_2006.pdf
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2005] <- 0

# envelopes(2)/Report 2006 - EU ETS Article 21/Article_21_Questionnaire_June_2007.pdf
ds$suspension[ds$country=="LU" & ds$year==2006] <- 1
ds$inspection[ds$country=="LU" & ds$year==2006] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2006] <- 1
ds$appointment[ds$country=="LU" & ds$year==2006] <- 0
ds$supervision[ds$country=="LU" & ds$year==2006] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2006] <- 0

# envelopes(2)/Report 2007 - EU ETS Article 21/Article_21_Questionnaire_June_2008.pdf
ds$suspension[ds$country=="LU" & ds$year==2007] <- 1
ds$inspection[ds$country=="LU" & ds$year==2007] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2007] <- 1
ds$appointment[ds$country=="LU" & ds$year==2007] <- 1
ds$supervision[ds$country=="LU" & ds$year==2007] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2007] <- 0

# envelopes(1)/Report 2008 - EU ETS Article 21/Article_21_Questionnaire_June_2009_final.pdf
ds$suspension[ds$country=="LU" & ds$year==2008] <- 1
ds$inspection[ds$country=="LU" & ds$year==2008] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2008] <- 1
ds$appointment[ds$country=="LU" & ds$year==2008] <- 1
ds$supervision[ds$country=="LU" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2008] <- 0

# envelopes(1)/Report 2009 - EU ETS Article 21/Article_21_Questionnaire_June_2010.pdf
ds$suspension[ds$country=="LU" & ds$year==2009] <- 1
ds$inspection[ds$country=="LU" & ds$year==2009] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2009] <- 1
ds$appointment[ds$country=="LU" & ds$year==2009] <- 1
ds$supervision[ds$country=="LU" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2009] <- 0

# envelopes(1)/Report 2010 - EU ETS Article 21/Article_21_Questionnaire_June_2011.pdf
ds$suspension[ds$country=="LU" & ds$year==2010] <- 1
ds$inspection[ds$country=="LU" & ds$year==2010] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2010] <- 1
ds$appointment[ds$country=="LU" & ds$year==2010] <- 1
ds$supervision[ds$country=="LU" & ds$year==2010] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2010] <- 0

# envelopes(1)/Report 2011 - EU ETS Article 21/Article_21_Questionnaire_June_2012.pdf
ds$suspension[ds$country=="LU" & ds$year==2011] <- 1
ds$inspection[ds$country=="LU" & ds$year==2011] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2011] <- 1
ds$appointment[ds$country=="LU" & ds$year==2011] <- 1
ds$supervision[ds$country=="LU" & ds$year==2011] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2011] <- 0

# envelopes(1)/Report 2012 - EU ETS Article 21/Article_21_Questionnaire_June_2013.pdf
ds$suspension[ds$country=="LU" & ds$year==2012] <- 1
ds$inspection[ds$country=="LU" & ds$year==2012] <- 0
ds$namingshaming[ds$country=="LU" & ds$year==2012] <- 1
ds$appointment[ds$country=="LU" & ds$year==2012] <- 1
ds$supervision[ds$country=="LU" & ds$year==2012] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2012] <- 0

# envelopes(1)/Report 2013 - EU ETS  Article 21/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="LU" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="LU" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate 
ds$inspection[ds$country=="LU" & ds$year==2013] <- 0								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="LU" & ds$year==2013] <- 1						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2013] <-0 			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2013] <- 	0			# S11_InstallationsPenaltiesImposed

# envelopes(1)/Report 2014 - EU ETS Article 21/lu-eu-emt-envu56ba.xml
ds$namingshaming[ds$country=="LU" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="LU" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="LU" & ds$year==2014] <- 0								# 11.1
ds$inspection.n[ds$country=="LU" & ds$year==2014] <- 9						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2014] <-0			# 11.3
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2014] <- 0				# 11.3

# envelopes/Report 2015 - EU ETS Article 21/lu.xml
ds$namingshaming[ds$country=="LU" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="LU" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="LU" & ds$year==2015] <- 0								# 11.1
ds$inspection.n[ds$country=="LU" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2015] <- 0				# 11.3

# envelopes/Report 2016 - EU ETS Article 21/lu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LU" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="LU" & ds$year==2016] <- 	1							# 4.2 
ds$inspection[ds$country=="LU" & ds$year==2016] <- 	0							# 11.1
ds$inspection.n[ds$country=="LU" & ds$year==2016] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2016] <- 	0		# 11.3
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2016] <- 	0			# 11.3

# envelopes/Report 2017 - EU ETS Article 21/lu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LU" & ds$year==2017] <-0 							# 11.1
ds$suspension[ds$country=="LU" & ds$year==2017] <- 	1							# 4.2 
ds$inspection[ds$country=="LU" & ds$year==2017] <- 	0							# 11.1
ds$inspection.n[ds$country=="LU" & ds$year==2017] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2017] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2017] <- 0				# 11.3

# envelopes/Report 2018 - EU ETS Article 21/lu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LU" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="LU" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="LU" & ds$year==2018] <- 0								# 11.1
ds$inspection.n[ds$country=="LU" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2018] <-	0		# 11.3
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2018] <- 	0			# 11.3

# envelopes/Report 2019 - EU ETS Article 21/lu_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LU" & ds$year==2019] <- 	0						# 11.1
ds$suspension[ds$country=="LU" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="LU" & ds$year==2019] <- 	0							# 11.1
ds$inspection.n[ds$country=="LU" & ds$year==2019] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LU" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LU" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LU" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LU" & ds$year==2019] <- 	0			# 11.3

# envelopes(2)/Art. 21 Dutch report 2008/Art_21_Report_Annex_EN_WORD.doc
ds$suspension[ds$country=="LV" & ds$year==2005] <- 0
ds$inspection[ds$country=="LV" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2005] <- 1
ds$supervision[ds$country=="LV" & ds$year==2005] <- 0
ds$appointment[ds$country=="LV" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2005] <- NA	# "Penalties were imposed in the reporting period." but no details given

# envelopes(2)/Report on the implementation of the Directive 2003/87/EC_2007/Direct_2003-27-EC_LV_2007_final.doc
ds$suspension[ds$country=="LV" & ds$year==2006] <- 0
ds$inspection[ds$country=="LV" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2006] <- 1
ds$supervision[ds$country=="LV" & ds$year==2006] <- 0
ds$appointment[ds$country=="LV" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2006] <- 0

# envelopes(2)/Report on the implementation of the Directive 2003/87/EC_2008/Direct_2003-27-EC_LV_2008_final.doc
ds$suspension[ds$country=="LV" & ds$year==2007] <- 0
ds$inspection[ds$country=="LV" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2007] <- 1
ds$supervision[ds$country=="LV" & ds$year==2007] <- 0
ds$appointment[ds$country=="LV" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2007] <- 0

# envelopes(2)/Report on the implementation of the Directive 2003/87/EC_2009/Direct_2003-27-EC_LV_2009.doc
ds$suspension[ds$country=="LV" & ds$year==2008] <- 0
ds$inspection[ds$country=="LV" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2008] <- 1
ds$supervision[ds$country=="LV" & ds$year==2008] <- 0
ds$appointment[ds$country=="LV" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2008] <- 1				# Based on 2009 report.
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2008] <- 1				# Based on 2009 report.
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2008] <- "To be calculated"	# Based on 2009 report. SIA “Livberzes energija” did not surrender their year 2008 allowances in time till 30th April 2009.
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2008] <- 0

# envelopes(1)/Report on the implementation of the Directive 2003/87/EC_2010/Direct_2003-87-EC_LV_2010.pdf
ds$suspension[ds$country=="LV" & ds$year==2009] <- 0
ds$inspection[ds$country=="LV" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2009] <- 1
ds$supervision[ds$country=="LV" & ds$year==2009] <- 0
ds$appointment[ds$country=="LV" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2009] <- 0

# envelopes(1)/Report on the implementation of the Directive 2003/87/EC_2011/Direct_2003-87-EC_LV_2011.pdf
ds$suspension[ds$country=="LV" & ds$year==2010] <- 0
ds$inspection[ds$country=="LV" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2010] <- 1
ds$supervision[ds$country=="LV" & ds$year==2010] <- 0
ds$appointment[ds$country=="LV" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2010] <- 0

# envelopes(1)/Report on the implementation of the Directive 2003/87/EC_2012/Direct_2003-87-EC_LV_2011.pdf
ds$suspension[ds$country=="LV" & ds$year==2011] <- 1
ds$inspection[ds$country=="LV" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2011] <- 1
ds$supervision[ds$country=="LV" & ds$year==2011] <- 1
ds$appointment[ds$country=="LV" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2011] <- 0

# envelopes(1)/Report on the implementation of the Directive 2003/87/EC_2013/Direct_2003-87-EC_LV_2012.pdf
ds$suspension[ds$country=="LV" & ds$year==2012] <- 1
ds$inspection[ds$country=="LV" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="LV" & ds$year==2012] <- 1
ds$supervision[ds$country=="LV" & ds$year==2012] <- 1
ds$appointment[ds$country=="LV" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2012] <- 0

# envelopes(1)/Report on the implementation of the Directive 2003/87/EC 2014/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="LV" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="LV" & ds$year==2013] <- 	1							# S04_IEDProcedureRequirementsIntegratedNo Seems like its the case - "Nosacījumi ir noteikti LV Ministru kabineta noteikumu Nr.769 "Noteikumi par stacionāro tehnoloģisko iekārtu dalību Eiropas Savienības emisijas kvotu tirdzniecības sistēmā" 46.punktā (http://likumi.lv/doc.php?id=253119)"
ds$inspection[ds$country=="LV" & ds$year==2013] <- 	1							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="LV" & ds$year==2013] <- 30 						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2013] <-0 			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2013] <-0 				# S11_InstallationsPenaltiesImposed

# envelopes/Report on the implementation of the Directive 2003/87/EC 2015_resubmission/Article_21_questionnaire_2014.xml
ds$namingshaming[ds$country=="LV" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="LV" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="LV" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="LV" & ds$year==2014] <- 30 						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2014] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2014] <- 0				# 11.3

# envelopes/Report on the implementation of the Directive 2003/87/EC 2016_resubmission/lv.1.xml
ds$namingshaming[ds$country=="LV" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="LV" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="LV" & ds$year==2015] <- 	1							# 11.1
ds$inspection.n[ds$country=="LV" & ds$year==2015] <- 12						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2015] <- 0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2015] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2015] <- 0				# 11.3

# envelopes/Report on the implementation of the Directive 2003/87/EC 2017/lv_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LV" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="LV" & ds$year==2016] <- 		1						# 4.2 
ds$inspection[ds$country=="LV" & ds$year==2016] <- 	1							# 11.1
ds$inspection.n[ds$country=="LV" & ds$year==2016] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2016] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2016] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2016] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2016] <- 0				# 11.3

# envelopes/Report on the implementation of the Directive 2003/87/EC 2019_resubmission/lv_eu-ets-art21_report_2018.xml
ds$namingshaming[ds$country=="LV" & ds$year==2018] <-1 							# 11.1
ds$suspension[ds$country=="LV" & ds$year==2018] <- 	1							# 4.2 
ds$inspection[ds$country=="LV" & ds$year==2018] <- 	1							# 11.1
ds$inspection.n[ds$country=="LV" & ds$year==2018] <- 5						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2018] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2018] <- 0				# 11.3

# envelopes/Report on the implementation of the Directive 2003/87/EC 2019/lv_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="LV" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="LV" & ds$year==2019] <- 	1							# 4.2 
ds$inspection[ds$country=="LV" & ds$year==2019] <- 	1							# 11.1
ds$inspection.n[ds$country=="LV" & ds$year==2019] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="LV" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="LV" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="LV" & ds$year==2019] <- 0			# 11.3
ds$otherfines.q.installations[ds$country=="LV" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Directive 2003/87/EC Malta 2005/MT_Replies_-_Direct_2003_87_EC_Article_21_questionnaire_reporting_year_2005_final.doc
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2005] <- 0

# envelopes(2)/Directive 2003/87/EC Malta 2006/MT_Article_21_Reporting_ETS_2006_803_EC_0107_1207.doc
ds$suspension[ds$country=="MT" & ds$year==2007] <- 1
ds$inspection[ds$country=="MT" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="MT" & ds$year==2007] <- 1
ds$supervision[ds$country=="MT" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2007] <- 0

# envelopes(2)/Directive 2003/87/EC Malta 2008/MT_Article_21_Reporting_ETS_2006_803_EC_0108_1208.doc
ds$suspension[ds$country=="MT" & ds$year==2008] <- 1
ds$inspection[ds$country=="MT" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="MT" & ds$year==2008] <- 1
ds$supervision[ds$country=="MT" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2008] <- 0

# envelopes(1)/Directive 2003/87/EC Malta 2010/MT_Article_21_Reporting_ETS_2006_803_EC_questionnaire_0110_1210.pdf
ds$suspension[ds$country=="MT" & ds$year==2010] <- 1
ds$inspection[ds$country=="MT" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="MT" & ds$year==2010] <- 1
ds$supervision[ds$country=="MT" & ds$year==2010] <- 1
ds$appointment[ds$country=="MT" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2010] <- 0

# envelopes(1)/Directive 2003/87/EC Malta 2012/MT_Article_21_report_part_1.pdf
ds$suspension[ds$country=="MT" & ds$year==2012] <- NA
ds$inspection[ds$country=="MT" & ds$year==2012] <- NA
ds$namingshaming[ds$country=="MT" & ds$year==2012] <- NA
ds$supervision[ds$country=="MT" & ds$year==2012] <- 1
ds$appointment[ds$country=="MT" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2012] <- 0

# envelopes(1)/Directive 2003/87/EC Malta 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="MT" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="MT" & ds$year==2013] <- 	1							#S04_NationalLawPermitUpdate 
ds$inspection[ds$country=="MT" & ds$year==2013] <- 	0							# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="MT" & ds$year==2013] <-0 						#S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2013] <- 0 	#S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2013] <- 0	#S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2013] <- 0			#S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2013] <-0 				# S11_InstallationsPenaltiesImposed

#**Not fully sure whether the stated file is for 2014 or 2015
# envelopes(1)/Directive 2003/87/EC Malta 2014/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="MT" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="MT" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="MT" & ds$year==2014] <- 0								# 11.1
ds$inspection.n[ds$country=="MT" & ds$year==2014] <-0 						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2014] <-0 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2014] <-0 			#11.3
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2014] <-0				# 11.3 

# envelopes/Directive 2003/87/EC Malta 2015_rev/mt.xml
ds$namingshaming[ds$country=="MT" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="MT" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="MT" & ds$year==2015] <- 	0							# 11.1
ds$inspection.n[ds$country=="MT" & ds$year==2015] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2015] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2015] <- 0			#11.3
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2015] <- 0				# 11.3 

# envelopes/Directive 2003/87/EC_Art 21_Malta 2017 (submission 2018 revised)/mt_eu-ets-art21_report_2018_submission_revised.xml
ds$namingshaming[ds$country=="MT" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="MT" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="MT" & ds$year==2017] <- 	0							# 11.1
ds$inspection.n[ds$country=="MT" & ds$year==2017] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2017] <- 0			#11.3
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2017] <- 0				# 11.3 


# envelopes/Directive 2003/87/EC_Art 21_Malta 2018 (submission 2019)/mt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="MT" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="MT" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="MT" & ds$year==2018] <- 	0							# 11.1
ds$inspection.n[ds$country=="MT" & ds$year==2018] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2018] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2018] <- 0			#11.3
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2018] <- 0				# 11.3 

# envelopes/Directive 2003/87/EC_Art 21_Malta 2019_(submissions 2020)/mt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="MT" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="MT" & ds$year==2019] <- 1							# 4.2 
ds$inspection[ds$country=="MT" & ds$year==2019] <- 	0							# 11.1
ds$inspection.n[ds$country=="MT" & ds$year==2019] <- 0							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year==2019] <- 0			#11.3
ds$otherfines.q.installations[ds$country=="MT" & ds$year==2019] <- 0				# 11.3 

# envelopes(2)/article 21 dutch report (directive 2003/87/EC)/part_1_main_document_questionnaire_2006.doc
ds$suspension[ds$country=="NL" & ds$year==2005] <- 0
ds$inspection[ds$country=="NL" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="NL" & ds$year==2005] <- 1
ds$supervision[ds$country=="NL" & ds$year==2005] <- 1
ds$appointment[ds$country=="NL" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2005] <- 0

# envelopes(2)/art 21 Dutch report 2007/Art_21_Report_Annex_EN_WORD.doc
ds$namingshaming[ds$country=="NL" & ds$year==2006] <- 1
ds$suspension[ds$country=="NL" & ds$year==2006] <- 1
ds$inspection[ds$country=="NL" & ds$year==2006] <- 1
ds$supervision[ds$country=="NL" & ds$year==2006] <- 1
ds$appointment[ds$country=="NL" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2006] <- 0

# envelopes(2)/Art. 21 Dutch report 2008/Art_21_Report_Annex_EN_WORD.doc
ds$suspension[ds$country=="NL" & ds$year==2007] <- 0
ds$inspection[ds$country=="NL" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="NL" & ds$year==2007] <- 1
ds$supervision[ds$country=="NL" & ds$year==2007] <- 1
ds$appointment[ds$country=="NL" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2007] <- 0

# envelopes(1)/Art. 21 Dutch report 2009/Questionnaire_2009_final.doc
ds$suspension[ds$country=="NL" & ds$year==2008] <- 0
ds$inspection[ds$country=="NL" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="NL" & ds$year==2008] <- 1
ds$supervision[ds$country=="NL" & ds$year==2008] <- 1
ds$appointment[ds$country=="NL" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2008] <- 0

# envelopes(1)/Art 21 Dutch Report 2010/Questionnaire_2010.doc
ds$suspension[ds$country=="NL" & ds$year==2009] <- 0
ds$inspection[ds$country=="NL" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="NL" & ds$year==2009] <- 1
ds$supervision[ds$country=="NL" & ds$year==2009] <- 1
ds$appointment[ds$country=="NL" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2009] <- 0

# envelopes(1)/Art 21 Dutch Report 2011/Questionnaire_2011NL.doc
ds$suspension[ds$country=="NL" & ds$year==2010] <- 0
ds$inspection[ds$country=="NL" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="NL" & ds$year==2010] <- 1
ds$supervision[ds$country=="NL" & ds$year==2010] <- 1
ds$appointment[ds$country=="NL" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2010] <- 0

# envelopes(1)/Questionnaire art. 21 2013/Questionnaire_2013.docx
# ds$suspension[ds$country=="NL" & ds$year==2012] <- # No change reported, but 2011 report not publicly available.
# ds$inspection[ds$country=="NL" & ds$year==2012] <- # No change reported, but 2011 report not publicly available.
# ds$namingshaming[ds$country=="NL" & ds$year==2012] <- # No change reported, but 2011 report not publicly available.
ds$supervision[ds$country=="NL" & ds$year==2012] <- 1
ds$appointment[ds$country=="NL" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2012] <- 0

# envelopes(1)/ETS art. 21 Questionnaire 2014 NL/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="NL" & ds$year==2013] <- 1							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="NL" & ds$year==2013] <- 	1							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="NL" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="NL" & ds$year==2013] <- 101						#S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2013] <- 0	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed

# envelopes(1)/ETS art. 21 Questionnaire 2015 NL/nl-eu-emt-envu4m2vw.xml
ds$namingshaming[ds$country=="NL" & ds$year==2014] <-0 							# 11.1
ds$suspension[ds$country=="NL" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="NL" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="NL" & ds$year==2014] <- 65							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2014] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2014] <- 0				# 11.3 none of the fines had ongoing proceedings and all we not executed

# envelopes/ETS art. 21 Questionnaire 2016 NL/nl.xml
ds$namingshaming[ds$country=="NL" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="NL" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="NL" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="NL" & ds$year==2015] <- 70							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2015] <- 2	# 11.4
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2015] <- "To be calculated"			# 11.3 IDs - 206407 (Rasenberg Wegenbouw B.V.), 206832 (Bruil Infra B.V.)
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2015] <- 251910				# 11.3 no/yes

# envelopes/ETS art. 21 Questionnaire 2018/nl_eu-ets-art21_report.xml
#this report is listed differently online. I checked online vs xml and they appear to be the same for relevant cateogires
#also listed under - envelopes/RESUBMISSION article 21 2017 NL/nl_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="NL" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="NL" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="NL" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="NL" & ds$year==2017] <- 39							# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2017] <- 0				# 11.3 

# envelopes/ETS art. 21 Questionnaire 2019/nl_eu-ets-art21_report.xml
# envelopes/Resubmission article 21 report Netherlands 2019/nl_eu-ets-art21_report_2019.xml
ds$namingshaming[ds$country=="NL" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="NL" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="NL" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="NL" & ds$year==2018] <- 42						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2018] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2018] <- "To be calculated"	# 11.3 IDs - 22 (Neptune Energy Netherlands B.V (ENGIE E&P Nederland B.V., K12-B platform))
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2018] <- 417987				# 11.3 376490 yes/yes, rest no, yes

# envelopes/ETS art. 21 Questionnaire 2020/nl_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="NL" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="NL" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="NL" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="NL" & ds$year==2019] <- 32						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NL" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NL" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NL" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NL" & ds$year==2019] <- 345780				# 11.3 all no/yes

# envelopes(1)/2009 submission/Norway_Article_21_questionnaire_2008.doc
ds$suspension[ds$country=="NO" & ds$year==2008] <- 1
ds$inspection[ds$country=="NO" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="NO" & ds$year==2008] <- 1
ds$supervision[ds$country=="NO" & ds$year==2008] <- NA	# Norway does not have accredited verifiers at this point. All verification is done by the government
ds$appointment[ds$country=="NO" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2008] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2008] <- 1
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2008] <- 9100
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2008] <- 0

# envelopes(1)/2010 submission/Norway_Article_21_questionnaire_2009.doc
ds$suspension[ds$country=="NO" & ds$year==2009] <- 1
ds$inspection[ds$country=="NO" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="NO" & ds$year==2009] <- 1
ds$supervision[ds$country=="NO" & ds$year==2009] <- NA	# Norway does not have accredited verifiers at this point. All verification is done by the government
ds$appointment[ds$country=="NO" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2009] <- 0

# envelopes(1)/2011 submission/Norway_Article_21_questionnaire_2010.doc
ds$suspension[ds$country=="NO" & ds$year==2010] <- 1
ds$inspection[ds$country=="NO" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="NO" & ds$year==2010] <- 1
ds$supervision[ds$country=="NO" & ds$year==2010] <- NA	# Norway does not have accredited verifiers at this point. All verification is done by the government
ds$appointment[ds$country=="NO" & ds$year==2010] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2010] <- 0

# envelopes(1)/2012 submission/Norway_Article_21_questionnaire_2011.doc
ds$suspension[ds$country=="NO" & ds$year==2011] <- 1
ds$inspection[ds$country=="NO" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="NO" & ds$year==2011] <- 1
ds$supervision[ds$country=="NO" & ds$year==2011] <- NA	# Norway does not have accredited verifiers at this point. All verification is done by the government
ds$appointment[ds$country=="NO" & ds$year==2011] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2011] <- 0

# envelopes(1)/2013 submission/Norway_Article_21_questionnaire_2012.doc
ds$suspension[ds$country=="NO" & ds$year==2012] <- 1
ds$inspection[ds$country=="NO" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="NO" & ds$year==2012] <- 1
ds$supervision[ds$country=="NO" & ds$year==2012] <- NA	# Norway does not have accredited verifiers at this point. All verification is done by the government
ds$appointment[ds$country=="NO" & ds$year==2012] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2012] <- 0

# envelopes(1)/Article 21 report - Norway - updated submission 2014/no-eu-colp0r8w-colsjs89w-envvabh7q.xml
ds$namingshaming[ds$country=="NO" & ds$year==2013] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2013] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2013] <- 1 								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2013] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2013] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2013] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2013] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2013] <- 0				# 11.3

# envelopes(1)/Article 21 report - Norway - submission 2015/no-eu-colp0r8w-colsjs89w-envvabh7q.xml
ds$namingshaming[ds$country=="NO" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2014] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2014] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2014] <- 0				# 11.3

# envelopes/Article 21 report - Norway - submission 2016/no.xml
ds$namingshaming[ds$country=="NO" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2015] <- 11						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2015] <- 0				# 11.3

# envelopes/Article 21 report - Norway - submission 2017/no.xml 
ds$namingshaming[ds$country=="NO" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2016] <- 9						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 report - Norway - submission 2018/no_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="NO" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2017] <- 9						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 report - Norway - submission 2019/no_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="NO" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2018] <- 9						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21 report - Norway - submission 2020/article21-questionnaire_1 .xml
ds$namingshaming[ds$country=="NO" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="NO" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="NO" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="NO" & ds$year==2019] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="NO" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="NO" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="NO" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="NO" & ds$year==2019] <- 0				# 11.3

# envelopes(2)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2007/Report_art._21_2007-English.doc
ds$suspension[ds$country=="PL" & ds$year==2007] <- 0
ds$inspection[ds$country=="PL" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="PL" & ds$year==2007] <- 1
ds$supervision[ds$country=="PL" & ds$year==2007] <- 1
ds$appointment[ds$country=="PL" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2007] <- 0

# envelopes(2)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2008/Report_art._21_2008-English.doc
ds$suspension[ds$country=="PL" & ds$year==2008] <- 0
ds$inspection[ds$country=="PL" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="PL" & ds$year==2008] <- 1
ds$supervision[ds$country=="PL" & ds$year==2008] <- 1
ds$appointment[ds$country=="PL" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2008] <- 0

# envelopes(1)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2009/Report_art._21_2009-English.doc
ds$suspension[ds$country=="PL" & ds$year==2009] <- 0
ds$inspection[ds$country=="PL" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="PL" & ds$year==2009] <- 1
ds$supervision[ds$country=="PL" & ds$year==2009] <- 1
ds$appointment[ds$country=="PL" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2009] <- 0

# envelopes(1)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2010/Report_art._21_2010-English.docx
ds$suspension[ds$country=="PL" & ds$year==2010] <- 0
ds$inspection[ds$country=="PL" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="PL" & ds$year==2010] <- 1
ds$supervision[ds$country=="PL" & ds$year==2010] <- 1
ds$appointment[ds$country=="PL" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2010] <- 0

# envelopes(1)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2011/Report_art._21_2011-English.docx
ds$suspension[ds$country=="PL" & ds$year==2011] <- 0
ds$inspection[ds$country=="PL" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="PL" & ds$year==2011] <- 1
ds$supervision[ds$country=="PL" & ds$year==2011] <- 1
ds$appointment[ds$country=="PL" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2011] <- 0

# envelopes(1)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2012/11-06-Raport_art_21_2012-ENG_final.doc
ds$suspension[ds$country=="PL" & ds$year==2012] <- 0
ds$inspection[ds$country=="PL" & ds$year==2012] <- 1
ds$namingshaming[ds$country=="PL" & ds$year==2012] <- 1
ds$supervision[ds$country=="PL" & ds$year==2012] <- 1
ds$appointment[ds$country=="PL" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2012] <- 0
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2012] <- 0
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2012] <- 0

# envelopes(1)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="PL" & ds$year==2013] <- 0							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="PL" & ds$year==2013] <- 0								# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="PL" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="PL" & ds$year==2013] <- 0						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2013] <- 0	# S11_OperatorPenalties -has not yet been executed
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2013] <-4 	# S11_OperatorPenalties 
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2013] <- 	2388100.24		# S11_InstallationsPenaltiesImposed   yes/no both columns are labelled the same in this version but I presume it has a similar meaning as the later version
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2013] <- 	10000			# S11_InstallationsPenaltiesImposed  yes/no

# envelopes(1)/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2014/pl-eu-emt-envu6p9ya.xml
ds$namingshaming[ds$country=="PL" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="PL" & ds$year==2014] <- 0								# 4.2 
ds$inspection[ds$country=="PL" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="PL" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2014] <- 0	# 11.4 - have not been executed
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2014] <-2 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2014] <- 	1679800		# 11.3   only yes/no
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2014] <- 	110000			# 11.3 10000 is yes/yes. the rest is yes/no

# envelopes/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2015/pl.xml
ds$namingshaming[ds$country=="PL" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="PL" & ds$year==2015] <- 0								# 4.2 
ds$inspection[ds$country=="PL" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="PL" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2015] <- 1	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2015] <-1 	# 11.4
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2015] <- 	14400		# 11.3 email - yes/yes
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2015] <- 	8000			# 11.3 1000 is yes/yes, the rest is yes/no indicating there ongoing proceedings and no penalty executed

# envelopes/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2016/pl.xml
ds$namingshaming[ds$country=="PL" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="PL" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="PL" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="PL" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2016] <- 0		# 11.3
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2016] <- 0				# 11.3  

# envelopes/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2017/pl_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="PL" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="PL" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="PL" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="PL" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2017] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2017] <- "To be calculated"			# 11.3 IDs - 634 (Ceramika Polska Sp. z o.o.)
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2017] <- 15549.51				# 11.3    all is no/yes indicating there are ongoing proceedings but no penalties executed

# envelopes/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2018/pl_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="PL" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="PL" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="PL" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="PL" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2018] <- 2	# 11.4
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2018] <- "To be calculated"			# 11.3 IDs - 130 ("LUBREM" sp.j. K. Dębski; J. Klepacki), 263(Fabryka Papieru sp. z o.o.)
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2018] <- 0				# 11.3 only listed fine is no/no

# envelopes/Report for the European Commission on the implementation of Directive 2003/87/EC of the European Parliament and of the Council establishing a scheme for greenhouse gas emission allowance trading - report for 2019/pl_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="PL" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="PL" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="PL" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="PL" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PL" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PL" & ds$year==2019] <- 2	# 11.4
ds$emissionsfines.q.installations[ds$country=="PL" & ds$year==2019] <-4915081.67 			# 11.3 -859526.48 no/no, rest yes/no indicating that there is ongoing proceedings/no penalty executed
ds$otherfines.q.installations[ds$country=="PL" & ds$year==2019] <- 21952.94			# 11.3 - 1/3 no/no. all remaining is yes/no

# envelopes(2)/Questionnaire on the implementation of Directive 2003/87/EC/PT_Art_21__questionnaire.doc
ds$namingshaming[ds$country=="PT" & ds$year==2005] <- 1
ds$supervision[ds$country=="PT" & ds$year==2005] <- 1
ds$appointment[ds$country=="PT" & ds$year==2005] <- 1
ds$supervision[ds$country=="PT" & ds$year==2005] <- 1
ds$appointment[ds$country=="PT" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2005] <- 3
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2005] <- 3
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2005] <- "To be calculated" # TEGEE	Nome do operador	Instalação	Localização
																		# 211	COMPAL - Companhia Produtora de Conservas Alimentares	Central Térmica	Almeirim
																		# 227	Cosbar - Cerâmica do Barlavento, S.A.	Cosbar	Algoz
																		# 230	Grésil	Grésil	Mourisca do Vouga
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2005] <- 3500+44890 # A range is given from 3500 to 44890 euro, but no indication of the number or distribution. The sum of the upper and lower end of the range is used as a lower bound.

# envelopes(2)/Article 21 Report - Final version/Art_21_Report_Annex_EN_20070706_FV.docc
ds$namingshaming[ds$country=="PT" & ds$year==2006] <- 1
ds$supervision[ds$country=="PT" & ds$year==2006] <- 1
ds$appointment[ds$country=="PT" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2006] <- 5
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2006] <- 5
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2006] <- "To be calculated"	# Penalties for ID permit 247, 166, 244, 62, 61.
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2006] <- 0

# envelopes(1)/PT_Art21_2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="PT" & ds$year==2013] <- 0							#S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="PT" & ds$year==2013] <- 	0							# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="PT" & ds$year==2013] <- 0								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="PT" & ds$year==2013] <- NA						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2013] <- 0			# S11_InstallationsPenaltiesImposed
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2013] <- 0				#S11_InstallationsPenaltiesImposed

# envelopes(1)/PT_Art21_2014/pt-eu-emt-envu42kpq.xml
ds$namingshaming[ds$country=="PT" & ds$year==2014] <- 0							# 11.1
ds$suspension[ds$country=="PT" & ds$year==2014] <- 	0							# 4.2 
ds$inspection[ds$country=="PT" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="PT" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2014] <-0 	# 11.4 #it doesnt say name yet-it says process is ongoing 
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2014] <-1 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2014] <- "To be calculated"			# 11.3 - does not say, process is ongoing
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2014] <- 99999			# 11.3 ongoing proceedings/no penalty -  yes/no

# envelopes/Article 21 - 2015/pt.xml 
ds$namingshaming[ds$country=="PT" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="PT" & ds$year==2015] <- 0								# 4.2 
ds$inspection[ds$country=="PT" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="PT" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2015] <- 2	# 11.4
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2015] <- "To be calculated"			# 11.3  232(Malhas Eical - Empresa Industrial do Cávado), Lda, 146(Tinturaria e Acabamentos de Tecidos Vale de Tábuas, Lda)
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2015] <- 0				# 11.3

# envelopes/pt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="PT" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="PT" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="PT" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="PT" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2016] <- 0			# 11.3  
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2016] <- 0				# 11.3

# envelopes/Portugal _Art.21_2017/pt_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="PT" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="PT" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="PT" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="PT" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2017] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2017] <- "To be calculated"			# 11.3 IDs - PTF(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.)
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2017] <- 2432912				# 11.3 ongoing proceedings, no penalty- yes/no

# envelopes/Art_21_2018/pt_eu-ets-art21_report.xml 
ds$namingshaming[ds$country=="PT" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="PT" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="PT" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="PT" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2018] <- 15	# 11.4 5 operators but 15 installations
#IDs - PTF-17(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-16(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-15(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-14(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-13(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTG-16(Natural - Industria de Papel, S.A.), PTG-15(Natural - Industria de Papel, S.A.), PTH-16(Lusotelha, Telhas e Tijolos de Águeda, Lda.), PTH-15(Lusotelha, Telhas e Tijolos de Águeda, Lda.), PTI-15(Fábrica de Papel de Medros, Lda.), PTI-14(Fábrica de Papel de Medros, Lda.), PTI-13(Fábrica de Papel de Medros, Lda.), PTJ-15(SUTOL - Indústrias Alimentares, Lda), PTK-14(Arco Têxteis, S.A.), PTL(Prélis Cerâmica, Lda.)
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2018] <- "To be calculated"			# 11.3 
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2018] <- 2432912				# 11.3 ongoing proceedings, no penalty -  yes/no

# envelopes/Article 21 - 2019_PT/pt_eu-ets-art21_reportl
ds$namingshaming[ds$country=="PT" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="PT" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="PT" & ds$year==2019] <- 1								# 11.1 
ds$inspection.n[ds$country=="PT" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="PT" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="PT" & ds$year==2019] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="PT" & ds$year==2019] <- 'To be calculated'	  # 11.3 IDs - PTF-18(MARGON - Materiais e Revestimentos Modernos para Edificações, S.A.)
ds$otherfines.q.installations[ds$country=="PT" & ds$year==2019] <- 0				# 11.3 

# envelopes(1)/Art. 21 Report Part I_RO 2009/Art21_Report_Part_I_RO_raportare_2009.doc
ds$namingshaming[ds$country=="RO" & ds$year==2009] <- 1
ds$suspension[ds$country=="RO" & ds$year==2009] <- 1
ds$inspection[ds$country=="RO" & ds$year==2009] <- 1
# ds$supervision[ds$country=="RO" & ds$year==2009] <- # No change reported
# ds$appointment[ds$country=="RO" & ds$year==2009] <- # No change reported
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2009] <- 12				# List available via homepage. n10_RTL_201068435690.pdf
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2009] <- 12				# List available via homepage. n10_RTL_201068435690.pdf
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2009] <- "To be calculated"	# List available via homepage. n10_RTL_201068435690.pdf
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2009] <- 0	# Fines listed as rules, not as actually imposed.

# envelopes(1)/Art. 21 Report RO_reporting year 2010/Art21_Report_Part_I_RO_reporting_year2010.doc
ds$namingshaming[ds$country=="RO" & ds$year==2010] <- 1
ds$suspension[ds$country=="RO" & ds$year==2010] <- 1
ds$inspection[ds$country=="RO" & ds$year==2010] <- 1
ds$supervision[ds$country=="RO" & ds$year==2010] <- 1
ds$appointment[ds$country=="RO" & ds$year==2010] <- 1
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2010] <- 0				# List available via homepage. None for 2010
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2010] <- 0				# List available via homepage. None for 2010
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2010] <- 0					# List available via homepage. None for 2010
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2010] <- 0	# Fines listed as rules, not as actually imposed.

# envelopes(1)/Art 21 Reporting year 2011/Art.21_Report_Part_I_RO_Reporting_year_2011.doc.docx
ds$namingshaming[ds$country=="RO" & ds$year==2011] <- 1
ds$suspension[ds$country=="RO" & ds$year==2011] <- 1
ds$inspection[ds$country=="RO" & ds$year==2011] <- 1
ds$supervision[ds$country=="RO" & ds$year==2011] <- 1
ds$appointment[ds$country=="RO" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2011] <- 0
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2011] <- 0
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2011] <- 12000*3	# There were three operators for which the fine was applied for the submission of veri-fied emissions reports after 30 of March. The fine for this is given to be in the range of euro 4800 to euro 12000. We impute the upper bound, to be generous to the regulator.

# envelopes(1)/art. 21 reporting year 2012/RAPORT_EU_ETS_2012.doc
ds$namingshaming[ds$country=="RO" & ds$year==2012] <- 1
ds$suspension[ds$country=="RO" & ds$year==2012] <- 1
ds$inspection[ds$country=="RO" & ds$year==2012] <- 1
ds$supervision[ds$country=="RO" & ds$year==2012] <- 1
ds$appointment[ds$country=="RO" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2012] <- 3
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2012] <- 3
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2012] <- "To be calculated"
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2012] <- 0

# envelopes(1)/Article 21 report - Romania 2013 - corrected version II/ro-eu-emt-envu4laqw.xml
ds$namingshaming[ds$country=="RO" & ds$year==2013] <-0 							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2013] <-1 								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2013] <-1 								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2013] <-0 						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2013] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2013] <-1 	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2013] <-"To be calculated" 			# 11.3 IDs - RO 32 (S.C. AEROSTAR S.A.)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2013] <- 36363				# 11.3 no/yes

# envelopes(1)/Article 21 report - Romania 2014/ro-eu-emt-envu4laqw.xml
ds$namingshaming[ds$country=="RO" & ds$year==2014] <-0 							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2014] <- 1								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2014] <-4 						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2014] <- 1	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2014] <- "To be calculated"			# 11.3 IDs - RO 28(Regia Autonomă pentru Activități Nucleare - RAAN, Sucursala ROMAG-TERMO)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2014] <- 54000				# 11.3 no/yes
  
# envelopes/Article 21 report - Romania 2015/ro.xml
ds$namingshaming[ds$country=="RO" & ds$year==2015] <- 0							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2015] <- 2						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2015] <- 3	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2015] <- "To be calculated"			# 11.3 Two of them charged but one not. TBC - RO 28 (Regia Autonoma pentru Activitati Nucleare – RAAN, Sucursala Romag Termo), RO 110 (Societatea Complexul Energetic Hunedoara S.A. - Electrocentrale Deva), RO 209(Societatea Complexul Energetic Hunedoara S.A. - Electrocentrale Paroșeni)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2015] <- 15600				# 11.3  no/yes

# envelopes/Article 21 report - Romania 2016/ro_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="RO" & ds$year==2016] <- 0							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2016] <- 0	# 11.4 one fine listed for deadline but other undetermined  
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2016] <- 6	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2016] <- "To be calculated"			# 11.3 IDs - RO 28(Regia Autonomă pentru Activități Nucleare - RAAN, Sucursala Romag Termo), RO 39 (S.C. Chemgas Holding Corporation S.R.L.), RO 110 (Complexul Energetic Hunedoara - SE Deva), RO 209 (Complexul Energetic Hunedoara - SE Paroșeni), RO 96 (S.C. Donau Chem S.R.L.), RO 112 (S.C. Electrocentrale Oradea S.A.)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2016] <- 8800				# 11.3 no/yes

# envelopes/Article 21 report - Romania 2017/ro_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="RO" & ds$year==2017] <- 0							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2017] <- 1						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2017] <- 0	# 11.4 one fine of 0 listed for deadline but other undetermined  
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2017] <- 6	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2017] <- "To be calculated"			# 11.3 TBC - RO 39 (S.C. Chemgas Holding Corporation S.R.L.), RO 110(Complexul Energetic Hunedoara S.A. - SE Deva), RO 209 (Complexul Energetic Hunedoara S.A. - SE Paroșeni), RO 112 (S.C. Electrocentrale Oradea S.A.), RO 28 (Regia Autonomă pentru Activități Nucleare - RAAN, Sucursala Romag Termo), RO 272 (S.C. Energy Cogeneration Group S.A.)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2017] <- 0				# 11.3 

# envelopes/Article 21 report - Romania 2018/ro_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="RO" & ds$year==2018] <- 0							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2018] <- 4						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2018] <- 0	# 11.4 one fine of 0 listed for deadline but other undetermined  
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2018] <- 4	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2018] <- "To be calculated"			# 11.3 IDs -RO 39 (S.C. Chemgas Holding Corporation S.R.L.), RO 110 (Complexul Energetic Hunedoara S.A. - SE Deva), RO 209 (Complexul Energetic Hunedoara S.A. - SE Paroșeni), RO 49 (S.C. Automobile Dacia S.A.)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2018] <- 9000				# 11.3 - no/yes

# envelopes/Article 21 report - Romania 2019/ro_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="RO" & ds$year==2019] <- 0							# 11.1
ds$suspension[ds$country=="RO" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="RO" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="RO" & ds$year==2019] <- 5						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2019] <- 7	# 11.4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2019] <- "To be calculated"			# 11.3 IDs - RO 110 (Complexul Energetic Hunedoara S.A. - SE Deva), RO 39 (S.C. Chemgas Holding Corporation S.R.L.), RO 209 (Complexul Energetic Hunedoara S.A. - SE Paroșeni), RO 237 (S.C. Viromet S.A.), RO 231 (Global Energy Production S.A.), RO 120 (S.C. Enet S.A.), RO 209602 (S.C. Gas Energy Ecotherm S.A.)
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2019] <- 6211				# 11.3 - no/yes

# envelopes(2)/Emission Trading Directive 2003/87/EC Article 21_delivery 2006/Swedish_article_21_report_2006.doc
ds$namingshaming[ds$country=="SE" & ds$year==2005] <- 1
ds$suspension[ds$country=="SE" & ds$year==2005] <- 1
ds$inspection[ds$country=="SE" & ds$year==2005] <- 1
ds$supervision[ds$country=="SE" & ds$year==2005] <- 1
ds$appointment[ds$country=="SE" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2005] <- 0

# envelopes(2)/Emission Trading Directive 2003/87/EC Article 21_delivery 2007/Art21questionnaireSWE2007.doc
ds$namingshaming[ds$country=="SE" & ds$year==2006] <- 1
ds$suspension[ds$country=="SE" & ds$year==2006] <- 1
ds$inspection[ds$country=="SE" & ds$year==2006] <- 1
ds$supervision[ds$country=="SE" & ds$year==2006] <- 1
ds$appointment[ds$country=="SE" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2006] <- 0

# envelopes(2)/Emission Trading Directive 2003/87/EC Article 21_delivery 2008/Art21_SWE_080624.doc
ds$namingshaming[ds$country=="SE" & ds$year==2007] <- 1
ds$suspension[ds$country=="SE" & ds$year==2007] <- 1
ds$inspection[ds$country=="SE" & ds$year==2007] <- 1
ds$supervision[ds$country=="SE" & ds$year==2007] <- 1
ds$appointment[ds$country=="SE" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2007] <- 29 # Report says list of operators is available at a link, but link is now broken. Numbers based on on annual report from the same agency (archived in data folder).
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2007] <- 29
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2007] <- 4764852	# Numbers based on agency annual report (archived in data folder). Reportedly levying SEK 44,119,000 in fines from noncompliant installations, using a historical exchange rate of 0.108 EUR to 1 SEK for the year 2007.
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2007] <- 2200

# envelopes(2)/Emission Trading Directive 2003/87/EC Article 21_delivery 2009/SE_art_21_report_2009.doc
ds$suspension[ds$country=="SE" & ds$year==2008] <- 1
ds$inspection[ds$country=="SE" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="SE" & ds$year==2008] <- 1
ds$supervision[ds$country=="SE" & ds$year==2008] <- 1
ds$appointment[ds$country=="SE" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2008] <-  2 # Report says list of operators is available at a link, but link is now broken. Numbers based on on annual report from the same agency (archived in data folder).
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2008] <- 2
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2008] <- 8085	# Number is based on agency annual report (archived in data folder). Reportedly levying SEK 77,000 in fines from noncompliant installations, using a historical exchange rate of 0.105 EUR to 1 SEK for 2008.
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2008] <- 1950+1950 

# envelopes(1)/Emission Trading Directive 2003/87/EC Article 21_delivery 2010/Art_21_SE_2010.doc
ds$suspension[ds$country=="SE" & ds$year==2009] <- 1
ds$inspection[ds$country=="SE" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="SE" & ds$year==2009] <- 1
ds$supervision[ds$country=="SE" & ds$year==2009] <- 1
ds$appointment[ds$country=="SE" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2009] <-  1950 +1950 

# envelopes(1)/Emission Trading Directive 2003/87/EC Article 21_delivery 2011/art_21_rapport_arbetsfil_slutgiltig.doc
ds$suspension[ds$country=="SE" & ds$year==2010] <- 1
ds$inspection[ds$country=="SE" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="SE" & ds$year==2010] <- 1
ds$supervision[ds$country=="SE" & ds$year==2010] <- 1
ds$appointment[ds$country=="SE" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2010] <- 0	
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2010] <- "Maximum" # It is too early to give a answer to this question because these issues have not been tried in court.
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2010] <- 0	# Number is based on agency annual report (archived in data folder).
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2010] <- 1950+1950 

# envelopes(1)/Emission Trading Directive 2003/87/EC Article 21_delivery 2012/art_21_rapport_slutgiltig_2011.doc
ds$suspension[ds$country=="SE" & ds$year==2011] <- 1
ds$inspection[ds$country=="SE" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="SE" & ds$year==2011] <- 1
ds$supervision[ds$country=="SE" & ds$year==2011] <- 1
ds$appointment[ds$country=="SE" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2011] <-  5 # "It is too early to give a answer to this question because these issues have not been tried in court." This number is based on on annual report from the agency.
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2011] <- 195470 # Number is based on on annual report from the agency. Reportedly collecting SEK 1,777,000 in fines from noncompliant installations, using a historical exchange rate of 0.11 EUR to 1 SEK.
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2011] <- 1950+1950

# envelopes(1)/Emission Trading Directive 2003/87/EC Article 21_delivery 2013/art_21_rapport_2012_final_clean.doc
ds$suspension[ds$country=="SE" & ds$year==2012] <- 1
ds$inspection[ds$country=="SE" & ds$year==2012] <- 0
ds$namingshaming[ds$country=="SE" & ds$year==2012] <- 1
ds$supervision[ds$country=="SE" & ds$year==2012] <- 1
ds$appointment[ds$country=="SE" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2012] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2012] <- 1	# "It is too early to give a answer to this question because these issues have not been tried in court." This number is based on on annual report from the agency.
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2012] <- 9200	# Number is based on on annual report from the agency. Reportedly collecting SEK 80,000 in fines from noncompliant installations, using a historical exchange rate of 0.115 EUR to 1 SEK.
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2012] <- 1950+1950

# envelopes/Emission Trading Directive 2003/87/EC Article 21_SE delivery 2014 rev 1/se-eu-emt-envu5vqcg.xml
ds$namingshaming[ds$country=="SE" & ds$year==2013] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2013] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2013] <- 		0						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2013] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2013] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2013] <- 	0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2013] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2013] <- 2200				# 11.3   yes/yes

# envelopes/Emission Trading Directive 2003/87/EC Article 21_SE delivery 2015 rev 1/se-eu-emt-envu5vqcg.xml
ds$namingshaming[ds$country=="SE" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2014] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2014] <- 		0						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2014] <- 	0					# 6.5 #not sure if not listed or zero
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2014] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2014] <- 	0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2014] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2014] <- 	0			# 11.3 

# envelopes/Emission Trading Directive 2003/87/EC Article 21_SE delivery 2016/se.xml
ds$namingshaming[ds$country=="SE" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2015] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2015] <- 		0						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2015] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2015] <- 	0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2015] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2015] <- 0				# 11.3 

# envelopes/Emission Trading Directive 2003/87/EC Article 21 SE Delivery 2017/se_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SE" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2016] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2016] <- 		0						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2016] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2016] <- 	0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2016] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2016] <- 0				# 11.3 

# envelopes/Emission Trading Directive 2003/87/EC Article 21 SE Delivery 2018/se_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SE" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2017] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2017] <- 		0						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2017] <- 	0					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2017] <- 0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2017] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2017] <- 0				# 11.3 

# envelopes/Emission Trading Directive 2003/87/EC Article 21 SE Delivery 2019/se_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SE" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2018] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2018] <- 		1						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2018] <- 	3					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2018] <- 0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2018] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2018] <- 0				# 11.3 

# envelopes/Emission Trading Directive 2003/87/EG Article 21 SE Delivery 2020 rev 1/se_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SE" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="SE" & ds$year==2019] <- 		1						# 4.2 
ds$inspection[ds$country=="SE" & ds$year==2019] <- 		1						# 11.1
ds$inspection.n[ds$country=="SE" & ds$year==2019] <- 	7					# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2019] <-	0# 11.4 
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2019] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2019] <- 0				# 11.3 

# envelopes(2)/SI ETS Report 2005/Report_Porocilo_2005.doc
# envelopes(2)/SI ETS Report 2006/SI_Report_Emission_trading_scheme_Art_21_questionnaire_240706.doc
ds$suspension[ds$country=="SI" & ds$year==2005] <- 1
ds$inspection[ds$country=="SI" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="SI" & ds$year==2005] <- 1
ds$supervision[ds$country=="SI" & ds$year==2005] <- 1
ds$appointment[ds$country=="SI" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2005] <- 0

# envelopes(2)/SI ETS Report 2007/Art_21_Report_Annex_EN_WORD2006_2007_Slovenia.doc
ds$suspension[ds$country=="SI" & ds$year==2006] <- 1
ds$inspection[ds$country=="SI" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="SI" & ds$year==2006] <- 1
ds$supervision[ds$country=="SI" & ds$year==2006] <- 1
ds$appointment[ds$country=="SI" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2006] <- 0

# envelopes(1)/SI ETS Report 2008/Art_21_Report_Annex_EN__2007_Slovenia_2007.doc
ds$suspension[ds$country=="SI" & ds$year==2007] <- 1
ds$inspection[ds$country=="SI" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="SI" & ds$year==2007] <- 1
ds$supervision[ds$country=="SI" & ds$year==2007] <- 1
ds$appointment[ds$country=="SI" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2007] <- 0

# envelopes(1)/SI ETS Report 2009/2009_Report_Art_21_Report_Annex_EN__2008_data__Slovenia.doc
ds$suspension[ds$country=="SI" & ds$year==2008] <- 1
ds$inspection[ds$country=="SI" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="SI" & ds$year==2008] <- 1
ds$supervision[ds$country=="SI" & ds$year==2008] <- 1
ds$appointment[ds$country=="SI" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2008] <- 0

# envelopes(1)/SI ETS Report 2010/Official_letter_to_the_Commission_2010 and 2010 REPORT.pdf
ds$suspension[ds$country=="SI" & ds$year==2009] <- 1
ds$inspection[ds$country=="SI" & ds$year==2009] <- 1
ds$namingshaming[ds$country=="SI" & ds$year==2009] <- 1
ds$supervision[ds$country=="SI" & ds$year==2009] <- 1
ds$appointment[ds$country=="SI" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2009] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2009] <- 0
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2009] <- 0
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2009] <- 0

# envelopes(1)/SI ETS Report 2011, Art. 21/Art_21_Report_Annex_EN__2011_Slovenia_.doc
ds$suspension[ds$country=="SI" & ds$year==2010] <- 1
ds$inspection[ds$country=="SI" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="SI" & ds$year==2010] <- 1
ds$supervision[ds$country=="SI" & ds$year==2010] <- 1
ds$appointment[ds$country=="SI" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2010] <- 0

# envelopes(1)/EU ETS Article 21 reporting SI 2013/Article_21_questionnaire__1.xml
ds$namingshaming[ds$country=="SI" & ds$year==2013] <- 1							# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="SI" & ds$year==2013] <- 1								# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="SI" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="SI" & ds$year==2013] <- 5						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2013] <-0 	# S11_OperatorPenalties
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2013] <-0 	# S11_OperatorPenalties 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2013] <- 0			#S11_InstallationsPenaltiesImposed 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2013] <- 0				#S11_InstallationsPenaltiesImposed

# envelopes(1)/EU ETS Article 21 reporting SI 2014/si-eu-emt-envu60klg.xml
ds$namingshaming[ds$country=="SI" & ds$year==2014] <- 1							# 11.1
ds$suspension[ds$country=="SI" & ds$year==2014] <- 	1							# 4.2 
ds$inspection[ds$country=="SI" & ds$year==2014] <- 	1							# 11.1
ds$inspection.n[ds$country=="SI" & ds$year==2014] <- 5						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2014] <-0 	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2014] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2014] <- 0				# 11.3 

# envelopes/EU ETS Article 21 reporting SI 2015/si.xml
ds$namingshaming[ds$country=="SI" & ds$year==2015] <- 1							# 11.1
ds$suspension[ds$country=="SI" & ds$year==2015] <- 	1							# 4.2 
ds$inspection[ds$country=="SI" & ds$year==2015] <- 	1							# 11.1
ds$inspection.n[ds$country=="SI" & ds$year==2015] <- 5						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2015] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2015] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2015] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2015] <- 0				# 11.3 

# envelopes/EU ETS Article 21 report SI_2016/si_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SI" & ds$year==2016] <- 1							# 11.1
ds$suspension[ds$country=="SI" & ds$year==2016] <- 	1							# 4.2 
ds$inspection[ds$country=="SI" & ds$year==2016] <- 	1							# 11.1
ds$inspection.n[ds$country=="SI" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2016] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2016] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2016] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2016] <- 0				# 11.3 

# envelopes/EU ETS Article 21 report SI_2017/si_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SI" & ds$year==2017] <- 1							# 11.1
ds$suspension[ds$country=="SI" & ds$year==2017] <- 	1							# 4.2 
ds$inspection[ds$country=="SI" & ds$year==2017] <- 	1							# 11.1
ds$inspection.n[ds$country=="SI" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2017] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2017] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2017] <- 0				# 11.3 

# envelopes/EU ETS Article21Report SI_2018/si_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SI" & ds$year==2018] <- 1							# 11.1
ds$suspension[ds$country=="SI" & ds$year==2018] <- 	1							# 4.2 
ds$inspection[ds$country=="SI" & ds$year==2018] <- 	1							# 11.1
ds$inspection.n[ds$country=="SI" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2018] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2018] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2018] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2018] <- 0				# 11.3 

# envelopes/EU ETS Article 21Report_SI_2019/si_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SI" & ds$year==2019] <- 1							# 11.1
ds$suspension[ds$country=="SI" & ds$year==2019] <- 	1							# 4.2 
ds$inspection[ds$country=="SI" & ds$year==2019] <- 	1							# 11.1
ds$inspection.n[ds$country=="SI" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SI" & ds$year==2019] <- 0	# 11.4
ds$emissionsfines.n.installations.upper[ds$country=="SI" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SI" & ds$year==2019] <- 0			#11.3 
ds$otherfines.q.installations[ds$country=="SI" & ds$year==2019] <- 0				# 11.3 

# envelopes(1)/Slovak Republic Article 21 Report - 2005/SVK_Quest_Article_21_10_July_2006.doc
# envelopes(1)/Slovak Republic Article 21 Report - 2005/SVK_Quest_Art21_Part1_July_2006.doc
ds$suspension[ds$country=="SK" & ds$year==2005] <- NA
ds$inspection[ds$country=="SK" & ds$year==2005] <- 1
ds$namingshaming[ds$country=="SK" & ds$year==2005] <- 1
ds$supervision[ds$country=="SK" & ds$year==2005] <- 0
ds$appointment[ds$country=="SK" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2005] <- 0

# envelopes(2)/Slovak Republic Article 21 Report - 2006/SVK_Report_Art21_Part1_July2007.doc
ds$suspension[ds$country=="SK" & ds$year==2006] <- NA
ds$inspection[ds$country=="SK" & ds$year==2006] <- 1
ds$namingshaming[ds$country=="SK" & ds$year==2006] <- 1
ds$supervision[ds$country=="SK" & ds$year==2006] <- 0
ds$appointment[ds$country=="SK" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2006] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2006] <- 0
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2006] <- 0
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2006] <- 0

# envelopes(2)/Slovak Republic Article 21 Report - 2006/SVK_Report_Art21_Part1_July2007.doc
ds$suspension[ds$country=="SK" & ds$year==2007] <- NA
ds$inspection[ds$country=="SK" & ds$year==2007] <- 1
ds$namingshaming[ds$country=="SK" & ds$year==2007] <- 1
ds$supervision[ds$country=="SK" & ds$year==2007] <- 0
ds$appointment[ds$country=="SK" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2007] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2007] <- 0
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2007] <- 0
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2007] <- 0

# envelopes(2)/Slovak Republic Article 21 Report - 2008/SK-Report_Art_21_Part_1_30_June_2009.doc
ds$suspension[ds$country=="SK" & ds$year==2008] <- NA
ds$inspection[ds$country=="SK" & ds$year==2008] <- 1
ds$namingshaming[ds$country=="SK" & ds$year==2008] <- 1
ds$supervision[ds$country=="SK" & ds$year==2008] <- 0
ds$appointment[ds$country=="SK" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2008] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2008] <- 0
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2008] <- 0
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2008] <- 0

# envelopes(1)/Slovak Republic Article 21 Report - 2010/2011/SK-Report_Art_21_Part_1_30_June_2011.doc
ds$suspension[ds$country=="SK" & ds$year==2010] <- NA
ds$inspection[ds$country=="SK" & ds$year==2010] <- 1
ds$namingshaming[ds$country=="SK" & ds$year==2010] <- 1
ds$supervision[ds$country=="SK" & ds$year==2010] <- 0
ds$appointment[ds$country=="SK" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2010] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2010] <- 0
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2010] <- 0
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2010] <- 1690

# envelopes(1)/Slovak Republic Article 21 Report - 2011/SK-Report_Art_21_Part_1_2012.doc
ds$suspension[ds$country=="SK" & ds$year==2011] <- NA
ds$inspection[ds$country=="SK" & ds$year==2011] <- 1
ds$namingshaming[ds$country=="SK" & ds$year==2011] <- 1
ds$supervision[ds$country=="SK" & ds$year==2011] <- 0
ds$appointment[ds$country=="SK" & ds$year==2011] <- 0
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2011] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2011] <- 2
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2011] <- "To be calculated"	# A: SK 179 METALURG STEEL, spol. s r.o., and SK 75 SLOVENERGIE, a. s. 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2011] <- 0

# envelopes(1)/Slovak Republic Article 21 Report - 2013/article21-instance-empty.xml.xml
ds$namingshaming[ds$country=="SK" & ds$year==2013] <-1  						# S11_InstallationsComplianceMeasures
ds$suspension[ds$country=="SK" & ds$year==2013] <-1 								# S04_NationalLawPermitUpdate
ds$inspection[ds$country=="SK" & ds$year==2013] <- 1								# S11_InstallationsComplianceMeasures
ds$inspection.n[ds$country=="SK" & ds$year==2013] <- 0						# S06_InstallationsVerificationReportChecksDetail1
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2013] <- 0	# S11_OperatorPenalties 
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2013] <- 1	# S11_OperatorPenalties
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2013] <- "To be calculated"			# S11_InstallationsPenaltiesImposed IDs - 179 (K2 Business s.r.o.)
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2013] <- 0				# S11_InstallationsPenaltiesImposed all listed were no/no

# envelopes(1)/Slovak Republic Article 21 Report - 2014/article21-instance-empty.xml.xml
ds$namingshaming[ds$country=="SK" & ds$year==2014] <- 1 						# 11.1
ds$suspension[ds$country=="SK" & ds$year==2014] <- 	1							# 4.2
ds$inspection[ds$country=="SK" & ds$year==2014] <- 1								# 11.1
ds$inspection.n[ds$country=="SK" & ds$year==2014] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2014] <-0 	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2014] <-0 	# 11.4
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2014] <-0 			# 11.3 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2014] <- 3000			# 11.3  2000 no/no. 3000 remaining no/yes

# envelopes/Article 21 Report - Slovakia 2015/sk.xml
ds$namingshaming[ds$country=="SK" & ds$year==2015] <- 1 						# 11.1
ds$suspension[ds$country=="SK" & ds$year==2015] <- 1								# 4.2 
ds$inspection[ds$country=="SK" & ds$year==2015] <- 1								# 11.1
ds$inspection.n[ds$country=="SK" & ds$year==2015] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2015] <- 0	# 11.4 one fine of 0 listed for deadline but other undetermined  
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2015] <- 0	# 11.4 None listed but there is a fine amount
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2015] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2015] <- 1000				# 11.3 no/yes

# envelopes/Article 21 Report - Slovakia 2016/sk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SK" & ds$year==2016] <- 1 						# 11.1
ds$suspension[ds$country=="SK" & ds$year==2016] <- 1								# 4.2 
ds$inspection[ds$country=="SK" & ds$year==2016] <- 1								# 11.1
ds$inspection.n[ds$country=="SK" & ds$year==2016] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2016] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2016] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2016] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2016] <- 0				# 11.3

# envelopes/Article 21 Report - Slovakia 2017/sk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SK" & ds$year==2017] <- 1 						# 11.1
ds$suspension[ds$country=="SK" & ds$year==2017] <- 1								# 4.2 
ds$inspection[ds$country=="SK" & ds$year==2017] <- 1								# 11.1
ds$inspection.n[ds$country=="SK" & ds$year==2017] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2017] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2017] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2017] <- 0				# 11.3

# envelopes/Article 21 Report - Slovakia 2018/sk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SK" & ds$year==2018] <- 1 						# 11.1
ds$suspension[ds$country=="SK" & ds$year==2018] <- 1								# 4.2 
ds$inspection[ds$country=="SK" & ds$year==2018] <- 1								# 11.1
ds$inspection.n[ds$country=="SK" & ds$year==2018] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2018] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2018] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2018] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2018] <- 0				# 11.3

# envelopes/Article 21 Report - Slovakia 2019/sk_eu-ets-art21_report.xml
ds$namingshaming[ds$country=="SK" & ds$year==2019] <- 1 						# 11.1
ds$suspension[ds$country=="SK" & ds$year==2019] <- 1								# 4.2 
ds$inspection[ds$country=="SK" & ds$year==2019] <- 1								# 11.1
ds$inspection.n[ds$country=="SK" & ds$year==2019] <- 0						# 6.5
ds$emissionsfines.n.installations.lower[ds$country=="SK" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.n.installations.upper[ds$country=="SK" & ds$year==2019] <- 0	# 11.4 
ds$emissionsfines.q.installations[ds$country=="SK" & ds$year==2019] <- 0			# 11.3 
ds$otherfines.q.installations[ds$country=="SK" & ds$year==2019] <- 0				# 11.3



## Overwrite using information obtained directly through correspondence with national regulators
# Correspondence with national regulators/Austria.pdf
# Correspondence with national regulators/Austria Phases 1 and 2.pdf
ds$emissionsfines.n.installations.lower[ds$country=="AT" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="AT" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="AT" & ds$year %in% c(2005:2020)] <- 0
ds$otherfines.q.installations[ds$country=="AT" & ds$year %in% c(2005:2020)] <- 0

# Correspondence with national regulators/Czech Republic.pdf
# Correspondence with national regulators/EU ETS Non-compliance and penalties - CZ overview 01.xlsx
ds$emissionsfines.n.installations.lower[ds$country=="CZ" %in% c(2006,2007,2008,2010,2011,2012,2014,2019)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" %in% c(2006,2007,2008,2010,2011,2012,2014,2019)] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" %in% c(2006,2007,2008,2010,2011,2012,2014,2019)] <- 0
ds$otherfines.q.installations[ds$country=="CZ" %in% c(2006,2007,2008,2010,2011,2012,2014,2019)] <- NA

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2005] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2005] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2005] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2005] <- NA

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2009] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2009] <- 1
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2009] <- 654343
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2009] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2013] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2013] <- 2
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2013] <- 2198037
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2013] <- 1961+302

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2015] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2015] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2015] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2015] <- 189+1961+196+196+392+157

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2016] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2016] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2016] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2016] <- 1000

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2017] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2017] <- 1
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2017] <- 1225169
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2017] <- 600+400

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2018] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2018] <- 2
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2018] <- 71455+93641
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2018] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="CZ" & ds$year==2020] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="CZ" & ds$year==2020] <- 0
ds$emissionsfines.q.installations[ds$country=="CZ" & ds$year==2020] <- 0
ds$otherfines.q.installations[ds$country=="CZ" & ds$year==2020] <- 736.65

# Correspondence with national regulators/Germany.pdf
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2005] <- 1727120.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2006] <- 1143640.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2007] <- 23400.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2008] <- 30100.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2009] <- 3800.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2010] <- 1500.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2011] <- 332300.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2012] <- 1076700.00
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2013] <- 32581.50
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2014] <- 345098.67
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2015] <- 445856.45
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2016] <- 35194.64
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2017] <- 650999.36
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2018] <- 114111.25
ds$emissionsfines.q.installations[ds$country=="DE" & ds$year==2019] <- 45884.46

# Correspondence with national regulators/Denmark.pdf
ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year %in% c(2006:2012,2014:2018)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year %in% c(2006:2012,2014:2018)] <- 0
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year %in% c(2006:2012,2014:2018)] <- 0
ds$otherfines.q.installations[ds$country=="DK" & ds$year %in% c(2005:2020)] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2005] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2005] <- 1
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2005] <- 1*40

ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2013] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2013] <- 1
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2013] <- 35*100

ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2019] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2019] <- 2
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2019] <- 2*100
ds$otherfines.q.installations[ds$country=="DK" & ds$year==2019] <- 4027

ds$emissionsfines.n.installations.lower[ds$country=="DK" & ds$year==2020] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="DK" & ds$year==2020] <- 1
ds$emissionsfines.q.installations[ds$country=="DK" & ds$year==2020] <- 334*100

# Correspondence with national regulators/Estonia.pdf
ds$emissionsfines.n.installations.lower[ds$country=="EE" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="EE" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="EE" & ds$year %in% c(2005:2020)] <- 0
ds$otherfines.q.installations[ds$country=="EE" & ds$year %in% c(2005:2020)] <- 0

# Correspondence with national regulators/Finland.pdf
ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year %in% c(2005:2020)] <- 0
ds$otherfines.q.installations[ds$country=="FI" & ds$year %in% c(2005:2020)] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2005] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2005] <- 1
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2005] <- 2200

ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2006] <- 5+4+1+1
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2006] <- 5+4+1+1
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2006] <- 55480+131760+2960+39360

ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2008] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2008] <- 2
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2008] <- 2960+16888100

ds$emissionsfines.n.installations.lower[ds$country=="FI" & ds$year==2012] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="FI" & ds$year==2012] <- 2
ds$emissionsfines.q.installations[ds$country=="FI" & ds$year==2012] <- 100+28996900 

# Correspondence with national regulators/Hungary.pdf
# Correspondence refers to year in which the failure to surrender occurred, but the failures occurred in reference to emissions in the preceeding years.
ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2018] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2018] <- 1
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2018] <- "To be calculated" # In 2019, there was one installation which did not surrender the appropriate amount of units (ID: 252)

ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2019] <- 2
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2019] <- 2
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2019] <- "To be calculated" # in 2020 there were two (IDs: 63, 252)

ds$emissionsfines.n.installations.lower[ds$country=="HU" & ds$year==2020] <- 9
ds$emissionsfines.n.installations.upper[ds$country=="HU" & ds$year==2020] <- 9
ds$emissionsfines.q.installations[ds$country=="HU" & ds$year==2020] <- "To be calculated" # in 2021 there were nine (IDs: 8, 49, 50, 51, 52, 63, 251, 252, 210527)

# Correspondence with national regulators/Iceland.pdf
ds$emissionsfines.n.installations.lower[ds$country=="IS" & ds$year %in% c(2012:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="IS" & ds$year %in% c(2012:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="IS" & ds$year %in% c(2012:2020)] <- 0
ds$otherfines.q.installations[ds$country=="IS" & ds$year %in% c(2012:2020)] <- 0

# Correspondence with national regulators/Liechtenstein.pdf
ds$emissionsfines.n.installations.lower[ds$country=="LI" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="LI" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="LI" & ds$year %in% c(2005:2020)] <- 0
ds$otherfines.q.installations[ds$country=="LI" & ds$year %in% c(2005:2020)] <- 0

# Correspondence with national regulators/Malta.pdf
ds$emissionsfines.n.installations.lower[ds$country=="MT" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="MT" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="MT" & ds$year %in% c(2005:2020)] <- 0
ds$otherfines.q.installations[ds$country=="MT" & ds$year %in% c(2005:2020)] <- 0

# Correspondence with national regulators/Sweden.pdf
ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year %in% c(2005,2007:2009,2016:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year %in% c(2005,2007:2009,2016:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year %in% c(2005,2007:2009,2016:2020)] <- 0
ds$otherfines.q.installations[ds$country=="SE" & ds$year %in% c(2012:2015,2019:2020)] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2006] <- 29
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2006] <- 29
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2006] <- (3959366+15516051+1341972+3801767+4754+3291+12149785+447934+197456+505708+328728+220859+17552+111892+84467+176248+1097+2399095+169301+5851+67647+2560+13529+438792+610287+146264+1317107+76789+1736885) * 0.10930 # Using historical SEK-EUR exchange rate from May 1, 2007 (the date on which fines would have been issued). Available from European Central Bank (https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/eurofxref-graph-sek.en.html, URL date July 29, 2021).

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2010] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2010] <- 1
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2010] <- (2771321) * 0.11200 # Using historical SEK-EUR exchange rate from May 2, 2011
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2010] <- (4*20000) * 0.11200

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2011] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2011] <- 1
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2011] <- (14338551) * 0.11210 # Using historical SEK-EUR exchange rate from May 1, 2012
ds$otherfines.q.installations[ds$country=="SE" & ds$year==2011] <- (2*20000) * 0.11210

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2012] <- 17
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2012] <- 17
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2012] <- (30014+562540+858+50398+897764+34168+44418+15376+29897+1714379+856763+15616484+4814271+199883+3416+5779+69190) * 0.11710 # Using historical SEK-EUR exchange rate from May 1, 2013

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2013] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2013] <- 1
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2013] <- (6450) * 0.11020 # Using historical SEK-EUR exchange rate from May 1, 2014

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2014] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2014] <- 1
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2014] <- (4340570) * 0.10720 # Using historical SEK-EUR exchange rate from May 1, 2015

ds$emissionsfines.n.installations.lower[ds$country=="SE" & ds$year==2015] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="SE" & ds$year==2015] <- 1
ds$emissionsfines.q.installations[ds$country=="SE" & ds$year==2015] <- (112000) * 0.10890 # Using historical SEK-EUR exchange rate from May 2, 2016

ds$otherfines.q.installations[ds$country=="SE" & ds$year==2016] <- (2*20000) * 0.10390 # Using historical SEK-EUR exchange rate from May 2, 2017

ds$otherfines.q.installations[ds$country=="SE" & ds$year==2017] <- (1*20000) * 0.09524 # Using historical SEK-EUR exchange rate from May 1, 2018

ds$otherfines.q.installations[ds$country=="SE" & ds$year==2018] <- (1*20000) * 0.09359 # Using historical SEK-EUR exchange rate from May 2, 2019

# Correspondence with national regulators/Romania.pdf
# Correspondence with national regulators/Situatie neconformi 2007-2020.xlsx
## Romania says it "has no atributions regarding the applyed penalties to operators of non-compliant installations and has no information on them", so we need to multiply number of missing allowances by the fine to get an upper bound estimate of the total fines collected.
ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year %in% c(2007,2008,2010,2011)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year %in% c(2007,2008,2010,2011)] <- 0
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year %in% c(2007,2008,2010,2011)] <- 0
ds$otherfines.q.installations[ds$country=="RO" & ds$year %in% c(2007,2008,2010,2011)] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2009] <- 12
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2009] <- 12
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2009] <- (143437+1900+1500+15913+34254+2572+2616+97+877+1171+4388+4422) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2009] <- NA

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2012] <- 3
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2012] <- 3
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2012] <- (334866+23115+3921) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2012] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2013] <- 1
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2013] <- 1
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2013] <- (3924) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2013] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2014] <- 3
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2014] <- 3
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2014] <- (435583+782157+1321207) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2014] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2015] <- 6
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2015] <- 6
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2015] <- (787263+880818+793778+216598+119667+830797) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2015] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2016] <- 6
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2016] <- 6
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2016] <- (70132+847909+289085+426310+196987+11295) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2016] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2017] <- 4
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2017] <- 4
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2017] <- (24761+837512+339424+67170) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2017] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2018] <- 7
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2018] <- 7
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2018] <- (2405+768103+263295+6201+1933+54262+14352) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2018] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2019] <- 5
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2019] <- 5
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2019] <- (859943+716723+146414+4304+22065) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2019] <- 0

ds$emissionsfines.n.installations.lower[ds$country=="RO" & ds$year==2020] <- 8
ds$emissionsfines.n.installations.upper[ds$country=="RO" & ds$year==2020] <- 8
ds$emissionsfines.q.installations[ds$country=="RO" & ds$year==2020] <- (612040+876946+201463+23469+2213+46977+2815+217801) * 100
ds$otherfines.q.installations[ds$country=="RO" & ds$year==2020] <- NA

# Correspondence with national regulators/UK.pdf
# Exchange rates
# 1.4521	# May 2, 2006
# 1.1246	# May 2, 2011
# 1.3552	# May 4, 2015
# 1.2780	# May 2, 2016
# 1.1832	# May 2, 2017
# 1.1358	# May 2, 2018
# 1.1637	# May 2, 2019
# 1.1377	# May 4, 2020
# 1.1516	# May 3, 2021
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year %in% c(2005:2020)] <- 0
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year %in% c(2005:2020)] <- 0
ds$otherfines.q.installations[ds$country=="GB" & ds$year %in% c(2005:2020)] <- 0

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005]) + (19618.84) * 1.4521
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005]) + (564559.93) * 1.4521
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005]) + (52532.22) * 1.4521
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2005]) + (122099.74) * 1.4521
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2005] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2005] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (1136.31) * 1.1246
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + 1379.81 * 1.1246
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + 21184.07 * 1.1246
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + 274716.80 * 1.1246
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + 844765.32 * 1.1246	# Year of infringement inferred
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + 2886800.00 * 1.1246	# Year of infringement inferred
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + 157857.13 * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (3000) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (1500) * 1.3552	# Year of infringement inferred

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1500) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (1500) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1500) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1500) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1500) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1500) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1000) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1000) * 1.3552

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1000) * 1.3552

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (73161.35/3) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + (73161.35/3) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + (73161.35/3) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2012] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2012] + (3000) * 1.3552

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (3823.35/4) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + (3823.35/4) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + (3823.35/4) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (3823.35/4) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + (1011.52) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (6264.87) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + (8118.05) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (3556.13/5) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + (3556.13/5) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + (3556.13/5) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (3556.13/5) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + (3556.13/5) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (1699.04) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (2940.49) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (31495.60) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (66584.78/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (66584.78/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (628.42/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (628.42/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (537.30/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (537.30/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (739.56/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (739.56/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (2651.52/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (2651.52/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (26589.40/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (26589.40/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (631.49/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (631.49/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (612.71/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (612.71/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (862.34/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (862.34/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (673.12/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (673.12/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (633.95/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (633.95/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (714.20/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (714.20/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (583.51/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (583.51/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1033.96/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (1033.96/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (3963.00/3) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (3963.00/3) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (3963.00/3) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (662.38/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (662.38/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (81000.00) * 1.2780	# Year and type of infringement inferred

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (675.63) * 1.2780	# Year and type of infringement inferred

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (25836/6) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + (25836/6) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + (25836/6) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (25836/6) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + (25836/6) * 1.3552
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (25836/6) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (1000) * 1.2780	# Year of infringement inferred

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (19242.31/2) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (19242.31/2) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (2275.00) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (84826.88/3) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (84826.88/3) * 1.2780
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (84826.88/3) * 1.2780

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (733.16) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1319.13/4) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (1319.13/4) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (1319.13/4) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (1319.13/4) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (1517.19/4) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (1517.19/4) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (1517.19/4) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (1517.19/4) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (3987.91) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (721.89) * 1.1832	# Year of infringement inferred

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (647.22/2) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (647.22/2) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (730.70/2) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (730.70/2) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (743.08/2) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (743.08/2) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (55022.96) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (31602.05) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (19242.31/2) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (19242.31/2) * 1.1832

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2008]) + (27664.27/9) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2009]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2010]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016]) + (27664.27/6) * 1.3552	# Year of infringement inferred
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2008] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2008] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2009] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2009] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2010] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2010] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2016] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (1000.00) * 1.1832	# Year of infringement inferred

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015]) + (460895.73) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (41285.32/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (41285.32/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (21384.22/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (21384.22/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (68120.28/3) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + (68120.28/3) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (68120.28/3) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016]) + (154533.90) * 1.3552
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2016] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] + (1000.00) * 1.1832	# Year of infringement inferred

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (9710.31) * 1.1832

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] + (1000.00) * 1.1832	# Year of infringement inferred

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (1668.63/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + (1668.63/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (567.09/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (567.09/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (218.90/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (218.90/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (76.45/2) * 1.1832
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (76.45/2) * 1.1832

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017]) + (16562.99) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2017] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017]) + (608591.41) * 1.1358	# Type of infringement inferred
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2017] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2013] + (6668.39) * 1.1832

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015]) + (22515.50/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2016]) + (22515.50/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2016] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2016] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (1555.33) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015]) + (1161.39) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2011]) + (68120.28/4) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2012]) + (68120.28/4) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2013]) + (68120.28/4) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (68120.28/4) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2011] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2011] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2012] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2012] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2013] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2013] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2014]) + (21384.22/2) * 1.1358
ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015]) + (21384.22/2) * 1.1358
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2014] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2014] + 1
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2015]) + (14759.64) * 1.1637
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2015] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2015] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2017] + (1000) * 1.1637

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2018] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2018]) + (3986626.08) * 1.1637
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2018] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2018] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2018] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2018] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2018]) + (1386345.72) * 1.1637
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2018] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2018] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2018] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2017]) + (1183.07) * 1.1377
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2017] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2017] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2014] + (42322.70) * 1.1377
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2015] + (42322.70) * 1.1377
ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (42322.70) * 1.1377

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2016] + (126787.35) * 1.1377

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019]) + (352722.30) * 1.1377
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2019] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019]) + (45019.83) * 1.1377
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2019] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] + 1

ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019] <- as.numeric(ds$emissionsfines.q.installations[ds$country=="GB" & ds$year==2019]) + (36980.57) * 1.1377
ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] <- ds$emissionsfines.n.installations.upper[ds$country=="GB" & ds$year==2019] <- ds$emissionsfines.n.installations.lower[ds$country=="GB" & ds$year==2019] + 1

ds$otherfines.q.installations[ds$country=="GB" & ds$year==2020] <- ds$otherfines.q.installations[ds$country=="GB" & ds$year==2020] + (1000) * 1.1516




## Remove countries that had not yet entered the EU ETS
ds <- ds[!(ds$country %in% c("BG","RO") & ds$year %in% 2005:2006),]		# Bulgaria and Romania entered the EU ETS in 2007.
ds <- ds[!(ds$country %in% c("NO","LI") & ds$year %in% 2005:2007),]		# Norway and Liechtenstein entered EU ETS in 2008.
ds <- ds[!(ds$country %in% c("HR","IS") & ds$year %in% 2005:2012),]		# Croatia entered EU ETS in 2013. Iceland officially entered earlier, but stationary installations only joined in 2013.


## Calculate fines imposed
d <- ds
load("Data/EUTL2021.Rdata")

### In cases where information about the non-compliant installations / operators is available, calculate total fine based on their collective excess emissions in a given year.
# d[d$emissionsfines.q.installations=="To be calculated" & !is.na(d$emissionsfines.q.installations),]

d$emissionsfines.q.installations[d$country=="BE.Fl" & d$year==2011] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="BE.Fl" & ds$Year==2011 & ds$InstallationOrAircraftOperatorID %in% c(309)],0, na.rm=TRUE), na.rm=TRUE) * 100 # NESAR (BE000000000000309)
d$emissionsfines.q.installations[d$country=="BE.Fl" & d$year==2017] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="BE.Fl" & ds$Year==2017 & ds$InstallationOrAircraftOperatorID %in% c(72)],0, na.rm=TRUE), na.rm=TRUE) * 100 # IDs - 72 (Langerlo NV – Centrale Langerlo)

d$emissionsfines.q.installations[d$country=="BG" & d$year==2016] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="BG" & ds$Year==2016 & ds$InstallationOrAircraftOperatorID %in% c(203535)],0, na.rm=TRUE), na.rm=TRUE) * 100 # IDs -  203535 (Tamara 2009 OOD)
d$emissionsfines.q.installations[d$country=="BG" & d$year==2017] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="BG" & ds$Year==2017 & ds$InstallationOrAircraftOperatorID %in% c(49,153,201534)],0, na.rm=TRUE), na.rm=TRUE) * 100 # IDs - 49 (Tera Co Ltd), 153 (V&VGD Greenhouse Petrich Ltd), 201534 (Polysan)
d$emissionsfines.q.installations[d$country=="BG" & d$year==2018] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="BG" & ds$Year==2018 & ds$InstallationOrAircraftOperatorID %in% c(49)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 49 (Tera Co Ltd)

d$emissionsfines.q.installations[d$country=="ES" & d$year==2014] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="ES" & ds$Year==2014 & ds$InstallationOrAircraftOperatorID %in% c(891)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 891 (ANTIBIÓTICOS, S.A.)
d$emissionsfines.q.installations[d$country=="ES" & d$year==2019] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="ES" & ds$Year==2019 & ds$InstallationOrAircraftOperatorID %in% c(474)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 474 (DICEPA HIGIENE, S.L.)

d$emissionsfines.q.installations[d$country=="HU" & d$year==2009] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2009 & ds$InstallationOrAircraftOperatorID %in% c(153, 77)],0, na.rm=TRUE), na.rm=TRUE) * 100	#	Nitrogénművek Zrt, and Globus Konzervipari Rt.
d$emissionsfines.q.installations[d$country=="HU" & d$year==2010] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2010 & ds$InstallationOrAircraftOperatorID %in% c(153, 77)],0, na.rm=TRUE), na.rm=TRUE) * 100	#	Nitrogénművek Zrt, and Globus Konzervipari Rt.
d$emissionsfines.q.installations[d$country=="HU" & d$year==2011] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2011 & ds$InstallationOrAircraftOperatorID %in% c(255)],0, na.rm=TRUE), na.rm=TRUE) * 100	# Higi Papírsoft Papírtermékeket Gyártó és Forgalmazó Zrt. This operator no longer exists, but old documentation submitted to the European Commission (https://ec.europa.eu/energy/sites/default/files/documents/Art%2014ReportHungary.pdf, URL date May 29, 2021) indicates that a combustion installation at "Piroskai u. 16." in "Szolnok" was previously operated by "HIGI Papírsoft ZRt. "f.a."". Only installation 255 matches these criteria.
d$emissionsfines.q.installations[d$country=="HU" & d$year==2012] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2012 & ds$InstallationOrAircraftOperatorID %in% c(255)],0, na.rm=TRUE), na.rm=TRUE) * 100	# Higi Papírsoft Papírtermékeket Gyártó és Forgalmazó Zrt. This operator no longer exists, but old documentation submitted to the European Commission (https://ec.europa.eu/energy/sites/default/files/documents/Art%2014ReportHungary.pdf, URL date May 29, 2021) indicates that a combustion installation at "Piroskai u. 16." in the city of "Szolnok" was previously operated by "HIGI Papírsoft ZRt. "f.a."". Only installation 255 matches these criteria.
d$emissionsfines.q.installations[d$country=="HU" & d$year==2018] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2018 & ds$InstallationOrAircraftOperatorID %in% c(252)],0, na.rm=TRUE), na.rm=TRUE) * 100	# ID: 252
d$emissionsfines.q.installations[d$country=="HU" & d$year==2019] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2019 & ds$InstallationOrAircraftOperatorID %in% c(63,252)],0, na.rm=TRUE), na.rm=TRUE) * 100	# # IDs: 63, 252
d$emissionsfines.q.installations[d$country=="HU" & d$year==2020] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="HU" & ds$Year==2020 & ds$InstallationOrAircraftOperatorID %in% c(8, 49, 50, 51, 52, 63, 251, 252, 210527)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs: 8, 49, 50, 51, 52, 63, 251, 252, 210527

d$emissionsfines.q.installations[d$country=="IE" & d$year==2018] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="IE" & ds$Year==2018 & ds$InstallationOrAircraftOperatorID %in% c(32,210246)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs -  32 (St James' Hospital Board, Dublin), 210246 (Vodafone Group Services Ireland Limited)

d$emissionsfines.q.installations[d$country=="LV" & d$year==2008] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="LV" & ds$Year==2008 & ds$InstallationOrAircraftOperatorID %in% c(93)],0, na.rm=TRUE), na.rm=TRUE) * 100	# SIA “Livberzes energija” did not surrender their year 2008 allowances in time till 30th April 2009.

d$emissionsfines.q.installations[d$country=="NL" & d$year==2015] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="NL" & ds$Year==2015 & ds$InstallationOrAircraftOperatorID %in% c(206407,206832)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 206407 (Rasenberg Wegenbouw B.V.), 206832 (Bruil Infra B.V.)
d$emissionsfines.q.installations[d$country=="NL" & d$year==2018] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="NL" & ds$Year==2018 & ds$InstallationOrAircraftOperatorID %in% c(22)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 22 (Neptune Energy Netherlands B.V (ENGIE E&P Nederland B.V., K12-B platform))

d$emissionsfines.q.installations[d$country=="PL" & d$year==2017] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PL" & ds$Year==2017 & ds$InstallationOrAircraftOperatorID %in% c(634)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 634 (Ceramika Polska Sp. z o.o.)
d$emissionsfines.q.installations[d$country=="PL" & d$year==2018] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PL" & ds$Year==2018 & ds$InstallationOrAircraftOperatorID %in% c(130,263)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - 130 ("LUBREM" sp.j. K. Dębski; J. Klepacki), 263(Fabryka Papieru sp. z o.o.)

d$emissionsfines.q.installations[d$country=="PT" & d$year==2005] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PT" & ds$Year==2005 & ds$InstallationOrAircraftOperatorID %in% c(211,227,230)],0, na.rm=TRUE), na.rm=TRUE) * 40	#
# 	211		COMPAL - Companhia Produtora de Conservas Alimentares	Central Térmica	Almeirim
# 	227		Cosbar - Cerâmica do Barlavento, S.A.	Cosbar	Algoz
# 	230		Grésil	Grésil	Mourisca do Vouga
d$emissionsfines.q.installations[d$country=="PT" & d$year==2006] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PT" & ds$Year==2006 & ds$InstallationOrAircraftOperatorID %in% c(247, 166, 244, 62, 61)],0, na.rm=TRUE), na.rm=TRUE) * 40	# Penalties for ID permit 247, 166, 244, 62, 61.
d$emissionsfines.q.installations[d$country=="PT" & d$year==2015] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PT" & ds$Year==2015 & ds$InstallationOrAircraftOperatorID %in% c(232,146)],0, na.rm=TRUE), na.rm=TRUE) * 100	# 232(Malhas Eical - Empresa Industrial do Cávado), Lda, 146(Tinturaria e Acabamentos de Tecidos Vale de Tábuas, Lda)
d$emissionsfines.q.installations[d$country=="PT" & d$year==2017] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PT" & ds$Year==2017 & ds$InstallationOrAircraftOperatorID %in% c(146)],0, na.rm=TRUE), na.rm=TRUE) * 100	# 11.3 IDs - PTF(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.)
d$emissionsfines.q.installations[d$country=="PT" & d$year==2018] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PT" & ds$Year==2018 & ds$InstallationOrAircraftOperatorID %in% c(146,141,223,156,208842,92,125)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - PTF-17(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-16(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-15(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-14(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTF-13(Tinturaria e Acabamentos de Tecidos, Vale de Tábuas, Lda.), PTG-16(Natural - Industria de Papel, S.A.), PTG-15(Natural - Industria de Papel, S.A.), PTH-16(Lusotelha, Telhas e Tijolos de Águeda, Lda.), PTH-15(Lusotelha, Telhas e Tijolos de Águeda, Lda.), PTI-15(Fábrica de Papel de Medros, Lda.), PTI-14(Fábrica de Papel de Medros, Lda.), PTI-13(Fábrica de Papel de Medros, Lda.), PTJ-15(SUTOL - Indústrias Alimentares, Lda), PTK-14(Arco Têxteis, S.A.), PTL(Prélis Cerâmica, Lda.)
d$emissionsfines.q.installations[d$country=="PT" & d$year==2019] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="PT" & ds$Year==2019 & ds$InstallationOrAircraftOperatorID %in% c(205572)],0, na.rm=TRUE), na.rm=TRUE) * 100	# IDs - PTF-18(MARGON - Materiais e Revestimentos Modernos para Edificações, S.A.)

d$emissionsfines.q.installations[d$country=="SK" & d$year==2011] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="SK" & ds$Year==2011 & ds$InstallationOrAircraftOperatorID %in% c(179,175)],0, na.rm=TRUE), na.rm=TRUE) * 100	# A: SK 179 METALURG STEEL, spol. s r.o., and SK 75 SLOVENERGIE, a. s. (75 should be 175 (typo), and 179 is now listed under K2 Business)
d$emissionsfines.q.installations[d$country=="SK" & d$year==2013] <- sum(pmax((ds$VerifiedEmissions - ds$UnitsSurrendered)[ds$NationalAdministratorCodeCorrected=="SK" & ds$Year==2013 & ds$InstallationOrAircraftOperatorID %in% c(179)],0, na.rm=TRUE), na.rm=TRUE) * 100	# 179 (K2 Business s.r.o.)


 
### In cases where information about the non-compliant installations / operators is not available, we use the following procedure to compute the total fines imposed.
#### (1) Compute annual excess emissions for all installations.
#### (2) Compute excess emissions as the sum of the N installations with greatest individual excess emissions, where N is the upper bound on the number of installations on which excess emissions fines were imposed. If this upper bound is sufficiently vague, assume that enforcement action was taken against all installations with excess emissions. This procedure will return the largest total shortfall consistent with reported information.
#### (3) Compute the fine by multiplying the total excess emissions from step (2) by the fine per tonne (€40 or €100).
rownames(d) <- 1:nrow(d)
tbcs <- as.numeric(rownames(d[d$emissionsfines.q.installations=="To be calculated" & !is.na(d$emissionsfines.q.installations),]))
for (i in tbcs) {
	shortfalls <- (ds$UnitsSurrendered - ds$VerifiedEmissions)[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]][order((ds$UnitsSurrendered - ds$VerifiedEmissions)[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]])]
	if (!is.na(d$emissionsfines.n.installations.upper[i]) & d$emissionsfines.n.installations.upper[i]!="Maximum") {
		total.shortfall <- -sum(shortfalls[1:d$emissionsfines.n.installations.upper[i]], na.rm=TRUE)
	}
	if (!is.na(d$emissionsfines.n.installations.upper[i]) & d$emissionsfines.n.installations.upper[i]=="Maximum") {
		total.shortfall <- -sum(shortfalls[shortfalls<0 & !is.na(shortfalls)], na.rm=TRUE)
	}
	if (d$year[i]<2008) { total.fine <- total.shortfall * 40 }
	if (d$year[i]>2007) { total.fine <- total.shortfall * 100 }
	d$emissionsfines.q.installations[i] <- total.fine
	rm(shortfalls,total.shortfall,total.fine)
}

## Transfer regulatory variables to main data
ds$otherfines.q.installations <- ds$emissionsfines.q.installations <- ds$emissionsfines.n.installations.upper <- ds$emissionsfines.n.installations.lower <- ds$inspection.n <- ds$inspection <- ds$suspension <- ds$namingshaming <- ds$supervision <- ds$appointment <- NA
for (i in 1:nrow(d)) {
	ds$appointment[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$appointment[i]
	ds$supervision[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$supervision[i]
	ds$namingshaming[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$namingshaming[i]
	ds$suspension[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$suspension[i]
	ds$inspection[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$inspection[i]
	ds$inspection.n[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$inspection.n[i]
	ds$emissionsfines.n.installations.lower[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$emissionsfines.n.installations.lower[i]
	ds$emissionsfines.n.installations.upper[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$emissionsfines.n.installations.upper[i]
	ds$emissionsfines.q.installations[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$emissionsfines.q.installations[i]
	ds$otherfines.q.installations[ds$NationalAdministratorCodeCorrected==d$country[i] & ds$Year==d$year[i]] <- d$otherfines.q.installations[i]
}

save(ds, d, file = "Data/EUTL2021.Rdata")

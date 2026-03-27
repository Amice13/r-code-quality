## Installing necessary packages ----
packages <-c(
  "ggplot2", "foreign", "data.table", "stargazer", "lfe", "stringr", "xtable", 
  "tidyverse", "lme4", "arm", "memisc", "vegan", "MASS", "haven", "zoo")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
## ----
# regressions on the cluster
rm(list = ls())
options(stringsAsFactors = FALSE)
seed_to_use <- 216
set.seed(seed_to_use)

# install.packages(pkgs = c("data.table", "stargazer"), repos = "https://cloud.r-project.org/")
### Libraries
library(ggplot2)
library(foreign)
library(data.table)
library(stargazer)
library(lfe)


#################################################################
####### 1. Load data
#################################################################

# load full data
load("~/geog_rotation_full.rdata")

# need to get rid of missingness in education
geog_rotation <- geog_rotation[eduname_cd != 999, ]
geog_rotation <- geog_rotation[years < 2016, ]

structural_sample <- geog_rotation[new_vals > 0, ]

######################################
### Only non-local employees

instansi_codes <- read.csv("~/Data Tabel Instansi.csv")
colnames(instansi_codes) <- c("instansi","Department")

geog_rotation <- merge(geog_rotation,instansi_codes,by=c("instansi"),all.x=T)
# table(geog_rotation$Department)

geog_rotation$non_local <- ifelse(geog_rotation$Department=="Kementerian Koordinator Bidang Politik, Hukum dan Keamanan"|
                                         geog_rotation$Department=="Kementerian Koordinator Bidang Perekonomian"|
                                         geog_rotation$Department=="Kementerian Koordinator Bidang Pembangunan Manusia dan Kebudayaan"|
                                         geog_rotation$Department=="Kementerian Koordinator Bidang Kemaritiman"|
                                         geog_rotation$Department=="Kementerian Riset, Teknologi, dan Pendidikan Tinggi"|
                                         geog_rotation$Department=="Kementerian Koperasi dan Usaha Kecil dan Menengah"|
                                         geog_rotation$Department=="Kementerian Lingkungan Hidup"|
                                         geog_rotation$Department=="Kementerian Badan Usaha Milik Negara"|
                                         geog_rotation$Department=="Kementerian Pemberdayaan Perempuan dan Perlindungan Anak"|
                                         geog_rotation$Department=="Kementerian Pendayagunaan Aparatur Negara dan Reformasi Birokrasi"|
                                         geog_rotation$Department=="Kementerian Pemuda dan Olahraga"|
                                         geog_rotation$Department=="Kementerian Perumahan Rakyat"|
                                         geog_rotation$Department=="Kementerian Desa, Pembangunan Daerah Tertinggal dan Transmigrasi"|
                                         geog_rotation$Department=="Kementerian Dalam Negeri"|
                                         geog_rotation$Department=="Kementerian Luar Negeri"|
                                         geog_rotation$Department=="Kementerian Pertahanan"|
                                         geog_rotation$Department=="Kementerian Hukum dan Hak Asasi Manusia"|
                                         geog_rotation$Department=="Kementerian Keuangan"|
                                         geog_rotation$Department=="Kementerian Pertanian"|
                                         geog_rotation$Department=="Kementerian Energi dan Sumber Daya Mineral"|
                                         geog_rotation$Department=="Kementerian Perhubungan"|
                                         geog_rotation$Department=="Kementerian Pendidikan dan Kebudayaan"|
                                         geog_rotation$Department=="Kementerian Kesehatan"|
                                         geog_rotation$Department=="Kementerian Agama"|
                                         geog_rotation$Department=="Kementerian Ketenagakerjaan"|
                                         geog_rotation$Department=="Kementerian Sosial"|
                                         geog_rotation$Department=="Kementerian Lingkungan Hidup dan Kehutanan"|
                                         geog_rotation$Department=="Kementerian Kelautan dan Perikanan"|
                                         geog_rotation$Department=="Kementerian Komunikasi dan Informatika"|
                                         geog_rotation$Department=="Kementerian Perdagangan"|
                                         geog_rotation$Department=="Kementerian Perindustrian"|
                                         geog_rotation$Department=="Kementerian Pekerjaan Umum dan Perumahan Rakyat"|
                                         geog_rotation$Department=="Kementerian Pariwisata"|
                                         geog_rotation$Department=="Kementerian Sekretariat Negara"|
                                         geog_rotation$Department=="Kejaksaan Agung"|
                                         geog_rotation$Department=="Badan Intelijen Negara"|
                                         geog_rotation$Department=="Sekretariat Jenderal MPR"|
                                         geog_rotation$Department=="Setjen DPA"|
                                         geog_rotation$Department=="Sekretariat Jenderal DPR RI"|
                                         geog_rotation$Department=="Mahkamah Agung RI"|
                                         geog_rotation$Department=="Badan Pemeriksa Keuangan "|
                                         geog_rotation$Department=="Setjen WANTANNAS"|
                                         geog_rotation$Department=="Lembaga Sandi Negara"|
                                         geog_rotation$Department=="Badan Kepegawaian Negara"|
                                         geog_rotation$Department=="Lembaga Administrasi Negara "| 
                                         geog_rotation$Department=="Lembaga Penerbangan dan Antariksa Nasional"|
                                         geog_rotation$Department=="Lembaga Ilmu Pengetahuan Indonesia"|
                                         geog_rotation$Department=="Badan Tenaga Nuklir Nasional "|
                                         geog_rotation$Department=="Badan Pusat Statistik "| 
                                         geog_rotation$Department=="Kementerian Perencanaan Pembangunan Nasional/Bappenas"|
                                         geog_rotation$Department=="Arsip Nasional Republik Indonesia "|
                                         geog_rotation$Department=="Badan Informasi Geospasial"|
                                         geog_rotation$Department=="Badan Kependudukan dan Keluarga Berencana Nasional"|
                                         geog_rotation$Department=="Badan Koordinasi Penanaman Modal"|
                                         geog_rotation$Department=="Badan Pengkajian dan Penerapan Teknologi"|
                                         geog_rotation$Department=="Badan Pengawasan Keuangan dan Pembangunan"|
                                         geog_rotation$Department=="Kementerian Agraria dan Tata Ruang/Badan Pertanahan Nasional"|
                                         geog_rotation$Department=="Perpustakaan Nasional RI"|
                                         geog_rotation$Department=="Badan Standardisasi Nasional "|
                                         geog_rotation$Department=="Badan Pengawas Tenaga Nuklir"| 
                                         geog_rotation$Department=="Badan Pengawas Obat dan Makanan "| 
                                         geog_rotation$Department=="Lembaga Ketahanan Nasional RI"|
                                         geog_rotation$Department=="Kepolisian Negara"|
                                         geog_rotation$Department=="MABES TNI"|
                                         geog_rotation$Department=="Badan Meteorologi, Klimatologi dan Geofisika"|
                                         geog_rotation$Department=="Sekretariat Kabinet "|
                                         geog_rotation$Department=="Sekretariat Presiden"|
                                         geog_rotation$Department=="Sekretariat Wakil Presiden"|
                                         geog_rotation$Department=="Sekretariat Militer"|
                                         geog_rotation$Department=="Badan Narkotika Nasional"|
                                         geog_rotation$Department=="Setjen Komisi Pemilihan Umum"|
                                         geog_rotation$Department=="Badan Nasional Penanggulangan Bencana"|
                                         geog_rotation$Department=="Setjen KOMNAS HAM"|
                                         geog_rotation$Department=="Badan Pengusahaan Kawasan Perdagangan Bebas dan Pelabuhan Bebas Batam"|
                                         geog_rotation$Department=="Kepaniteraan dan Sekretariat Jenderal Mahkamah Konstitusi RI"|
                                         geog_rotation$Department=="Setjen Komisi Pemberantasan Korupsi"|
                                         geog_rotation$Department=="Setjen KORPRI"|
                                         geog_rotation$Department=="Sekretariat Jenderal Komisi Yudisial"|
                                         geog_rotation$Department=="Setjen Dewan Perwakilan Daerah"|
                                         geog_rotation$Department=="Badan Nasional Penempatan Perlindungan TKI"|
                                         geog_rotation$Department=="Badan Keamanan Laut RI"|
                                         geog_rotation$Department=="Badan SAR Nasional"|
                                         geog_rotation$Department=="Lembaga Kebijakan Pengadaan Barang/Jasa Pemerintah"|
                                         geog_rotation$Department=="Pusat Pelaporan dan Analisis Transaksi Keuangan"|
                                         geog_rotation$Department=="Ombudsman Republik Indonesia"|
                                         geog_rotation$Department=="TELEVISI REPUBLIK INDONESIA"|
                                         geog_rotation$Department=="RADIO REPUBLIK INDONESIA"|
                                         geog_rotation$Department=="Badan Nasional Pengelola Perbatasan"|
                                         geog_rotation$Department=="Badan Nasional Penanggulangan Terorisme"|
                                         geog_rotation$Department=="Setjen Komisi Pengawas Persaingan Usaha"|
                                         geog_rotation$Department=="Badan Pengawas Pemilihan Umum"|
                                         geog_rotation$Department=="Komisi Aparatur Sipil Negara"|
                                         geog_rotation$Department=="Badan Ekonomi Kreatif"|
                                         geog_rotation$Department=="BUMN"|
                                         geog_rotation$Department=="BUMD"|
                                         geog_rotation$Department=="SWASTA"|
                                         geog_rotation$Department=="Luar Negeri"|
                                         geog_rotation$Department=="KEMENAG TESTING"|
                                         geog_rotation$Department=="Lain-Lain",1,0)


# code islamic minister
geog_rotation$islamic_minister <- 0

#1998-1999 State Minister of Agrarian and horticulture Affairs
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Pertanian"&geog_rotation$years>1997 &geog_rotation$years<2000,1,geog_rotation$islamic_minister)
                                         
#1998-1999 State Minister of Investment
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Badan Koordinasi Penanaman Modal"&geog_rotation$years>1997 &geog_rotation$years<2000,1,geog_rotation$islamic_minister)

# 1999-2001 Minister of Laws and Legislation
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Hukum dan Hak Asasi Manusia"&geog_rotation$years>1998 &geog_rotation$years<2002,1,geog_rotation$islamic_minister)

#1999 Coordinating Minister of People's Welfare and Abolition of Poverty
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Badan Kependudukan dan Keluarga Berencana Nasional"&geog_rotation$years>1998 &geog_rotation$years<2000,1,geog_rotation$islamic_minister)

#1999-2001 State Minister of Cooperatives and Small to Medium Businesses
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Koperasi dan Usaha Kecil dan Menengah"&geog_rotation$years>1998 &geog_rotation$years<2002,1,geog_rotation$islamic_minister)

#2001-2004 Minister of Justice and Human Rights
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Hukum dan Hak Asasi Manusia"&geog_rotation$years>2000 &geog_rotation$years<2005,1,geog_rotation$islamic_minister)

# 2001-2004 Minister of Social Affairs
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Sosial"&geog_rotation$years>2000 &geog_rotation$years<2005,1,geog_rotation$islamic_minister)

# 2001-2004 State Minister of Cooperatives and Small and Medium Businesses
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Koperasi dan Usaha Kecil dan Menengah"&geog_rotation$years>2000 &geog_rotation$years<2005,1,geog_rotation$islamic_minister)

# 2004-2007 State Secretary
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Sekretariat Negara"&geog_rotation$years>2003 &geog_rotation$years<2008,1,geog_rotation$islamic_minister)

# 2004-2009 Minister of Agriculture
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Pertanian"&geog_rotation$years>2003 &geog_rotation$years<2010,1,geog_rotation$islamic_minister)

# 2004-2009 Minister of Forestry
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Lingkungan Hidup dan Kehutanan"&geog_rotation$years>2003 &geog_rotation$years<2010,1,geog_rotation$islamic_minister)

# 2004-2009 Minister of Social Affairs
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Sosial"&geog_rotation$years>2003 &geog_rotation$years<2010,1,geog_rotation$islamic_minister)

# 2004-2009 State Minister for Cooperatives and Small and Medium Enterprises
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Koperasi dan Usaha Kecil dan Menengah"&geog_rotation$years>2003 &geog_rotation$years<2010,1,geog_rotation$islamic_minister)

# 2004-2007 State Minister for State Enterprises
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Badan Usaha Milik Negara"&geog_rotation$years>2003 &geog_rotation$years<2008,1,geog_rotation$islamic_minister)

# 2004 - 2009 State Minister for Public Housing
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Pekerjaan Umum dan Perumahan Rakyat"&geog_rotation$years>2003 &geog_rotation$years<2010,1,geog_rotation$islamic_minister)

# 2004-2009 State Minister for Youth and Sports Affairs (Indonesia)
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Pemuda dan Olahraga"&geog_rotation$years>2003 &geog_rotation$years<2010,1,geog_rotation$islamic_minister)

# 2009-2014 Minister of Agriculture
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Pertanian"&geog_rotation$years>2008 &geog_rotation$years<2015,1,geog_rotation$islamic_minister)

# 2009-2014 Minister of Social Affairs
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Sosial"&geog_rotation$years>2008 &geog_rotation$years<2015,1,geog_rotation$islamic_minister)

# 2009-2014 Minister of Religious Affairs
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Agama"&geog_rotation$years>2008 &geog_rotation$years<2015,1,geog_rotation$islamic_minister)

# 2009-2014 Minister of Communication and Informatics
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Komunikasi dan Informatika"&geog_rotation$years>2008 &geog_rotation$years<2015,1,geog_rotation$islamic_minister)

# 2009-2011 Minister of Research and Technology
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Riset, Teknologi, dan Pendidikan Tinggi"&geog_rotation$years>2008 &geog_rotation$years<2015,1,geog_rotation$islamic_minister)

# 2009-2014 Minister of Public Housing
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Pekerjaan Umum dan Perumahan Rakyat"&geog_rotation$years>2008 &geog_rotation$years<2015,1,geog_rotation$islamic_minister)

# 2014- now Minister of Religious Affairs
geog_rotation$islamic_minister <- ifelse(geog_rotation$Department=="Kementerian Agama"&geog_rotation$years>2013,1,geog_rotation$islamic_minister)



########### Table 2: Gender
dv_list <- c("new_vals","jab_change","promotion")
control_list <- "+gender_cd+umur_skrg+eduname_cd+factor(religion_cd)+yrs_in_bkn"

fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "islamic_minister+islamic_minister*gender_cd",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "islamic_minister+islamic_minister*gender_cd",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "islamic_minister+islamic_minister*gender_cd",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation[years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation[years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla3, data = geog_rotation[years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0 & years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0 &years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0 &years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)








# Get standard errors
se1 <- m1$cse
se2 <- m2$cse
se3 <- m3$cse
se4 <- m4$cse
se5 <- m5$cse
se6 <- m6$cse


# Table:
stargazer(m1,m2,m3,m4,m5,m6,type="latex",notes = "Standard errors are clustered at the individual level.",
          style="qje", 
          title            = "Promotion Analysis: Gender and Islamic Leadership, National Employees",
          covariate.labels = c("Islamic Party Minister",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Years in Civil Service",
                               "Female*Islamic Party Minister"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Promotion","Promotion"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_a16_islamic_minister_gender.tex"
)
# save(m1,m2,m3,m4,m5,m6, file = "~/paper_table2_national_islam_full_models.rdata")



########### Table: Religion
fmla1 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "islamic_minister+factor(religion_cd)*islamic_minister",
                           paste(control_list),
                           paste("| prov_tempat_lahir+instansi+years | 0 | pns_id")))

fmla2 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "islamic_minister+factor(religion_cd)*islamic_minister",
                           paste(control_list),
                           paste("| pns_id+instansi+years | 0 | pns_id")))

fmla3 <-  as.formula(paste(paste(dv_list[3],"~"),
                           "islamic_minister+factor(religion_cd)*islamic_minister",
                           paste(control_list),
                           paste("| pns_id+instansi+golongan_id+years | 0 | pns_id")))

# Estimation
m1 <- felm(fmla1, data = geog_rotation[years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m2 <- felm(fmla2, data = geog_rotation[years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m3 <- felm(fmla3, data = geog_rotation[years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m4 <- felm(fmla1, data = geog_rotation[post_dem_hire == 0 & years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m5 <- felm(fmla2, data = geog_rotation[post_dem_hire == 0 &years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)
m6 <- felm(fmla3, data = geog_rotation[post_dem_hire == 0 &years > 1998 & non_local == 1], keepX = FALSE, keepCX = FALSE)








# Get standard errors
se1 <- m1$cse
se2 <- m2$cse
se3 <- m3$cse
se4 <- m4$cse
se5 <- m5$cse
se6 <- m6$cse


# Table:
stargazer(m1,m2,m3,m4,m5,m6,type="latex",notes = "Standard errors are clustered at the individual level.",
          style="qje", 
          title            = "Promotion Analysis: Religion and Islamic Leadership, National Employees",
          covariate.labels = c("Islamic Party Minister",
                               "Protestant",
                               "Catholic",
                               "Buddhist",
                               "Hindu",
                               "Confucian",
                               "Other",
                               "Female",
                               "Age",
                               "Education: Junior High",
                               "Education: Senior High",
                               "Education: Diploma I/II/III",
                               "Education: Diploma IV/S1",
                               "Education: Post-Graduate",
                               "Years in Civil Service",
                               "Protestant*Islamic Party Minister",
                               "Catholic*Islamic Party Minister",
                               "Buddhist*Islamic Party Minister",
                               "Hindu*Islamic Party Minister",
                               "Confucian*Islamic Party Minister",
                               "Other*Islamic Party Minister"),
          dep.var.caption  = "Echelon Level",
          dep.var.labels.include = FALSE,
          column.labels   = c("Promotion","Promotion","Promotion","Promotion","Promotion","Promotion"),
          se=list(se1,se2,se3,se4,se5,se6),
          add.lines = list(c("Sample", "Full", "Full","Full","Pre-1999","Pre-1999", "Pre-1999"),
                           c("Department FE", "Yes", "Yes","Yes","Yes","Yes", "Yes"),
                           c("Province of Birth FE", "Yes", "No","No","Yes", "No","No"),
                           c("Individual FE", "No","Yes","Yes","No","Yes","Yes"),
                           c("Golongan FE", "No","No","Yes","No","No","Yes"),
                           c("Year FE","Yes","Yes","Yes","Yes","Yes","Yes")),
          digits=2,
          out="~/appendix_table_a17_islamic_minister_religion.tex"
)
# save(m1,m2,m3,m4,m5,m6, file = "~/paper_table3_national_islam_full_models.rdata")


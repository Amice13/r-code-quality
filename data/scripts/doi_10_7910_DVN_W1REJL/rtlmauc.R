# RADIO RTLM: NON-BINARY ROC ANALYSIS

# Gordon Danning
# 2018-03-02

# Load required packages

install.packages("readstata13")
library(readstata13)

install.packages("https://cran.cnr.berkeley.edu/src/contrib/Archive/nonbinROC/nonbinROC_1.0.1.tar.gz", repos=NULL)
library(nonbinROC)


## import test dataframes from Stata ##

temp = list.files(pattern="*.dta")
for (i in 1:length(temp)) assign(temp[i], read.dta13(temp[i]))


## calculate non-binary ROC measure ##

# all violence #

Results12a1 <-as.data.frame(contROC(testa1.dta$lpro12x, testa1.dta$drottmod12x, testa1.dta$nortlm12x))
Results12a2 <-as.data.frame(contROC(testa2.dta$lpro12x, testa2.dta$drottmod12x, testa2.dta$nortlm12x))
Results12a3 <-as.data.frame(contROC(testa3.dta$lpro12x, testa3.dta$drottmod12x, testa3.dta$nortlm12x))
Results12a4 <-as.data.frame(contROC(testa4.dta$lpro12x, testa4.dta$drottmod12x, testa4.dta$nortlm12x))
Results12a5 <-as.data.frame(contROC(testa5.dta$lpro12x, testa5.dta$drottmod12x, testa5.dta$nortlm12x))
Results12a6 <-as.data.frame(contROC(testa6.dta$lpro12x, testa6.dta$drottmod12x, testa6.dta$nortlm12x))
Results12a7 <-as.data.frame(contROC(testa7.dta$lpro12x, testa7.dta$drottmod12x, testa7.dta$nortlm12x))
Results12a8 <-as.data.frame(contROC(testa8.dta$lpro12x, testa8.dta$drottmod12x, testa8.dta$nortlm12x))
Results12a9 <-as.data.frame(contROC(testa9.dta$lpro12x, testa9.dta$drottmod12x, testa9.dta$nortlm12x))
Results12a10 <-as.data.frame(contROC(testa10.dta$lpro12x, testa10.dta$drottmod12x, testa10.dta$nortlm12x))
Results12a11 <-as.data.frame(contROC(testa11.dta$lpro12x, testa11.dta$drottmod12x, testa11.dta$nortlm12x))
Results12a12 <-as.data.frame(contROC(testa12.dta$lpro12x, testa12.dta$drottmod12x, testa12.dta$nortlm12x))
Results12a13 <-as.data.frame(contROC(testa13.dta$lpro12x, testa13.dta$drottmod12x, testa13.dta$nortlm12x))
Results12a14 <-as.data.frame(contROC(testa14.dta$lpro12x, testa14.dta$drottmod12x, testa14.dta$nortlm12x))
Results12a15 <-as.data.frame(contROC(testa15.dta$lpro12x, testa15.dta$drottmod12x, testa15.dta$nortlm12x))
Results12a16 <-as.data.frame(contROC(testa16.dta$lpro12x, testa16.dta$drottmod12x, testa16.dta$nortlm12x))
Results12a17 <-as.data.frame(contROC(testa17.dta$lpro12x, testa17.dta$drottmod12x, testa17.dta$nortlm12x))
Results12a18 <-as.data.frame(contROC(testa18.dta$lpro12x, testa18.dta$drottmod12x, testa18.dta$nortlm12x))
Results12a19 <-as.data.frame(contROC(testa19.dta$lpro12x, testa19.dta$drottmod12x, testa19.dta$nortlm12x))
Results12a20 <-as.data.frame(contROC(testa20.dta$lpro12x, testa20.dta$drottmod12x, testa20.dta$nortlm12x))
Results12a21 <-as.data.frame(contROC(testa21.dta$lpro12x, testa21.dta$drottmod12x, testa21.dta$nortlm12x))
Results12a22 <-as.data.frame(contROC(testa22.dta$lpro12x, testa22.dta$drottmod12x, testa22.dta$nortlm12x))
Results12a23 <-as.data.frame(contROC(testa23.dta$lpro12x, testa23.dta$drottmod12x, testa23.dta$nortlm12x))
Results12a24 <-as.data.frame(contROC(testa24.dta$lpro12x, testa24.dta$drottmod12x, testa24.dta$nortlm12x))
Results12a25 <-as.data.frame(contROC(testa25.dta$lpro12x, testa25.dta$drottmod12x, testa25.dta$nortlm12x))
Results12a26 <-as.data.frame(contROC(testa26.dta$lpro12x, testa26.dta$drottmod12x, testa26.dta$nortlm12x))
Results12a27 <-as.data.frame(contROC(testa27.dta$lpro12x, testa27.dta$drottmod12x, testa27.dta$nortlm12x))
Results12a28 <-as.data.frame(contROC(testa28.dta$lpro12x, testa28.dta$drottmod12x, testa28.dta$nortlm12x))
Results12a29 <-as.data.frame(contROC(testa29.dta$lpro12x, testa29.dta$drottmod12x, testa29.dta$nortlm12x))
Results12a30 <-as.data.frame(contROC(testa30.dta$lpro12x, testa30.dta$drottmod12x, testa30.dta$nortlm12x))
Results12a31 <-as.data.frame(contROC(testa31.dta$lpro12x, testa31.dta$drottmod12x, testa31.dta$nortlm12x))
Results12a32 <-as.data.frame(contROC(testa32.dta$lpro12x, testa32.dta$drottmod12x, testa32.dta$nortlm12x))
Results12a33 <-as.data.frame(contROC(testa33.dta$lpro12x, testa33.dta$drottmod12x, testa33.dta$nortlm12x))
Results12a34 <-as.data.frame(contROC(testa34.dta$lpro12x, testa34.dta$drottmod12x, testa34.dta$nortlm12x))
Results12a35 <-as.data.frame(contROC(testa35.dta$lpro12x, testa35.dta$drottmod12x, testa35.dta$nortlm12x))
Results12a36 <-as.data.frame(contROC(testa36.dta$lpro12x, testa36.dta$drottmod12x, testa36.dta$nortlm12x))
Results12a37 <-as.data.frame(contROC(testa37.dta$lpro12x, testa37.dta$drottmod12x, testa37.dta$nortlm12x))
Results12a38 <-as.data.frame(contROC(testa38.dta$lpro12x, testa38.dta$drottmod12x, testa38.dta$nortlm12x))
Results12a39 <-as.data.frame(contROC(testa39.dta$lpro12x, testa39.dta$drottmod12x, testa39.dta$nortlm12x))
Results12a40 <-as.data.frame(contROC(testa40.dta$lpro12x, testa40.dta$drottmod12x, testa40.dta$nortlm12x))
Results12a41 <-as.data.frame(contROC(testa41.dta$lpro12x, testa41.dta$drottmod12x, testa41.dta$nortlm12x))
Results12a42 <-as.data.frame(contROC(testa42.dta$lpro12x, testa42.dta$drottmod12x, testa42.dta$nortlm12x))
Results12a43 <-as.data.frame(contROC(testa43.dta$lpro12x, testa43.dta$drottmod12x, testa43.dta$nortlm12x))
Results12a44 <-as.data.frame(contROC(testa44.dta$lpro12x, testa44.dta$drottmod12x, testa44.dta$nortlm12x))
Results12a45 <-as.data.frame(contROC(testa45.dta$lpro12x, testa45.dta$drottmod12x, testa45.dta$nortlm12x))
Results12a46 <-as.data.frame(contROC(testa46.dta$lpro12x, testa46.dta$drottmod12x, testa46.dta$nortlm12x))
Results12a47 <-as.data.frame(contROC(testa47.dta$lpro12x, testa47.dta$drottmod12x, testa47.dta$nortlm12x))
Results12a48 <-as.data.frame(contROC(testa48.dta$lpro12x, testa48.dta$drottmod12x, testa48.dta$nortlm12x))
Results12a49 <-as.data.frame(contROC(testa49.dta$lpro12x, testa49.dta$drottmod12x, testa49.dta$nortlm12x))
Results12a50 <-as.data.frame(contROC(testa50.dta$lpro12x, testa50.dta$drottmod12x, testa50.dta$nortlm12x))
Results12a51 <-as.data.frame(contROC(testa51.dta$lpro12x, testa51.dta$drottmod12x, testa51.dta$nortlm12x))
Results12a52 <-as.data.frame(contROC(testa52.dta$lpro12x, testa52.dta$drottmod12x, testa52.dta$nortlm12x))
Results12a53 <-as.data.frame(contROC(testa53.dta$lpro12x, testa53.dta$drottmod12x, testa53.dta$nortlm12x))
Results12a54 <-as.data.frame(contROC(testa54.dta$lpro12x, testa54.dta$drottmod12x, testa54.dta$nortlm12x))
Results12a55 <-as.data.frame(contROC(testa55.dta$lpro12x, testa55.dta$drottmod12x, testa55.dta$nortlm12x))
Results12a56 <-as.data.frame(contROC(testa56.dta$lpro12x, testa56.dta$drottmod12x, testa56.dta$nortlm12x))
Results12a57 <-as.data.frame(contROC(testa57.dta$lpro12x, testa57.dta$drottmod12x, testa57.dta$nortlm12x))
Results12a58 <-as.data.frame(contROC(testa58.dta$lpro12x, testa58.dta$drottmod12x, testa58.dta$nortlm12x))
Results12a59 <-as.data.frame(contROC(testa59.dta$lpro12x, testa59.dta$drottmod12x, testa59.dta$nortlm12x))
Results12a60 <-as.data.frame(contROC(testa60.dta$lpro12x, testa60.dta$drottmod12x, testa60.dta$nortlm12x))
Results12a61 <-as.data.frame(contROC(testa61.dta$lpro12x, testa61.dta$drottmod12x, testa61.dta$nortlm12x))
Results12a62 <-as.data.frame(contROC(testa62.dta$lpro12x, testa62.dta$drottmod12x, testa62.dta$nortlm12x))
Results12a63 <-as.data.frame(contROC(testa63.dta$lpro12x, testa63.dta$drottmod12x, testa63.dta$nortlm12x))
Results12a64 <-as.data.frame(contROC(testa64.dta$lpro12x, testa64.dta$drottmod12x, testa64.dta$nortlm12x))
Results12a65 <-as.data.frame(contROC(testa65.dta$lpro12x, testa65.dta$drottmod12x, testa65.dta$nortlm12x))
Results12a66 <-as.data.frame(contROC(testa66.dta$lpro12x, testa66.dta$drottmod12x, testa66.dta$nortlm12x))
Results12a67 <-as.data.frame(contROC(testa67.dta$lpro12x, testa67.dta$drottmod12x, testa67.dta$nortlm12x))
Results12a68 <-as.data.frame(contROC(testa68.dta$lpro12x, testa68.dta$drottmod12x, testa68.dta$nortlm12x))
Results12a69 <-as.data.frame(contROC(testa69.dta$lpro12x, testa69.dta$drottmod12x, testa69.dta$nortlm12x))
Results12a70 <-as.data.frame(contROC(testa70.dta$lpro12x, testa70.dta$drottmod12x, testa70.dta$nortlm12x))
Results12a71 <-as.data.frame(contROC(testa71.dta$lpro12x, testa71.dta$drottmod12x, testa71.dta$nortlm12x))
Results12a72 <-as.data.frame(contROC(testa72.dta$lpro12x, testa72.dta$drottmod12x, testa72.dta$nortlm12x))
Results12a73 <-as.data.frame(contROC(testa73.dta$lpro12x, testa73.dta$drottmod12x, testa73.dta$nortlm12x))
Results12a74 <-as.data.frame(contROC(testa74.dta$lpro12x, testa74.dta$drottmod12x, testa74.dta$nortlm12x))
Results12a75 <-as.data.frame(contROC(testa75.dta$lpro12x, testa75.dta$drottmod12x, testa75.dta$nortlm12x))
Results12a76 <-as.data.frame(contROC(testa76.dta$lpro12x, testa76.dta$drottmod12x, testa76.dta$nortlm12x))
Results12a77 <-as.data.frame(contROC(testa77.dta$lpro12x, testa77.dta$drottmod12x, testa77.dta$nortlm12x))
Results12a78 <-as.data.frame(contROC(testa78.dta$lpro12x, testa78.dta$drottmod12x, testa78.dta$nortlm12x))
Results12a79 <-as.data.frame(contROC(testa79.dta$lpro12x, testa79.dta$drottmod12x, testa79.dta$nortlm12x))
Results12a80 <-as.data.frame(contROC(testa80.dta$lpro12x, testa80.dta$drottmod12x, testa80.dta$nortlm12x))
Results12a81 <-as.data.frame(contROC(testa81.dta$lpro12x, testa81.dta$drottmod12x, testa81.dta$nortlm12x))
Results12a82 <-as.data.frame(contROC(testa82.dta$lpro12x, testa82.dta$drottmod12x, testa82.dta$nortlm12x))
Results12a83 <-as.data.frame(contROC(testa83.dta$lpro12x, testa83.dta$drottmod12x, testa83.dta$nortlm12x))
Results12a84 <-as.data.frame(contROC(testa84.dta$lpro12x, testa84.dta$drottmod12x, testa84.dta$nortlm12x))
Results12a85 <-as.data.frame(contROC(testa85.dta$lpro12x, testa85.dta$drottmod12x, testa85.dta$nortlm12x))
Results12a86 <-as.data.frame(contROC(testa86.dta$lpro12x, testa86.dta$drottmod12x, testa86.dta$nortlm12x))
Results12a87 <-as.data.frame(contROC(testa87.dta$lpro12x, testa87.dta$drottmod12x, testa87.dta$nortlm12x))
Results12a88 <-as.data.frame(contROC(testa88.dta$lpro12x, testa88.dta$drottmod12x, testa88.dta$nortlm12x))
Results12a89 <-as.data.frame(contROC(testa89.dta$lpro12x, testa89.dta$drottmod12x, testa89.dta$nortlm12x))
Results12a90 <-as.data.frame(contROC(testa90.dta$lpro12x, testa90.dta$drottmod12x, testa90.dta$nortlm12x))
Results12a91 <-as.data.frame(contROC(testa91.dta$lpro12x, testa91.dta$drottmod12x, testa91.dta$nortlm12x))
Results12a92 <-as.data.frame(contROC(testa92.dta$lpro12x, testa92.dta$drottmod12x, testa92.dta$nortlm12x))
Results12a93 <-as.data.frame(contROC(testa93.dta$lpro12x, testa93.dta$drottmod12x, testa93.dta$nortlm12x))
Results12a94 <-as.data.frame(contROC(testa94.dta$lpro12x, testa94.dta$drottmod12x, testa94.dta$nortlm12x))
Results12a95 <-as.data.frame(contROC(testa95.dta$lpro12x, testa95.dta$drottmod12x, testa95.dta$nortlm12x))
Results12a96 <-as.data.frame(contROC(testa96.dta$lpro12x, testa96.dta$drottmod12x, testa96.dta$nortlm12x))
Results12a97 <-as.data.frame(contROC(testa97.dta$lpro12x, testa97.dta$drottmod12x, testa97.dta$nortlm12x))
Results12a98 <-as.data.frame(contROC(testa98.dta$lpro12x, testa98.dta$drottmod12x, testa98.dta$nortlm12x))
Results12a99 <-as.data.frame(contROC(testa99.dta$lpro12x, testa99.dta$drottmod12x, testa99.dta$nortlm12x))
Results12a100 <-as.data.frame(contROC(testa100.dta$lpro12x, testa100.dta$drottmod12x, testa100.dta$nortlm12x))

Results12b1 <-as.data.frame(contROC(testb1.dta$lpro12x, testb1.dta$drottmod12x, testb1.dta$nortlm12x))
Results12b2 <-as.data.frame(contROC(testb2.dta$lpro12x, testb2.dta$drottmod12x, testb2.dta$nortlm12x))
Results12b3 <-as.data.frame(contROC(testb3.dta$lpro12x, testb3.dta$drottmod12x, testb3.dta$nortlm12x))
Results12b4 <-as.data.frame(contROC(testb4.dta$lpro12x, testb4.dta$drottmod12x, testb4.dta$nortlm12x))
Results12b5 <-as.data.frame(contROC(testb5.dta$lpro12x, testb5.dta$drottmod12x, testb5.dta$nortlm12x))
Results12b6 <-as.data.frame(contROC(testb6.dta$lpro12x, testb6.dta$drottmod12x, testb6.dta$nortlm12x))
Results12b7 <-as.data.frame(contROC(testb7.dta$lpro12x, testb7.dta$drottmod12x, testb7.dta$nortlm12x))
Results12b8 <-as.data.frame(contROC(testb8.dta$lpro12x, testb8.dta$drottmod12x, testb8.dta$nortlm12x))
Results12b9 <-as.data.frame(contROC(testb9.dta$lpro12x, testb9.dta$drottmod12x, testb9.dta$nortlm12x))
Results12b10 <-as.data.frame(contROC(testb10.dta$lpro12x, testb10.dta$drottmod12x, testb10.dta$nortlm12x))
Results12b11 <-as.data.frame(contROC(testb11.dta$lpro12x, testb11.dta$drottmod12x, testb11.dta$nortlm12x))
Results12b12 <-as.data.frame(contROC(testb12.dta$lpro12x, testb12.dta$drottmod12x, testb12.dta$nortlm12x))
Results12b13 <-as.data.frame(contROC(testb13.dta$lpro12x, testb13.dta$drottmod12x, testb13.dta$nortlm12x))
Results12b14 <-as.data.frame(contROC(testb14.dta$lpro12x, testb14.dta$drottmod12x, testb14.dta$nortlm12x))
Results12b15 <-as.data.frame(contROC(testb15.dta$lpro12x, testb15.dta$drottmod12x, testb15.dta$nortlm12x))
Results12b16 <-as.data.frame(contROC(testb16.dta$lpro12x, testb16.dta$drottmod12x, testb16.dta$nortlm12x))
Results12b17 <-as.data.frame(contROC(testb17.dta$lpro12x, testb17.dta$drottmod12x, testb17.dta$nortlm12x))
Results12b18 <-as.data.frame(contROC(testb18.dta$lpro12x, testb18.dta$drottmod12x, testb18.dta$nortlm12x))
Results12b19 <-as.data.frame(contROC(testb19.dta$lpro12x, testb19.dta$drottmod12x, testb19.dta$nortlm12x))
Results12b20 <-as.data.frame(contROC(testb20.dta$lpro12x, testb20.dta$drottmod12x, testb20.dta$nortlm12x))
Results12b21 <-as.data.frame(contROC(testb21.dta$lpro12x, testb21.dta$drottmod12x, testb21.dta$nortlm12x))
Results12b22 <-as.data.frame(contROC(testb22.dta$lpro12x, testb22.dta$drottmod12x, testb22.dta$nortlm12x))
Results12b23 <-as.data.frame(contROC(testb23.dta$lpro12x, testb23.dta$drottmod12x, testb23.dta$nortlm12x))
Results12b24 <-as.data.frame(contROC(testb24.dta$lpro12x, testb24.dta$drottmod12x, testb24.dta$nortlm12x))
Results12b25 <-as.data.frame(contROC(testb25.dta$lpro12x, testb25.dta$drottmod12x, testb25.dta$nortlm12x))
Results12b26 <-as.data.frame(contROC(testb26.dta$lpro12x, testb26.dta$drottmod12x, testb26.dta$nortlm12x))
Results12b27 <-as.data.frame(contROC(testb27.dta$lpro12x, testb27.dta$drottmod12x, testb27.dta$nortlm12x))
Results12b28 <-as.data.frame(contROC(testb28.dta$lpro12x, testb28.dta$drottmod12x, testb28.dta$nortlm12x))
Results12b29 <-as.data.frame(contROC(testb29.dta$lpro12x, testb29.dta$drottmod12x, testb29.dta$nortlm12x))
Results12b30 <-as.data.frame(contROC(testb30.dta$lpro12x, testb30.dta$drottmod12x, testb30.dta$nortlm12x))
Results12b31 <-as.data.frame(contROC(testb31.dta$lpro12x, testb31.dta$drottmod12x, testb31.dta$nortlm12x))
Results12b32 <-as.data.frame(contROC(testb32.dta$lpro12x, testb32.dta$drottmod12x, testb32.dta$nortlm12x))
Results12b33 <-as.data.frame(contROC(testb33.dta$lpro12x, testb33.dta$drottmod12x, testb33.dta$nortlm12x))
Results12b34 <-as.data.frame(contROC(testb34.dta$lpro12x, testb34.dta$drottmod12x, testb34.dta$nortlm12x))
Results12b35 <-as.data.frame(contROC(testb35.dta$lpro12x, testb35.dta$drottmod12x, testb35.dta$nortlm12x))
Results12b36 <-as.data.frame(contROC(testb36.dta$lpro12x, testb36.dta$drottmod12x, testb36.dta$nortlm12x))
Results12b37 <-as.data.frame(contROC(testb37.dta$lpro12x, testb37.dta$drottmod12x, testb37.dta$nortlm12x))
Results12b38 <-as.data.frame(contROC(testb38.dta$lpro12x, testb38.dta$drottmod12x, testb38.dta$nortlm12x))
Results12b39 <-as.data.frame(contROC(testb39.dta$lpro12x, testb39.dta$drottmod12x, testb39.dta$nortlm12x))
Results12b40 <-as.data.frame(contROC(testb40.dta$lpro12x, testb40.dta$drottmod12x, testb40.dta$nortlm12x))
Results12b41 <-as.data.frame(contROC(testb41.dta$lpro12x, testb41.dta$drottmod12x, testb41.dta$nortlm12x))
Results12b42 <-as.data.frame(contROC(testb42.dta$lpro12x, testb42.dta$drottmod12x, testb42.dta$nortlm12x))
Results12b43 <-as.data.frame(contROC(testb43.dta$lpro12x, testb43.dta$drottmod12x, testb43.dta$nortlm12x))
Results12b44 <-as.data.frame(contROC(testb44.dta$lpro12x, testb44.dta$drottmod12x, testb44.dta$nortlm12x))
Results12b45 <-as.data.frame(contROC(testb45.dta$lpro12x, testb45.dta$drottmod12x, testb45.dta$nortlm12x))
Results12b46 <-as.data.frame(contROC(testb46.dta$lpro12x, testb46.dta$drottmod12x, testb46.dta$nortlm12x))
Results12b47 <-as.data.frame(contROC(testb47.dta$lpro12x, testb47.dta$drottmod12x, testb47.dta$nortlm12x))
Results12b48 <-as.data.frame(contROC(testb48.dta$lpro12x, testb48.dta$drottmod12x, testb48.dta$nortlm12x))
Results12b49 <-as.data.frame(contROC(testb49.dta$lpro12x, testb49.dta$drottmod12x, testb49.dta$nortlm12x))
Results12b50 <-as.data.frame(contROC(testb50.dta$lpro12x, testb50.dta$drottmod12x, testb50.dta$nortlm12x))
Results12b51 <-as.data.frame(contROC(testb51.dta$lpro12x, testb51.dta$drottmod12x, testb51.dta$nortlm12x))
Results12b52 <-as.data.frame(contROC(testb52.dta$lpro12x, testb52.dta$drottmod12x, testb52.dta$nortlm12x))
Results12b53 <-as.data.frame(contROC(testb53.dta$lpro12x, testb53.dta$drottmod12x, testb53.dta$nortlm12x))
Results12b54 <-as.data.frame(contROC(testb54.dta$lpro12x, testb54.dta$drottmod12x, testb54.dta$nortlm12x))
Results12b55 <-as.data.frame(contROC(testb55.dta$lpro12x, testb55.dta$drottmod12x, testb55.dta$nortlm12x))
Results12b56 <-as.data.frame(contROC(testb56.dta$lpro12x, testb56.dta$drottmod12x, testb56.dta$nortlm12x))
Results12b57 <-as.data.frame(contROC(testb57.dta$lpro12x, testb57.dta$drottmod12x, testb57.dta$nortlm12x))
Results12b58 <-as.data.frame(contROC(testb58.dta$lpro12x, testb58.dta$drottmod12x, testb58.dta$nortlm12x))
Results12b59 <-as.data.frame(contROC(testb59.dta$lpro12x, testb59.dta$drottmod12x, testb59.dta$nortlm12x))
Results12b60 <-as.data.frame(contROC(testb60.dta$lpro12x, testb60.dta$drottmod12x, testb60.dta$nortlm12x))
Results12b61 <-as.data.frame(contROC(testb61.dta$lpro12x, testb61.dta$drottmod12x, testb61.dta$nortlm12x))
Results12b62 <-as.data.frame(contROC(testb62.dta$lpro12x, testb62.dta$drottmod12x, testb62.dta$nortlm12x))
Results12b63 <-as.data.frame(contROC(testb63.dta$lpro12x, testb63.dta$drottmod12x, testb63.dta$nortlm12x))
Results12b64 <-as.data.frame(contROC(testb64.dta$lpro12x, testb64.dta$drottmod12x, testb64.dta$nortlm12x))
Results12b65 <-as.data.frame(contROC(testb65.dta$lpro12x, testb65.dta$drottmod12x, testb65.dta$nortlm12x))
Results12b66 <-as.data.frame(contROC(testb66.dta$lpro12x, testb66.dta$drottmod12x, testb66.dta$nortlm12x))
Results12b67 <-as.data.frame(contROC(testb67.dta$lpro12x, testb67.dta$drottmod12x, testb67.dta$nortlm12x))
Results12b68 <-as.data.frame(contROC(testb68.dta$lpro12x, testb68.dta$drottmod12x, testb68.dta$nortlm12x))
Results12b69 <-as.data.frame(contROC(testb69.dta$lpro12x, testb69.dta$drottmod12x, testb69.dta$nortlm12x))
Results12b70 <-as.data.frame(contROC(testb70.dta$lpro12x, testb70.dta$drottmod12x, testb70.dta$nortlm12x))
Results12b71 <-as.data.frame(contROC(testb71.dta$lpro12x, testb71.dta$drottmod12x, testb71.dta$nortlm12x))
Results12b72 <-as.data.frame(contROC(testb72.dta$lpro12x, testb72.dta$drottmod12x, testb72.dta$nortlm12x))
Results12b73 <-as.data.frame(contROC(testb73.dta$lpro12x, testb73.dta$drottmod12x, testb73.dta$nortlm12x))
Results12b74 <-as.data.frame(contROC(testb74.dta$lpro12x, testb74.dta$drottmod12x, testb74.dta$nortlm12x))
Results12b75 <-as.data.frame(contROC(testb75.dta$lpro12x, testb75.dta$drottmod12x, testb75.dta$nortlm12x))
Results12b76 <-as.data.frame(contROC(testb76.dta$lpro12x, testb76.dta$drottmod12x, testb76.dta$nortlm12x))
Results12b77 <-as.data.frame(contROC(testb77.dta$lpro12x, testb77.dta$drottmod12x, testb77.dta$nortlm12x))
Results12b78 <-as.data.frame(contROC(testb78.dta$lpro12x, testb78.dta$drottmod12x, testb78.dta$nortlm12x))
Results12b79 <-as.data.frame(contROC(testb79.dta$lpro12x, testb79.dta$drottmod12x, testb79.dta$nortlm12x))
Results12b80 <-as.data.frame(contROC(testb80.dta$lpro12x, testb80.dta$drottmod12x, testb80.dta$nortlm12x))
Results12b81 <-as.data.frame(contROC(testb81.dta$lpro12x, testb81.dta$drottmod12x, testb81.dta$nortlm12x))
Results12b82 <-as.data.frame(contROC(testb82.dta$lpro12x, testb82.dta$drottmod12x, testb82.dta$nortlm12x))
Results12b83 <-as.data.frame(contROC(testb83.dta$lpro12x, testb83.dta$drottmod12x, testb83.dta$nortlm12x))
Results12b84 <-as.data.frame(contROC(testb84.dta$lpro12x, testb84.dta$drottmod12x, testb84.dta$nortlm12x))
Results12b85 <-as.data.frame(contROC(testb85.dta$lpro12x, testb85.dta$drottmod12x, testb85.dta$nortlm12x))
Results12b86 <-as.data.frame(contROC(testb86.dta$lpro12x, testb86.dta$drottmod12x, testb86.dta$nortlm12x))
Results12b87 <-as.data.frame(contROC(testb87.dta$lpro12x, testb87.dta$drottmod12x, testb87.dta$nortlm12x))
Results12b88 <-as.data.frame(contROC(testb88.dta$lpro12x, testb88.dta$drottmod12x, testb88.dta$nortlm12x))
Results12b89 <-as.data.frame(contROC(testb89.dta$lpro12x, testb89.dta$drottmod12x, testb89.dta$nortlm12x))
Results12b90 <-as.data.frame(contROC(testb90.dta$lpro12x, testb90.dta$drottmod12x, testb90.dta$nortlm12x))
Results12b91 <-as.data.frame(contROC(testb91.dta$lpro12x, testb91.dta$drottmod12x, testb91.dta$nortlm12x))
Results12b92 <-as.data.frame(contROC(testb92.dta$lpro12x, testb92.dta$drottmod12x, testb92.dta$nortlm12x))
Results12b93 <-as.data.frame(contROC(testb93.dta$lpro12x, testb93.dta$drottmod12x, testb93.dta$nortlm12x))
Results12b94 <-as.data.frame(contROC(testb94.dta$lpro12x, testb94.dta$drottmod12x, testb94.dta$nortlm12x))
Results12b95 <-as.data.frame(contROC(testb95.dta$lpro12x, testb95.dta$drottmod12x, testb95.dta$nortlm12x))
Results12b96 <-as.data.frame(contROC(testb96.dta$lpro12x, testb96.dta$drottmod12x, testb96.dta$nortlm12x))
Results12b97 <-as.data.frame(contROC(testb97.dta$lpro12x, testb97.dta$drottmod12x, testb97.dta$nortlm12x))
Results12b98 <-as.data.frame(contROC(testb98.dta$lpro12x, testb98.dta$drottmod12x, testb98.dta$nortlm12x))
Results12b99 <-as.data.frame(contROC(testb99.dta$lpro12x, testb99.dta$drottmod12x, testb99.dta$nortlm12x))
Results12b100 <-as.data.frame(contROC(testb100.dta$lpro12x, testb100.dta$drottmod12x, testb100.dta$nortlm12x))

Results12c1 <-as.data.frame(contROC(testc1.dta$lpro12x, testc1.dta$drottmod12x, testc1.dta$nortlm12x))
Results12c2 <-as.data.frame(contROC(testc2.dta$lpro12x, testc2.dta$drottmod12x, testc2.dta$nortlm12x))
Results12c3 <-as.data.frame(contROC(testc3.dta$lpro12x, testc3.dta$drottmod12x, testc3.dta$nortlm12x))
Results12c4 <-as.data.frame(contROC(testc4.dta$lpro12x, testc4.dta$drottmod12x, testc4.dta$nortlm12x))
Results12c5 <-as.data.frame(contROC(testc5.dta$lpro12x, testc5.dta$drottmod12x, testc5.dta$nortlm12x))
Results12c6 <-as.data.frame(contROC(testc6.dta$lpro12x, testc6.dta$drottmod12x, testc6.dta$nortlm12x))
Results12c7 <-as.data.frame(contROC(testc7.dta$lpro12x, testc7.dta$drottmod12x, testc7.dta$nortlm12x))
Results12c8 <-as.data.frame(contROC(testc8.dta$lpro12x, testc8.dta$drottmod12x, testc8.dta$nortlm12x))
Results12c9 <-as.data.frame(contROC(testc9.dta$lpro12x, testc9.dta$drottmod12x, testc9.dta$nortlm12x))
Results12c10 <-as.data.frame(contROC(testc10.dta$lpro12x, testc10.dta$drottmod12x, testc10.dta$nortlm12x))
Results12c11 <-as.data.frame(contROC(testc11.dta$lpro12x, testc11.dta$drottmod12x, testc11.dta$nortlm12x))
Results12c12 <-as.data.frame(contROC(testc12.dta$lpro12x, testc12.dta$drottmod12x, testc12.dta$nortlm12x))
Results12c13 <-as.data.frame(contROC(testc13.dta$lpro12x, testc13.dta$drottmod12x, testc13.dta$nortlm12x))
Results12c14 <-as.data.frame(contROC(testc14.dta$lpro12x, testc14.dta$drottmod12x, testc14.dta$nortlm12x))
Results12c15 <-as.data.frame(contROC(testc15.dta$lpro12x, testc15.dta$drottmod12x, testc15.dta$nortlm12x))
Results12c16 <-as.data.frame(contROC(testc16.dta$lpro12x, testc16.dta$drottmod12x, testc16.dta$nortlm12x))
Results12c17 <-as.data.frame(contROC(testc17.dta$lpro12x, testc17.dta$drottmod12x, testc17.dta$nortlm12x))
Results12c18 <-as.data.frame(contROC(testc18.dta$lpro12x, testc18.dta$drottmod12x, testc18.dta$nortlm12x))
Results12c19 <-as.data.frame(contROC(testc19.dta$lpro12x, testc19.dta$drottmod12x, testc19.dta$nortlm12x))
Results12c20 <-as.data.frame(contROC(testc20.dta$lpro12x, testc20.dta$drottmod12x, testc20.dta$nortlm12x))
Results12c21 <-as.data.frame(contROC(testc21.dta$lpro12x, testc21.dta$drottmod12x, testc21.dta$nortlm12x))
Results12c22 <-as.data.frame(contROC(testc22.dta$lpro12x, testc22.dta$drottmod12x, testc22.dta$nortlm12x))
Results12c23 <-as.data.frame(contROC(testc23.dta$lpro12x, testc23.dta$drottmod12x, testc23.dta$nortlm12x))
Results12c24 <-as.data.frame(contROC(testc24.dta$lpro12x, testc24.dta$drottmod12x, testc24.dta$nortlm12x))
Results12c25 <-as.data.frame(contROC(testc25.dta$lpro12x, testc25.dta$drottmod12x, testc25.dta$nortlm12x))
Results12c26 <-as.data.frame(contROC(testc26.dta$lpro12x, testc26.dta$drottmod12x, testc26.dta$nortlm12x))
Results12c27 <-as.data.frame(contROC(testc27.dta$lpro12x, testc27.dta$drottmod12x, testc27.dta$nortlm12x))
Results12c28 <-as.data.frame(contROC(testc28.dta$lpro12x, testc28.dta$drottmod12x, testc28.dta$nortlm12x))
Results12c29 <-as.data.frame(contROC(testc29.dta$lpro12x, testc29.dta$drottmod12x, testc29.dta$nortlm12x))
Results12c30 <-as.data.frame(contROC(testc30.dta$lpro12x, testc30.dta$drottmod12x, testc30.dta$nortlm12x))
Results12c31 <-as.data.frame(contROC(testc31.dta$lpro12x, testc31.dta$drottmod12x, testc31.dta$nortlm12x))
Results12c32 <-as.data.frame(contROC(testc32.dta$lpro12x, testc32.dta$drottmod12x, testc32.dta$nortlm12x))
Results12c33 <-as.data.frame(contROC(testc33.dta$lpro12x, testc33.dta$drottmod12x, testc33.dta$nortlm12x))
Results12c34 <-as.data.frame(contROC(testc34.dta$lpro12x, testc34.dta$drottmod12x, testc34.dta$nortlm12x))
Results12c35 <-as.data.frame(contROC(testc35.dta$lpro12x, testc35.dta$drottmod12x, testc35.dta$nortlm12x))
Results12c36 <-as.data.frame(contROC(testc36.dta$lpro12x, testc36.dta$drottmod12x, testc36.dta$nortlm12x))
Results12c37 <-as.data.frame(contROC(testc37.dta$lpro12x, testc37.dta$drottmod12x, testc37.dta$nortlm12x))
Results12c38 <-as.data.frame(contROC(testc38.dta$lpro12x, testc38.dta$drottmod12x, testc38.dta$nortlm12x))
Results12c39 <-as.data.frame(contROC(testc39.dta$lpro12x, testc39.dta$drottmod12x, testc39.dta$nortlm12x))
Results12c40 <-as.data.frame(contROC(testc40.dta$lpro12x, testc40.dta$drottmod12x, testc40.dta$nortlm12x))
Results12c41 <-as.data.frame(contROC(testc41.dta$lpro12x, testc41.dta$drottmod12x, testc41.dta$nortlm12x))
Results12c42 <-as.data.frame(contROC(testc42.dta$lpro12x, testc42.dta$drottmod12x, testc42.dta$nortlm12x))
Results12c43 <-as.data.frame(contROC(testc43.dta$lpro12x, testc43.dta$drottmod12x, testc43.dta$nortlm12x))
Results12c44 <-as.data.frame(contROC(testc44.dta$lpro12x, testc44.dta$drottmod12x, testc44.dta$nortlm12x))
Results12c45 <-as.data.frame(contROC(testc45.dta$lpro12x, testc45.dta$drottmod12x, testc45.dta$nortlm12x))
Results12c46 <-as.data.frame(contROC(testc46.dta$lpro12x, testc46.dta$drottmod12x, testc46.dta$nortlm12x))
Results12c47 <-as.data.frame(contROC(testc47.dta$lpro12x, testc47.dta$drottmod12x, testc47.dta$nortlm12x))
Results12c48 <-as.data.frame(contROC(testc48.dta$lpro12x, testc48.dta$drottmod12x, testc48.dta$nortlm12x))
Results12c49 <-as.data.frame(contROC(testc49.dta$lpro12x, testc49.dta$drottmod12x, testc49.dta$nortlm12x))
Results12c50 <-as.data.frame(contROC(testc50.dta$lpro12x, testc50.dta$drottmod12x, testc50.dta$nortlm12x))
Results12c51 <-as.data.frame(contROC(testc51.dta$lpro12x, testc51.dta$drottmod12x, testc51.dta$nortlm12x))
Results12c52 <-as.data.frame(contROC(testc52.dta$lpro12x, testc52.dta$drottmod12x, testc52.dta$nortlm12x))
Results12c53 <-as.data.frame(contROC(testc53.dta$lpro12x, testc53.dta$drottmod12x, testc53.dta$nortlm12x))
Results12c54 <-as.data.frame(contROC(testc54.dta$lpro12x, testc54.dta$drottmod12x, testc54.dta$nortlm12x))
Results12c55 <-as.data.frame(contROC(testc55.dta$lpro12x, testc55.dta$drottmod12x, testc55.dta$nortlm12x))
Results12c56 <-as.data.frame(contROC(testc56.dta$lpro12x, testc56.dta$drottmod12x, testc56.dta$nortlm12x))
Results12c57 <-as.data.frame(contROC(testc57.dta$lpro12x, testc57.dta$drottmod12x, testc57.dta$nortlm12x))
Results12c58 <-as.data.frame(contROC(testc58.dta$lpro12x, testc58.dta$drottmod12x, testc58.dta$nortlm12x))
Results12c59 <-as.data.frame(contROC(testc59.dta$lpro12x, testc59.dta$drottmod12x, testc59.dta$nortlm12x))
Results12c60 <-as.data.frame(contROC(testc60.dta$lpro12x, testc60.dta$drottmod12x, testc60.dta$nortlm12x))
Results12c61 <-as.data.frame(contROC(testc61.dta$lpro12x, testc61.dta$drottmod12x, testc61.dta$nortlm12x))
Results12c62 <-as.data.frame(contROC(testc62.dta$lpro12x, testc62.dta$drottmod12x, testc62.dta$nortlm12x))
Results12c63 <-as.data.frame(contROC(testc63.dta$lpro12x, testc63.dta$drottmod12x, testc63.dta$nortlm12x))
Results12c64 <-as.data.frame(contROC(testc64.dta$lpro12x, testc64.dta$drottmod12x, testc64.dta$nortlm12x))
Results12c65 <-as.data.frame(contROC(testc65.dta$lpro12x, testc65.dta$drottmod12x, testc65.dta$nortlm12x))
Results12c66 <-as.data.frame(contROC(testc66.dta$lpro12x, testc66.dta$drottmod12x, testc66.dta$nortlm12x))
Results12c67 <-as.data.frame(contROC(testc67.dta$lpro12x, testc67.dta$drottmod12x, testc67.dta$nortlm12x))
Results12c68 <-as.data.frame(contROC(testc68.dta$lpro12x, testc68.dta$drottmod12x, testc68.dta$nortlm12x))
Results12c69 <-as.data.frame(contROC(testc69.dta$lpro12x, testc69.dta$drottmod12x, testc69.dta$nortlm12x))
Results12c70 <-as.data.frame(contROC(testc70.dta$lpro12x, testc70.dta$drottmod12x, testc70.dta$nortlm12x))
Results12c71 <-as.data.frame(contROC(testc71.dta$lpro12x, testc71.dta$drottmod12x, testc71.dta$nortlm12x))
Results12c72 <-as.data.frame(contROC(testc72.dta$lpro12x, testc72.dta$drottmod12x, testc72.dta$nortlm12x))
Results12c73 <-as.data.frame(contROC(testc73.dta$lpro12x, testc73.dta$drottmod12x, testc73.dta$nortlm12x))
Results12c74 <-as.data.frame(contROC(testc74.dta$lpro12x, testc74.dta$drottmod12x, testc74.dta$nortlm12x))
Results12c75 <-as.data.frame(contROC(testc75.dta$lpro12x, testc75.dta$drottmod12x, testc75.dta$nortlm12x))
Results12c76 <-as.data.frame(contROC(testc76.dta$lpro12x, testc76.dta$drottmod12x, testc76.dta$nortlm12x))
Results12c77 <-as.data.frame(contROC(testc77.dta$lpro12x, testc77.dta$drottmod12x, testc77.dta$nortlm12x))
Results12c78 <-as.data.frame(contROC(testc78.dta$lpro12x, testc78.dta$drottmod12x, testc78.dta$nortlm12x))
Results12c79 <-as.data.frame(contROC(testc79.dta$lpro12x, testc79.dta$drottmod12x, testc79.dta$nortlm12x))
Results12c80 <-as.data.frame(contROC(testc80.dta$lpro12x, testc80.dta$drottmod12x, testc80.dta$nortlm12x))
Results12c81 <-as.data.frame(contROC(testc81.dta$lpro12x, testc81.dta$drottmod12x, testc81.dta$nortlm12x))
Results12c82 <-as.data.frame(contROC(testc82.dta$lpro12x, testc82.dta$drottmod12x, testc82.dta$nortlm12x))
Results12c83 <-as.data.frame(contROC(testc83.dta$lpro12x, testc83.dta$drottmod12x, testc83.dta$nortlm12x))
Results12c84 <-as.data.frame(contROC(testc84.dta$lpro12x, testc84.dta$drottmod12x, testc84.dta$nortlm12x))
Results12c85 <-as.data.frame(contROC(testc85.dta$lpro12x, testc85.dta$drottmod12x, testc85.dta$nortlm12x))
Results12c86 <-as.data.frame(contROC(testc86.dta$lpro12x, testc86.dta$drottmod12x, testc86.dta$nortlm12x))
Results12c87 <-as.data.frame(contROC(testc87.dta$lpro12x, testc87.dta$drottmod12x, testc87.dta$nortlm12x))
Results12c88 <-as.data.frame(contROC(testc88.dta$lpro12x, testc88.dta$drottmod12x, testc88.dta$nortlm12x))
Results12c89 <-as.data.frame(contROC(testc89.dta$lpro12x, testc89.dta$drottmod12x, testc89.dta$nortlm12x))
Results12c90 <-as.data.frame(contROC(testc90.dta$lpro12x, testc90.dta$drottmod12x, testc90.dta$nortlm12x))
Results12c91 <-as.data.frame(contROC(testc91.dta$lpro12x, testc91.dta$drottmod12x, testc91.dta$nortlm12x))
Results12c92 <-as.data.frame(contROC(testc92.dta$lpro12x, testc92.dta$drottmod12x, testc92.dta$nortlm12x))
Results12c93 <-as.data.frame(contROC(testc93.dta$lpro12x, testc93.dta$drottmod12x, testc93.dta$nortlm12x))
Results12c94 <-as.data.frame(contROC(testc94.dta$lpro12x, testc94.dta$drottmod12x, testc94.dta$nortlm12x))
Results12c95 <-as.data.frame(contROC(testc95.dta$lpro12x, testc95.dta$drottmod12x, testc95.dta$nortlm12x))
Results12c96 <-as.data.frame(contROC(testc96.dta$lpro12x, testc96.dta$drottmod12x, testc96.dta$nortlm12x))
Results12c97 <-as.data.frame(contROC(testc97.dta$lpro12x, testc97.dta$drottmod12x, testc97.dta$nortlm12x))
Results12c98 <-as.data.frame(contROC(testc98.dta$lpro12x, testc98.dta$drottmod12x, testc98.dta$nortlm12x))
Results12c99 <-as.data.frame(contROC(testc99.dta$lpro12x, testc99.dta$drottmod12x, testc99.dta$nortlm12x))
Results12c100 <-as.data.frame(contROC(testc100.dta$lpro12x, testc100.dta$drottmod12x, testc100.dta$nortlm12x))

Results12d1 <-as.data.frame(contROC(testd1.dta$lpro12x, testd1.dta$drottmod12x, testd1.dta$nortlm12x))
Results12d2 <-as.data.frame(contROC(testd2.dta$lpro12x, testd2.dta$drottmod12x, testd2.dta$nortlm12x))
Results12d3 <-as.data.frame(contROC(testd3.dta$lpro12x, testd3.dta$drottmod12x, testd3.dta$nortlm12x))
Results12d4 <-as.data.frame(contROC(testd4.dta$lpro12x, testd4.dta$drottmod12x, testd4.dta$nortlm12x))
Results12d5 <-as.data.frame(contROC(testd5.dta$lpro12x, testd5.dta$drottmod12x, testd5.dta$nortlm12x))
Results12d6 <-as.data.frame(contROC(testd6.dta$lpro12x, testd6.dta$drottmod12x, testd6.dta$nortlm12x))
Results12d7 <-as.data.frame(contROC(testd7.dta$lpro12x, testd7.dta$drottmod12x, testd7.dta$nortlm12x))
Results12d8 <-as.data.frame(contROC(testd8.dta$lpro12x, testd8.dta$drottmod12x, testd8.dta$nortlm12x))
Results12d9 <-as.data.frame(contROC(testd9.dta$lpro12x, testd9.dta$drottmod12x, testd9.dta$nortlm12x))
Results12d10 <-as.data.frame(contROC(testd10.dta$lpro12x, testd10.dta$drottmod12x, testd10.dta$nortlm12x))
Results12d11 <-as.data.frame(contROC(testd11.dta$lpro12x, testd11.dta$drottmod12x, testd11.dta$nortlm12x))
Results12d12 <-as.data.frame(contROC(testd12.dta$lpro12x, testd12.dta$drottmod12x, testd12.dta$nortlm12x))
Results12d13 <-as.data.frame(contROC(testd13.dta$lpro12x, testd13.dta$drottmod12x, testd13.dta$nortlm12x))
Results12d14 <-as.data.frame(contROC(testd14.dta$lpro12x, testd14.dta$drottmod12x, testd14.dta$nortlm12x))
Results12d15 <-as.data.frame(contROC(testd15.dta$lpro12x, testd15.dta$drottmod12x, testd15.dta$nortlm12x))
Results12d16 <-as.data.frame(contROC(testd16.dta$lpro12x, testd16.dta$drottmod12x, testd16.dta$nortlm12x))
Results12d17 <-as.data.frame(contROC(testd17.dta$lpro12x, testd17.dta$drottmod12x, testd17.dta$nortlm12x))
Results12d18 <-as.data.frame(contROC(testd18.dta$lpro12x, testd18.dta$drottmod12x, testd18.dta$nortlm12x))
Results12d19 <-as.data.frame(contROC(testd19.dta$lpro12x, testd19.dta$drottmod12x, testd19.dta$nortlm12x))
Results12d20 <-as.data.frame(contROC(testd20.dta$lpro12x, testd20.dta$drottmod12x, testd20.dta$nortlm12x))
Results12d21 <-as.data.frame(contROC(testd21.dta$lpro12x, testd21.dta$drottmod12x, testd21.dta$nortlm12x))
Results12d22 <-as.data.frame(contROC(testd22.dta$lpro12x, testd22.dta$drottmod12x, testd22.dta$nortlm12x))
Results12d23 <-as.data.frame(contROC(testd23.dta$lpro12x, testd23.dta$drottmod12x, testd23.dta$nortlm12x))
Results12d24 <-as.data.frame(contROC(testd24.dta$lpro12x, testd24.dta$drottmod12x, testd24.dta$nortlm12x))
Results12d25 <-as.data.frame(contROC(testd25.dta$lpro12x, testd25.dta$drottmod12x, testd25.dta$nortlm12x))
Results12d26 <-as.data.frame(contROC(testd26.dta$lpro12x, testd26.dta$drottmod12x, testd26.dta$nortlm12x))
Results12d27 <-as.data.frame(contROC(testd27.dta$lpro12x, testd27.dta$drottmod12x, testd27.dta$nortlm12x))
Results12d28 <-as.data.frame(contROC(testd28.dta$lpro12x, testd28.dta$drottmod12x, testd28.dta$nortlm12x))
Results12d29 <-as.data.frame(contROC(testd29.dta$lpro12x, testd29.dta$drottmod12x, testd29.dta$nortlm12x))
Results12d30 <-as.data.frame(contROC(testd30.dta$lpro12x, testd30.dta$drottmod12x, testd30.dta$nortlm12x))
Results12d31 <-as.data.frame(contROC(testd31.dta$lpro12x, testd31.dta$drottmod12x, testd31.dta$nortlm12x))
Results12d32 <-as.data.frame(contROC(testd32.dta$lpro12x, testd32.dta$drottmod12x, testd32.dta$nortlm12x))
Results12d33 <-as.data.frame(contROC(testd33.dta$lpro12x, testd33.dta$drottmod12x, testd33.dta$nortlm12x))
Results12d34 <-as.data.frame(contROC(testd34.dta$lpro12x, testd34.dta$drottmod12x, testd34.dta$nortlm12x))
Results12d35 <-as.data.frame(contROC(testd35.dta$lpro12x, testd35.dta$drottmod12x, testd35.dta$nortlm12x))
Results12d36 <-as.data.frame(contROC(testd36.dta$lpro12x, testd36.dta$drottmod12x, testd36.dta$nortlm12x))
Results12d37 <-as.data.frame(contROC(testd37.dta$lpro12x, testd37.dta$drottmod12x, testd37.dta$nortlm12x))
Results12d38 <-as.data.frame(contROC(testd38.dta$lpro12x, testd38.dta$drottmod12x, testd38.dta$nortlm12x))
Results12d39 <-as.data.frame(contROC(testd39.dta$lpro12x, testd39.dta$drottmod12x, testd39.dta$nortlm12x))
Results12d40 <-as.data.frame(contROC(testd40.dta$lpro12x, testd40.dta$drottmod12x, testd40.dta$nortlm12x))
Results12d41 <-as.data.frame(contROC(testd41.dta$lpro12x, testd41.dta$drottmod12x, testd41.dta$nortlm12x))
Results12d42 <-as.data.frame(contROC(testd42.dta$lpro12x, testd42.dta$drottmod12x, testd42.dta$nortlm12x))
Results12d43 <-as.data.frame(contROC(testd43.dta$lpro12x, testd43.dta$drottmod12x, testd43.dta$nortlm12x))
Results12d44 <-as.data.frame(contROC(testd44.dta$lpro12x, testd44.dta$drottmod12x, testd44.dta$nortlm12x))
Results12d45 <-as.data.frame(contROC(testd45.dta$lpro12x, testd45.dta$drottmod12x, testd45.dta$nortlm12x))
Results12d46 <-as.data.frame(contROC(testd46.dta$lpro12x, testd46.dta$drottmod12x, testd46.dta$nortlm12x))
Results12d47 <-as.data.frame(contROC(testd47.dta$lpro12x, testd47.dta$drottmod12x, testd47.dta$nortlm12x))
Results12d48 <-as.data.frame(contROC(testd48.dta$lpro12x, testd48.dta$drottmod12x, testd48.dta$nortlm12x))
Results12d49 <-as.data.frame(contROC(testd49.dta$lpro12x, testd49.dta$drottmod12x, testd49.dta$nortlm12x))
Results12d50 <-as.data.frame(contROC(testd50.dta$lpro12x, testd50.dta$drottmod12x, testd50.dta$nortlm12x))
Results12d51 <-as.data.frame(contROC(testd51.dta$lpro12x, testd51.dta$drottmod12x, testd51.dta$nortlm12x))
Results12d52 <-as.data.frame(contROC(testd52.dta$lpro12x, testd52.dta$drottmod12x, testd52.dta$nortlm12x))
Results12d53 <-as.data.frame(contROC(testd53.dta$lpro12x, testd53.dta$drottmod12x, testd53.dta$nortlm12x))
Results12d54 <-as.data.frame(contROC(testd54.dta$lpro12x, testd54.dta$drottmod12x, testd54.dta$nortlm12x))
Results12d55 <-as.data.frame(contROC(testd55.dta$lpro12x, testd55.dta$drottmod12x, testd55.dta$nortlm12x))
Results12d56 <-as.data.frame(contROC(testd56.dta$lpro12x, testd56.dta$drottmod12x, testd56.dta$nortlm12x))
Results12d57 <-as.data.frame(contROC(testd57.dta$lpro12x, testd57.dta$drottmod12x, testd57.dta$nortlm12x))
Results12d58 <-as.data.frame(contROC(testd58.dta$lpro12x, testd58.dta$drottmod12x, testd58.dta$nortlm12x))
Results12d59 <-as.data.frame(contROC(testd59.dta$lpro12x, testd59.dta$drottmod12x, testd59.dta$nortlm12x))
Results12d60 <-as.data.frame(contROC(testd60.dta$lpro12x, testd60.dta$drottmod12x, testd60.dta$nortlm12x))
Results12d61 <-as.data.frame(contROC(testd61.dta$lpro12x, testd61.dta$drottmod12x, testd61.dta$nortlm12x))
Results12d62 <-as.data.frame(contROC(testd62.dta$lpro12x, testd62.dta$drottmod12x, testd62.dta$nortlm12x))
Results12d63 <-as.data.frame(contROC(testd63.dta$lpro12x, testd63.dta$drottmod12x, testd63.dta$nortlm12x))
Results12d64 <-as.data.frame(contROC(testd64.dta$lpro12x, testd64.dta$drottmod12x, testd64.dta$nortlm12x))
Results12d65 <-as.data.frame(contROC(testd65.dta$lpro12x, testd65.dta$drottmod12x, testd65.dta$nortlm12x))
Results12d66 <-as.data.frame(contROC(testd66.dta$lpro12x, testd66.dta$drottmod12x, testd66.dta$nortlm12x))
Results12d67 <-as.data.frame(contROC(testd67.dta$lpro12x, testd67.dta$drottmod12x, testd67.dta$nortlm12x))
Results12d68 <-as.data.frame(contROC(testd68.dta$lpro12x, testd68.dta$drottmod12x, testd68.dta$nortlm12x))
Results12d69 <-as.data.frame(contROC(testd69.dta$lpro12x, testd69.dta$drottmod12x, testd69.dta$nortlm12x))
Results12d70 <-as.data.frame(contROC(testd70.dta$lpro12x, testd70.dta$drottmod12x, testd70.dta$nortlm12x))
Results12d71 <-as.data.frame(contROC(testd71.dta$lpro12x, testd71.dta$drottmod12x, testd71.dta$nortlm12x))
Results12d72 <-as.data.frame(contROC(testd72.dta$lpro12x, testd72.dta$drottmod12x, testd72.dta$nortlm12x))
Results12d73 <-as.data.frame(contROC(testd73.dta$lpro12x, testd73.dta$drottmod12x, testd73.dta$nortlm12x))
Results12d74 <-as.data.frame(contROC(testd74.dta$lpro12x, testd74.dta$drottmod12x, testd74.dta$nortlm12x))
Results12d75 <-as.data.frame(contROC(testd75.dta$lpro12x, testd75.dta$drottmod12x, testd75.dta$nortlm12x))
Results12d76 <-as.data.frame(contROC(testd76.dta$lpro12x, testd76.dta$drottmod12x, testd76.dta$nortlm12x))
Results12d77 <-as.data.frame(contROC(testd77.dta$lpro12x, testd77.dta$drottmod12x, testd77.dta$nortlm12x))
Results12d78 <-as.data.frame(contROC(testd78.dta$lpro12x, testd78.dta$drottmod12x, testd78.dta$nortlm12x))
Results12d79 <-as.data.frame(contROC(testd79.dta$lpro12x, testd79.dta$drottmod12x, testd79.dta$nortlm12x))
Results12d80 <-as.data.frame(contROC(testd80.dta$lpro12x, testd80.dta$drottmod12x, testd80.dta$nortlm12x))
Results12d81 <-as.data.frame(contROC(testd81.dta$lpro12x, testd81.dta$drottmod12x, testd81.dta$nortlm12x))
Results12d82 <-as.data.frame(contROC(testd82.dta$lpro12x, testd82.dta$drottmod12x, testd82.dta$nortlm12x))
Results12d83 <-as.data.frame(contROC(testd83.dta$lpro12x, testd83.dta$drottmod12x, testd83.dta$nortlm12x))
Results12d84 <-as.data.frame(contROC(testd84.dta$lpro12x, testd84.dta$drottmod12x, testd84.dta$nortlm12x))
Results12d85 <-as.data.frame(contROC(testd85.dta$lpro12x, testd85.dta$drottmod12x, testd85.dta$nortlm12x))
Results12d86 <-as.data.frame(contROC(testd86.dta$lpro12x, testd86.dta$drottmod12x, testd86.dta$nortlm12x))
Results12d87 <-as.data.frame(contROC(testd87.dta$lpro12x, testd87.dta$drottmod12x, testd87.dta$nortlm12x))
Results12d88 <-as.data.frame(contROC(testd88.dta$lpro12x, testd88.dta$drottmod12x, testd88.dta$nortlm12x))
Results12d89 <-as.data.frame(contROC(testd89.dta$lpro12x, testd89.dta$drottmod12x, testd89.dta$nortlm12x))
Results12d90 <-as.data.frame(contROC(testd90.dta$lpro12x, testd90.dta$drottmod12x, testd90.dta$nortlm12x))
Results12d91 <-as.data.frame(contROC(testd91.dta$lpro12x, testd91.dta$drottmod12x, testd91.dta$nortlm12x))
Results12d92 <-as.data.frame(contROC(testd92.dta$lpro12x, testd92.dta$drottmod12x, testd92.dta$nortlm12x))
Results12d93 <-as.data.frame(contROC(testd93.dta$lpro12x, testd93.dta$drottmod12x, testd93.dta$nortlm12x))
Results12d94 <-as.data.frame(contROC(testd94.dta$lpro12x, testd94.dta$drottmod12x, testd94.dta$nortlm12x))
Results12d95 <-as.data.frame(contROC(testd95.dta$lpro12x, testd95.dta$drottmod12x, testd95.dta$nortlm12x))
Results12d96 <-as.data.frame(contROC(testd96.dta$lpro12x, testd96.dta$drottmod12x, testd96.dta$nortlm12x))
Results12d97 <-as.data.frame(contROC(testd97.dta$lpro12x, testd97.dta$drottmod12x, testd97.dta$nortlm12x))
Results12d98 <-as.data.frame(contROC(testd98.dta$lpro12x, testd98.dta$drottmod12x, testd98.dta$nortlm12x))
Results12d99 <-as.data.frame(contROC(testd99.dta$lpro12x, testd99.dta$drottmod12x, testd99.dta$nortlm12x))
Results12d100 <-as.data.frame(contROC(testd100.dta$lpro12x, testd100.dta$drottmod12x, testd100.dta$nortlm12x))



# append results #

Results12aALL <- rbind(Results12a1, Results12a2, Results12a3, Results12a4, Results12a5, Results12a6, Results12a7, Results12a8, Results12a9, Results12a10, Results12a11, Results12a12, Results12a13, Results12a14, Results12a15, Results12a16, Results12a17, Results12a18, Results12a19, Results12a20, Results12a21, Results12a22, Results12a23, Results12a24, Results12a25, Results12a26, Results12a27, Results12a28, Results12a29, Results12a30, Results12a31, Results12a32, Results12a33, Results12a34, Results12a35, Results12a36, Results12a37, Results12a38, Results12a39, Results12a40, Results12a41, Results12a42, Results12a43, Results12a44, Results12a45, Results12a46, Results12a47, Results12a48, Results12a49, Results12a50, Results12a51, Results12a52, Results12a53, Results12a54, Results12a55, Results12a56, Results12a57, Results12a58, Results12a59, Results12a60, Results12a61, Results12a62, Results12a63, Results12a64, Results12a65, Results12a66, Results12a67, Results12a68, Results12a69, Results12a70, Results12a71, Results12a72, Results12a73, Results12a74, Results12a75, Results12a76, Results12a77, Results12a78, Results12a79, Results12a80, Results12a81, Results12a82, Results12a83, Results12a84, Results12a85, Results12a86, Results12a87, Results12a88, Results12a89, Results12a90, Results12a91, Results12a92, Results12a93, Results12a94, Results12a95, Results12a96, Results12a97, Results12a98, Results12a99, Results12a100)

Results12bALL <- rbind(Results12b1, Results12b2, Results12b3, Results12b4, Results12b5, Results12b6, Results12b7, Results12b8, Results12b9, Results12b10, Results12b11, Results12b12, Results12b13, Results12b14, Results12b15, Results12b16, Results12b17, Results12b18, Results12b19, Results12b20, Results12b21, Results12b22, Results12b23, Results12b24, Results12b25, Results12b26, Results12b27, Results12b28, Results12b29, Results12b30, Results12b31, Results12b32, Results12b33, Results12b34, Results12b35, Results12b36, Results12b37, Results12b38, Results12b39, Results12b40, Results12b41, Results12b42, Results12b43, Results12b44, Results12b45, Results12b46, Results12b47, Results12b48, Results12b49, Results12b50, Results12b51, Results12b52, Results12b53, Results12b54, Results12b55, Results12b56, Results12b57, Results12b58, Results12b59, Results12b60, Results12b61, Results12b62, Results12b63, Results12b64, Results12b65, Results12b66, Results12b67, Results12b68, Results12b69, Results12b70, Results12b71, Results12b72, Results12b73, Results12b74, Results12b75, Results12b76, Results12b77, Results12b78, Results12b79, Results12b80, Results12b81, Results12b82, Results12b83, Results12b84, Results12b85, Results12b86, Results12b87, Results12b88, Results12b89, Results12b90, Results12b91, Results12b92, Results12b93, Results12b94, Results12b95, Results12b96, Results12b97, Results12b98, Results12b99, Results12b100)


Results12cALL <- rbind(Results12c1, Results12c2, Results12c3, Results12c4, Results12c5, Results12c6, Results12c7, Results12c8, Results12c9, Results12c10, Results12c11, Results12c12, Results12c13, Results12c14, Results12c15, Results12c16, Results12c17, Results12c18, Results12c19, Results12c20, Results12c21, Results12c22, Results12c23, Results12c24, Results12c25, Results12c26, Results12c27, Results12c28, Results12c29, Results12c30, Results12c31, Results12c32, Results12c33, Results12c34, Results12c35, Results12c36, Results12c37, Results12c38, Results12c39, Results12c40, Results12c41, Results12c42, Results12c43, Results12c44, Results12c45, Results12c46, Results12c47, Results12c48, Results12c49, Results12c50, Results12c51, Results12c52, Results12c53, Results12c54, Results12c55, Results12c56, Results12c57, Results12c58, Results12c59, Results12c60, Results12c61, Results12c62, Results12c63, Results12c64, Results12c65, Results12c66, Results12c67, Results12c68, Results12c69, Results12c70, Results12c71, Results12c72, Results12c73, Results12c74, Results12c75, Results12c76, Results12c77, Results12c78, Results12c79, Results12c80, Results12c81, Results12c82, Results12c83, Results12c84, Results12c85, Results12c86, Results12c87, Results12c88, Results12c89, Results12c90, Results12c91, Results12c92, Results12c93, Results12c94, Results12c95, Results12c96, Results12c97, Results12c98, Results12c99, Results12c100)

Results12dALL <- rbind(Results12d1, Results12d2, Results12d3, Results12d4, Results12d5, Results12d6, Results12d7, Results12d8, Results12d9, Results12d10, Results12d11, Results12d12, Results12d13, Results12d14, Results12d15, Results12d16, Results12d17, Results12d18, Results12d19, Results12d20, Results12d21, Results12d22, Results12d23, Results12d24, Results12d25, Results12d26, Results12d27, Results12d28, Results12d29, Results12d30, Results12d31, Results12d32, Results12d33, Results12d34, Results12d35, Results12d36, Results12d37, Results12d38, Results12d39, Results12d40, Results12d41, Results12d42, Results12d43, Results12d44, Results12d45, Results12d46, Results12d47, Results12d48, Results12d49, Results12d50, Results12d51, Results12d52, Results12d53, Results12d54, Results12d55, Results12d56, Results12d57, Results12d58, Results12d59, Results12d60, Results12d61, Results12d62, Results12d63, Results12d64, Results12d65, Results12d66, Results12d67, Results12d68, Results12d69, Results12d70, Results12d71, Results12d72, Results12d73, Results12d74, Results12d75, Results12d76, Results12d77, Results12d78, Results12d79, Results12d80, Results12d81, Results12d82, Results12d83, Results12d84, Results12d85, Results12d86, Results12d87, Results12d88, Results12d89, Results12d90, Results12d91, Results12d92, Results12d93, Results12d94, Results12d95, Results12d96, Results12d97, Results12d98, Results12d99, Results12d100)

Results12ALL <- rbind(Results12aALL, Results12bALL, Results12cALL, Results12dALL)
YD12ALL <- Results12ALL[seq(1, nrow(Results12ALL), 2), ]
NoRTLM12ALL <- Results12ALL[seq(2, nrow(Results12ALL), 2), ]



## militia violence ##

Results1a1 <-as.data.frame(contROC(testa1.dta$lpro1x, testa1.dta$drottmod1x, testa1.dta$nortlm1x))
Results1a2 <-as.data.frame(contROC(testa2.dta$lpro1x, testa2.dta$drottmod1x, testa2.dta$nortlm1x))
Results1a3 <-as.data.frame(contROC(testa3.dta$lpro1x, testa3.dta$drottmod1x, testa3.dta$nortlm1x))
Results1a4 <-as.data.frame(contROC(testa4.dta$lpro1x, testa4.dta$drottmod1x, testa4.dta$nortlm1x))
Results1a5 <-as.data.frame(contROC(testa5.dta$lpro1x, testa5.dta$drottmod1x, testa5.dta$nortlm1x))
Results1a6 <-as.data.frame(contROC(testa6.dta$lpro1x, testa6.dta$drottmod1x, testa6.dta$nortlm1x))
Results1a7 <-as.data.frame(contROC(testa7.dta$lpro1x, testa7.dta$drottmod1x, testa7.dta$nortlm1x))
Results1a8 <-as.data.frame(contROC(testa8.dta$lpro1x, testa8.dta$drottmod1x, testa8.dta$nortlm1x))
Results1a9 <-as.data.frame(contROC(testa9.dta$lpro1x, testa9.dta$drottmod1x, testa9.dta$nortlm1x))
Results1a10 <-as.data.frame(contROC(testa10.dta$lpro1x, testa10.dta$drottmod1x, testa10.dta$nortlm1x))
Results1a11 <-as.data.frame(contROC(testa11.dta$lpro1x, testa11.dta$drottmod1x, testa11.dta$nortlm1x))
Results1a12 <-as.data.frame(contROC(testa12.dta$lpro1x, testa12.dta$drottmod1x, testa12.dta$nortlm1x))
Results1a13 <-as.data.frame(contROC(testa13.dta$lpro1x, testa13.dta$drottmod1x, testa13.dta$nortlm1x))
Results1a14 <-as.data.frame(contROC(testa14.dta$lpro1x, testa14.dta$drottmod1x, testa14.dta$nortlm1x))
Results1a15 <-as.data.frame(contROC(testa15.dta$lpro1x, testa15.dta$drottmod1x, testa15.dta$nortlm1x))
Results1a16 <-as.data.frame(contROC(testa16.dta$lpro1x, testa16.dta$drottmod1x, testa16.dta$nortlm1x))
Results1a17 <-as.data.frame(contROC(testa17.dta$lpro1x, testa17.dta$drottmod1x, testa17.dta$nortlm1x))
Results1a18 <-as.data.frame(contROC(testa18.dta$lpro1x, testa18.dta$drottmod1x, testa18.dta$nortlm1x))
Results1a19 <-as.data.frame(contROC(testa19.dta$lpro1x, testa19.dta$drottmod1x, testa19.dta$nortlm1x))
Results1a20 <-as.data.frame(contROC(testa20.dta$lpro1x, testa20.dta$drottmod1x, testa20.dta$nortlm1x))
Results1a21 <-as.data.frame(contROC(testa21.dta$lpro1x, testa21.dta$drottmod1x, testa21.dta$nortlm1x))
Results1a22 <-as.data.frame(contROC(testa22.dta$lpro1x, testa22.dta$drottmod1x, testa22.dta$nortlm1x))
Results1a23 <-as.data.frame(contROC(testa23.dta$lpro1x, testa23.dta$drottmod1x, testa23.dta$nortlm1x))
Results1a24 <-as.data.frame(contROC(testa24.dta$lpro1x, testa24.dta$drottmod1x, testa24.dta$nortlm1x))
Results1a25 <-as.data.frame(contROC(testa25.dta$lpro1x, testa25.dta$drottmod1x, testa25.dta$nortlm1x))
Results1a26 <-as.data.frame(contROC(testa26.dta$lpro1x, testa26.dta$drottmod1x, testa26.dta$nortlm1x))
Results1a27 <-as.data.frame(contROC(testa27.dta$lpro1x, testa27.dta$drottmod1x, testa27.dta$nortlm1x))
Results1a28 <-as.data.frame(contROC(testa28.dta$lpro1x, testa28.dta$drottmod1x, testa28.dta$nortlm1x))
Results1a29 <-as.data.frame(contROC(testa29.dta$lpro1x, testa29.dta$drottmod1x, testa29.dta$nortlm1x))
Results1a30 <-as.data.frame(contROC(testa30.dta$lpro1x, testa30.dta$drottmod1x, testa30.dta$nortlm1x))
Results1a31 <-as.data.frame(contROC(testa31.dta$lpro1x, testa31.dta$drottmod1x, testa31.dta$nortlm1x))
Results1a32 <-as.data.frame(contROC(testa32.dta$lpro1x, testa32.dta$drottmod1x, testa32.dta$nortlm1x))
Results1a33 <-as.data.frame(contROC(testa33.dta$lpro1x, testa33.dta$drottmod1x, testa33.dta$nortlm1x))
Results1a34 <-as.data.frame(contROC(testa34.dta$lpro1x, testa34.dta$drottmod1x, testa34.dta$nortlm1x))
Results1a35 <-as.data.frame(contROC(testa35.dta$lpro1x, testa35.dta$drottmod1x, testa35.dta$nortlm1x))
Results1a36 <-as.data.frame(contROC(testa36.dta$lpro1x, testa36.dta$drottmod1x, testa36.dta$nortlm1x))
Results1a37 <-as.data.frame(contROC(testa37.dta$lpro1x, testa37.dta$drottmod1x, testa37.dta$nortlm1x))
Results1a38 <-as.data.frame(contROC(testa38.dta$lpro1x, testa38.dta$drottmod1x, testa38.dta$nortlm1x))
Results1a39 <-as.data.frame(contROC(testa39.dta$lpro1x, testa39.dta$drottmod1x, testa39.dta$nortlm1x))
Results1a40 <-as.data.frame(contROC(testa40.dta$lpro1x, testa40.dta$drottmod1x, testa40.dta$nortlm1x))
Results1a41 <-as.data.frame(contROC(testa41.dta$lpro1x, testa41.dta$drottmod1x, testa41.dta$nortlm1x))
Results1a42 <-as.data.frame(contROC(testa42.dta$lpro1x, testa42.dta$drottmod1x, testa42.dta$nortlm1x))
Results1a43 <-as.data.frame(contROC(testa43.dta$lpro1x, testa43.dta$drottmod1x, testa43.dta$nortlm1x))
Results1a44 <-as.data.frame(contROC(testa44.dta$lpro1x, testa44.dta$drottmod1x, testa44.dta$nortlm1x))
Results1a45 <-as.data.frame(contROC(testa45.dta$lpro1x, testa45.dta$drottmod1x, testa45.dta$nortlm1x))
Results1a46 <-as.data.frame(contROC(testa46.dta$lpro1x, testa46.dta$drottmod1x, testa46.dta$nortlm1x))
Results1a47 <-as.data.frame(contROC(testa47.dta$lpro1x, testa47.dta$drottmod1x, testa47.dta$nortlm1x))
Results1a48 <-as.data.frame(contROC(testa48.dta$lpro1x, testa48.dta$drottmod1x, testa48.dta$nortlm1x))
Results1a49 <-as.data.frame(contROC(testa49.dta$lpro1x, testa49.dta$drottmod1x, testa49.dta$nortlm1x))
Results1a50 <-as.data.frame(contROC(testa50.dta$lpro1x, testa50.dta$drottmod1x, testa50.dta$nortlm1x))
Results1a51 <-as.data.frame(contROC(testa51.dta$lpro1x, testa51.dta$drottmod1x, testa51.dta$nortlm1x))
Results1a52 <-as.data.frame(contROC(testa52.dta$lpro1x, testa52.dta$drottmod1x, testa52.dta$nortlm1x))
Results1a53 <-as.data.frame(contROC(testa53.dta$lpro1x, testa53.dta$drottmod1x, testa53.dta$nortlm1x))
Results1a54 <-as.data.frame(contROC(testa54.dta$lpro1x, testa54.dta$drottmod1x, testa54.dta$nortlm1x))
Results1a55 <-as.data.frame(contROC(testa55.dta$lpro1x, testa55.dta$drottmod1x, testa55.dta$nortlm1x))
Results1a56 <-as.data.frame(contROC(testa56.dta$lpro1x, testa56.dta$drottmod1x, testa56.dta$nortlm1x))
Results1a57 <-as.data.frame(contROC(testa57.dta$lpro1x, testa57.dta$drottmod1x, testa57.dta$nortlm1x))
Results1a58 <-as.data.frame(contROC(testa58.dta$lpro1x, testa58.dta$drottmod1x, testa58.dta$nortlm1x))
Results1a59 <-as.data.frame(contROC(testa59.dta$lpro1x, testa59.dta$drottmod1x, testa59.dta$nortlm1x))
Results1a60 <-as.data.frame(contROC(testa60.dta$lpro1x, testa60.dta$drottmod1x, testa60.dta$nortlm1x))
Results1a61 <-as.data.frame(contROC(testa61.dta$lpro1x, testa61.dta$drottmod1x, testa61.dta$nortlm1x))
Results1a62 <-as.data.frame(contROC(testa62.dta$lpro1x, testa62.dta$drottmod1x, testa62.dta$nortlm1x))
Results1a63 <-as.data.frame(contROC(testa63.dta$lpro1x, testa63.dta$drottmod1x, testa63.dta$nortlm1x))
Results1a64 <-as.data.frame(contROC(testa64.dta$lpro1x, testa64.dta$drottmod1x, testa64.dta$nortlm1x))
Results1a65 <-as.data.frame(contROC(testa65.dta$lpro1x, testa65.dta$drottmod1x, testa65.dta$nortlm1x))
Results1a66 <-as.data.frame(contROC(testa66.dta$lpro1x, testa66.dta$drottmod1x, testa66.dta$nortlm1x))
Results1a67 <-as.data.frame(contROC(testa67.dta$lpro1x, testa67.dta$drottmod1x, testa67.dta$nortlm1x))
Results1a68 <-as.data.frame(contROC(testa68.dta$lpro1x, testa68.dta$drottmod1x, testa68.dta$nortlm1x))
Results1a69 <-as.data.frame(contROC(testa69.dta$lpro1x, testa69.dta$drottmod1x, testa69.dta$nortlm1x))
Results1a70 <-as.data.frame(contROC(testa70.dta$lpro1x, testa70.dta$drottmod1x, testa70.dta$nortlm1x))
Results1a71 <-as.data.frame(contROC(testa71.dta$lpro1x, testa71.dta$drottmod1x, testa71.dta$nortlm1x))
Results1a72 <-as.data.frame(contROC(testa72.dta$lpro1x, testa72.dta$drottmod1x, testa72.dta$nortlm1x))
Results1a73 <-as.data.frame(contROC(testa73.dta$lpro1x, testa73.dta$drottmod1x, testa73.dta$nortlm1x))
Results1a74 <-as.data.frame(contROC(testa74.dta$lpro1x, testa74.dta$drottmod1x, testa74.dta$nortlm1x))
Results1a75 <-as.data.frame(contROC(testa75.dta$lpro1x, testa75.dta$drottmod1x, testa75.dta$nortlm1x))
Results1a76 <-as.data.frame(contROC(testa76.dta$lpro1x, testa76.dta$drottmod1x, testa76.dta$nortlm1x))
Results1a77 <-as.data.frame(contROC(testa77.dta$lpro1x, testa77.dta$drottmod1x, testa77.dta$nortlm1x))
Results1a78 <-as.data.frame(contROC(testa78.dta$lpro1x, testa78.dta$drottmod1x, testa78.dta$nortlm1x))
Results1a79 <-as.data.frame(contROC(testa79.dta$lpro1x, testa79.dta$drottmod1x, testa79.dta$nortlm1x))
Results1a80 <-as.data.frame(contROC(testa80.dta$lpro1x, testa80.dta$drottmod1x, testa80.dta$nortlm1x))
Results1a81 <-as.data.frame(contROC(testa81.dta$lpro1x, testa81.dta$drottmod1x, testa81.dta$nortlm1x))
Results1a82 <-as.data.frame(contROC(testa82.dta$lpro1x, testa82.dta$drottmod1x, testa82.dta$nortlm1x))
Results1a83 <-as.data.frame(contROC(testa83.dta$lpro1x, testa83.dta$drottmod1x, testa83.dta$nortlm1x))
Results1a84 <-as.data.frame(contROC(testa84.dta$lpro1x, testa84.dta$drottmod1x, testa84.dta$nortlm1x))
Results1a85 <-as.data.frame(contROC(testa85.dta$lpro1x, testa85.dta$drottmod1x, testa85.dta$nortlm1x))
Results1a86 <-as.data.frame(contROC(testa86.dta$lpro1x, testa86.dta$drottmod1x, testa86.dta$nortlm1x))
Results1a87 <-as.data.frame(contROC(testa87.dta$lpro1x, testa87.dta$drottmod1x, testa87.dta$nortlm1x))
Results1a88 <-as.data.frame(contROC(testa88.dta$lpro1x, testa88.dta$drottmod1x, testa88.dta$nortlm1x))
Results1a89 <-as.data.frame(contROC(testa89.dta$lpro1x, testa89.dta$drottmod1x, testa89.dta$nortlm1x))
Results1a90 <-as.data.frame(contROC(testa90.dta$lpro1x, testa90.dta$drottmod1x, testa90.dta$nortlm1x))
Results1a91 <-as.data.frame(contROC(testa91.dta$lpro1x, testa91.dta$drottmod1x, testa91.dta$nortlm1x))
Results1a92 <-as.data.frame(contROC(testa92.dta$lpro1x, testa92.dta$drottmod1x, testa92.dta$nortlm1x))
Results1a93 <-as.data.frame(contROC(testa93.dta$lpro1x, testa93.dta$drottmod1x, testa93.dta$nortlm1x))
Results1a94 <-as.data.frame(contROC(testa94.dta$lpro1x, testa94.dta$drottmod1x, testa94.dta$nortlm1x))
Results1a95 <-as.data.frame(contROC(testa95.dta$lpro1x, testa95.dta$drottmod1x, testa95.dta$nortlm1x))
Results1a96 <-as.data.frame(contROC(testa96.dta$lpro1x, testa96.dta$drottmod1x, testa96.dta$nortlm1x))
Results1a97 <-as.data.frame(contROC(testa97.dta$lpro1x, testa97.dta$drottmod1x, testa97.dta$nortlm1x))
Results1a98 <-as.data.frame(contROC(testa98.dta$lpro1x, testa98.dta$drottmod1x, testa98.dta$nortlm1x))
Results1a99 <-as.data.frame(contROC(testa99.dta$lpro1x, testa99.dta$drottmod1x, testa99.dta$nortlm1x))
Results1a100 <-as.data.frame(contROC(testa100.dta$lpro1x, testa100.dta$drottmod1x, testa100.dta$nortlm1x))

Results1b1 <-as.data.frame(contROC(testb1.dta$lpro1x, testb1.dta$drottmod1x, testb1.dta$nortlm1x))
Results1b2 <-as.data.frame(contROC(testb2.dta$lpro1x, testb2.dta$drottmod1x, testb2.dta$nortlm1x))
Results1b3 <-as.data.frame(contROC(testb3.dta$lpro1x, testb3.dta$drottmod1x, testb3.dta$nortlm1x))
Results1b4 <-as.data.frame(contROC(testb4.dta$lpro1x, testb4.dta$drottmod1x, testb4.dta$nortlm1x))
Results1b5 <-as.data.frame(contROC(testb5.dta$lpro1x, testb5.dta$drottmod1x, testb5.dta$nortlm1x))
Results1b6 <-as.data.frame(contROC(testb6.dta$lpro1x, testb6.dta$drottmod1x, testb6.dta$nortlm1x))
Results1b7 <-as.data.frame(contROC(testb7.dta$lpro1x, testb7.dta$drottmod1x, testb7.dta$nortlm1x))
Results1b8 <-as.data.frame(contROC(testb8.dta$lpro1x, testb8.dta$drottmod1x, testb8.dta$nortlm1x))
Results1b9 <-as.data.frame(contROC(testb9.dta$lpro1x, testb9.dta$drottmod1x, testb9.dta$nortlm1x))
Results1b10 <-as.data.frame(contROC(testb10.dta$lpro1x, testb10.dta$drottmod1x, testb10.dta$nortlm1x))
Results1b11 <-as.data.frame(contROC(testb11.dta$lpro1x, testb11.dta$drottmod1x, testb11.dta$nortlm1x))
Results1b12 <-as.data.frame(contROC(testb12.dta$lpro1x, testb12.dta$drottmod1x, testb12.dta$nortlm1x))
Results1b13 <-as.data.frame(contROC(testb13.dta$lpro1x, testb13.dta$drottmod1x, testb13.dta$nortlm1x))
Results1b14 <-as.data.frame(contROC(testb14.dta$lpro1x, testb14.dta$drottmod1x, testb14.dta$nortlm1x))
Results1b15 <-as.data.frame(contROC(testb15.dta$lpro1x, testb15.dta$drottmod1x, testb15.dta$nortlm1x))
Results1b16 <-as.data.frame(contROC(testb16.dta$lpro1x, testb16.dta$drottmod1x, testb16.dta$nortlm1x))
Results1b17 <-as.data.frame(contROC(testb17.dta$lpro1x, testb17.dta$drottmod1x, testb17.dta$nortlm1x))
Results1b18 <-as.data.frame(contROC(testb18.dta$lpro1x, testb18.dta$drottmod1x, testb18.dta$nortlm1x))
Results1b19 <-as.data.frame(contROC(testb19.dta$lpro1x, testb19.dta$drottmod1x, testb19.dta$nortlm1x))
Results1b20 <-as.data.frame(contROC(testb20.dta$lpro1x, testb20.dta$drottmod1x, testb20.dta$nortlm1x))
Results1b21 <-as.data.frame(contROC(testb21.dta$lpro1x, testb21.dta$drottmod1x, testb21.dta$nortlm1x))
Results1b22 <-as.data.frame(contROC(testb22.dta$lpro1x, testb22.dta$drottmod1x, testb22.dta$nortlm1x))
Results1b23 <-as.data.frame(contROC(testb23.dta$lpro1x, testb23.dta$drottmod1x, testb23.dta$nortlm1x))
Results1b24 <-as.data.frame(contROC(testb24.dta$lpro1x, testb24.dta$drottmod1x, testb24.dta$nortlm1x))
Results1b25 <-as.data.frame(contROC(testb25.dta$lpro1x, testb25.dta$drottmod1x, testb25.dta$nortlm1x))
Results1b26 <-as.data.frame(contROC(testb26.dta$lpro1x, testb26.dta$drottmod1x, testb26.dta$nortlm1x))
Results1b27 <-as.data.frame(contROC(testb27.dta$lpro1x, testb27.dta$drottmod1x, testb27.dta$nortlm1x))
Results1b28 <-as.data.frame(contROC(testb28.dta$lpro1x, testb28.dta$drottmod1x, testb28.dta$nortlm1x))
Results1b29 <-as.data.frame(contROC(testb29.dta$lpro1x, testb29.dta$drottmod1x, testb29.dta$nortlm1x))
Results1b30 <-as.data.frame(contROC(testb30.dta$lpro1x, testb30.dta$drottmod1x, testb30.dta$nortlm1x))
Results1b31 <-as.data.frame(contROC(testb31.dta$lpro1x, testb31.dta$drottmod1x, testb31.dta$nortlm1x))
Results1b32 <-as.data.frame(contROC(testb32.dta$lpro1x, testb32.dta$drottmod1x, testb32.dta$nortlm1x))
Results1b33 <-as.data.frame(contROC(testb33.dta$lpro1x, testb33.dta$drottmod1x, testb33.dta$nortlm1x))
Results1b34 <-as.data.frame(contROC(testb34.dta$lpro1x, testb34.dta$drottmod1x, testb34.dta$nortlm1x))
Results1b35 <-as.data.frame(contROC(testb35.dta$lpro1x, testb35.dta$drottmod1x, testb35.dta$nortlm1x))
Results1b36 <-as.data.frame(contROC(testb36.dta$lpro1x, testb36.dta$drottmod1x, testb36.dta$nortlm1x))
Results1b37 <-as.data.frame(contROC(testb37.dta$lpro1x, testb37.dta$drottmod1x, testb37.dta$nortlm1x))
Results1b38 <-as.data.frame(contROC(testb38.dta$lpro1x, testb38.dta$drottmod1x, testb38.dta$nortlm1x))
Results1b39 <-as.data.frame(contROC(testb39.dta$lpro1x, testb39.dta$drottmod1x, testb39.dta$nortlm1x))
Results1b40 <-as.data.frame(contROC(testb40.dta$lpro1x, testb40.dta$drottmod1x, testb40.dta$nortlm1x))
Results1b41 <-as.data.frame(contROC(testb41.dta$lpro1x, testb41.dta$drottmod1x, testb41.dta$nortlm1x))
Results1b42 <-as.data.frame(contROC(testb42.dta$lpro1x, testb42.dta$drottmod1x, testb42.dta$nortlm1x))
Results1b43 <-as.data.frame(contROC(testb43.dta$lpro1x, testb43.dta$drottmod1x, testb43.dta$nortlm1x))
Results1b44 <-as.data.frame(contROC(testb44.dta$lpro1x, testb44.dta$drottmod1x, testb44.dta$nortlm1x))
Results1b45 <-as.data.frame(contROC(testb45.dta$lpro1x, testb45.dta$drottmod1x, testb45.dta$nortlm1x))
Results1b46 <-as.data.frame(contROC(testb46.dta$lpro1x, testb46.dta$drottmod1x, testb46.dta$nortlm1x))
Results1b47 <-as.data.frame(contROC(testb47.dta$lpro1x, testb47.dta$drottmod1x, testb47.dta$nortlm1x))
Results1b48 <-as.data.frame(contROC(testb48.dta$lpro1x, testb48.dta$drottmod1x, testb48.dta$nortlm1x))
Results1b49 <-as.data.frame(contROC(testb49.dta$lpro1x, testb49.dta$drottmod1x, testb49.dta$nortlm1x))
Results1b50 <-as.data.frame(contROC(testb50.dta$lpro1x, testb50.dta$drottmod1x, testb50.dta$nortlm1x))
Results1b51 <-as.data.frame(contROC(testb51.dta$lpro1x, testb51.dta$drottmod1x, testb51.dta$nortlm1x))
Results1b52 <-as.data.frame(contROC(testb52.dta$lpro1x, testb52.dta$drottmod1x, testb52.dta$nortlm1x))
Results1b53 <-as.data.frame(contROC(testb53.dta$lpro1x, testb53.dta$drottmod1x, testb53.dta$nortlm1x))
Results1b54 <-as.data.frame(contROC(testb54.dta$lpro1x, testb54.dta$drottmod1x, testb54.dta$nortlm1x))
Results1b55 <-as.data.frame(contROC(testb55.dta$lpro1x, testb55.dta$drottmod1x, testb55.dta$nortlm1x))
Results1b56 <-as.data.frame(contROC(testb56.dta$lpro1x, testb56.dta$drottmod1x, testb56.dta$nortlm1x))
Results1b57 <-as.data.frame(contROC(testb57.dta$lpro1x, testb57.dta$drottmod1x, testb57.dta$nortlm1x))
Results1b58 <-as.data.frame(contROC(testb58.dta$lpro1x, testb58.dta$drottmod1x, testb58.dta$nortlm1x))
Results1b59 <-as.data.frame(contROC(testb59.dta$lpro1x, testb59.dta$drottmod1x, testb59.dta$nortlm1x))
Results1b60 <-as.data.frame(contROC(testb60.dta$lpro1x, testb60.dta$drottmod1x, testb60.dta$nortlm1x))
Results1b61 <-as.data.frame(contROC(testb61.dta$lpro1x, testb61.dta$drottmod1x, testb61.dta$nortlm1x))
Results1b62 <-as.data.frame(contROC(testb62.dta$lpro1x, testb62.dta$drottmod1x, testb62.dta$nortlm1x))
Results1b63 <-as.data.frame(contROC(testb63.dta$lpro1x, testb63.dta$drottmod1x, testb63.dta$nortlm1x))
Results1b64 <-as.data.frame(contROC(testb64.dta$lpro1x, testb64.dta$drottmod1x, testb64.dta$nortlm1x))
Results1b65 <-as.data.frame(contROC(testb65.dta$lpro1x, testb65.dta$drottmod1x, testb65.dta$nortlm1x))
Results1b66 <-as.data.frame(contROC(testb66.dta$lpro1x, testb66.dta$drottmod1x, testb66.dta$nortlm1x))
Results1b67 <-as.data.frame(contROC(testb67.dta$lpro1x, testb67.dta$drottmod1x, testb67.dta$nortlm1x))
Results1b68 <-as.data.frame(contROC(testb68.dta$lpro1x, testb68.dta$drottmod1x, testb68.dta$nortlm1x))
Results1b69 <-as.data.frame(contROC(testb69.dta$lpro1x, testb69.dta$drottmod1x, testb69.dta$nortlm1x))
Results1b70 <-as.data.frame(contROC(testb70.dta$lpro1x, testb70.dta$drottmod1x, testb70.dta$nortlm1x))
Results1b71 <-as.data.frame(contROC(testb71.dta$lpro1x, testb71.dta$drottmod1x, testb71.dta$nortlm1x))
Results1b72 <-as.data.frame(contROC(testb72.dta$lpro1x, testb72.dta$drottmod1x, testb72.dta$nortlm1x))
Results1b73 <-as.data.frame(contROC(testb73.dta$lpro1x, testb73.dta$drottmod1x, testb73.dta$nortlm1x))
Results1b74 <-as.data.frame(contROC(testb74.dta$lpro1x, testb74.dta$drottmod1x, testb74.dta$nortlm1x))
Results1b75 <-as.data.frame(contROC(testb75.dta$lpro1x, testb75.dta$drottmod1x, testb75.dta$nortlm1x))
Results1b76 <-as.data.frame(contROC(testb76.dta$lpro1x, testb76.dta$drottmod1x, testb76.dta$nortlm1x))
Results1b77 <-as.data.frame(contROC(testb77.dta$lpro1x, testb77.dta$drottmod1x, testb77.dta$nortlm1x))
Results1b78 <-as.data.frame(contROC(testb78.dta$lpro1x, testb78.dta$drottmod1x, testb78.dta$nortlm1x))
Results1b79 <-as.data.frame(contROC(testb79.dta$lpro1x, testb79.dta$drottmod1x, testb79.dta$nortlm1x))
Results1b80 <-as.data.frame(contROC(testb80.dta$lpro1x, testb80.dta$drottmod1x, testb80.dta$nortlm1x))
Results1b81 <-as.data.frame(contROC(testb81.dta$lpro1x, testb81.dta$drottmod1x, testb81.dta$nortlm1x))
Results1b82 <-as.data.frame(contROC(testb82.dta$lpro1x, testb82.dta$drottmod1x, testb82.dta$nortlm1x))
Results1b83 <-as.data.frame(contROC(testb83.dta$lpro1x, testb83.dta$drottmod1x, testb83.dta$nortlm1x))
Results1b84 <-as.data.frame(contROC(testb84.dta$lpro1x, testb84.dta$drottmod1x, testb84.dta$nortlm1x))
Results1b85 <-as.data.frame(contROC(testb85.dta$lpro1x, testb85.dta$drottmod1x, testb85.dta$nortlm1x))
Results1b86 <-as.data.frame(contROC(testb86.dta$lpro1x, testb86.dta$drottmod1x, testb86.dta$nortlm1x))
Results1b87 <-as.data.frame(contROC(testb87.dta$lpro1x, testb87.dta$drottmod1x, testb87.dta$nortlm1x))
Results1b88 <-as.data.frame(contROC(testb88.dta$lpro1x, testb88.dta$drottmod1x, testb88.dta$nortlm1x))
Results1b89 <-as.data.frame(contROC(testb89.dta$lpro1x, testb89.dta$drottmod1x, testb89.dta$nortlm1x))
Results1b90 <-as.data.frame(contROC(testb90.dta$lpro1x, testb90.dta$drottmod1x, testb90.dta$nortlm1x))
Results1b91 <-as.data.frame(contROC(testb91.dta$lpro1x, testb91.dta$drottmod1x, testb91.dta$nortlm1x))
Results1b92 <-as.data.frame(contROC(testb92.dta$lpro1x, testb92.dta$drottmod1x, testb92.dta$nortlm1x))
Results1b93 <-as.data.frame(contROC(testb93.dta$lpro1x, testb93.dta$drottmod1x, testb93.dta$nortlm1x))
Results1b94 <-as.data.frame(contROC(testb94.dta$lpro1x, testb94.dta$drottmod1x, testb94.dta$nortlm1x))
Results1b95 <-as.data.frame(contROC(testb95.dta$lpro1x, testb95.dta$drottmod1x, testb95.dta$nortlm1x))
Results1b96 <-as.data.frame(contROC(testb96.dta$lpro1x, testb96.dta$drottmod1x, testb96.dta$nortlm1x))
Results1b97 <-as.data.frame(contROC(testb97.dta$lpro1x, testb97.dta$drottmod1x, testb97.dta$nortlm1x))
Results1b98 <-as.data.frame(contROC(testb98.dta$lpro1x, testb98.dta$drottmod1x, testb98.dta$nortlm1x))
Results1b99 <-as.data.frame(contROC(testb99.dta$lpro1x, testb99.dta$drottmod1x, testb99.dta$nortlm1x))
Results1b100 <-as.data.frame(contROC(testb100.dta$lpro1x, testb100.dta$drottmod1x, testb100.dta$nortlm1x))

Results1c1 <-as.data.frame(contROC(testc1.dta$lpro1x, testc1.dta$drottmod1x, testc1.dta$nortlm1x))
Results1c2 <-as.data.frame(contROC(testc2.dta$lpro1x, testc2.dta$drottmod1x, testc2.dta$nortlm1x))
Results1c3 <-as.data.frame(contROC(testc3.dta$lpro1x, testc3.dta$drottmod1x, testc3.dta$nortlm1x))
Results1c4 <-as.data.frame(contROC(testc4.dta$lpro1x, testc4.dta$drottmod1x, testc4.dta$nortlm1x))
Results1c5 <-as.data.frame(contROC(testc5.dta$lpro1x, testc5.dta$drottmod1x, testc5.dta$nortlm1x))
Results1c6 <-as.data.frame(contROC(testc6.dta$lpro1x, testc6.dta$drottmod1x, testc6.dta$nortlm1x))
Results1c7 <-as.data.frame(contROC(testc7.dta$lpro1x, testc7.dta$drottmod1x, testc7.dta$nortlm1x))
Results1c8 <-as.data.frame(contROC(testc8.dta$lpro1x, testc8.dta$drottmod1x, testc8.dta$nortlm1x))
Results1c9 <-as.data.frame(contROC(testc9.dta$lpro1x, testc9.dta$drottmod1x, testc9.dta$nortlm1x))
Results1c10 <-as.data.frame(contROC(testc10.dta$lpro1x, testc10.dta$drottmod1x, testc10.dta$nortlm1x))
Results1c11 <-as.data.frame(contROC(testc11.dta$lpro1x, testc11.dta$drottmod1x, testc11.dta$nortlm1x))
Results1c12 <-as.data.frame(contROC(testc12.dta$lpro1x, testc12.dta$drottmod1x, testc12.dta$nortlm1x))
Results1c13 <-as.data.frame(contROC(testc13.dta$lpro1x, testc13.dta$drottmod1x, testc13.dta$nortlm1x))
Results1c14 <-as.data.frame(contROC(testc14.dta$lpro1x, testc14.dta$drottmod1x, testc14.dta$nortlm1x))
Results1c15 <-as.data.frame(contROC(testc15.dta$lpro1x, testc15.dta$drottmod1x, testc15.dta$nortlm1x))
Results1c16 <-as.data.frame(contROC(testc16.dta$lpro1x, testc16.dta$drottmod1x, testc16.dta$nortlm1x))
Results1c17 <-as.data.frame(contROC(testc17.dta$lpro1x, testc17.dta$drottmod1x, testc17.dta$nortlm1x))
Results1c18 <-as.data.frame(contROC(testc18.dta$lpro1x, testc18.dta$drottmod1x, testc18.dta$nortlm1x))
Results1c19 <-as.data.frame(contROC(testc19.dta$lpro1x, testc19.dta$drottmod1x, testc19.dta$nortlm1x))
Results1c20 <-as.data.frame(contROC(testc20.dta$lpro1x, testc20.dta$drottmod1x, testc20.dta$nortlm1x))
Results1c21 <-as.data.frame(contROC(testc21.dta$lpro1x, testc21.dta$drottmod1x, testc21.dta$nortlm1x))
Results1c22 <-as.data.frame(contROC(testc22.dta$lpro1x, testc22.dta$drottmod1x, testc22.dta$nortlm1x))
Results1c23 <-as.data.frame(contROC(testc23.dta$lpro1x, testc23.dta$drottmod1x, testc23.dta$nortlm1x))
Results1c24 <-as.data.frame(contROC(testc24.dta$lpro1x, testc24.dta$drottmod1x, testc24.dta$nortlm1x))
Results1c25 <-as.data.frame(contROC(testc25.dta$lpro1x, testc25.dta$drottmod1x, testc25.dta$nortlm1x))
Results1c26 <-as.data.frame(contROC(testc26.dta$lpro1x, testc26.dta$drottmod1x, testc26.dta$nortlm1x))
Results1c27 <-as.data.frame(contROC(testc27.dta$lpro1x, testc27.dta$drottmod1x, testc27.dta$nortlm1x))
Results1c28 <-as.data.frame(contROC(testc28.dta$lpro1x, testc28.dta$drottmod1x, testc28.dta$nortlm1x))
Results1c29 <-as.data.frame(contROC(testc29.dta$lpro1x, testc29.dta$drottmod1x, testc29.dta$nortlm1x))
Results1c30 <-as.data.frame(contROC(testc30.dta$lpro1x, testc30.dta$drottmod1x, testc30.dta$nortlm1x))
Results1c31 <-as.data.frame(contROC(testc31.dta$lpro1x, testc31.dta$drottmod1x, testc31.dta$nortlm1x))
Results1c32 <-as.data.frame(contROC(testc32.dta$lpro1x, testc32.dta$drottmod1x, testc32.dta$nortlm1x))
Results1c33 <-as.data.frame(contROC(testc33.dta$lpro1x, testc33.dta$drottmod1x, testc33.dta$nortlm1x))
Results1c34 <-as.data.frame(contROC(testc34.dta$lpro1x, testc34.dta$drottmod1x, testc34.dta$nortlm1x))
Results1c35 <-as.data.frame(contROC(testc35.dta$lpro1x, testc35.dta$drottmod1x, testc35.dta$nortlm1x))
Results1c36 <-as.data.frame(contROC(testc36.dta$lpro1x, testc36.dta$drottmod1x, testc36.dta$nortlm1x))
Results1c37 <-as.data.frame(contROC(testc37.dta$lpro1x, testc37.dta$drottmod1x, testc37.dta$nortlm1x))
Results1c38 <-as.data.frame(contROC(testc38.dta$lpro1x, testc38.dta$drottmod1x, testc38.dta$nortlm1x))
Results1c39 <-as.data.frame(contROC(testc39.dta$lpro1x, testc39.dta$drottmod1x, testc39.dta$nortlm1x))
Results1c40 <-as.data.frame(contROC(testc40.dta$lpro1x, testc40.dta$drottmod1x, testc40.dta$nortlm1x))
Results1c41 <-as.data.frame(contROC(testc41.dta$lpro1x, testc41.dta$drottmod1x, testc41.dta$nortlm1x))
Results1c42 <-as.data.frame(contROC(testc42.dta$lpro1x, testc42.dta$drottmod1x, testc42.dta$nortlm1x))
Results1c43 <-as.data.frame(contROC(testc43.dta$lpro1x, testc43.dta$drottmod1x, testc43.dta$nortlm1x))
Results1c44 <-as.data.frame(contROC(testc44.dta$lpro1x, testc44.dta$drottmod1x, testc44.dta$nortlm1x))
Results1c45 <-as.data.frame(contROC(testc45.dta$lpro1x, testc45.dta$drottmod1x, testc45.dta$nortlm1x))
Results1c46 <-as.data.frame(contROC(testc46.dta$lpro1x, testc46.dta$drottmod1x, testc46.dta$nortlm1x))
Results1c47 <-as.data.frame(contROC(testc47.dta$lpro1x, testc47.dta$drottmod1x, testc47.dta$nortlm1x))
Results1c48 <-as.data.frame(contROC(testc48.dta$lpro1x, testc48.dta$drottmod1x, testc48.dta$nortlm1x))
Results1c49 <-as.data.frame(contROC(testc49.dta$lpro1x, testc49.dta$drottmod1x, testc49.dta$nortlm1x))
Results1c50 <-as.data.frame(contROC(testc50.dta$lpro1x, testc50.dta$drottmod1x, testc50.dta$nortlm1x))
Results1c51 <-as.data.frame(contROC(testc51.dta$lpro1x, testc51.dta$drottmod1x, testc51.dta$nortlm1x))
Results1c52 <-as.data.frame(contROC(testc52.dta$lpro1x, testc52.dta$drottmod1x, testc52.dta$nortlm1x))
Results1c53 <-as.data.frame(contROC(testc53.dta$lpro1x, testc53.dta$drottmod1x, testc53.dta$nortlm1x))
Results1c54 <-as.data.frame(contROC(testc54.dta$lpro1x, testc54.dta$drottmod1x, testc54.dta$nortlm1x))
Results1c55 <-as.data.frame(contROC(testc55.dta$lpro1x, testc55.dta$drottmod1x, testc55.dta$nortlm1x))
Results1c56 <-as.data.frame(contROC(testc56.dta$lpro1x, testc56.dta$drottmod1x, testc56.dta$nortlm1x))
Results1c57 <-as.data.frame(contROC(testc57.dta$lpro1x, testc57.dta$drottmod1x, testc57.dta$nortlm1x))
Results1c58 <-as.data.frame(contROC(testc58.dta$lpro1x, testc58.dta$drottmod1x, testc58.dta$nortlm1x))
Results1c59 <-as.data.frame(contROC(testc59.dta$lpro1x, testc59.dta$drottmod1x, testc59.dta$nortlm1x))
Results1c60 <-as.data.frame(contROC(testc60.dta$lpro1x, testc60.dta$drottmod1x, testc60.dta$nortlm1x))
Results1c61 <-as.data.frame(contROC(testc61.dta$lpro1x, testc61.dta$drottmod1x, testc61.dta$nortlm1x))
Results1c62 <-as.data.frame(contROC(testc62.dta$lpro1x, testc62.dta$drottmod1x, testc62.dta$nortlm1x))
Results1c63 <-as.data.frame(contROC(testc63.dta$lpro1x, testc63.dta$drottmod1x, testc63.dta$nortlm1x))
Results1c64 <-as.data.frame(contROC(testc64.dta$lpro1x, testc64.dta$drottmod1x, testc64.dta$nortlm1x))
Results1c65 <-as.data.frame(contROC(testc65.dta$lpro1x, testc65.dta$drottmod1x, testc65.dta$nortlm1x))
Results1c66 <-as.data.frame(contROC(testc66.dta$lpro1x, testc66.dta$drottmod1x, testc66.dta$nortlm1x))
Results1c67 <-as.data.frame(contROC(testc67.dta$lpro1x, testc67.dta$drottmod1x, testc67.dta$nortlm1x))
Results1c68 <-as.data.frame(contROC(testc68.dta$lpro1x, testc68.dta$drottmod1x, testc68.dta$nortlm1x))
Results1c69 <-as.data.frame(contROC(testc69.dta$lpro1x, testc69.dta$drottmod1x, testc69.dta$nortlm1x))
Results1c70 <-as.data.frame(contROC(testc70.dta$lpro1x, testc70.dta$drottmod1x, testc70.dta$nortlm1x))
Results1c71 <-as.data.frame(contROC(testc71.dta$lpro1x, testc71.dta$drottmod1x, testc71.dta$nortlm1x))
Results1c72 <-as.data.frame(contROC(testc72.dta$lpro1x, testc72.dta$drottmod1x, testc72.dta$nortlm1x))
Results1c73 <-as.data.frame(contROC(testc73.dta$lpro1x, testc73.dta$drottmod1x, testc73.dta$nortlm1x))
Results1c74 <-as.data.frame(contROC(testc74.dta$lpro1x, testc74.dta$drottmod1x, testc74.dta$nortlm1x))
Results1c75 <-as.data.frame(contROC(testc75.dta$lpro1x, testc75.dta$drottmod1x, testc75.dta$nortlm1x))
Results1c76 <-as.data.frame(contROC(testc76.dta$lpro1x, testc76.dta$drottmod1x, testc76.dta$nortlm1x))
Results1c77 <-as.data.frame(contROC(testc77.dta$lpro1x, testc77.dta$drottmod1x, testc77.dta$nortlm1x))
Results1c78 <-as.data.frame(contROC(testc78.dta$lpro1x, testc78.dta$drottmod1x, testc78.dta$nortlm1x))
Results1c79 <-as.data.frame(contROC(testc79.dta$lpro1x, testc79.dta$drottmod1x, testc79.dta$nortlm1x))
Results1c80 <-as.data.frame(contROC(testc80.dta$lpro1x, testc80.dta$drottmod1x, testc80.dta$nortlm1x))
Results1c81 <-as.data.frame(contROC(testc81.dta$lpro1x, testc81.dta$drottmod1x, testc81.dta$nortlm1x))
Results1c82 <-as.data.frame(contROC(testc82.dta$lpro1x, testc82.dta$drottmod1x, testc82.dta$nortlm1x))
Results1c83 <-as.data.frame(contROC(testc83.dta$lpro1x, testc83.dta$drottmod1x, testc83.dta$nortlm1x))
Results1c84 <-as.data.frame(contROC(testc84.dta$lpro1x, testc84.dta$drottmod1x, testc84.dta$nortlm1x))
Results1c85 <-as.data.frame(contROC(testc85.dta$lpro1x, testc85.dta$drottmod1x, testc85.dta$nortlm1x))
Results1c86 <-as.data.frame(contROC(testc86.dta$lpro1x, testc86.dta$drottmod1x, testc86.dta$nortlm1x))
Results1c87 <-as.data.frame(contROC(testc87.dta$lpro1x, testc87.dta$drottmod1x, testc87.dta$nortlm1x))
Results1c88 <-as.data.frame(contROC(testc88.dta$lpro1x, testc88.dta$drottmod1x, testc88.dta$nortlm1x))
Results1c89 <-as.data.frame(contROC(testc89.dta$lpro1x, testc89.dta$drottmod1x, testc89.dta$nortlm1x))
Results1c90 <-as.data.frame(contROC(testc90.dta$lpro1x, testc90.dta$drottmod1x, testc90.dta$nortlm1x))
Results1c91 <-as.data.frame(contROC(testc91.dta$lpro1x, testc91.dta$drottmod1x, testc91.dta$nortlm1x))
Results1c92 <-as.data.frame(contROC(testc92.dta$lpro1x, testc92.dta$drottmod1x, testc92.dta$nortlm1x))
Results1c93 <-as.data.frame(contROC(testc93.dta$lpro1x, testc93.dta$drottmod1x, testc93.dta$nortlm1x))
Results1c94 <-as.data.frame(contROC(testc94.dta$lpro1x, testc94.dta$drottmod1x, testc94.dta$nortlm1x))
Results1c95 <-as.data.frame(contROC(testc95.dta$lpro1x, testc95.dta$drottmod1x, testc95.dta$nortlm1x))
Results1c96 <-as.data.frame(contROC(testc96.dta$lpro1x, testc96.dta$drottmod1x, testc96.dta$nortlm1x))
Results1c97 <-as.data.frame(contROC(testc97.dta$lpro1x, testc97.dta$drottmod1x, testc97.dta$nortlm1x))
Results1c98 <-as.data.frame(contROC(testc98.dta$lpro1x, testc98.dta$drottmod1x, testc98.dta$nortlm1x))
Results1c99 <-as.data.frame(contROC(testc99.dta$lpro1x, testc99.dta$drottmod1x, testc99.dta$nortlm1x))
Results1c100 <-as.data.frame(contROC(testc100.dta$lpro1x, testc100.dta$drottmod1x, testc100.dta$nortlm1x))

Results1d1 <-as.data.frame(contROC(testd1.dta$lpro1x, testd1.dta$drottmod1x, testd1.dta$nortlm1x))
Results1d2 <-as.data.frame(contROC(testd2.dta$lpro1x, testd2.dta$drottmod1x, testd2.dta$nortlm1x))
Results1d3 <-as.data.frame(contROC(testd3.dta$lpro1x, testd3.dta$drottmod1x, testd3.dta$nortlm1x))
Results1d4 <-as.data.frame(contROC(testd4.dta$lpro1x, testd4.dta$drottmod1x, testd4.dta$nortlm1x))
Results1d5 <-as.data.frame(contROC(testd5.dta$lpro1x, testd5.dta$drottmod1x, testd5.dta$nortlm1x))
Results1d6 <-as.data.frame(contROC(testd6.dta$lpro1x, testd6.dta$drottmod1x, testd6.dta$nortlm1x))
Results1d7 <-as.data.frame(contROC(testd7.dta$lpro1x, testd7.dta$drottmod1x, testd7.dta$nortlm1x))
Results1d8 <-as.data.frame(contROC(testd8.dta$lpro1x, testd8.dta$drottmod1x, testd8.dta$nortlm1x))
Results1d9 <-as.data.frame(contROC(testd9.dta$lpro1x, testd9.dta$drottmod1x, testd9.dta$nortlm1x))
Results1d10 <-as.data.frame(contROC(testd10.dta$lpro1x, testd10.dta$drottmod1x, testd10.dta$nortlm1x))
Results1d11 <-as.data.frame(contROC(testd11.dta$lpro1x, testd11.dta$drottmod1x, testd11.dta$nortlm1x))
Results1d12 <-as.data.frame(contROC(testd12.dta$lpro1x, testd12.dta$drottmod1x, testd12.dta$nortlm1x))
Results1d13 <-as.data.frame(contROC(testd13.dta$lpro1x, testd13.dta$drottmod1x, testd13.dta$nortlm1x))
Results1d14 <-as.data.frame(contROC(testd14.dta$lpro1x, testd14.dta$drottmod1x, testd14.dta$nortlm1x))
Results1d15 <-as.data.frame(contROC(testd15.dta$lpro1x, testd15.dta$drottmod1x, testd15.dta$nortlm1x))
Results1d16 <-as.data.frame(contROC(testd16.dta$lpro1x, testd16.dta$drottmod1x, testd16.dta$nortlm1x))
Results1d17 <-as.data.frame(contROC(testd17.dta$lpro1x, testd17.dta$drottmod1x, testd17.dta$nortlm1x))
Results1d18 <-as.data.frame(contROC(testd18.dta$lpro1x, testd18.dta$drottmod1x, testd18.dta$nortlm1x))
Results1d19 <-as.data.frame(contROC(testd19.dta$lpro1x, testd19.dta$drottmod1x, testd19.dta$nortlm1x))
Results1d20 <-as.data.frame(contROC(testd20.dta$lpro1x, testd20.dta$drottmod1x, testd20.dta$nortlm1x))
Results1d21 <-as.data.frame(contROC(testd21.dta$lpro1x, testd21.dta$drottmod1x, testd21.dta$nortlm1x))
Results1d22 <-as.data.frame(contROC(testd22.dta$lpro1x, testd22.dta$drottmod1x, testd22.dta$nortlm1x))
Results1d23 <-as.data.frame(contROC(testd23.dta$lpro1x, testd23.dta$drottmod1x, testd23.dta$nortlm1x))
Results1d24 <-as.data.frame(contROC(testd24.dta$lpro1x, testd24.dta$drottmod1x, testd24.dta$nortlm1x))
Results1d25 <-as.data.frame(contROC(testd25.dta$lpro1x, testd25.dta$drottmod1x, testd25.dta$nortlm1x))
Results1d26 <-as.data.frame(contROC(testd26.dta$lpro1x, testd26.dta$drottmod1x, testd26.dta$nortlm1x))
Results1d27 <-as.data.frame(contROC(testd27.dta$lpro1x, testd27.dta$drottmod1x, testd27.dta$nortlm1x))
Results1d28 <-as.data.frame(contROC(testd28.dta$lpro1x, testd28.dta$drottmod1x, testd28.dta$nortlm1x))
Results1d29 <-as.data.frame(contROC(testd29.dta$lpro1x, testd29.dta$drottmod1x, testd29.dta$nortlm1x))
Results1d30 <-as.data.frame(contROC(testd30.dta$lpro1x, testd30.dta$drottmod1x, testd30.dta$nortlm1x))
Results1d31 <-as.data.frame(contROC(testd31.dta$lpro1x, testd31.dta$drottmod1x, testd31.dta$nortlm1x))
Results1d32 <-as.data.frame(contROC(testd32.dta$lpro1x, testd32.dta$drottmod1x, testd32.dta$nortlm1x))
Results1d33 <-as.data.frame(contROC(testd33.dta$lpro1x, testd33.dta$drottmod1x, testd33.dta$nortlm1x))
Results1d34 <-as.data.frame(contROC(testd34.dta$lpro1x, testd34.dta$drottmod1x, testd34.dta$nortlm1x))
Results1d35 <-as.data.frame(contROC(testd35.dta$lpro1x, testd35.dta$drottmod1x, testd35.dta$nortlm1x))
Results1d36 <-as.data.frame(contROC(testd36.dta$lpro1x, testd36.dta$drottmod1x, testd36.dta$nortlm1x))
Results1d37 <-as.data.frame(contROC(testd37.dta$lpro1x, testd37.dta$drottmod1x, testd37.dta$nortlm1x))
Results1d38 <-as.data.frame(contROC(testd38.dta$lpro1x, testd38.dta$drottmod1x, testd38.dta$nortlm1x))
Results1d39 <-as.data.frame(contROC(testd39.dta$lpro1x, testd39.dta$drottmod1x, testd39.dta$nortlm1x))
Results1d40 <-as.data.frame(contROC(testd40.dta$lpro1x, testd40.dta$drottmod1x, testd40.dta$nortlm1x))
Results1d41 <-as.data.frame(contROC(testd41.dta$lpro1x, testd41.dta$drottmod1x, testd41.dta$nortlm1x))
Results1d42 <-as.data.frame(contROC(testd42.dta$lpro1x, testd42.dta$drottmod1x, testd42.dta$nortlm1x))
Results1d43 <-as.data.frame(contROC(testd43.dta$lpro1x, testd43.dta$drottmod1x, testd43.dta$nortlm1x))
Results1d44 <-as.data.frame(contROC(testd44.dta$lpro1x, testd44.dta$drottmod1x, testd44.dta$nortlm1x))
Results1d45 <-as.data.frame(contROC(testd45.dta$lpro1x, testd45.dta$drottmod1x, testd45.dta$nortlm1x))
Results1d46 <-as.data.frame(contROC(testd46.dta$lpro1x, testd46.dta$drottmod1x, testd46.dta$nortlm1x))
Results1d47 <-as.data.frame(contROC(testd47.dta$lpro1x, testd47.dta$drottmod1x, testd47.dta$nortlm1x))
Results1d48 <-as.data.frame(contROC(testd48.dta$lpro1x, testd48.dta$drottmod1x, testd48.dta$nortlm1x))
Results1d49 <-as.data.frame(contROC(testd49.dta$lpro1x, testd49.dta$drottmod1x, testd49.dta$nortlm1x))
Results1d50 <-as.data.frame(contROC(testd50.dta$lpro1x, testd50.dta$drottmod1x, testd50.dta$nortlm1x))
Results1d51 <-as.data.frame(contROC(testd51.dta$lpro1x, testd51.dta$drottmod1x, testd51.dta$nortlm1x))
Results1d52 <-as.data.frame(contROC(testd52.dta$lpro1x, testd52.dta$drottmod1x, testd52.dta$nortlm1x))
Results1d53 <-as.data.frame(contROC(testd53.dta$lpro1x, testd53.dta$drottmod1x, testd53.dta$nortlm1x))
Results1d54 <-as.data.frame(contROC(testd54.dta$lpro1x, testd54.dta$drottmod1x, testd54.dta$nortlm1x))
Results1d55 <-as.data.frame(contROC(testd55.dta$lpro1x, testd55.dta$drottmod1x, testd55.dta$nortlm1x))
Results1d56 <-as.data.frame(contROC(testd56.dta$lpro1x, testd56.dta$drottmod1x, testd56.dta$nortlm1x))
Results1d57 <-as.data.frame(contROC(testd57.dta$lpro1x, testd57.dta$drottmod1x, testd57.dta$nortlm1x))
Results1d58 <-as.data.frame(contROC(testd58.dta$lpro1x, testd58.dta$drottmod1x, testd58.dta$nortlm1x))
Results1d59 <-as.data.frame(contROC(testd59.dta$lpro1x, testd59.dta$drottmod1x, testd59.dta$nortlm1x))
Results1d60 <-as.data.frame(contROC(testd60.dta$lpro1x, testd60.dta$drottmod1x, testd60.dta$nortlm1x))
Results1d61 <-as.data.frame(contROC(testd61.dta$lpro1x, testd61.dta$drottmod1x, testd61.dta$nortlm1x))
Results1d62 <-as.data.frame(contROC(testd62.dta$lpro1x, testd62.dta$drottmod1x, testd62.dta$nortlm1x))
Results1d63 <-as.data.frame(contROC(testd63.dta$lpro1x, testd63.dta$drottmod1x, testd63.dta$nortlm1x))
Results1d64 <-as.data.frame(contROC(testd64.dta$lpro1x, testd64.dta$drottmod1x, testd64.dta$nortlm1x))
Results1d65 <-as.data.frame(contROC(testd65.dta$lpro1x, testd65.dta$drottmod1x, testd65.dta$nortlm1x))
Results1d66 <-as.data.frame(contROC(testd66.dta$lpro1x, testd66.dta$drottmod1x, testd66.dta$nortlm1x))
Results1d67 <-as.data.frame(contROC(testd67.dta$lpro1x, testd67.dta$drottmod1x, testd67.dta$nortlm1x))
Results1d68 <-as.data.frame(contROC(testd68.dta$lpro1x, testd68.dta$drottmod1x, testd68.dta$nortlm1x))
Results1d69 <-as.data.frame(contROC(testd69.dta$lpro1x, testd69.dta$drottmod1x, testd69.dta$nortlm1x))
Results1d70 <-as.data.frame(contROC(testd70.dta$lpro1x, testd70.dta$drottmod1x, testd70.dta$nortlm1x))
Results1d71 <-as.data.frame(contROC(testd71.dta$lpro1x, testd71.dta$drottmod1x, testd71.dta$nortlm1x))
Results1d72 <-as.data.frame(contROC(testd72.dta$lpro1x, testd72.dta$drottmod1x, testd72.dta$nortlm1x))
Results1d73 <-as.data.frame(contROC(testd73.dta$lpro1x, testd73.dta$drottmod1x, testd73.dta$nortlm1x))
Results1d74 <-as.data.frame(contROC(testd74.dta$lpro1x, testd74.dta$drottmod1x, testd74.dta$nortlm1x))
Results1d75 <-as.data.frame(contROC(testd75.dta$lpro1x, testd75.dta$drottmod1x, testd75.dta$nortlm1x))
Results1d76 <-as.data.frame(contROC(testd76.dta$lpro1x, testd76.dta$drottmod1x, testd76.dta$nortlm1x))
Results1d77 <-as.data.frame(contROC(testd77.dta$lpro1x, testd77.dta$drottmod1x, testd77.dta$nortlm1x))
Results1d78 <-as.data.frame(contROC(testd78.dta$lpro1x, testd78.dta$drottmod1x, testd78.dta$nortlm1x))
Results1d79 <-as.data.frame(contROC(testd79.dta$lpro1x, testd79.dta$drottmod1x, testd79.dta$nortlm1x))
Results1d80 <-as.data.frame(contROC(testd80.dta$lpro1x, testd80.dta$drottmod1x, testd80.dta$nortlm1x))
Results1d81 <-as.data.frame(contROC(testd81.dta$lpro1x, testd81.dta$drottmod1x, testd81.dta$nortlm1x))
Results1d82 <-as.data.frame(contROC(testd82.dta$lpro1x, testd82.dta$drottmod1x, testd82.dta$nortlm1x))
Results1d83 <-as.data.frame(contROC(testd83.dta$lpro1x, testd83.dta$drottmod1x, testd83.dta$nortlm1x))
Results1d84 <-as.data.frame(contROC(testd84.dta$lpro1x, testd84.dta$drottmod1x, testd84.dta$nortlm1x))
Results1d85 <-as.data.frame(contROC(testd85.dta$lpro1x, testd85.dta$drottmod1x, testd85.dta$nortlm1x))
Results1d86 <-as.data.frame(contROC(testd86.dta$lpro1x, testd86.dta$drottmod1x, testd86.dta$nortlm1x))
Results1d87 <-as.data.frame(contROC(testd87.dta$lpro1x, testd87.dta$drottmod1x, testd87.dta$nortlm1x))
Results1d88 <-as.data.frame(contROC(testd88.dta$lpro1x, testd88.dta$drottmod1x, testd88.dta$nortlm1x))
Results1d89 <-as.data.frame(contROC(testd89.dta$lpro1x, testd89.dta$drottmod1x, testd89.dta$nortlm1x))
Results1d90 <-as.data.frame(contROC(testd90.dta$lpro1x, testd90.dta$drottmod1x, testd90.dta$nortlm1x))
Results1d91 <-as.data.frame(contROC(testd91.dta$lpro1x, testd91.dta$drottmod1x, testd91.dta$nortlm1x))
Results1d92 <-as.data.frame(contROC(testd92.dta$lpro1x, testd92.dta$drottmod1x, testd92.dta$nortlm1x))
Results1d93 <-as.data.frame(contROC(testd93.dta$lpro1x, testd93.dta$drottmod1x, testd93.dta$nortlm1x))
Results1d94 <-as.data.frame(contROC(testd94.dta$lpro1x, testd94.dta$drottmod1x, testd94.dta$nortlm1x))
Results1d95 <-as.data.frame(contROC(testd95.dta$lpro1x, testd95.dta$drottmod1x, testd95.dta$nortlm1x))
Results1d96 <-as.data.frame(contROC(testd96.dta$lpro1x, testd96.dta$drottmod1x, testd96.dta$nortlm1x))
Results1d97 <-as.data.frame(contROC(testd97.dta$lpro1x, testd97.dta$drottmod1x, testd97.dta$nortlm1x))
Results1d98 <-as.data.frame(contROC(testd98.dta$lpro1x, testd98.dta$drottmod1x, testd98.dta$nortlm1x))
Results1d99 <-as.data.frame(contROC(testd99.dta$lpro1x, testd99.dta$drottmod1x, testd99.dta$nortlm1x))
Results1d100 <-as.data.frame(contROC(testd100.dta$lpro1x, testd100.dta$drottmod1x, testd100.dta$nortlm1x))


# append results #

Results1aALL <- rbind(Results1a1, Results1a2, Results1a3, Results1a4, Results1a5, Results1a6, Results1a7, Results1a8, Results1a9, Results1a10, Results1a11, Results1a12, Results1a13, Results1a14, Results1a15, Results1a16, Results1a17, Results1a18, Results1a19, Results1a20, Results1a21, Results1a22, Results1a23, Results1a24, Results1a25, Results1a26, Results1a27, Results1a28, Results1a29, Results1a30, Results1a31, Results1a32, Results1a33, Results1a34, Results1a35, Results1a36, Results1a37, Results1a38, Results1a39, Results1a40, Results1a41, Results1a42, Results1a43, Results1a44, Results1a45, Results1a46, Results1a47, Results1a48, Results1a49, Results1a50, Results1a51, Results1a52, Results1a53, Results1a54, Results1a55, Results1a56, Results1a57, Results1a58, Results1a59, Results1a60, Results1a61, Results1a62, Results1a63, Results1a64, Results1a65, Results1a66, Results1a67, Results1a68, Results1a69, Results1a70, Results1a71, Results1a72, Results1a73, Results1a74, Results1a75, Results1a76, Results1a77, Results1a78, Results1a79, Results1a80, Results1a81, Results1a82, Results1a83, Results1a84, Results1a85, Results1a86, Results1a87, Results1a88, Results1a89, Results1a90, Results1a91, Results1a92, Results1a93, Results1a94, Results1a95, Results1a96, Results1a97, Results1a98, Results1a99, Results1a100)



Results1bALL <- rbind(Results1b1, Results1b2, Results1b3, Results1b4, Results1b5, Results1b6, Results1b7, Results1b8, Results1b9, Results1b10, Results1b11, Results1b12, Results1b13, Results1b14, Results1b15, Results1b16, Results1b17, Results1b18, Results1b19, Results1b20, Results1b21, Results1b22, Results1b23, Results1b24, Results1b25, Results1b26, Results1b27, Results1b28, Results1b29, Results1b30, Results1b31, Results1b32, Results1b33, Results1b34, Results1b35, Results1b36, Results1b37, Results1b38, Results1b39, Results1b40, Results1b41, Results1b42, Results1b43, Results1b44, Results1b45, Results1b46, Results1b47, Results1b48, Results1b49, Results1b50, Results1b51, Results1b52, Results1b53, Results1b54, Results1b55, Results1b56, Results1b57, Results1b58, Results1b59, Results1b60, Results1b61, Results1b62, Results1b63, Results1b64, Results1b65, Results1b66, Results1b67, Results1b68, Results1b69, Results1b70, Results1b71, Results1b72, Results1b73, Results1b74, Results1b75, Results1b76, Results1b77, Results1b78, Results1b79, Results1b80, Results1b81, Results1b82, Results1b83, Results1b84, Results1b85, Results1b86, Results1b87, Results1b88, Results1b89, Results1b90, Results1b91, Results1b92, Results1b93, Results1b94, Results1b95, Results1b96, Results1b97, Results1b98, Results1b99, Results1b100)

Results1cALL <- rbind(Results1c1, Results1c2, Results1c3, Results1c4, Results1c5, Results1c6, Results1c7, Results1c8, Results1c9, Results1c10, Results1c11, Results1c12, Results1c13, Results1c14, Results1c15, Results1c16, Results1c17, Results1c18, Results1c19, Results1c20, Results1c21, Results1c22, Results1c23, Results1c24, Results1c25, Results1c26, Results1c27, Results1c28, Results1c29, Results1c30, Results1c31, Results1c32, Results1c33, Results1c34, Results1c35, Results1c36, Results1c37, Results1c38, Results1c39, Results1c40, Results1c41, Results1c42, Results1c43, Results1c44, Results1c45, Results1c46, Results1c47, Results1c48, Results1c49, Results1c50, Results1c51, Results1c52, Results1c53, Results1c54, Results1c55, Results1c56, Results1c57, Results1c58, Results1c59, Results1c60, Results1c61, Results1c62, Results1c63, Results1c64, Results1c65, Results1c66, Results1c67, Results1c68, Results1c69, Results1c70, Results1c71, Results1c72, Results1c73, Results1c74, Results1c75, Results1c76, Results1c77, Results1c78, Results1c79, Results1c80, Results1c81, Results1c82, Results1c83, Results1c84, Results1c85, Results1c86, Results1c87, Results1c88, Results1c89, Results1c90, Results1c91, Results1c92, Results1c93, Results1c94, Results1c95, Results1c96, Results1c97, Results1c98, Results1c99, Results1c100)

Results1dALL <- rbind(Results1d1, Results1d2, Results1d3, Results1d4, Results1d5, Results1d6, Results1d7, Results1d8, Results1d9, Results1d10, Results1d11, Results1d12, Results1d13, Results1d14, Results1d15, Results1d16, Results1d17, Results1d18, Results1d19, Results1d20, Results1d21, Results1d22, Results1d23, Results1d24, Results1d25, Results1d26, Results1d27, Results1d28, Results1d29, Results1d30, Results1d31, Results1d32, Results1d33, Results1d34, Results1d35, Results1d36, Results1d37, Results1d38, Results1d39, Results1d40, Results1d41, Results1d42, Results1d43, Results1d44, Results1d45, Results1d46, Results1d47, Results1d48, Results1d49, Results1d50, Results1d51, Results1d52, Results1d53, Results1d54, Results1d55, Results1d56, Results1d57, Results1d58, Results1d59, Results1d60, Results1d61, Results1d62, Results1d63, Results1d64, Results1d65, Results1d66, Results1d67, Results1d68, Results1d69, Results1d70, Results1d71, Results1d72, Results1d73, Results1d74, Results1d75, Results1d76, Results1d77, Results1d78, Results1d79, Results1d80, Results1d81, Results1d82, Results1d83, Results1d84, Results1d85, Results1d86, Results1d87, Results1d88, Results1d89, Results1d90, Results1d91, Results1d92, Results1d93, Results1d94, Results1d95, Results1d96, Results1d97, Results1d98, Results1d99, Results1d100)

Results1ALL <- rbind(Results1aALL, Results1bALL, Results1cALL, Results1dALL)
YD1ALL <- Results1ALL[seq(1, nrow(Results1ALL), 2), ]
NoRTLM1ALL <- Results1ALL[seq(2, nrow(Results1ALL), 2), ]




# individual violence #

Results2a1 <-as.data.frame(contROC(testa1.dta$lpro2x, testa1.dta$drottmod2x, testa1.dta$nortlm2x))
Results2a2 <-as.data.frame(contROC(testa2.dta$lpro2x, testa2.dta$drottmod2x, testa2.dta$nortlm2x))
Results2a3 <-as.data.frame(contROC(testa3.dta$lpro2x, testa3.dta$drottmod2x, testa3.dta$nortlm2x))
Results2a4 <-as.data.frame(contROC(testa4.dta$lpro2x, testa4.dta$drottmod2x, testa4.dta$nortlm2x))
Results2a5 <-as.data.frame(contROC(testa5.dta$lpro2x, testa5.dta$drottmod2x, testa5.dta$nortlm2x))
Results2a6 <-as.data.frame(contROC(testa6.dta$lpro2x, testa6.dta$drottmod2x, testa6.dta$nortlm2x))
Results2a7 <-as.data.frame(contROC(testa7.dta$lpro2x, testa7.dta$drottmod2x, testa7.dta$nortlm2x))
Results2a8 <-as.data.frame(contROC(testa8.dta$lpro2x, testa8.dta$drottmod2x, testa8.dta$nortlm2x))
Results2a9 <-as.data.frame(contROC(testa9.dta$lpro2x, testa9.dta$drottmod2x, testa9.dta$nortlm2x))
Results2a10 <-as.data.frame(contROC(testa10.dta$lpro2x, testa10.dta$drottmod2x, testa10.dta$nortlm2x))
Results2a11 <-as.data.frame(contROC(testa11.dta$lpro2x, testa11.dta$drottmod2x, testa11.dta$nortlm2x))
Results2a12 <-as.data.frame(contROC(testa12.dta$lpro2x, testa12.dta$drottmod2x, testa12.dta$nortlm2x))
Results2a13 <-as.data.frame(contROC(testa13.dta$lpro2x, testa13.dta$drottmod2x, testa13.dta$nortlm2x))
Results2a14 <-as.data.frame(contROC(testa14.dta$lpro2x, testa14.dta$drottmod2x, testa14.dta$nortlm2x))
Results2a15 <-as.data.frame(contROC(testa15.dta$lpro2x, testa15.dta$drottmod2x, testa15.dta$nortlm2x))
Results2a16 <-as.data.frame(contROC(testa16.dta$lpro2x, testa16.dta$drottmod2x, testa16.dta$nortlm2x))
Results2a17 <-as.data.frame(contROC(testa17.dta$lpro2x, testa17.dta$drottmod2x, testa17.dta$nortlm2x))
Results2a18 <-as.data.frame(contROC(testa18.dta$lpro2x, testa18.dta$drottmod2x, testa18.dta$nortlm2x))
Results2a19 <-as.data.frame(contROC(testa19.dta$lpro2x, testa19.dta$drottmod2x, testa19.dta$nortlm2x))
Results2a20 <-as.data.frame(contROC(testa20.dta$lpro2x, testa20.dta$drottmod2x, testa20.dta$nortlm2x))
Results2a21 <-as.data.frame(contROC(testa21.dta$lpro2x, testa21.dta$drottmod2x, testa21.dta$nortlm2x))
Results2a22 <-as.data.frame(contROC(testa22.dta$lpro2x, testa22.dta$drottmod2x, testa22.dta$nortlm2x))
Results2a23 <-as.data.frame(contROC(testa23.dta$lpro2x, testa23.dta$drottmod2x, testa23.dta$nortlm2x))
Results2a24 <-as.data.frame(contROC(testa24.dta$lpro2x, testa24.dta$drottmod2x, testa24.dta$nortlm2x))
Results2a25 <-as.data.frame(contROC(testa25.dta$lpro2x, testa25.dta$drottmod2x, testa25.dta$nortlm2x))
Results2a26 <-as.data.frame(contROC(testa26.dta$lpro2x, testa26.dta$drottmod2x, testa26.dta$nortlm2x))
Results2a27 <-as.data.frame(contROC(testa27.dta$lpro2x, testa27.dta$drottmod2x, testa27.dta$nortlm2x))
Results2a28 <-as.data.frame(contROC(testa28.dta$lpro2x, testa28.dta$drottmod2x, testa28.dta$nortlm2x))
Results2a29 <-as.data.frame(contROC(testa29.dta$lpro2x, testa29.dta$drottmod2x, testa29.dta$nortlm2x))
Results2a30 <-as.data.frame(contROC(testa30.dta$lpro2x, testa30.dta$drottmod2x, testa30.dta$nortlm2x))
Results2a31 <-as.data.frame(contROC(testa31.dta$lpro2x, testa31.dta$drottmod2x, testa31.dta$nortlm2x))
Results2a32 <-as.data.frame(contROC(testa32.dta$lpro2x, testa32.dta$drottmod2x, testa32.dta$nortlm2x))
Results2a33 <-as.data.frame(contROC(testa33.dta$lpro2x, testa33.dta$drottmod2x, testa33.dta$nortlm2x))
Results2a34 <-as.data.frame(contROC(testa34.dta$lpro2x, testa34.dta$drottmod2x, testa34.dta$nortlm2x))
Results2a35 <-as.data.frame(contROC(testa35.dta$lpro2x, testa35.dta$drottmod2x, testa35.dta$nortlm2x))
Results2a36 <-as.data.frame(contROC(testa36.dta$lpro2x, testa36.dta$drottmod2x, testa36.dta$nortlm2x))
Results2a37 <-as.data.frame(contROC(testa37.dta$lpro2x, testa37.dta$drottmod2x, testa37.dta$nortlm2x))
Results2a38 <-as.data.frame(contROC(testa38.dta$lpro2x, testa38.dta$drottmod2x, testa38.dta$nortlm2x))
Results2a39 <-as.data.frame(contROC(testa39.dta$lpro2x, testa39.dta$drottmod2x, testa39.dta$nortlm2x))
Results2a40 <-as.data.frame(contROC(testa40.dta$lpro2x, testa40.dta$drottmod2x, testa40.dta$nortlm2x))
Results2a41 <-as.data.frame(contROC(testa41.dta$lpro2x, testa41.dta$drottmod2x, testa41.dta$nortlm2x))
Results2a42 <-as.data.frame(contROC(testa42.dta$lpro2x, testa42.dta$drottmod2x, testa42.dta$nortlm2x))
Results2a43 <-as.data.frame(contROC(testa43.dta$lpro2x, testa43.dta$drottmod2x, testa43.dta$nortlm2x))
Results2a44 <-as.data.frame(contROC(testa44.dta$lpro2x, testa44.dta$drottmod2x, testa44.dta$nortlm2x))
Results2a45 <-as.data.frame(contROC(testa45.dta$lpro2x, testa45.dta$drottmod2x, testa45.dta$nortlm2x))
Results2a46 <-as.data.frame(contROC(testa46.dta$lpro2x, testa46.dta$drottmod2x, testa46.dta$nortlm2x))
Results2a47 <-as.data.frame(contROC(testa47.dta$lpro2x, testa47.dta$drottmod2x, testa47.dta$nortlm2x))
Results2a48 <-as.data.frame(contROC(testa48.dta$lpro2x, testa48.dta$drottmod2x, testa48.dta$nortlm2x))
Results2a49 <-as.data.frame(contROC(testa49.dta$lpro2x, testa49.dta$drottmod2x, testa49.dta$nortlm2x))
Results2a50 <-as.data.frame(contROC(testa50.dta$lpro2x, testa50.dta$drottmod2x, testa50.dta$nortlm2x))
Results2a51 <-as.data.frame(contROC(testa51.dta$lpro2x, testa51.dta$drottmod2x, testa51.dta$nortlm2x))
Results2a52 <-as.data.frame(contROC(testa52.dta$lpro2x, testa52.dta$drottmod2x, testa52.dta$nortlm2x))
Results2a53 <-as.data.frame(contROC(testa53.dta$lpro2x, testa53.dta$drottmod2x, testa53.dta$nortlm2x))
Results2a54 <-as.data.frame(contROC(testa54.dta$lpro2x, testa54.dta$drottmod2x, testa54.dta$nortlm2x))
Results2a55 <-as.data.frame(contROC(testa55.dta$lpro2x, testa55.dta$drottmod2x, testa55.dta$nortlm2x))
Results2a56 <-as.data.frame(contROC(testa56.dta$lpro2x, testa56.dta$drottmod2x, testa56.dta$nortlm2x))
Results2a57 <-as.data.frame(contROC(testa57.dta$lpro2x, testa57.dta$drottmod2x, testa57.dta$nortlm2x))
Results2a58 <-as.data.frame(contROC(testa58.dta$lpro2x, testa58.dta$drottmod2x, testa58.dta$nortlm2x))
Results2a59 <-as.data.frame(contROC(testa59.dta$lpro2x, testa59.dta$drottmod2x, testa59.dta$nortlm2x))
Results2a60 <-as.data.frame(contROC(testa60.dta$lpro2x, testa60.dta$drottmod2x, testa60.dta$nortlm2x))
Results2a61 <-as.data.frame(contROC(testa61.dta$lpro2x, testa61.dta$drottmod2x, testa61.dta$nortlm2x))
Results2a62 <-as.data.frame(contROC(testa62.dta$lpro2x, testa62.dta$drottmod2x, testa62.dta$nortlm2x))
Results2a63 <-as.data.frame(contROC(testa63.dta$lpro2x, testa63.dta$drottmod2x, testa63.dta$nortlm2x))
Results2a64 <-as.data.frame(contROC(testa64.dta$lpro2x, testa64.dta$drottmod2x, testa64.dta$nortlm2x))
Results2a65 <-as.data.frame(contROC(testa65.dta$lpro2x, testa65.dta$drottmod2x, testa65.dta$nortlm2x))
Results2a66 <-as.data.frame(contROC(testa66.dta$lpro2x, testa66.dta$drottmod2x, testa66.dta$nortlm2x))
Results2a67 <-as.data.frame(contROC(testa67.dta$lpro2x, testa67.dta$drottmod2x, testa67.dta$nortlm2x))
Results2a68 <-as.data.frame(contROC(testa68.dta$lpro2x, testa68.dta$drottmod2x, testa68.dta$nortlm2x))
Results2a69 <-as.data.frame(contROC(testa69.dta$lpro2x, testa69.dta$drottmod2x, testa69.dta$nortlm2x))
Results2a70 <-as.data.frame(contROC(testa70.dta$lpro2x, testa70.dta$drottmod2x, testa70.dta$nortlm2x))
Results2a71 <-as.data.frame(contROC(testa71.dta$lpro2x, testa71.dta$drottmod2x, testa71.dta$nortlm2x))
Results2a72 <-as.data.frame(contROC(testa72.dta$lpro2x, testa72.dta$drottmod2x, testa72.dta$nortlm2x))
Results2a73 <-as.data.frame(contROC(testa73.dta$lpro2x, testa73.dta$drottmod2x, testa73.dta$nortlm2x))
Results2a74 <-as.data.frame(contROC(testa74.dta$lpro2x, testa74.dta$drottmod2x, testa74.dta$nortlm2x))
Results2a75 <-as.data.frame(contROC(testa75.dta$lpro2x, testa75.dta$drottmod2x, testa75.dta$nortlm2x))
Results2a76 <-as.data.frame(contROC(testa76.dta$lpro2x, testa76.dta$drottmod2x, testa76.dta$nortlm2x))
Results2a77 <-as.data.frame(contROC(testa77.dta$lpro2x, testa77.dta$drottmod2x, testa77.dta$nortlm2x))
Results2a78 <-as.data.frame(contROC(testa78.dta$lpro2x, testa78.dta$drottmod2x, testa78.dta$nortlm2x))
Results2a79 <-as.data.frame(contROC(testa79.dta$lpro2x, testa79.dta$drottmod2x, testa79.dta$nortlm2x))
Results2a80 <-as.data.frame(contROC(testa80.dta$lpro2x, testa80.dta$drottmod2x, testa80.dta$nortlm2x))
Results2a81 <-as.data.frame(contROC(testa81.dta$lpro2x, testa81.dta$drottmod2x, testa81.dta$nortlm2x))
Results2a82 <-as.data.frame(contROC(testa82.dta$lpro2x, testa82.dta$drottmod2x, testa82.dta$nortlm2x))
Results2a83 <-as.data.frame(contROC(testa83.dta$lpro2x, testa83.dta$drottmod2x, testa83.dta$nortlm2x))
Results2a84 <-as.data.frame(contROC(testa84.dta$lpro2x, testa84.dta$drottmod2x, testa84.dta$nortlm2x))
Results2a85 <-as.data.frame(contROC(testa85.dta$lpro2x, testa85.dta$drottmod2x, testa85.dta$nortlm2x))
Results2a86 <-as.data.frame(contROC(testa86.dta$lpro2x, testa86.dta$drottmod2x, testa86.dta$nortlm2x))
Results2a87 <-as.data.frame(contROC(testa87.dta$lpro2x, testa87.dta$drottmod2x, testa87.dta$nortlm2x))
Results2a88 <-as.data.frame(contROC(testa88.dta$lpro2x, testa88.dta$drottmod2x, testa88.dta$nortlm2x))
Results2a89 <-as.data.frame(contROC(testa89.dta$lpro2x, testa89.dta$drottmod2x, testa89.dta$nortlm2x))
Results2a90 <-as.data.frame(contROC(testa90.dta$lpro2x, testa90.dta$drottmod2x, testa90.dta$nortlm2x))
Results2a91 <-as.data.frame(contROC(testa91.dta$lpro2x, testa91.dta$drottmod2x, testa91.dta$nortlm2x))
Results2a92 <-as.data.frame(contROC(testa92.dta$lpro2x, testa92.dta$drottmod2x, testa92.dta$nortlm2x))
Results2a93 <-as.data.frame(contROC(testa93.dta$lpro2x, testa93.dta$drottmod2x, testa93.dta$nortlm2x))
Results2a94 <-as.data.frame(contROC(testa94.dta$lpro2x, testa94.dta$drottmod2x, testa94.dta$nortlm2x))
Results2a95 <-as.data.frame(contROC(testa95.dta$lpro2x, testa95.dta$drottmod2x, testa95.dta$nortlm2x))
Results2a96 <-as.data.frame(contROC(testa96.dta$lpro2x, testa96.dta$drottmod2x, testa96.dta$nortlm2x))
Results2a97 <-as.data.frame(contROC(testa97.dta$lpro2x, testa97.dta$drottmod2x, testa97.dta$nortlm2x))
Results2a98 <-as.data.frame(contROC(testa98.dta$lpro2x, testa98.dta$drottmod2x, testa98.dta$nortlm2x))
Results2a99 <-as.data.frame(contROC(testa99.dta$lpro2x, testa99.dta$drottmod2x, testa99.dta$nortlm2x))
Results2a100 <-as.data.frame(contROC(testa100.dta$lpro2x, testa100.dta$drottmod2x, testa100.dta$nortlm2x))

Results2b1 <-as.data.frame(contROC(testb1.dta$lpro2x, testb1.dta$drottmod2x, testb1.dta$nortlm2x))
Results2b2 <-as.data.frame(contROC(testb2.dta$lpro2x, testb2.dta$drottmod2x, testb2.dta$nortlm2x))
Results2b3 <-as.data.frame(contROC(testb3.dta$lpro2x, testb3.dta$drottmod2x, testb3.dta$nortlm2x))
Results2b4 <-as.data.frame(contROC(testb4.dta$lpro2x, testb4.dta$drottmod2x, testb4.dta$nortlm2x))
Results2b5 <-as.data.frame(contROC(testb5.dta$lpro2x, testb5.dta$drottmod2x, testb5.dta$nortlm2x))
Results2b6 <-as.data.frame(contROC(testb6.dta$lpro2x, testb6.dta$drottmod2x, testb6.dta$nortlm2x))
Results2b7 <-as.data.frame(contROC(testb7.dta$lpro2x, testb7.dta$drottmod2x, testb7.dta$nortlm2x))
Results2b8 <-as.data.frame(contROC(testb8.dta$lpro2x, testb8.dta$drottmod2x, testb8.dta$nortlm2x))
Results2b9 <-as.data.frame(contROC(testb9.dta$lpro2x, testb9.dta$drottmod2x, testb9.dta$nortlm2x))
Results2b10 <-as.data.frame(contROC(testb10.dta$lpro2x, testb10.dta$drottmod2x, testb10.dta$nortlm2x))
Results2b11 <-as.data.frame(contROC(testb11.dta$lpro2x, testb11.dta$drottmod2x, testb11.dta$nortlm2x))
Results2b12 <-as.data.frame(contROC(testb12.dta$lpro2x, testb12.dta$drottmod2x, testb12.dta$nortlm2x))
Results2b13 <-as.data.frame(contROC(testb13.dta$lpro2x, testb13.dta$drottmod2x, testb13.dta$nortlm2x))
Results2b14 <-as.data.frame(contROC(testb14.dta$lpro2x, testb14.dta$drottmod2x, testb14.dta$nortlm2x))
Results2b15 <-as.data.frame(contROC(testb15.dta$lpro2x, testb15.dta$drottmod2x, testb15.dta$nortlm2x))
Results2b16 <-as.data.frame(contROC(testb16.dta$lpro2x, testb16.dta$drottmod2x, testb16.dta$nortlm2x))
Results2b17 <-as.data.frame(contROC(testb17.dta$lpro2x, testb17.dta$drottmod2x, testb17.dta$nortlm2x))
Results2b18 <-as.data.frame(contROC(testb18.dta$lpro2x, testb18.dta$drottmod2x, testb18.dta$nortlm2x))
Results2b19 <-as.data.frame(contROC(testb19.dta$lpro2x, testb19.dta$drottmod2x, testb19.dta$nortlm2x))
Results2b20 <-as.data.frame(contROC(testb20.dta$lpro2x, testb20.dta$drottmod2x, testb20.dta$nortlm2x))
Results2b21 <-as.data.frame(contROC(testb21.dta$lpro2x, testb21.dta$drottmod2x, testb21.dta$nortlm2x))
Results2b22 <-as.data.frame(contROC(testb22.dta$lpro2x, testb22.dta$drottmod2x, testb22.dta$nortlm2x))
Results2b23 <-as.data.frame(contROC(testb23.dta$lpro2x, testb23.dta$drottmod2x, testb23.dta$nortlm2x))
Results2b24 <-as.data.frame(contROC(testb24.dta$lpro2x, testb24.dta$drottmod2x, testb24.dta$nortlm2x))
Results2b25 <-as.data.frame(contROC(testb25.dta$lpro2x, testb25.dta$drottmod2x, testb25.dta$nortlm2x))
Results2b26 <-as.data.frame(contROC(testb26.dta$lpro2x, testb26.dta$drottmod2x, testb26.dta$nortlm2x))
Results2b27 <-as.data.frame(contROC(testb27.dta$lpro2x, testb27.dta$drottmod2x, testb27.dta$nortlm2x))
Results2b28 <-as.data.frame(contROC(testb28.dta$lpro2x, testb28.dta$drottmod2x, testb28.dta$nortlm2x))
Results2b29 <-as.data.frame(contROC(testb29.dta$lpro2x, testb29.dta$drottmod2x, testb29.dta$nortlm2x))
Results2b30 <-as.data.frame(contROC(testb30.dta$lpro2x, testb30.dta$drottmod2x, testb30.dta$nortlm2x))
Results2b31 <-as.data.frame(contROC(testb31.dta$lpro2x, testb31.dta$drottmod2x, testb31.dta$nortlm2x))
Results2b32 <-as.data.frame(contROC(testb32.dta$lpro2x, testb32.dta$drottmod2x, testb32.dta$nortlm2x))
Results2b33 <-as.data.frame(contROC(testb33.dta$lpro2x, testb33.dta$drottmod2x, testb33.dta$nortlm2x))
Results2b34 <-as.data.frame(contROC(testb34.dta$lpro2x, testb34.dta$drottmod2x, testb34.dta$nortlm2x))
Results2b35 <-as.data.frame(contROC(testb35.dta$lpro2x, testb35.dta$drottmod2x, testb35.dta$nortlm2x))
Results2b36 <-as.data.frame(contROC(testb36.dta$lpro2x, testb36.dta$drottmod2x, testb36.dta$nortlm2x))
Results2b37 <-as.data.frame(contROC(testb37.dta$lpro2x, testb37.dta$drottmod2x, testb37.dta$nortlm2x))
Results2b38 <-as.data.frame(contROC(testb38.dta$lpro2x, testb38.dta$drottmod2x, testb38.dta$nortlm2x))
Results2b39 <-as.data.frame(contROC(testb39.dta$lpro2x, testb39.dta$drottmod2x, testb39.dta$nortlm2x))
Results2b40 <-as.data.frame(contROC(testb40.dta$lpro2x, testb40.dta$drottmod2x, testb40.dta$nortlm2x))
Results2b41 <-as.data.frame(contROC(testb41.dta$lpro2x, testb41.dta$drottmod2x, testb41.dta$nortlm2x))
Results2b42 <-as.data.frame(contROC(testb42.dta$lpro2x, testb42.dta$drottmod2x, testb42.dta$nortlm2x))
Results2b43 <-as.data.frame(contROC(testb43.dta$lpro2x, testb43.dta$drottmod2x, testb43.dta$nortlm2x))
Results2b44 <-as.data.frame(contROC(testb44.dta$lpro2x, testb44.dta$drottmod2x, testb44.dta$nortlm2x))
Results2b45 <-as.data.frame(contROC(testb45.dta$lpro2x, testb45.dta$drottmod2x, testb45.dta$nortlm2x))
Results2b46 <-as.data.frame(contROC(testb46.dta$lpro2x, testb46.dta$drottmod2x, testb46.dta$nortlm2x))
Results2b47 <-as.data.frame(contROC(testb47.dta$lpro2x, testb47.dta$drottmod2x, testb47.dta$nortlm2x))
Results2b48 <-as.data.frame(contROC(testb48.dta$lpro2x, testb48.dta$drottmod2x, testb48.dta$nortlm2x))
Results2b49 <-as.data.frame(contROC(testb49.dta$lpro2x, testb49.dta$drottmod2x, testb49.dta$nortlm2x))
Results2b50 <-as.data.frame(contROC(testb50.dta$lpro2x, testb50.dta$drottmod2x, testb50.dta$nortlm2x))
Results2b51 <-as.data.frame(contROC(testb51.dta$lpro2x, testb51.dta$drottmod2x, testb51.dta$nortlm2x))
Results2b52 <-as.data.frame(contROC(testb52.dta$lpro2x, testb52.dta$drottmod2x, testb52.dta$nortlm2x))
Results2b53 <-as.data.frame(contROC(testb53.dta$lpro2x, testb53.dta$drottmod2x, testb53.dta$nortlm2x))
Results2b54 <-as.data.frame(contROC(testb54.dta$lpro2x, testb54.dta$drottmod2x, testb54.dta$nortlm2x))
Results2b55 <-as.data.frame(contROC(testb55.dta$lpro2x, testb55.dta$drottmod2x, testb55.dta$nortlm2x))
Results2b56 <-as.data.frame(contROC(testb56.dta$lpro2x, testb56.dta$drottmod2x, testb56.dta$nortlm2x))
Results2b57 <-as.data.frame(contROC(testb57.dta$lpro2x, testb57.dta$drottmod2x, testb57.dta$nortlm2x))
Results2b58 <-as.data.frame(contROC(testb58.dta$lpro2x, testb58.dta$drottmod2x, testb58.dta$nortlm2x))
Results2b59 <-as.data.frame(contROC(testb59.dta$lpro2x, testb59.dta$drottmod2x, testb59.dta$nortlm2x))
Results2b60 <-as.data.frame(contROC(testb60.dta$lpro2x, testb60.dta$drottmod2x, testb60.dta$nortlm2x))
Results2b61 <-as.data.frame(contROC(testb61.dta$lpro2x, testb61.dta$drottmod2x, testb61.dta$nortlm2x))
Results2b62 <-as.data.frame(contROC(testb62.dta$lpro2x, testb62.dta$drottmod2x, testb62.dta$nortlm2x))
Results2b63 <-as.data.frame(contROC(testb63.dta$lpro2x, testb63.dta$drottmod2x, testb63.dta$nortlm2x))
Results2b64 <-as.data.frame(contROC(testb64.dta$lpro2x, testb64.dta$drottmod2x, testb64.dta$nortlm2x))
Results2b65 <-as.data.frame(contROC(testb65.dta$lpro2x, testb65.dta$drottmod2x, testb65.dta$nortlm2x))
Results2b66 <-as.data.frame(contROC(testb66.dta$lpro2x, testb66.dta$drottmod2x, testb66.dta$nortlm2x))
Results2b67 <-as.data.frame(contROC(testb67.dta$lpro2x, testb67.dta$drottmod2x, testb67.dta$nortlm2x))
Results2b68 <-as.data.frame(contROC(testb68.dta$lpro2x, testb68.dta$drottmod2x, testb68.dta$nortlm2x))
Results2b69 <-as.data.frame(contROC(testb69.dta$lpro2x, testb69.dta$drottmod2x, testb69.dta$nortlm2x))
Results2b70 <-as.data.frame(contROC(testb70.dta$lpro2x, testb70.dta$drottmod2x, testb70.dta$nortlm2x))
Results2b71 <-as.data.frame(contROC(testb71.dta$lpro2x, testb71.dta$drottmod2x, testb71.dta$nortlm2x))
Results2b72 <-as.data.frame(contROC(testb72.dta$lpro2x, testb72.dta$drottmod2x, testb72.dta$nortlm2x))
Results2b73 <-as.data.frame(contROC(testb73.dta$lpro2x, testb73.dta$drottmod2x, testb73.dta$nortlm2x))
Results2b74 <-as.data.frame(contROC(testb74.dta$lpro2x, testb74.dta$drottmod2x, testb74.dta$nortlm2x))
Results2b75 <-as.data.frame(contROC(testb75.dta$lpro2x, testb75.dta$drottmod2x, testb75.dta$nortlm2x))
Results2b76 <-as.data.frame(contROC(testb76.dta$lpro2x, testb76.dta$drottmod2x, testb76.dta$nortlm2x))
Results2b77 <-as.data.frame(contROC(testb77.dta$lpro2x, testb77.dta$drottmod2x, testb77.dta$nortlm2x))
Results2b78 <-as.data.frame(contROC(testb78.dta$lpro2x, testb78.dta$drottmod2x, testb78.dta$nortlm2x))
Results2b79 <-as.data.frame(contROC(testb79.dta$lpro2x, testb79.dta$drottmod2x, testb79.dta$nortlm2x))
Results2b80 <-as.data.frame(contROC(testb80.dta$lpro2x, testb80.dta$drottmod2x, testb80.dta$nortlm2x))
Results2b81 <-as.data.frame(contROC(testb81.dta$lpro2x, testb81.dta$drottmod2x, testb81.dta$nortlm2x))
Results2b82 <-as.data.frame(contROC(testb82.dta$lpro2x, testb82.dta$drottmod2x, testb82.dta$nortlm2x))
Results2b83 <-as.data.frame(contROC(testb83.dta$lpro2x, testb83.dta$drottmod2x, testb83.dta$nortlm2x))
Results2b84 <-as.data.frame(contROC(testb84.dta$lpro2x, testb84.dta$drottmod2x, testb84.dta$nortlm2x))
Results2b85 <-as.data.frame(contROC(testb85.dta$lpro2x, testb85.dta$drottmod2x, testb85.dta$nortlm2x))
Results2b86 <-as.data.frame(contROC(testb86.dta$lpro2x, testb86.dta$drottmod2x, testb86.dta$nortlm2x))
Results2b87 <-as.data.frame(contROC(testb87.dta$lpro2x, testb87.dta$drottmod2x, testb87.dta$nortlm2x))
Results2b88 <-as.data.frame(contROC(testb88.dta$lpro2x, testb88.dta$drottmod2x, testb88.dta$nortlm2x))
Results2b89 <-as.data.frame(contROC(testb89.dta$lpro2x, testb89.dta$drottmod2x, testb89.dta$nortlm2x))
Results2b90 <-as.data.frame(contROC(testb90.dta$lpro2x, testb90.dta$drottmod2x, testb90.dta$nortlm2x))
Results2b91 <-as.data.frame(contROC(testb91.dta$lpro2x, testb91.dta$drottmod2x, testb91.dta$nortlm2x))
Results2b92 <-as.data.frame(contROC(testb92.dta$lpro2x, testb92.dta$drottmod2x, testb92.dta$nortlm2x))
Results2b93 <-as.data.frame(contROC(testb93.dta$lpro2x, testb93.dta$drottmod2x, testb93.dta$nortlm2x))
Results2b94 <-as.data.frame(contROC(testb94.dta$lpro2x, testb94.dta$drottmod2x, testb94.dta$nortlm2x))
Results2b95 <-as.data.frame(contROC(testb95.dta$lpro2x, testb95.dta$drottmod2x, testb95.dta$nortlm2x))
Results2b96 <-as.data.frame(contROC(testb96.dta$lpro2x, testb96.dta$drottmod2x, testb96.dta$nortlm2x))
Results2b97 <-as.data.frame(contROC(testb97.dta$lpro2x, testb97.dta$drottmod2x, testb97.dta$nortlm2x))
Results2b98 <-as.data.frame(contROC(testb98.dta$lpro2x, testb98.dta$drottmod2x, testb98.dta$nortlm2x))
Results2b99 <-as.data.frame(contROC(testb99.dta$lpro2x, testb99.dta$drottmod2x, testb99.dta$nortlm2x))
Results2b100 <-as.data.frame(contROC(testb100.dta$lpro2x, testb100.dta$drottmod2x, testb100.dta$nortlm2x))

Results2c1 <-as.data.frame(contROC(testc1.dta$lpro2x, testc1.dta$drottmod2x, testc1.dta$nortlm2x))
Results2c2 <-as.data.frame(contROC(testc2.dta$lpro2x, testc2.dta$drottmod2x, testc2.dta$nortlm2x))
Results2c3 <-as.data.frame(contROC(testc3.dta$lpro2x, testc3.dta$drottmod2x, testc3.dta$nortlm2x))
Results2c4 <-as.data.frame(contROC(testc4.dta$lpro2x, testc4.dta$drottmod2x, testc4.dta$nortlm2x))
Results2c5 <-as.data.frame(contROC(testc5.dta$lpro2x, testc5.dta$drottmod2x, testc5.dta$nortlm2x))
Results2c6 <-as.data.frame(contROC(testc6.dta$lpro2x, testc6.dta$drottmod2x, testc6.dta$nortlm2x))
Results2c7 <-as.data.frame(contROC(testc7.dta$lpro2x, testc7.dta$drottmod2x, testc7.dta$nortlm2x))
Results2c8 <-as.data.frame(contROC(testc8.dta$lpro2x, testc8.dta$drottmod2x, testc8.dta$nortlm2x))
Results2c9 <-as.data.frame(contROC(testc9.dta$lpro2x, testc9.dta$drottmod2x, testc9.dta$nortlm2x))
Results2c10 <-as.data.frame(contROC(testc10.dta$lpro2x, testc10.dta$drottmod2x, testc10.dta$nortlm2x))
Results2c11 <-as.data.frame(contROC(testc11.dta$lpro2x, testc11.dta$drottmod2x, testc11.dta$nortlm2x))
Results2c12 <-as.data.frame(contROC(testc12.dta$lpro2x, testc12.dta$drottmod2x, testc12.dta$nortlm2x))
Results2c13 <-as.data.frame(contROC(testc13.dta$lpro2x, testc13.dta$drottmod2x, testc13.dta$nortlm2x))
Results2c14 <-as.data.frame(contROC(testc14.dta$lpro2x, testc14.dta$drottmod2x, testc14.dta$nortlm2x))
Results2c15 <-as.data.frame(contROC(testc15.dta$lpro2x, testc15.dta$drottmod2x, testc15.dta$nortlm2x))
Results2c16 <-as.data.frame(contROC(testc16.dta$lpro2x, testc16.dta$drottmod2x, testc16.dta$nortlm2x))
Results2c17 <-as.data.frame(contROC(testc17.dta$lpro2x, testc17.dta$drottmod2x, testc17.dta$nortlm2x))
Results2c18 <-as.data.frame(contROC(testc18.dta$lpro2x, testc18.dta$drottmod2x, testc18.dta$nortlm2x))
Results2c19 <-as.data.frame(contROC(testc19.dta$lpro2x, testc19.dta$drottmod2x, testc19.dta$nortlm2x))
Results2c20 <-as.data.frame(contROC(testc20.dta$lpro2x, testc20.dta$drottmod2x, testc20.dta$nortlm2x))
Results2c21 <-as.data.frame(contROC(testc21.dta$lpro2x, testc21.dta$drottmod2x, testc21.dta$nortlm2x))
Results2c22 <-as.data.frame(contROC(testc22.dta$lpro2x, testc22.dta$drottmod2x, testc22.dta$nortlm2x))
Results2c23 <-as.data.frame(contROC(testc23.dta$lpro2x, testc23.dta$drottmod2x, testc23.dta$nortlm2x))
Results2c24 <-as.data.frame(contROC(testc24.dta$lpro2x, testc24.dta$drottmod2x, testc24.dta$nortlm2x))
Results2c25 <-as.data.frame(contROC(testc25.dta$lpro2x, testc25.dta$drottmod2x, testc25.dta$nortlm2x))
Results2c26 <-as.data.frame(contROC(testc26.dta$lpro2x, testc26.dta$drottmod2x, testc26.dta$nortlm2x))
Results2c27 <-as.data.frame(contROC(testc27.dta$lpro2x, testc27.dta$drottmod2x, testc27.dta$nortlm2x))
Results2c28 <-as.data.frame(contROC(testc28.dta$lpro2x, testc28.dta$drottmod2x, testc28.dta$nortlm2x))
Results2c29 <-as.data.frame(contROC(testc29.dta$lpro2x, testc29.dta$drottmod2x, testc29.dta$nortlm2x))
Results2c30 <-as.data.frame(contROC(testc30.dta$lpro2x, testc30.dta$drottmod2x, testc30.dta$nortlm2x))
Results2c31 <-as.data.frame(contROC(testc31.dta$lpro2x, testc31.dta$drottmod2x, testc31.dta$nortlm2x))
Results2c32 <-as.data.frame(contROC(testc32.dta$lpro2x, testc32.dta$drottmod2x, testc32.dta$nortlm2x))
Results2c33 <-as.data.frame(contROC(testc33.dta$lpro2x, testc33.dta$drottmod2x, testc33.dta$nortlm2x))
Results2c34 <-as.data.frame(contROC(testc34.dta$lpro2x, testc34.dta$drottmod2x, testc34.dta$nortlm2x))
Results2c35 <-as.data.frame(contROC(testc35.dta$lpro2x, testc35.dta$drottmod2x, testc35.dta$nortlm2x))
Results2c36 <-as.data.frame(contROC(testc36.dta$lpro2x, testc36.dta$drottmod2x, testc36.dta$nortlm2x))
Results2c37 <-as.data.frame(contROC(testc37.dta$lpro2x, testc37.dta$drottmod2x, testc37.dta$nortlm2x))
Results2c38 <-as.data.frame(contROC(testc38.dta$lpro2x, testc38.dta$drottmod2x, testc38.dta$nortlm2x))
Results2c39 <-as.data.frame(contROC(testc39.dta$lpro2x, testc39.dta$drottmod2x, testc39.dta$nortlm2x))
Results2c40 <-as.data.frame(contROC(testc40.dta$lpro2x, testc40.dta$drottmod2x, testc40.dta$nortlm2x))
Results2c41 <-as.data.frame(contROC(testc41.dta$lpro2x, testc41.dta$drottmod2x, testc41.dta$nortlm2x))
Results2c42 <-as.data.frame(contROC(testc42.dta$lpro2x, testc42.dta$drottmod2x, testc42.dta$nortlm2x))
Results2c43 <-as.data.frame(contROC(testc43.dta$lpro2x, testc43.dta$drottmod2x, testc43.dta$nortlm2x))
Results2c44 <-as.data.frame(contROC(testc44.dta$lpro2x, testc44.dta$drottmod2x, testc44.dta$nortlm2x))
Results2c45 <-as.data.frame(contROC(testc45.dta$lpro2x, testc45.dta$drottmod2x, testc45.dta$nortlm2x))
Results2c46 <-as.data.frame(contROC(testc46.dta$lpro2x, testc46.dta$drottmod2x, testc46.dta$nortlm2x))
Results2c47 <-as.data.frame(contROC(testc47.dta$lpro2x, testc47.dta$drottmod2x, testc47.dta$nortlm2x))
Results2c48 <-as.data.frame(contROC(testc48.dta$lpro2x, testc48.dta$drottmod2x, testc48.dta$nortlm2x))
Results2c49 <-as.data.frame(contROC(testc49.dta$lpro2x, testc49.dta$drottmod2x, testc49.dta$nortlm2x))
Results2c50 <-as.data.frame(contROC(testc50.dta$lpro2x, testc50.dta$drottmod2x, testc50.dta$nortlm2x))
Results2c51 <-as.data.frame(contROC(testc51.dta$lpro2x, testc51.dta$drottmod2x, testc51.dta$nortlm2x))
Results2c52 <-as.data.frame(contROC(testc52.dta$lpro2x, testc52.dta$drottmod2x, testc52.dta$nortlm2x))
Results2c53 <-as.data.frame(contROC(testc53.dta$lpro2x, testc53.dta$drottmod2x, testc53.dta$nortlm2x))
Results2c54 <-as.data.frame(contROC(testc54.dta$lpro2x, testc54.dta$drottmod2x, testc54.dta$nortlm2x))
Results2c55 <-as.data.frame(contROC(testc55.dta$lpro2x, testc55.dta$drottmod2x, testc55.dta$nortlm2x))
Results2c56 <-as.data.frame(contROC(testc56.dta$lpro2x, testc56.dta$drottmod2x, testc56.dta$nortlm2x))
Results2c57 <-as.data.frame(contROC(testc57.dta$lpro2x, testc57.dta$drottmod2x, testc57.dta$nortlm2x))
Results2c58 <-as.data.frame(contROC(testc58.dta$lpro2x, testc58.dta$drottmod2x, testc58.dta$nortlm2x))
Results2c59 <-as.data.frame(contROC(testc59.dta$lpro2x, testc59.dta$drottmod2x, testc59.dta$nortlm2x))
Results2c60 <-as.data.frame(contROC(testc60.dta$lpro2x, testc60.dta$drottmod2x, testc60.dta$nortlm2x))
Results2c61 <-as.data.frame(contROC(testc61.dta$lpro2x, testc61.dta$drottmod2x, testc61.dta$nortlm2x))
Results2c62 <-as.data.frame(contROC(testc62.dta$lpro2x, testc62.dta$drottmod2x, testc62.dta$nortlm2x))
Results2c63 <-as.data.frame(contROC(testc63.dta$lpro2x, testc63.dta$drottmod2x, testc63.dta$nortlm2x))
Results2c64 <-as.data.frame(contROC(testc64.dta$lpro2x, testc64.dta$drottmod2x, testc64.dta$nortlm2x))
Results2c65 <-as.data.frame(contROC(testc65.dta$lpro2x, testc65.dta$drottmod2x, testc65.dta$nortlm2x))
Results2c66 <-as.data.frame(contROC(testc66.dta$lpro2x, testc66.dta$drottmod2x, testc66.dta$nortlm2x))
Results2c67 <-as.data.frame(contROC(testc67.dta$lpro2x, testc67.dta$drottmod2x, testc67.dta$nortlm2x))
Results2c68 <-as.data.frame(contROC(testc68.dta$lpro2x, testc68.dta$drottmod2x, testc68.dta$nortlm2x))
Results2c69 <-as.data.frame(contROC(testc69.dta$lpro2x, testc69.dta$drottmod2x, testc69.dta$nortlm2x))
Results2c70 <-as.data.frame(contROC(testc70.dta$lpro2x, testc70.dta$drottmod2x, testc70.dta$nortlm2x))
Results2c71 <-as.data.frame(contROC(testc71.dta$lpro2x, testc71.dta$drottmod2x, testc71.dta$nortlm2x))
Results2c72 <-as.data.frame(contROC(testc72.dta$lpro2x, testc72.dta$drottmod2x, testc72.dta$nortlm2x))
Results2c73 <-as.data.frame(contROC(testc73.dta$lpro2x, testc73.dta$drottmod2x, testc73.dta$nortlm2x))
Results2c74 <-as.data.frame(contROC(testc74.dta$lpro2x, testc74.dta$drottmod2x, testc74.dta$nortlm2x))
Results2c75 <-as.data.frame(contROC(testc75.dta$lpro2x, testc75.dta$drottmod2x, testc75.dta$nortlm2x))
Results2c76 <-as.data.frame(contROC(testc76.dta$lpro2x, testc76.dta$drottmod2x, testc76.dta$nortlm2x))
Results2c77 <-as.data.frame(contROC(testc77.dta$lpro2x, testc77.dta$drottmod2x, testc77.dta$nortlm2x))
Results2c78 <-as.data.frame(contROC(testc78.dta$lpro2x, testc78.dta$drottmod2x, testc78.dta$nortlm2x))
Results2c79 <-as.data.frame(contROC(testc79.dta$lpro2x, testc79.dta$drottmod2x, testc79.dta$nortlm2x))
Results2c80 <-as.data.frame(contROC(testc80.dta$lpro2x, testc80.dta$drottmod2x, testc80.dta$nortlm2x))
Results2c81 <-as.data.frame(contROC(testc81.dta$lpro2x, testc81.dta$drottmod2x, testc81.dta$nortlm2x))
Results2c82 <-as.data.frame(contROC(testc82.dta$lpro2x, testc82.dta$drottmod2x, testc82.dta$nortlm2x))
Results2c83 <-as.data.frame(contROC(testc83.dta$lpro2x, testc83.dta$drottmod2x, testc83.dta$nortlm2x))
Results2c84 <-as.data.frame(contROC(testc84.dta$lpro2x, testc84.dta$drottmod2x, testc84.dta$nortlm2x))
Results2c85 <-as.data.frame(contROC(testc85.dta$lpro2x, testc85.dta$drottmod2x, testc85.dta$nortlm2x))
Results2c86 <-as.data.frame(contROC(testc86.dta$lpro2x, testc86.dta$drottmod2x, testc86.dta$nortlm2x))
Results2c87 <-as.data.frame(contROC(testc87.dta$lpro2x, testc87.dta$drottmod2x, testc87.dta$nortlm2x))
Results2c88 <-as.data.frame(contROC(testc88.dta$lpro2x, testc88.dta$drottmod2x, testc88.dta$nortlm2x))
Results2c89 <-as.data.frame(contROC(testc89.dta$lpro2x, testc89.dta$drottmod2x, testc89.dta$nortlm2x))
Results2c90 <-as.data.frame(contROC(testc90.dta$lpro2x, testc90.dta$drottmod2x, testc90.dta$nortlm2x))
Results2c91 <-as.data.frame(contROC(testc91.dta$lpro2x, testc91.dta$drottmod2x, testc91.dta$nortlm2x))
Results2c92 <-as.data.frame(contROC(testc92.dta$lpro2x, testc92.dta$drottmod2x, testc92.dta$nortlm2x))
Results2c93 <-as.data.frame(contROC(testc93.dta$lpro2x, testc93.dta$drottmod2x, testc93.dta$nortlm2x))
Results2c94 <-as.data.frame(contROC(testc94.dta$lpro2x, testc94.dta$drottmod2x, testc94.dta$nortlm2x))
Results2c95 <-as.data.frame(contROC(testc95.dta$lpro2x, testc95.dta$drottmod2x, testc95.dta$nortlm2x))
Results2c96 <-as.data.frame(contROC(testc96.dta$lpro2x, testc96.dta$drottmod2x, testc96.dta$nortlm2x))
Results2c97 <-as.data.frame(contROC(testc97.dta$lpro2x, testc97.dta$drottmod2x, testc97.dta$nortlm2x))
Results2c98 <-as.data.frame(contROC(testc98.dta$lpro2x, testc98.dta$drottmod2x, testc98.dta$nortlm2x))
Results2c99 <-as.data.frame(contROC(testc99.dta$lpro2x, testc99.dta$drottmod2x, testc99.dta$nortlm2x))
Results2c100 <-as.data.frame(contROC(testc100.dta$lpro2x, testc100.dta$drottmod2x, testc100.dta$nortlm2x))

Results2d1 <-as.data.frame(contROC(testd1.dta$lpro2x, testd1.dta$drottmod2x, testd1.dta$nortlm2x))
Results2d2 <-as.data.frame(contROC(testd2.dta$lpro2x, testd2.dta$drottmod2x, testd2.dta$nortlm2x))
Results2d3 <-as.data.frame(contROC(testd3.dta$lpro2x, testd3.dta$drottmod2x, testd3.dta$nortlm2x))
Results2d4 <-as.data.frame(contROC(testd4.dta$lpro2x, testd4.dta$drottmod2x, testd4.dta$nortlm2x))
Results2d5 <-as.data.frame(contROC(testd5.dta$lpro2x, testd5.dta$drottmod2x, testd5.dta$nortlm2x))
Results2d6 <-as.data.frame(contROC(testd6.dta$lpro2x, testd6.dta$drottmod2x, testd6.dta$nortlm2x))
Results2d7 <-as.data.frame(contROC(testd7.dta$lpro2x, testd7.dta$drottmod2x, testd7.dta$nortlm2x))
Results2d8 <-as.data.frame(contROC(testd8.dta$lpro2x, testd8.dta$drottmod2x, testd8.dta$nortlm2x))
Results2d9 <-as.data.frame(contROC(testd9.dta$lpro2x, testd9.dta$drottmod2x, testd9.dta$nortlm2x))
Results2d10 <-as.data.frame(contROC(testd10.dta$lpro2x, testd10.dta$drottmod2x, testd10.dta$nortlm2x))
Results2d11 <-as.data.frame(contROC(testd11.dta$lpro2x, testd11.dta$drottmod2x, testd11.dta$nortlm2x))
Results2d12 <-as.data.frame(contROC(testd12.dta$lpro2x, testd12.dta$drottmod2x, testd12.dta$nortlm2x))
Results2d13 <-as.data.frame(contROC(testd13.dta$lpro2x, testd13.dta$drottmod2x, testd13.dta$nortlm2x))
Results2d14 <-as.data.frame(contROC(testd14.dta$lpro2x, testd14.dta$drottmod2x, testd14.dta$nortlm2x))
Results2d15 <-as.data.frame(contROC(testd15.dta$lpro2x, testd15.dta$drottmod2x, testd15.dta$nortlm2x))
Results2d16 <-as.data.frame(contROC(testd16.dta$lpro2x, testd16.dta$drottmod2x, testd16.dta$nortlm2x))
Results2d17 <-as.data.frame(contROC(testd17.dta$lpro2x, testd17.dta$drottmod2x, testd17.dta$nortlm2x))
Results2d18 <-as.data.frame(contROC(testd18.dta$lpro2x, testd18.dta$drottmod2x, testd18.dta$nortlm2x))
Results2d19 <-as.data.frame(contROC(testd19.dta$lpro2x, testd19.dta$drottmod2x, testd19.dta$nortlm2x))
Results2d20 <-as.data.frame(contROC(testd20.dta$lpro2x, testd20.dta$drottmod2x, testd20.dta$nortlm2x))
Results2d21 <-as.data.frame(contROC(testd21.dta$lpro2x, testd21.dta$drottmod2x, testd21.dta$nortlm2x))
Results2d22 <-as.data.frame(contROC(testd22.dta$lpro2x, testd22.dta$drottmod2x, testd22.dta$nortlm2x))
Results2d23 <-as.data.frame(contROC(testd23.dta$lpro2x, testd23.dta$drottmod2x, testd23.dta$nortlm2x))
Results2d24 <-as.data.frame(contROC(testd24.dta$lpro2x, testd24.dta$drottmod2x, testd24.dta$nortlm2x))
Results2d25 <-as.data.frame(contROC(testd25.dta$lpro2x, testd25.dta$drottmod2x, testd25.dta$nortlm2x))
Results2d26 <-as.data.frame(contROC(testd26.dta$lpro2x, testd26.dta$drottmod2x, testd26.dta$nortlm2x))
Results2d27 <-as.data.frame(contROC(testd27.dta$lpro2x, testd27.dta$drottmod2x, testd27.dta$nortlm2x))
Results2d28 <-as.data.frame(contROC(testd28.dta$lpro2x, testd28.dta$drottmod2x, testd28.dta$nortlm2x))
Results2d29 <-as.data.frame(contROC(testd29.dta$lpro2x, testd29.dta$drottmod2x, testd29.dta$nortlm2x))
Results2d30 <-as.data.frame(contROC(testd30.dta$lpro2x, testd30.dta$drottmod2x, testd30.dta$nortlm2x))
Results2d31 <-as.data.frame(contROC(testd31.dta$lpro2x, testd31.dta$drottmod2x, testd31.dta$nortlm2x))
Results2d32 <-as.data.frame(contROC(testd32.dta$lpro2x, testd32.dta$drottmod2x, testd32.dta$nortlm2x))
Results2d33 <-as.data.frame(contROC(testd33.dta$lpro2x, testd33.dta$drottmod2x, testd33.dta$nortlm2x))
Results2d34 <-as.data.frame(contROC(testd34.dta$lpro2x, testd34.dta$drottmod2x, testd34.dta$nortlm2x))
Results2d35 <-as.data.frame(contROC(testd35.dta$lpro2x, testd35.dta$drottmod2x, testd35.dta$nortlm2x))
Results2d36 <-as.data.frame(contROC(testd36.dta$lpro2x, testd36.dta$drottmod2x, testd36.dta$nortlm2x))
Results2d37 <-as.data.frame(contROC(testd37.dta$lpro2x, testd37.dta$drottmod2x, testd37.dta$nortlm2x))
Results2d38 <-as.data.frame(contROC(testd38.dta$lpro2x, testd38.dta$drottmod2x, testd38.dta$nortlm2x))
Results2d39 <-as.data.frame(contROC(testd39.dta$lpro2x, testd39.dta$drottmod2x, testd39.dta$nortlm2x))
Results2d40 <-as.data.frame(contROC(testd40.dta$lpro2x, testd40.dta$drottmod2x, testd40.dta$nortlm2x))
Results2d41 <-as.data.frame(contROC(testd41.dta$lpro2x, testd41.dta$drottmod2x, testd41.dta$nortlm2x))
Results2d42 <-as.data.frame(contROC(testd42.dta$lpro2x, testd42.dta$drottmod2x, testd42.dta$nortlm2x))
Results2d43 <-as.data.frame(contROC(testd43.dta$lpro2x, testd43.dta$drottmod2x, testd43.dta$nortlm2x))
Results2d44 <-as.data.frame(contROC(testd44.dta$lpro2x, testd44.dta$drottmod2x, testd44.dta$nortlm2x))
Results2d45 <-as.data.frame(contROC(testd45.dta$lpro2x, testd45.dta$drottmod2x, testd45.dta$nortlm2x))
Results2d46 <-as.data.frame(contROC(testd46.dta$lpro2x, testd46.dta$drottmod2x, testd46.dta$nortlm2x))
Results2d47 <-as.data.frame(contROC(testd47.dta$lpro2x, testd47.dta$drottmod2x, testd47.dta$nortlm2x))
Results2d48 <-as.data.frame(contROC(testd48.dta$lpro2x, testd48.dta$drottmod2x, testd48.dta$nortlm2x))
Results2d49 <-as.data.frame(contROC(testd49.dta$lpro2x, testd49.dta$drottmod2x, testd49.dta$nortlm2x))
Results2d50 <-as.data.frame(contROC(testd50.dta$lpro2x, testd50.dta$drottmod2x, testd50.dta$nortlm2x))
Results2d51 <-as.data.frame(contROC(testd51.dta$lpro2x, testd51.dta$drottmod2x, testd51.dta$nortlm2x))
Results2d52 <-as.data.frame(contROC(testd52.dta$lpro2x, testd52.dta$drottmod2x, testd52.dta$nortlm2x))
Results2d53 <-as.data.frame(contROC(testd53.dta$lpro2x, testd53.dta$drottmod2x, testd53.dta$nortlm2x))
Results2d54 <-as.data.frame(contROC(testd54.dta$lpro2x, testd54.dta$drottmod2x, testd54.dta$nortlm2x))
Results2d55 <-as.data.frame(contROC(testd55.dta$lpro2x, testd55.dta$drottmod2x, testd55.dta$nortlm2x))
Results2d56 <-as.data.frame(contROC(testd56.dta$lpro2x, testd56.dta$drottmod2x, testd56.dta$nortlm2x))
Results2d57 <-as.data.frame(contROC(testd57.dta$lpro2x, testd57.dta$drottmod2x, testd57.dta$nortlm2x))
Results2d58 <-as.data.frame(contROC(testd58.dta$lpro2x, testd58.dta$drottmod2x, testd58.dta$nortlm2x))
Results2d59 <-as.data.frame(contROC(testd59.dta$lpro2x, testd59.dta$drottmod2x, testd59.dta$nortlm2x))
Results2d60 <-as.data.frame(contROC(testd60.dta$lpro2x, testd60.dta$drottmod2x, testd60.dta$nortlm2x))
Results2d61 <-as.data.frame(contROC(testd61.dta$lpro2x, testd61.dta$drottmod2x, testd61.dta$nortlm2x))
Results2d62 <-as.data.frame(contROC(testd62.dta$lpro2x, testd62.dta$drottmod2x, testd62.dta$nortlm2x))
Results2d63 <-as.data.frame(contROC(testd63.dta$lpro2x, testd63.dta$drottmod2x, testd63.dta$nortlm2x))
Results2d64 <-as.data.frame(contROC(testd64.dta$lpro2x, testd64.dta$drottmod2x, testd64.dta$nortlm2x))
Results2d65 <-as.data.frame(contROC(testd65.dta$lpro2x, testd65.dta$drottmod2x, testd65.dta$nortlm2x))
Results2d66 <-as.data.frame(contROC(testd66.dta$lpro2x, testd66.dta$drottmod2x, testd66.dta$nortlm2x))
Results2d67 <-as.data.frame(contROC(testd67.dta$lpro2x, testd67.dta$drottmod2x, testd67.dta$nortlm2x))
Results2d68 <-as.data.frame(contROC(testd68.dta$lpro2x, testd68.dta$drottmod2x, testd68.dta$nortlm2x))
Results2d69 <-as.data.frame(contROC(testd69.dta$lpro2x, testd69.dta$drottmod2x, testd69.dta$nortlm2x))
Results2d70 <-as.data.frame(contROC(testd70.dta$lpro2x, testd70.dta$drottmod2x, testd70.dta$nortlm2x))
Results2d71 <-as.data.frame(contROC(testd71.dta$lpro2x, testd71.dta$drottmod2x, testd71.dta$nortlm2x))
Results2d72 <-as.data.frame(contROC(testd72.dta$lpro2x, testd72.dta$drottmod2x, testd72.dta$nortlm2x))
Results2d73 <-as.data.frame(contROC(testd73.dta$lpro2x, testd73.dta$drottmod2x, testd73.dta$nortlm2x))
Results2d74 <-as.data.frame(contROC(testd74.dta$lpro2x, testd74.dta$drottmod2x, testd74.dta$nortlm2x))
Results2d75 <-as.data.frame(contROC(testd75.dta$lpro2x, testd75.dta$drottmod2x, testd75.dta$nortlm2x))
Results2d76 <-as.data.frame(contROC(testd76.dta$lpro2x, testd76.dta$drottmod2x, testd76.dta$nortlm2x))
Results2d77 <-as.data.frame(contROC(testd77.dta$lpro2x, testd77.dta$drottmod2x, testd77.dta$nortlm2x))
Results2d78 <-as.data.frame(contROC(testd78.dta$lpro2x, testd78.dta$drottmod2x, testd78.dta$nortlm2x))
Results2d79 <-as.data.frame(contROC(testd79.dta$lpro2x, testd79.dta$drottmod2x, testd79.dta$nortlm2x))
Results2d80 <-as.data.frame(contROC(testd80.dta$lpro2x, testd80.dta$drottmod2x, testd80.dta$nortlm2x))
Results2d81 <-as.data.frame(contROC(testd81.dta$lpro2x, testd81.dta$drottmod2x, testd81.dta$nortlm2x))
Results2d82 <-as.data.frame(contROC(testd82.dta$lpro2x, testd82.dta$drottmod2x, testd82.dta$nortlm2x))
Results2d83 <-as.data.frame(contROC(testd83.dta$lpro2x, testd83.dta$drottmod2x, testd83.dta$nortlm2x))
Results2d84 <-as.data.frame(contROC(testd84.dta$lpro2x, testd84.dta$drottmod2x, testd84.dta$nortlm2x))
Results2d85 <-as.data.frame(contROC(testd85.dta$lpro2x, testd85.dta$drottmod2x, testd85.dta$nortlm2x))
Results2d86 <-as.data.frame(contROC(testd86.dta$lpro2x, testd86.dta$drottmod2x, testd86.dta$nortlm2x))
Results2d87 <-as.data.frame(contROC(testd87.dta$lpro2x, testd87.dta$drottmod2x, testd87.dta$nortlm2x))
Results2d88 <-as.data.frame(contROC(testd88.dta$lpro2x, testd88.dta$drottmod2x, testd88.dta$nortlm2x))
Results2d89 <-as.data.frame(contROC(testd89.dta$lpro2x, testd89.dta$drottmod2x, testd89.dta$nortlm2x))
Results2d90 <-as.data.frame(contROC(testd90.dta$lpro2x, testd90.dta$drottmod2x, testd90.dta$nortlm2x))
Results2d91 <-as.data.frame(contROC(testd91.dta$lpro2x, testd91.dta$drottmod2x, testd91.dta$nortlm2x))
Results2d92 <-as.data.frame(contROC(testd92.dta$lpro2x, testd92.dta$drottmod2x, testd92.dta$nortlm2x))
Results2d93 <-as.data.frame(contROC(testd93.dta$lpro2x, testd93.dta$drottmod2x, testd93.dta$nortlm2x))
Results2d94 <-as.data.frame(contROC(testd94.dta$lpro2x, testd94.dta$drottmod2x, testd94.dta$nortlm2x))
Results2d95 <-as.data.frame(contROC(testd95.dta$lpro2x, testd95.dta$drottmod2x, testd95.dta$nortlm2x))
Results2d96 <-as.data.frame(contROC(testd96.dta$lpro2x, testd96.dta$drottmod2x, testd96.dta$nortlm2x))
Results2d97 <-as.data.frame(contROC(testd97.dta$lpro2x, testd97.dta$drottmod2x, testd97.dta$nortlm2x))
Results2d98 <-as.data.frame(contROC(testd98.dta$lpro2x, testd98.dta$drottmod2x, testd98.dta$nortlm2x))
Results2d99 <-as.data.frame(contROC(testd99.dta$lpro2x, testd99.dta$drottmod2x, testd99.dta$nortlm2x))
Results2d100 <-as.data.frame(contROC(testd100.dta$lpro2x, testd100.dta$drottmod2x, testd100.dta$nortlm2x))


# append results # 

Results2aALL <- rbind(Results2a1, Results2a2, Results2a3, Results2a4, Results2a5, Results2a6, Results2a7, Results2a8, Results2a9, Results2a10, Results2a11, Results2a12, Results2a13, Results2a14, Results2a15, Results2a16, Results2a17, Results2a18, Results2a19, Results2a20, Results2a21, Results2a22, Results2a23, Results2a24, Results2a25, Results2a26, Results2a27, Results2a28, Results2a29, Results2a30, Results2a31, Results2a32, Results2a33, Results2a34, Results2a35, Results2a36, Results2a37, Results2a38, Results2a39, Results2a40, Results2a41, Results2a42, Results2a43, Results2a44, Results2a45, Results2a46, Results2a47, Results2a48, Results2a49, Results2a50, Results2a51, Results2a52, Results2a53, Results2a54, Results2a55, Results2a56, Results2a57, Results2a58, Results2a59, Results2a60, Results2a61, Results2a62, Results2a63, Results2a64, Results2a65, Results2a66, Results2a67, Results2a68, Results2a69, Results2a70, Results2a71, Results2a72, Results2a73, Results2a74, Results2a75, Results2a76, Results2a77, Results2a78, Results2a79, Results2a80, Results2a81, Results2a82, Results2a83, Results2a84, Results2a85, Results2a86, Results2a87, Results2a88, Results2a89, Results2a90, Results2a91, Results2a92, Results2a93, Results2a94, Results2a95, Results2a96, Results2a97, Results2a98, Results2a99, Results2a100)

Results2bALL <- rbind(Results2b1, Results2b2, Results2b3, Results2b4, Results2b5, Results2b6, Results2b7, Results2b8, Results2b9, Results2b10, Results2b11, Results2b12, Results2b13, Results2b14, Results2b15, Results2b16, Results2b17, Results2b18, Results2b19, Results2b20, Results2b21, Results2b22, Results2b23, Results2b24, Results2b25, Results2b26, Results2b27, Results2b28, Results2b29, Results2b30, Results2b31, Results2b32, Results2b33, Results2b34, Results2b35, Results2b36, Results2b37, Results2b38, Results2b39, Results2b40, Results2b41, Results2b42, Results2b43, Results2b44, Results2b45, Results2b46, Results2b47, Results2b48, Results2b49, Results2b50, Results2b51, Results2b52, Results2b53, Results2b54, Results2b55, Results2b56, Results2b57, Results2b58, Results2b59, Results2b60, Results2b61, Results2b62, Results2b63, Results2b64, Results2b65, Results2b66, Results2b67, Results2b68, Results2b69, Results2b70, Results2b71, Results2b72, Results2b73, Results2b74, Results2b75, Results2b76, Results2b77, Results2b78, Results2b79, Results2b80, Results2b81, Results2b82, Results2b83, Results2b84, Results2b85, Results2b86, Results2b87, Results2b88, Results2b89, Results2b90, Results2b91, Results2b92, Results2b93, Results2b94, Results2b95, Results2b96, Results2b97, Results2b98, Results2b99, Results2b100)

Results2cALL <- rbind(Results2c1, Results2c2, Results2c3, Results2c4, Results2c5, Results2c6, Results2c7, Results2c8, Results2c9, Results2c10, Results2c11, Results2c12, Results2c13, Results2c14, Results2c15, Results2c16, Results2c17, Results2c18, Results2c19, Results2c20, Results2c21, Results2c22, Results2c23, Results2c24, Results2c25, Results2c26, Results2c27, Results2c28, Results2c29, Results2c30, Results2c31, Results2c32, Results2c33, Results2c34, Results2c35, Results2c36, Results2c37, Results2c38, Results2c39, Results2c40, Results2c41, Results2c42, Results2c43, Results2c44, Results2c45, Results2c46, Results2c47, Results2c48, Results2c49, Results2c50, Results2c51, Results2c52, Results2c53, Results2c54, Results2c55, Results2c56, Results2c57, Results2c58, Results2c59, Results2c60, Results2c61, Results2c62, Results2c63, Results2c64, Results2c65, Results2c66, Results2c67, Results2c68, Results2c69, Results2c70, Results2c71, Results2c72, Results2c73, Results2c74, Results2c75, Results2c76, Results2c77, Results2c78, Results2c79, Results2c80, Results2c81, Results2c82, Results2c83, Results2c84, Results2c85, Results2c86, Results2c87, Results2c88, Results2c89, Results2c90, Results2c91, Results2c92, Results2c93, Results2c94, Results2c95, Results2c96, Results2c97, Results2c98, Results2c99, Results2c100)

Results2dALL <- rbind(Results2d1, Results2d2, Results2d3, Results2d4, Results2d5, Results2d6, Results2d7, Results2d8, Results2d9, Results2d10, Results2d11, Results2d12, Results2d13, Results2d14, Results2d15, Results2d16, Results2d17, Results2d18, Results2d19, Results2d20, Results2d21, Results2d22, Results2d23, Results2d24, Results2d25, Results2d26, Results2d27, Results2d28, Results2d29, Results2d30, Results2d31, Results2d32, Results2d33, Results2d34, Results2d35, Results2d36, Results2d37, Results2d38, Results2d39, Results2d40, Results2d41, Results2d42, Results2d43, Results2d44, Results2d45, Results2d46, Results2d47, Results2d48, Results2d49, Results2d50, Results2d51, Results2d52, Results2d53, Results2d54, Results2d55, Results2d56, Results2d57, Results2d58, Results2d59, Results2d60, Results2d61, Results2d62, Results2d63, Results2d64, Results2d65, Results2d66, Results2d67, Results2d68, Results2d69, Results2d70, Results2d71, Results2d72, Results2d73, Results2d74, Results2d75, Results2d76, Results2d77, Results2d78, Results2d79, Results2d80, Results2d81, Results2d82, Results2d83, Results2d84, Results2d85, Results2d86, Results2d87, Results2d88, Results2d89, Results2d90, Results2d91, Results2d92, Results2d93, Results2d94, Results2d95, Results2d96, Results2d97, Results2d98, Results2d99, Results2d100)


Results2ALL <- rbind(Results2aALL, Results2bALL, Results2cALL, Results2dALL)
YD2ALL <- Results2ALL[seq(1, nrow(Results2ALL), 2), ]
NoRTLM2ALL <- Results2ALL[seq(2, nrow(Results2ALL), 2), ]


# Rename first column names #

colnames(YD12ALL)[1] <- "YD"
colnames(NoRTLM12ALL)[1] <- "No RTLM"
colnames(YD1ALL)[1] <- "YD"
colnames(NoRTLM1ALL)[1] <- "No RTLM"
colnames(YD2ALL)[1] <- "YD"
colnames(NoRTLM2ALL)[1] <- "No RTLM"


#merge pairs of dataframes

Rw12ALL<- cbind(YD12ALL, NoRTLM12ALL)  # all violence
Rw1ALL<- cbind(YD1ALL, NoRTLM1ALL) # militia violence
Rw2ALL<- cbind(YD2ALL, NoRTLM2ALL) # individual violence


#reorder column names

Rw12ALL <- Rw12ALL[,c(1, 6, 2, 3, 4, 5)]
Rw1ALL <- Rw1ALL[,c(1, 6, 2, 3, 4, 5)]
Rw2ALL <- Rw2ALL[,c(1, 6, 2, 3, 4, 5)]




## calculate paired t-test ##

#subset YD and “No RTLM” columns

TTest12 <- Rw12ALL[ , 1:2]
TTest1 <- Rw1ALL[ , 1:2]
TTest2 <- Rw2ALL[ , 1:2]


# randomize and divide into 10 sets of 40 each

for (i in 1:10) { 
  #randomize dataframe
  TTest12 <- TTest12[sample(1:nrow(TTest12)), ] 
  
  #split into 10 sets
  
  #all violence
  
  TTest12a <- TTest12[1:40, ]
  TTest12b <- TTest12[41:80, ]
  TTest12c <- TTest12[81:120, ]
  TTest12d <- TTest12[121:160, ]
  TTest12e <- TTest12[161:200, ]
  TTest12f <- TTest12[201:240, ]
  TTest12g <- TTest12[241:280, ]
  TTest12h <- TTest12[281:320, ]
  TTest12i <- TTest12[321:360, ]
  TTest12j <- TTest12[361:400, ]
  
  assign(paste("TTest12", i, sep = ""), TTest12)
  assign(paste("TTest12a", i, sep = ""), TTest12a)
  assign(paste("TTest12b", i, sep = ""), TTest12b)
  assign(paste("TTest12c", i, sep = ""), TTest12c)
  assign(paste("TTest12d", i, sep = ""), TTest12d)
  assign(paste("TTest12e", i, sep = ""), TTest12e)
  assign(paste("TTest12f", i, sep = ""), TTest12f)
  assign(paste("TTest12g", i, sep = ""), TTest12g)
  assign(paste("TTest12h", i, sep = ""), TTest12h)
  
  assign(paste("TTest12i", i, sep = ""), TTest12i)
  assign(paste("TTest12j", i, sep = ""), TTest12j)
  
}

#militia violence

for (i in 1:10) { 
  #randomize dataframe
  TTest1 <- TTest1[sample(1:nrow(TTest1)), ] 
  
  
  #split into 10 sets
  
  TTest1a <- TTest1[1:40, ]
  TTest1b <- TTest1[41:80, ]
  TTest1c <- TTest1[81:120, ]
  TTest1d <- TTest1[121:160, ]
  TTest1e <- TTest1[161:200, ]
  TTest1f <- TTest1[201:240, ]
  TTest1g <- TTest1[241:280, ]
  TTest1h <- TTest1[281:320, ]
  TTest1i <- TTest1[321:360, ]
  TTest1j <- TTest1[361:400, ]
  
  assign(paste("TTest1", i, sep = ""), TTest1)
  assign(paste("TTest1a", i, sep = ""), TTest1a)
  assign(paste("TTest1b", i, sep = ""), TTest1b)
  assign(paste("TTest1c", i, sep = ""), TTest1c)
  assign(paste("TTest1d", i, sep = ""), TTest1d)
  assign(paste("TTest1e", i, sep = ""), TTest1e)
  assign(paste("TTest1f", i, sep = ""), TTest1f)
  assign(paste("TTest1g", i, sep = ""), TTest1g)
  assign(paste("TTest1h", i, sep = ""), TTest1h)
  assign(paste("TTest1i", i, sep = ""), TTest1i)
  assign(paste("TTest1j", i, sep = ""), TTest1j)
  
}

#individual violence

for (i in 1:10) { 
  #randomize dataframe
  TTest2 <- TTest2[sample(1:nrow(TTest2)), ] 
  
  
  #split into 10 sets
  
  TTest2a <- TTest2[1:40, ]
  TTest2b <- TTest2[41:80, ]
  TTest2c <- TTest2[81:120, ]
  TTest2d <- TTest2[121:160, ]
  TTest2e <- TTest2[161:200, ]
  TTest2f <- TTest2[201:240, ]
  TTest2g <- TTest2[241:280, ]
  TTest2h <- TTest2[281:320, ]
  TTest2i <- TTest2[321:360, ]
  TTest2j <- TTest2[361:400, ]
  
  assign(paste("TTest2", i, sep = ""), TTest2)
  assign(paste("TTest2a", i, sep = ""), TTest2a)
  assign(paste("TTest2b", i, sep = ""), TTest2b)
  assign(paste("TTest2c", i, sep = ""), TTest2c)
  assign(paste("TTest2d", i, sep = ""), TTest2d)
  assign(paste("TTest2e", i, sep = ""), TTest2e)
  assign(paste("TTest2f", i, sep = ""), TTest2f)
  assign(paste("TTest2g", i, sep = ""), TTest2g)
  assign(paste("TTest2h", i, sep = ""), TTest2h)
  assign(paste("TTest2i", i, sep = ""), TTest2i)
  assign(paste("TTest2j", i, sep = ""), TTest2j)
  
}



#Convert YDs to vectors

# all violence

YD12a1 <- TTest12a1[ , "YD"]
YD12b1 <- TTest12b1[ , "YD"]
YD12c1<- TTest12c1[ , "YD"]
YD12d1 <- TTest12d1[ , "YD"]
YD12e1 <- TTest12e1[ , "YD"]
YD12f1<- TTest12f1[ , "YD"]
YD12g1<- TTest12g1[ , "YD"]
YD12h1 <- TTest12h1[ , "YD"] 
YD12i1<- TTest12i1[ , "YD"]
YD12j1 <- TTest12j1[ , "YD"]

YD12a2 <- TTest12a2[ , "YD"]
YD12b2 <- TTest12b2[ , "YD"]
YD12c2 <- TTest12c2[ , "YD"]
YD12d2 <- TTest12d2[ , "YD"]
YD12e2 <- TTest12e2[ , "YD"]
YD12f2 <- TTest12f2[ , "YD"]
YD12g2 <- TTest12g2[ , "YD"]
YD12h2 <- TTest12h2[ , "YD"] 
YD12i2 <- TTest12i2[ , "YD"]
YD12j2 <- TTest12j2[ , "YD"]

YD12a3 <- TTest12a3[ , "YD"]
YD12b3 <- TTest12b3[ , "YD"]
YD12c3 <- TTest12c3[ , "YD"]
YD12d3 <- TTest12d3[ , "YD"]
YD12e3 <- TTest12e3[ , "YD"]
YD12f3 <- TTest12f3[ , "YD"]
YD12g3 <- TTest12g3[ , "YD"]
YD12h3 <- TTest12h3[ , "YD"] 
YD12i3 <- TTest12i3[ , "YD"]
YD12j3 <- TTest12j3[ , "YD"]

YD12a4 <- TTest12a4[ , "YD"]
YD12b4 <- TTest12b4[ , "YD"]
YD12c4 <- TTest12c4[ , "YD"]
YD12d4 <- TTest12d4[ , "YD"]
YD12e4 <- TTest12e4[ , "YD"]
YD12f4 <- TTest12f4[ , "YD"]
YD12g4 <- TTest12g4[ , "YD"]
YD12h4 <- TTest12h4[ , "YD"] 
YD12i4 <- TTest12i4[ , "YD"]
YD12j4 <- TTest12j4[ , "YD"]

YD12a5 <- TTest12a5[ , "YD"]
YD12b5 <- TTest12b5[ , "YD"]
YD12c5 <- TTest12c5[ , "YD"]
YD12d5 <- TTest12d5[ , "YD"]
YD12e5 <- TTest12e5[ , "YD"]
YD12f5 <- TTest12f5[ , "YD"]
YD12g5 <- TTest12g5[ , "YD"]
YD12h5 <- TTest12h5[ , "YD"] 
YD12i5 <- TTest12i5[ , "YD"]
YD12j5 <- TTest12j5[ , "YD"]

YD12a6 <- TTest12a6[ , "YD"]
YD12b6 <- TTest12b6[ , "YD"]
YD12c6 <- TTest12c6[ , "YD"]
YD12d6 <- TTest12d6[ , "YD"]
YD12e6 <- TTest12e6[ , "YD"]
YD12f6 <- TTest12f6[ , "YD"]
YD12g6 <- TTest12g6[ , "YD"]
YD12h6 <- TTest12h6[ , "YD"] 
YD12i6 <- TTest12i6[ , "YD"]
YD12j6 <- TTest12j6[ , "YD"]

YD12a7 <- TTest12a7[ , "YD"]
YD12b7 <- TTest12b7[ , "YD"]
YD12c7 <- TTest12c7[ , "YD"]
YD12d7 <- TTest12d7[ , "YD"]
YD12e7 <- TTest12e7[ , "YD"]
YD12f7 <- TTest12f7[ , "YD"]
YD12g7 <- TTest12g7[ , "YD"]
YD12h7 <- TTest12h7[ , "YD"] 
YD12i7 <- TTest12i7[ , "YD"]
YD12j7 <- TTest12j7[ , "YD"]

YD12a8 <- TTest12a8[ , "YD"]
YD12b8 <- TTest12b8[ , "YD"]
YD12c8 <- TTest12c8[ , "YD"]
YD12d8 <- TTest12d8[ , "YD"]
YD12e8 <- TTest12e8[ , "YD"]
YD12f8 <- TTest12f8[ , "YD"]
YD12g8 <- TTest12g8[ , "YD"]
YD12h8 <- TTest12h8[ , "YD"] 
YD12i8 <- TTest12i8[ , "YD"]
YD12j8 <- TTest12j8[ , "YD"]

YD12a9 <- TTest12a9[ , "YD"]
YD12b9 <- TTest12b9[ , "YD"]
YD12c9 <- TTest12c9[ , "YD"]
YD12d9 <- TTest12d9[ , "YD"]
YD12e9 <- TTest12e9[ , "YD"]
YD12f9 <- TTest12f9[ , "YD"]
YD12g9 <- TTest12g9[ , "YD"]
YD12h9 <- TTest12h9[ , "YD"] 
YD12i9 <- TTest12i9[ , "YD"]
YD12j9 <- TTest12j9[ , "YD"]

YD12a10 <- TTest12a10[ , "YD"]
YD12b10 <- TTest12b10[ , "YD"]
YD12c10 <- TTest12c10[ , "YD"]
YD12d10 <- TTest12d10[ , "YD"]
YD12e10 <- TTest12e10[ , "YD"]
YD12f10 <- TTest12f10[ , "YD"]
YD12g10 <- TTest12g10[ , "YD"]
YD12h10 <- TTest12h10[ , "YD"] 
YD12i10 <- TTest12i10[ , "YD"]
YD12j10 <- TTest12j10[ , "YD"]


# militia violence

YD1a1 <- TTest1a1[ , "YD"]
YD1b1 <- TTest1b1[ , "YD"]
YD1c1<- TTest1c1[ , "YD"]
YD1d1 <- TTest1d1[ , "YD"]
YD1e1 <- TTest1e1[ , "YD"]
YD1f1<- TTest1f1[ , "YD"]
YD1g1<- TTest1g1[ , "YD"]
YD1h1 <- TTest1h1[ , "YD"] 
YD1i1<- TTest1i1[ , "YD"]
YD1j1 <- TTest1j1[ , "YD"]

YD1a2 <- TTest1a2[ , "YD"]
YD1b2 <- TTest1b2[ , "YD"]
YD1c2 <- TTest1c2[ , "YD"]
YD1d2 <- TTest1d2[ , "YD"]
YD1e2 <- TTest1e2[ , "YD"]
YD1f2 <- TTest1f2[ , "YD"]
YD1g2 <- TTest1g2[ , "YD"]
YD1h2 <- TTest1h2[ , "YD"] 
YD1i2 <- TTest1i2[ , "YD"]
YD1j2 <- TTest1j2[ , "YD"]

YD1a3 <- TTest1a3[ , "YD"]
YD1b3 <- TTest1b3[ , "YD"]
YD1c3 <- TTest1c3[ , "YD"]
YD1d3 <- TTest1d3[ , "YD"]
YD1e3 <- TTest1e3[ , "YD"]
YD1f3 <- TTest1f3[ , "YD"]
YD1g3 <- TTest1g3[ , "YD"]
YD1h3 <- TTest1h3[ , "YD"] 
YD1i3 <- TTest1i3[ , "YD"]
YD1j3 <- TTest1j3[ , "YD"]

YD1a4 <- TTest1a4[ , "YD"]
YD1b4 <- TTest1b4[ , "YD"]
YD1c4 <- TTest1c4[ , "YD"]
YD1d4 <- TTest1d4[ , "YD"]
YD1e4 <- TTest1e4[ , "YD"]
YD1f4 <- TTest1f4[ , "YD"]
YD1g4 <- TTest1g4[ , "YD"]
YD1h4 <- TTest1h4[ , "YD"] 
YD1i4 <- TTest1i4[ , "YD"]
YD1j4 <- TTest1j4[ , "YD"]

YD1a5 <- TTest1a5[ , "YD"]
YD1b5 <- TTest1b5[ , "YD"]
YD1c5 <- TTest1c5[ , "YD"]
YD1d5 <- TTest1d5[ , "YD"]
YD1e5 <- TTest1e5[ , "YD"]
YD1f5 <- TTest1f5[ , "YD"]
YD1g5 <- TTest1g5[ , "YD"]
YD1h5 <- TTest1h5[ , "YD"] 
YD1i5 <- TTest1i5[ , "YD"]
YD1j5 <- TTest1j5[ , "YD"]

YD1a6 <- TTest1a6[ , "YD"]
YD1b6 <- TTest1b6[ , "YD"]
YD1c6 <- TTest1c6[ , "YD"]
YD1d6 <- TTest1d6[ , "YD"]
YD1e6 <- TTest1e6[ , "YD"]
YD1f6 <- TTest1f6[ , "YD"]
YD1g6 <- TTest1g6[ , "YD"]
YD1h6 <- TTest1h6[ , "YD"] 
YD1i6 <- TTest1i6[ , "YD"]
YD1j6 <- TTest1j6[ , "YD"]

YD1a7 <- TTest1a7[ , "YD"]
YD1b7 <- TTest1b7[ , "YD"]
YD1c7 <- TTest1c7[ , "YD"]
YD1d7 <- TTest1d7[ , "YD"]
YD1e7 <- TTest1e7[ , "YD"]
YD1f7 <- TTest1f7[ , "YD"]
YD1g7 <- TTest1g7[ , "YD"]
YD1h7 <- TTest1h7[ , "YD"] 
YD1i7 <- TTest1i7[ , "YD"]
YD1j7 <- TTest1j7[ , "YD"]

YD1a8 <- TTest1a8[ , "YD"]
YD1b8 <- TTest1b8[ , "YD"]
YD1c8 <- TTest1c8[ , "YD"]
YD1d8 <- TTest1d8[ , "YD"]
YD1e8 <- TTest1e8[ , "YD"]
YD1f8 <- TTest1f8[ , "YD"]
YD1g8 <- TTest1g8[ , "YD"]
YD1h8 <- TTest1h8[ , "YD"] 
YD1i8 <- TTest1i8[ , "YD"]
YD1j8 <- TTest1j8[ , "YD"]

YD1a9 <- TTest1a9[ , "YD"]
YD1b9 <- TTest1b9[ , "YD"]
YD1c9 <- TTest1c9[ , "YD"]
YD1d9 <- TTest1d9[ , "YD"]
YD1e9 <- TTest1e9[ , "YD"]
YD1f9 <- TTest1f9[ , "YD"]
YD1g9 <- TTest1g9[ , "YD"]
YD1h9 <- TTest1h9[ , "YD"] 
YD1i9 <- TTest1i9[ , "YD"]
YD1j9 <- TTest1j9[ , "YD"]

YD1a10 <- TTest1a10[ , "YD"]
YD1b10 <- TTest1b10[ , "YD"]
YD1c10 <- TTest1c10[ , "YD"]
YD1d10 <- TTest1d10[ , "YD"]
YD1e10 <- TTest1e10[ , "YD"]
YD1f10 <- TTest1f10[ , "YD"]
YD1g10 <- TTest1g10[ , "YD"]
YD1h10 <- TTest1h10[ , "YD"] 
YD1i10 <- TTest1i10[ , "YD"]
YD1j10 <- TTest1j10[ , "YD"]


# individual violence

YD2a1 <- TTest2a1[ , "YD"]
YD2b1 <- TTest2b1[ , "YD"]
YD2c1<- TTest2c1[ , "YD"]
YD2d1 <- TTest2d1[ , "YD"]
YD2e1 <- TTest2e1[ , "YD"]
YD2f1<- TTest2f1[ , "YD"]
YD2g1<- TTest2g1[ , "YD"]
YD2h1 <- TTest2h1[ , "YD"] 
YD2i1<- TTest2i1[ , "YD"]
YD2j1 <- TTest2j1[ , "YD"]

YD2a2 <- TTest2a2[ , "YD"]
YD2b2 <- TTest2b2[ , "YD"]
YD2c2 <- TTest2c2[ , "YD"]
YD2d2 <- TTest2d2[ , "YD"]
YD2e2 <- TTest2e2[ , "YD"]
YD2f2 <- TTest2f2[ , "YD"]
YD2g2 <- TTest2g2[ , "YD"]
YD2h2 <- TTest2h2[ , "YD"] 
YD2i2 <- TTest2i2[ , "YD"]
YD2j2 <- TTest2j2[ , "YD"]

YD2a3 <- TTest2a3[ , "YD"]
YD2b3 <- TTest2b3[ , "YD"]
YD2c3 <- TTest2c3[ , "YD"]
YD2d3 <- TTest2d3[ , "YD"]
YD2e3 <- TTest2e3[ , "YD"]
YD2f3 <- TTest2f3[ , "YD"]
YD2g3 <- TTest2g3[ , "YD"]
YD2h3 <- TTest2h3[ , "YD"] 
YD2i3 <- TTest2i3[ , "YD"]
YD2j3 <- TTest2j3[ , "YD"]

YD2a4 <- TTest2a4[ , "YD"]
YD2b4 <- TTest2b4[ , "YD"]
YD2c4 <- TTest2c4[ , "YD"]
YD2d4 <- TTest2d4[ , "YD"]
YD2e4 <- TTest2e4[ , "YD"]
YD2f4 <- TTest2f4[ , "YD"]
YD2g4 <- TTest2g4[ , "YD"]
YD2h4 <- TTest2h4[ , "YD"] 
YD2i4 <- TTest2i4[ , "YD"]
YD2j4 <- TTest2j4[ , "YD"]

YD2a5 <- TTest2a5[ , "YD"]
YD2b5 <- TTest2b5[ , "YD"]
YD2c5 <- TTest2c5[ , "YD"]
YD2d5 <- TTest2d5[ , "YD"]
YD2e5 <- TTest2e5[ , "YD"]
YD2f5 <- TTest2f5[ , "YD"]
YD2g5 <- TTest2g5[ , "YD"]
YD2h5 <- TTest2h5[ , "YD"] 
YD2i5 <- TTest2i5[ , "YD"]
YD2j5 <- TTest2j5[ , "YD"]

YD2a6 <- TTest2a6[ , "YD"]
YD2b6 <- TTest2b6[ , "YD"]
YD2c6 <- TTest2c6[ , "YD"]
YD2d6 <- TTest2d6[ , "YD"]
YD2e6 <- TTest2e6[ , "YD"]
YD2f6 <- TTest2f6[ , "YD"]
YD2g6 <- TTest2g6[ , "YD"]
YD2h6 <- TTest2h6[ , "YD"] 
YD2i6 <- TTest2i6[ , "YD"]
YD2j6 <- TTest2j6[ , "YD"]

YD2a7 <- TTest2a7[ , "YD"]
YD2b7 <- TTest2b7[ , "YD"]
YD2c7 <- TTest2c7[ , "YD"]
YD2d7 <- TTest2d7[ , "YD"]
YD2e7 <- TTest2e7[ , "YD"]
YD2f7 <- TTest2f7[ , "YD"]
YD2g7 <- TTest2g7[ , "YD"]
YD2h7 <- TTest2h7[ , "YD"] 
YD2i7 <- TTest2i7[ , "YD"]
YD2j7 <- TTest2j7[ , "YD"]

YD2a8 <- TTest2a8[ , "YD"]
YD2b8 <- TTest2b8[ , "YD"]
YD2c8 <- TTest2c8[ , "YD"]
YD2d8 <- TTest2d8[ , "YD"]
YD2e8 <- TTest2e8[ , "YD"]
YD2f8 <- TTest2f8[ , "YD"]
YD2g8 <- TTest2g8[ , "YD"]
YD2h8 <- TTest2h8[ , "YD"] 
YD2i8 <- TTest2i8[ , "YD"]
YD2j8 <- TTest2j8[ , "YD"]

YD2a9 <- TTest2a9[ , "YD"]
YD2b9 <- TTest2b9[ , "YD"]
YD2c9 <- TTest2c9[ , "YD"]
YD2d9 <- TTest2d9[ , "YD"]
YD2e9 <- TTest2e9[ , "YD"]
YD2f9 <- TTest2f9[ , "YD"]
YD2g9 <- TTest2g9[ , "YD"]
YD2h9 <- TTest2h9[ , "YD"] 
YD2i9 <- TTest2i9[ , "YD"]
YD2j9 <- TTest2j9[ , "YD"]

YD2a10 <- TTest2a10[ , "YD"]
YD2b10 <- TTest2b10[ , "YD"]
YD2c10 <- TTest2c10[ , "YD"]
YD2d10 <- TTest2d10[ , "YD"]
YD2e10 <- TTest2e10[ , "YD"]
YD2f10 <- TTest2f10[ , "YD"]
YD2g10 <- TTest2g10[ , "YD"]
YD2h10 <- TTest2h10[ , "YD"] 
YD2i10 <- TTest2i10[ , "YD"]
YD2j10 <- TTest2j10[ , "YD"]



#Convert No RTLMs to vectors

# all violence

NoRTLM12a1 <- TTest12a1[ , "No RTLM"]
NoRTLM12b1 <- TTest12b1[ , "No RTLM"]
NoRTLM12c1<- TTest12c1[ , "No RTLM"]
NoRTLM12d1 <- TTest12d1[ , "No RTLM"]
NoRTLM12e1 <- TTest12e1[ , "No RTLM"]
NoRTLM12f1<- TTest12f1[ , "No RTLM"]
NoRTLM12g1<- TTest12g1[ , "No RTLM"]
NoRTLM12h1 <- TTest12h1[ , "No RTLM"] 
NoRTLM12i1<- TTest12i1[ , "No RTLM"]
NoRTLM12j1 <- TTest12j1[ , "No RTLM"]

NoRTLM12a2 <- TTest12a2[ , "No RTLM"]
NoRTLM12b2 <- TTest12b2[ , "No RTLM"]
NoRTLM12c2 <- TTest12c2[ , "No RTLM"]
NoRTLM12d2 <- TTest12d2[ , "No RTLM"]
NoRTLM12e2 <- TTest12e2[ , "No RTLM"]
NoRTLM12f2 <- TTest12f2[ , "No RTLM"]
NoRTLM12g2 <- TTest12g2[ , "No RTLM"]
NoRTLM12h2 <- TTest12h2[ , "No RTLM"] 
NoRTLM12i2 <- TTest12i2[ , "No RTLM"]
NoRTLM12j2 <- TTest12j2[ , "No RTLM"]

NoRTLM12a3 <- TTest12a3[ , "No RTLM"]
NoRTLM12b3 <- TTest12b3[ , "No RTLM"]
NoRTLM12c3 <- TTest12c3[ , "No RTLM"]
NoRTLM12d3 <- TTest12d3[ , "No RTLM"]
NoRTLM12e3 <- TTest12e3[ , "No RTLM"]
NoRTLM12f3 <- TTest12f3[ , "No RTLM"]
NoRTLM12g3 <- TTest12g3[ , "No RTLM"]
NoRTLM12h3 <- TTest12h3[ , "No RTLM"] 
NoRTLM12i3 <- TTest12i3[ , "No RTLM"]
NoRTLM12j3 <- TTest12j3[ , "No RTLM"]

NoRTLM12a4 <- TTest12a4[ , "No RTLM"]
NoRTLM12b4 <- TTest12b4[ , "No RTLM"]
NoRTLM12c4 <- TTest12c4[ , "No RTLM"]
NoRTLM12d4 <- TTest12d4[ , "No RTLM"]
NoRTLM12e4 <- TTest12e4[ , "No RTLM"]
NoRTLM12f4 <- TTest12f4[ , "No RTLM"]
NoRTLM12g4 <- TTest12g4[ , "No RTLM"]
NoRTLM12h4 <- TTest12h4[ , "No RTLM"] 
NoRTLM12i4 <- TTest12i4[ , "No RTLM"]
NoRTLM12j4 <- TTest12j4[ , "No RTLM"]

NoRTLM12a5 <- TTest12a5[ , "No RTLM"]
NoRTLM12b5 <- TTest12b5[ , "No RTLM"]
NoRTLM12c5 <- TTest12c5[ , "No RTLM"]
NoRTLM12d5 <- TTest12d5[ , "No RTLM"]
NoRTLM12e5 <- TTest12e5[ , "No RTLM"]
NoRTLM12f5 <- TTest12f5[ , "No RTLM"]
NoRTLM12g5 <- TTest12g5[ , "No RTLM"]
NoRTLM12h5 <- TTest12h5[ , "No RTLM"] 
NoRTLM12i5 <- TTest12i5[ , "No RTLM"]
NoRTLM12j5 <- TTest12j5[ , "No RTLM"]

NoRTLM12a6 <- TTest12a6[ , "No RTLM"]
NoRTLM12b6 <- TTest12b6[ , "No RTLM"]
NoRTLM12c6 <- TTest12c6[ , "No RTLM"]
NoRTLM12d6 <- TTest12d6[ , "No RTLM"]
NoRTLM12e6 <- TTest12e6[ , "No RTLM"]
NoRTLM12f6 <- TTest12f6[ , "No RTLM"]
NoRTLM12g6 <- TTest12g6[ , "No RTLM"]
NoRTLM12h6 <- TTest12h6[ , "No RTLM"] 
NoRTLM12i6 <- TTest12i6[ , "No RTLM"]
NoRTLM12j6 <- TTest12j6[ , "No RTLM"]

NoRTLM12a7 <- TTest12a7[ , "No RTLM"]
NoRTLM12b7 <- TTest12b7[ , "No RTLM"]
NoRTLM12c7 <- TTest12c7[ , "No RTLM"]
NoRTLM12d7 <- TTest12d7[ , "No RTLM"]
NoRTLM12e7 <- TTest12e7[ , "No RTLM"]
NoRTLM12f7 <- TTest12f7[ , "No RTLM"]
NoRTLM12g7 <- TTest12g7[ , "No RTLM"]
NoRTLM12h7 <- TTest12h7[ , "No RTLM"] 
NoRTLM12i7 <- TTest12i7[ , "No RTLM"]
NoRTLM12j7 <- TTest12j7[ , "No RTLM"]

NoRTLM12a8 <- TTest12a8[ , "No RTLM"]
NoRTLM12b8 <- TTest12b8[ , "No RTLM"]
NoRTLM12c8 <- TTest12c8[ , "No RTLM"]
NoRTLM12d8 <- TTest12d8[ , "No RTLM"]
NoRTLM12e8 <- TTest12e8[ , "No RTLM"]
NoRTLM12f8 <- TTest12f8[ , "No RTLM"]
NoRTLM12g8 <- TTest12g8[ , "No RTLM"]
NoRTLM12h8 <- TTest12h8[ , "No RTLM"] 
NoRTLM12i8 <- TTest12i8[ , "No RTLM"]
NoRTLM12j8 <- TTest12j8[ , "No RTLM"]

NoRTLM12a9 <- TTest12a9[ , "No RTLM"]
NoRTLM12b9 <- TTest12b9[ , "No RTLM"]
NoRTLM12c9 <- TTest12c9[ , "No RTLM"]
NoRTLM12d9 <- TTest12d9[ , "No RTLM"]
NoRTLM12e9 <- TTest12e9[ , "No RTLM"]
NoRTLM12f9 <- TTest12f9[ , "No RTLM"]
NoRTLM12g9 <- TTest12g9[ , "No RTLM"]
NoRTLM12h9 <- TTest12h9[ , "No RTLM"] 
NoRTLM12i9 <- TTest12i9[ , "No RTLM"]
NoRTLM12j9 <- TTest12j9[ , "No RTLM"]

NoRTLM12a10 <- TTest12a10[ , "No RTLM"]
NoRTLM12b10 <- TTest12b10[ , "No RTLM"]
NoRTLM12c10 <- TTest12c10[ , "No RTLM"]
NoRTLM12d10 <- TTest12d10[ , "No RTLM"]
NoRTLM12e10 <- TTest12e10[ , "No RTLM"]
NoRTLM12f10 <- TTest12f10[ , "No RTLM"]
NoRTLM12g10 <- TTest12g10[ , "No RTLM"]
NoRTLM12h10 <- TTest12h10[ , "No RTLM"] 
NoRTLM12i10 <- TTest12i10[ , "No RTLM"]
NoRTLM12j10 <- TTest12j10[ , "No RTLM"]


# militia violence

NoRTLM1a1 <- TTest1a1[ , "No RTLM"]
NoRTLM1b1 <- TTest1b1[ , "No RTLM"]
NoRTLM1c1<- TTest1c1[ , "No RTLM"]
NoRTLM1d1 <- TTest1d1[ , "No RTLM"]
NoRTLM1e1 <- TTest1e1[ , "No RTLM"]
NoRTLM1f1<- TTest1f1[ , "No RTLM"]
NoRTLM1g1<- TTest1g1[ , "No RTLM"]
NoRTLM1h1 <- TTest1h1[ , "No RTLM"] 
NoRTLM1i1<- TTest1i1[ , "No RTLM"]
NoRTLM1j1 <- TTest1j1[ , "No RTLM"]

NoRTLM1a2 <- TTest1a2[ , "No RTLM"]
NoRTLM1b2 <- TTest1b2[ , "No RTLM"]
NoRTLM1c2 <- TTest1c2[ , "No RTLM"]
NoRTLM1d2 <- TTest1d2[ , "No RTLM"]
NoRTLM1e2 <- TTest1e2[ , "No RTLM"]
NoRTLM1f2 <- TTest1f2[ , "No RTLM"]
NoRTLM1g2 <- TTest1g2[ , "No RTLM"]
NoRTLM1h2 <- TTest1h2[ , "No RTLM"] 
NoRTLM1i2 <- TTest1i2[ , "No RTLM"]
NoRTLM1j2 <- TTest1j2[ , "No RTLM"]

NoRTLM1a3 <- TTest1a3[ , "No RTLM"]
NoRTLM1b3 <- TTest1b3[ , "No RTLM"]
NoRTLM1c3 <- TTest1c3[ , "No RTLM"]
NoRTLM1d3 <- TTest1d3[ , "No RTLM"]
NoRTLM1e3 <- TTest1e3[ , "No RTLM"]
NoRTLM1f3 <- TTest1f3[ , "No RTLM"]
NoRTLM1g3 <- TTest1g3[ , "No RTLM"]
NoRTLM1h3 <- TTest1h3[ , "No RTLM"] 
NoRTLM1i3 <- TTest1i3[ , "No RTLM"]
NoRTLM1j3 <- TTest1j3[ , "No RTLM"]

NoRTLM1a4 <- TTest1a4[ , "No RTLM"]
NoRTLM1b4 <- TTest1b4[ , "No RTLM"]
NoRTLM1c4 <- TTest1c4[ , "No RTLM"]
NoRTLM1d4 <- TTest1d4[ , "No RTLM"]
NoRTLM1e4 <- TTest1e4[ , "No RTLM"]
NoRTLM1f4 <- TTest1f4[ , "No RTLM"]
NoRTLM1g4 <- TTest1g4[ , "No RTLM"]
NoRTLM1h4 <- TTest1h4[ , "No RTLM"] 
NoRTLM1i4 <- TTest1i4[ , "No RTLM"]
NoRTLM1j4 <- TTest1j4[ , "No RTLM"]

NoRTLM1a5 <- TTest1a5[ , "No RTLM"]
NoRTLM1b5 <- TTest1b5[ , "No RTLM"]
NoRTLM1c5 <- TTest1c5[ , "No RTLM"]
NoRTLM1d5 <- TTest1d5[ , "No RTLM"]
NoRTLM1e5 <- TTest1e5[ , "No RTLM"]
NoRTLM1f5 <- TTest1f5[ , "No RTLM"]
NoRTLM1g5 <- TTest1g5[ , "No RTLM"]
NoRTLM1h5 <- TTest1h5[ , "No RTLM"] 
NoRTLM1i5 <- TTest1i5[ , "No RTLM"]
NoRTLM1j5 <- TTest1j5[ , "No RTLM"]

NoRTLM1a6 <- TTest1a6[ , "No RTLM"]
NoRTLM1b6 <- TTest1b6[ , "No RTLM"]
NoRTLM1c6 <- TTest1c6[ , "No RTLM"]
NoRTLM1d6 <- TTest1d6[ , "No RTLM"]
NoRTLM1e6 <- TTest1e6[ , "No RTLM"]
NoRTLM1f6 <- TTest1f6[ , "No RTLM"]
NoRTLM1g6 <- TTest1g6[ , "No RTLM"]
NoRTLM1h6 <- TTest1h6[ , "No RTLM"] 
NoRTLM1i6 <- TTest1i6[ , "No RTLM"]
NoRTLM1j6 <- TTest1j6[ , "No RTLM"]

NoRTLM1a7 <- TTest1a7[ , "No RTLM"]
NoRTLM1b7 <- TTest1b7[ , "No RTLM"]
NoRTLM1c7 <- TTest1c7[ , "No RTLM"]
NoRTLM1d7 <- TTest1d7[ , "No RTLM"]
NoRTLM1e7 <- TTest1e7[ , "No RTLM"]
NoRTLM1f7 <- TTest1f7[ , "No RTLM"]
NoRTLM1g7 <- TTest1g7[ , "No RTLM"]
NoRTLM1h7 <- TTest1h7[ , "No RTLM"] 
NoRTLM1i7 <- TTest1i7[ , "No RTLM"]
NoRTLM1j7 <- TTest1j7[ , "No RTLM"]

NoRTLM1a8 <- TTest1a8[ , "No RTLM"]
NoRTLM1b8 <- TTest1b8[ , "No RTLM"]
NoRTLM1c8 <- TTest1c8[ , "No RTLM"]
NoRTLM1d8 <- TTest1d8[ , "No RTLM"]
NoRTLM1e8 <- TTest1e8[ , "No RTLM"]
NoRTLM1f8 <- TTest1f8[ , "No RTLM"]
NoRTLM1g8 <- TTest1g8[ , "No RTLM"]
NoRTLM1h8 <- TTest1h8[ , "No RTLM"] 
NoRTLM1i8 <- TTest1i8[ , "No RTLM"]
NoRTLM1j8 <- TTest1j8[ , "No RTLM"]

NoRTLM1a9 <- TTest1a9[ , "No RTLM"]
NoRTLM1b9 <- TTest1b9[ , "No RTLM"]
NoRTLM1c9 <- TTest1c9[ , "No RTLM"]
NoRTLM1d9 <- TTest1d9[ , "No RTLM"]
NoRTLM1e9 <- TTest1e9[ , "No RTLM"]
NoRTLM1f9 <- TTest1f9[ , "No RTLM"]
NoRTLM1g9 <- TTest1g9[ , "No RTLM"]
NoRTLM1h9 <- TTest1h9[ , "No RTLM"] 
NoRTLM1i9 <- TTest1i9[ , "No RTLM"]
NoRTLM1j9 <- TTest1j9[ , "No RTLM"]

NoRTLM1a10 <- TTest1a10[ , "No RTLM"]
NoRTLM1b10 <- TTest1b10[ , "No RTLM"]
NoRTLM1c10 <- TTest1c10[ , "No RTLM"]
NoRTLM1d10 <- TTest1d10[ , "No RTLM"]
NoRTLM1e10 <- TTest1e10[ , "No RTLM"]
NoRTLM1f10 <- TTest1f10[ , "No RTLM"]
NoRTLM1g10 <- TTest1g10[ , "No RTLM"]
NoRTLM1h10 <- TTest1h10[ , "No RTLM"] 
NoRTLM1i10 <- TTest1i10[ , "No RTLM"]
NoRTLM1j10 <- TTest1j10[ , "No RTLM"]


# individual violence

NoRTLM2a1 <- TTest2a1[ , "No RTLM"]
NoRTLM2b1 <- TTest2b1[ , "No RTLM"]
NoRTLM2c1<- TTest2c1[ , "No RTLM"]
NoRTLM2d1 <- TTest2d1[ , "No RTLM"]
NoRTLM2e1 <- TTest2e1[ , "No RTLM"]
NoRTLM2f1<- TTest2f1[ , "No RTLM"]
NoRTLM2g1<- TTest2g1[ , "No RTLM"]
NoRTLM2h1 <- TTest2h1[ , "No RTLM"] 
NoRTLM2i1<- TTest2i1[ , "No RTLM"]
NoRTLM2j1 <- TTest2j1[ , "No RTLM"]

NoRTLM2a2 <- TTest2a2[ , "No RTLM"]
NoRTLM2b2 <- TTest2b2[ , "No RTLM"]
NoRTLM2c2 <- TTest2c2[ , "No RTLM"]
NoRTLM2d2 <- TTest2d2[ , "No RTLM"]
NoRTLM2e2 <- TTest2e2[ , "No RTLM"]
NoRTLM2f2 <- TTest2f2[ , "No RTLM"]
NoRTLM2g2 <- TTest2g2[ , "No RTLM"]
NoRTLM2h2 <- TTest2h2[ , "No RTLM"] 
NoRTLM2i2 <- TTest2i2[ , "No RTLM"]
NoRTLM2j2 <- TTest2j2[ , "No RTLM"]

NoRTLM2a3 <- TTest2a3[ , "No RTLM"]
NoRTLM2b3 <- TTest2b3[ , "No RTLM"]
NoRTLM2c3 <- TTest2c3[ , "No RTLM"]
NoRTLM2d3 <- TTest2d3[ , "No RTLM"]
NoRTLM2e3 <- TTest2e3[ , "No RTLM"]
NoRTLM2f3 <- TTest2f3[ , "No RTLM"]
NoRTLM2g3 <- TTest2g3[ , "No RTLM"]
NoRTLM2h3 <- TTest2h3[ , "No RTLM"] 
NoRTLM2i3 <- TTest2i3[ , "No RTLM"]
NoRTLM2j3 <- TTest2j3[ , "No RTLM"]

NoRTLM2a4 <- TTest2a4[ , "No RTLM"]
NoRTLM2b4 <- TTest2b4[ , "No RTLM"]
NoRTLM2c4 <- TTest2c4[ , "No RTLM"]
NoRTLM2d4 <- TTest2d4[ , "No RTLM"]
NoRTLM2e4 <- TTest2e4[ , "No RTLM"]
NoRTLM2f4 <- TTest2f4[ , "No RTLM"]
NoRTLM2g4 <- TTest2g4[ , "No RTLM"]
NoRTLM2h4 <- TTest2h4[ , "No RTLM"] 
NoRTLM2i4 <- TTest2i4[ , "No RTLM"]
NoRTLM2j4 <- TTest2j4[ , "No RTLM"]

NoRTLM2a5 <- TTest2a5[ , "No RTLM"]
NoRTLM2b5 <- TTest2b5[ , "No RTLM"]
NoRTLM2c5 <- TTest2c5[ , "No RTLM"]
NoRTLM2d5 <- TTest2d5[ , "No RTLM"]
NoRTLM2e5 <- TTest2e5[ , "No RTLM"]
NoRTLM2f5 <- TTest2f5[ , "No RTLM"]
NoRTLM2g5 <- TTest2g5[ , "No RTLM"]
NoRTLM2h5 <- TTest2h5[ , "No RTLM"] 
NoRTLM2i5 <- TTest2i5[ , "No RTLM"]
NoRTLM2j5 <- TTest2j5[ , "No RTLM"]

NoRTLM2a6 <- TTest2a6[ , "No RTLM"]
NoRTLM2b6 <- TTest2b6[ , "No RTLM"]
NoRTLM2c6 <- TTest2c6[ , "No RTLM"]
NoRTLM2d6 <- TTest2d6[ , "No RTLM"]
NoRTLM2e6 <- TTest2e6[ , "No RTLM"]
NoRTLM2f6 <- TTest2f6[ , "No RTLM"]
NoRTLM2g6 <- TTest2g6[ , "No RTLM"]
NoRTLM2h6 <- TTest2h6[ , "No RTLM"] 
NoRTLM2i6 <- TTest2i6[ , "No RTLM"]
NoRTLM2j6 <- TTest2j6[ , "No RTLM"]

NoRTLM2a7 <- TTest2a7[ , "No RTLM"]
NoRTLM2b7 <- TTest2b7[ , "No RTLM"]
NoRTLM2c7 <- TTest2c7[ , "No RTLM"]
NoRTLM2d7 <- TTest2d7[ , "No RTLM"]
NoRTLM2e7 <- TTest2e7[ , "No RTLM"]
NoRTLM2f7 <- TTest2f7[ , "No RTLM"]
NoRTLM2g7 <- TTest2g7[ , "No RTLM"]
NoRTLM2h7 <- TTest2h7[ , "No RTLM"] 
NoRTLM2i7 <- TTest2i7[ , "No RTLM"]
NoRTLM2j7 <- TTest2j7[ , "No RTLM"]

NoRTLM2a8 <- TTest2a8[ , "No RTLM"]
NoRTLM2b8 <- TTest2b8[ , "No RTLM"]
NoRTLM2c8 <- TTest2c8[ , "No RTLM"]
NoRTLM2d8 <- TTest2d8[ , "No RTLM"]
NoRTLM2e8 <- TTest2e8[ , "No RTLM"]
NoRTLM2f8 <- TTest2f8[ , "No RTLM"]
NoRTLM2g8 <- TTest2g8[ , "No RTLM"]
NoRTLM2h8 <- TTest2h8[ , "No RTLM"] 
NoRTLM2i8 <- TTest2i8[ , "No RTLM"]
NoRTLM2j8 <- TTest2j8[ , "No RTLM"]

NoRTLM2a9 <- TTest2a9[ , "No RTLM"]
NoRTLM2b9 <- TTest2b9[ , "No RTLM"]
NoRTLM2c9 <- TTest2c9[ , "No RTLM"]
NoRTLM2d9 <- TTest2d9[ , "No RTLM"]
NoRTLM2e9 <- TTest2e9[ , "No RTLM"]
NoRTLM2f9 <- TTest2f9[ , "No RTLM"]
NoRTLM2g9 <- TTest2g9[ , "No RTLM"]
NoRTLM2h9 <- TTest2h9[ , "No RTLM"] 
NoRTLM2i9 <- TTest2i9[ , "No RTLM"]
NoRTLM2j9 <- TTest2j9[ , "No RTLM"]

NoRTLM2a10 <- TTest2a10[ , "No RTLM"]
NoRTLM2b10 <- TTest2b10[ , "No RTLM"]
NoRTLM2c10 <- TTest2c10[ , "No RTLM"]
NoRTLM2d10 <- TTest2d10[ , "No RTLM"]
NoRTLM2e10 <- TTest2e10[ , "No RTLM"]
NoRTLM2f10 <- TTest2f10[ , "No RTLM"]
NoRTLM2g10 <- TTest2g10[ , "No RTLM"]
NoRTLM2h10 <- TTest2h10[ , "No RTLM"] 
NoRTLM2i10 <- TTest2i10[ , "No RTLM"]
NoRTLM2j10 <- TTest2j10[ , "No RTLM"]


#Run paired t-tests

# all violence

TTres.12a1<- t.test(YD12a1, NoRTLM12a1, paired =TRUE)
TTres.12b1<-t.test(YD12b1, NoRTLM12b1, paired =TRUE)
TTres.12c1<-t.test(YD12c1, NoRTLM12c1, paired =TRUE)
TTres.12d1<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e1<-t.test(YD12e1, NoRTLM12e1, paired =TRUE)
TTres.12f1<-t.test(YD12f1, NoRTLM12f1, paired =TRUE)
TTres.12g1<-t.test(YD12g1, NoRTLM12g1, paired =TRUE)
TTres.12h1<-t.test(YD12h1, NoRTLM12h1, paired =TRUE)
TTres.12i1<-t.test(YD12i1, NoRTLM12i1, paired =TRUE)
TTres.12j1<-t.test(YD12j1, NoRTLM12j1, paired =TRUE)

TTres.12a2<- t.test(YD12a2, NoRTLM12a2, paired =TRUE)
TTres.12b2<-t.test(YD12b2, NoRTLM12b2, paired =TRUE)
TTres.12c2<-t.test(YD12c2, NoRTLM12c2, paired =TRUE)
TTres.12d2<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e2<-t.test(YD12e2, NoRTLM12e2, paired =TRUE)
TTres.12f2<-t.test(YD12f2, NoRTLM12f2, paired =TRUE)
TTres.12g2<-t.test(YD12g2, NoRTLM12g2, paired =TRUE)
TTres.12h2<-t.test(YD12h2, NoRTLM12h2, paired =TRUE)
TTres.12i2<-t.test(YD12i2, NoRTLM12i2, paired =TRUE)
TTres.12j2<-t.test(YD12j2, NoRTLM12j2, paired =TRUE)

TTres.12a3<- t.test(YD12a3, NoRTLM12a3, paired =TRUE)
TTres.12b3<-t.test(YD12b3, NoRTLM12b3, paired =TRUE)
TTres.12c3<-t.test(YD12c3, NoRTLM12c3, paired =TRUE)
TTres.12d3<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e3<-t.test(YD12e3, NoRTLM12e3, paired =TRUE)
TTres.12f3<-t.test(YD12f3, NoRTLM12f3, paired =TRUE)
TTres.12g3<-t.test(YD12g3, NoRTLM12g3, paired =TRUE)
TTres.12h3<-t.test(YD12h3, NoRTLM12h3, paired =TRUE)
TTres.12i3<-t.test(YD12i3, NoRTLM12i3, paired =TRUE)
TTres.12j3<-t.test(YD12j3, NoRTLM12j3, paired =TRUE)

TTres.12a4<- t.test(YD12a4, NoRTLM12a4, paired =TRUE)
TTres.12b4<-t.test(YD12b4, NoRTLM12b4, paired =TRUE)
TTres.12c4<-t.test(YD12c4, NoRTLM12c4, paired =TRUE)
TTres.12d4<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e4<-t.test(YD12e4, NoRTLM12e4, paired =TRUE)
TTres.12f4<-t.test(YD12f4, NoRTLM12f4, paired =TRUE)
TTres.12g4<-t.test(YD12g4, NoRTLM12g4, paired =TRUE)
TTres.12h4<-t.test(YD12h4, NoRTLM12h4, paired =TRUE)
TTres.12i4<-t.test(YD12i4, NoRTLM12i4, paired =TRUE)
TTres.12j4<-t.test(YD12j4, NoRTLM12j4, paired =TRUE)

TTres.12a5<- t.test(YD12a5, NoRTLM12a5, paired =TRUE)
TTres.12b5<-t.test(YD12b5, NoRTLM12b5, paired =TRUE)
TTres.12c5<-t.test(YD12c5, NoRTLM12c5, paired =TRUE)
TTres.12d5<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e5<-t.test(YD12e5, NoRTLM12e5, paired =TRUE)
TTres.12f5<-t.test(YD12f5, NoRTLM12f5, paired =TRUE)
TTres.12g5<-t.test(YD12g5, NoRTLM12g5, paired =TRUE)
TTres.12h5<-t.test(YD12h5, NoRTLM12h5, paired =TRUE)
TTres.12i5<-t.test(YD12i5, NoRTLM12i5, paired =TRUE)
TTres.12j5<-t.test(YD12j5, NoRTLM12j5, paired =TRUE)

TTres.12a6<- t.test(YD12a6, NoRTLM12a6, paired =TRUE)
TTres.12b6<-t.test(YD12b6, NoRTLM12b6, paired =TRUE)
TTres.12c6<-t.test(YD12c6, NoRTLM12c6, paired =TRUE)
TTres.12d6<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e6<-t.test(YD12e6, NoRTLM12e6, paired =TRUE)
TTres.12f6<-t.test(YD12f6, NoRTLM12f6, paired =TRUE)
TTres.12g6<-t.test(YD12g6, NoRTLM12g6, paired =TRUE)
TTres.12h6<-t.test(YD12h6, NoRTLM12h6, paired =TRUE)
TTres.12i6<-t.test(YD12i6, NoRTLM12i6, paired =TRUE)
TTres.12j6<-t.test(YD12j6, NoRTLM12j6, paired =TRUE)

TTres.12a7<- t.test(YD12a7, NoRTLM12a7, paired =TRUE)
TTres.12b7<-t.test(YD12b7, NoRTLM12b7, paired =TRUE)
TTres.12c7<-t.test(YD12c7, NoRTLM12c7, paired =TRUE)
TTres.12d7<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e7<-t.test(YD12e7, NoRTLM12e7, paired =TRUE)
TTres.12f7<-t.test(YD12f7, NoRTLM12f7, paired =TRUE)
TTres.12g7<-t.test(YD12g7, NoRTLM12g7, paired =TRUE)
TTres.12h7<-t.test(YD12h7, NoRTLM12h7, paired =TRUE)
TTres.12i7<-t.test(YD12i7, NoRTLM12i7, paired =TRUE)
TTres.12j7<-t.test(YD12j7, NoRTLM12j7, paired =TRUE)

TTres.12a8<- t.test(YD12a8, NoRTLM12a8, paired =TRUE)
TTres.12b8<-t.test(YD12b8, NoRTLM12b8, paired =TRUE)
TTres.12c8<-t.test(YD12c8, NoRTLM12c8, paired =TRUE)
TTres.12d8 <-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e8<-t.test(YD12e8, NoRTLM12e8, paired =TRUE)
TTres.12f8<-t.test(YD12f8, NoRTLM12f8, paired =TRUE)
TTres.12g8<-t.test(YD12g8, NoRTLM12g8, paired =TRUE)
TTres.12h8<-t.test(YD12h8, NoRTLM12h8, paired =TRUE)
TTres.12i8<-t.test(YD12i8, NoRTLM12i8, paired =TRUE)
TTres.12j8<-t.test(YD12j8, NoRTLM12j8, paired =TRUE)

TTres.12a9<- t.test(YD12a9, NoRTLM12a9, paired =TRUE)
TTres.12b9<-t.test(YD12b9, NoRTLM12b9, paired =TRUE)
TTres.12c9<-t.test(YD12c9, NoRTLM12c9, paired =TRUE)
TTres.12d9<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e9<-t.test(YD12e9, NoRTLM12e9, paired =TRUE)
TTres.12f9<-t.test(YD12f9, NoRTLM12f9, paired =TRUE)
TTres.12g9<-t.test(YD12g9, NoRTLM12g9, paired =TRUE)
TTres.12h9<-t.test(YD12h9, NoRTLM12h9, paired =TRUE)
TTres.12i9<-t.test(YD12i9, NoRTLM12i9, paired =TRUE)
TTres.12j9<-t.test(YD12j9, NoRTLM12j9, paired =TRUE)

TTres.12a10<- t.test(YD12a10, NoRTLM12a10, paired =TRUE)
TTres.12b10<-t.test(YD12b10, NoRTLM12b10, paired =TRUE)
TTres.12c10<-t.test(YD12c10, NoRTLM12c10, paired =TRUE)
TTres.12d10<-t.test(YD12d1, NoRTLM12d1, paired =TRUE)
TTres.12e10<-t.test(YD12e10, NoRTLM12e10, paired =TRUE)
TTres.12f10<-t.test(YD12f10, NoRTLM12f10, paired =TRUE)
TTres.12g10<-t.test(YD12g10, NoRTLM12g10, paired =TRUE)
TTres.12h10<-t.test(YD12h10, NoRTLM12h10, paired =TRUE)
TTres.12i10<-t.test(YD12i10, NoRTLM12i10, paired =TRUE)
TTres.12j10<-t.test(YD12j10, NoRTLM12j10, paired =TRUE)



# milita violence

TTres.1a1<- t.test(YD1a1, NoRTLM1a1, paired =TRUE)
TTres.1b1<-t.test(YD1b1, NoRTLM1b1, paired =TRUE)
TTres.1c1<-t.test(YD1c1, NoRTLM1c1, paired =TRUE)
TTres.1d1<-t.test(YD1d1, NoRTLM1d1, paired =TRUE)
TTres.1e1<-t.test(YD1e1, NoRTLM1e1, paired =TRUE)
TTres.1f1<-t.test(YD1f1, NoRTLM1f1, paired =TRUE)
TTres.1g1<-t.test(YD1g1, NoRTLM1g1, paired =TRUE)
TTres.1h1<-t.test(YD1h1, NoRTLM1h1, paired =TRUE)
TTres.1i1<-t.test(YD1i1, NoRTLM1i1, paired =TRUE)
TTres.1j1<-t.test(YD1j1, NoRTLM1j1, paired =TRUE)

TTres.1a2<- t.test(YD1a2, NoRTLM1a2, paired =TRUE)
TTres.1b2<-t.test(YD1b2, NoRTLM1b2, paired =TRUE)
TTres.1c2<-t.test(YD1c2, NoRTLM1c2, paired =TRUE)
TTres.1d2<-t.test(YD1d2, NoRTLM1d2, paired =TRUE)
TTres.1e2<-t.test(YD1e2, NoRTLM1e2, paired =TRUE)
TTres.1f2<-t.test(YD1f2, NoRTLM1f2, paired =TRUE)
TTres.1g2<-t.test(YD1g2, NoRTLM1g2, paired =TRUE)
TTres.1h2<-t.test(YD1h2, NoRTLM1h2, paired =TRUE)
TTres.1i2<-t.test(YD1i2, NoRTLM1i2, paired =TRUE)
TTres.1j2<-t.test(YD1j2, NoRTLM1j2, paired =TRUE)

TTres.1a3<- t.test(YD1a3, NoRTLM1a3, paired =TRUE)
TTres.1b3<-t.test(YD1b3, NoRTLM1b3, paired =TRUE)
TTres.1c3<-t.test(YD1c3, NoRTLM1c3, paired =TRUE)
TTres.1d3<-t.test(YD1d3, NoRTLM1d3, paired =TRUE)
TTres.1e3<-t.test(YD1e3, NoRTLM1e3, paired =TRUE)
TTres.1f3<-t.test(YD1f3, NoRTLM1f3, paired =TRUE)
TTres.1g3<-t.test(YD1g3, NoRTLM1g3, paired =TRUE)
TTres.1h3<-t.test(YD1h3, NoRTLM1h3, paired =TRUE)
TTres.1i3<-t.test(YD1i3, NoRTLM1i3, paired =TRUE)
TTres.1j3<-t.test(YD1j3, NoRTLM1j3, paired =TRUE)

TTres.1a4<- t.test(YD1a4, NoRTLM1a4, paired =TRUE)
TTres.1b4<-t.test(YD1b4, NoRTLM1b4, paired =TRUE)
TTres.1c4<-t.test(YD1c4, NoRTLM1c4, paired =TRUE)
TTres.1d4<-t.test(YD1d4, NoRTLM1d4, paired =TRUE)
TTres.1e4<-t.test(YD1e4, NoRTLM1e4, paired =TRUE)
TTres.1f4<-t.test(YD1f4, NoRTLM1f4, paired =TRUE)
TTres.1g4<-t.test(YD1g4, NoRTLM1g4, paired =TRUE)
TTres.1h4<-t.test(YD1h4, NoRTLM1h4, paired =TRUE)
TTres.1i4<-t.test(YD1i4, NoRTLM1i4, paired =TRUE)
TTres.1j4<-t.test(YD1j4, NoRTLM1j4, paired =TRUE)

TTres.1a5<- t.test(YD1a5, NoRTLM1a5, paired =TRUE)
TTres.1b5<-t.test(YD1b5, NoRTLM1b5, paired =TRUE)
TTres.1c5<-t.test(YD1c5, NoRTLM1c5, paired =TRUE)
TTres.1d5<-t.test(YD1d5, NoRTLM1d5, paired =TRUE)
TTres.1e5<-t.test(YD1e5, NoRTLM1e5, paired =TRUE)
TTres.1f5<-t.test(YD1f5, NoRTLM1f5, paired =TRUE)
TTres.1g5<-t.test(YD1g5, NoRTLM1g5, paired =TRUE)
TTres.1h5<-t.test(YD1h5, NoRTLM1h5, paired =TRUE)
TTres.1i5<-t.test(YD1i5, NoRTLM1i5, paired =TRUE)
TTres.1j5<-t.test(YD1j5, NoRTLM1j5, paired =TRUE)

TTres.1a6<- t.test(YD1a6, NoRTLM1a6, paired =TRUE)
TTres.1b6<-t.test(YD1b6, NoRTLM1b6, paired =TRUE)
TTres.1c6<-t.test(YD1c6, NoRTLM1c6, paired =TRUE)
TTres.1d6<-t.test(YD1d6, NoRTLM1d6, paired =TRUE)
TTres.1e6<-t.test(YD1e6, NoRTLM1e6, paired =TRUE)
TTres.1f6<-t.test(YD1f6, NoRTLM1f6, paired =TRUE)
TTres.1g6<-t.test(YD1g6, NoRTLM1g6, paired =TRUE)
TTres.1h6<-t.test(YD1h6, NoRTLM1h6, paired =TRUE)
TTres.1i6<-t.test(YD1i6, NoRTLM1i6, paired =TRUE)
TTres.1j6<-t.test(YD1j6, NoRTLM1j6, paired =TRUE)

TTres.1a7<- t.test(YD1a7, NoRTLM1a7, paired =TRUE)
TTres.1b7<-t.test(YD1b7, NoRTLM1b7, paired =TRUE)
TTres.1c7<-t.test(YD1c7, NoRTLM1c7, paired =TRUE)
TTres.1d7<-t.test(YD1d7, NoRTLM1d7, paired =TRUE)
TTres.1e7<-t.test(YD1e7, NoRTLM1e7, paired =TRUE)
TTres.1f7<-t.test(YD1f7, NoRTLM1f7, paired =TRUE)
TTres.1g7<-t.test(YD1g7, NoRTLM1g7, paired =TRUE)
TTres.1h7<-t.test(YD1h7, NoRTLM1h7, paired =TRUE)
TTres.1i7<-t.test(YD1i7, NoRTLM1i7, paired =TRUE)
TTres.1j7<-t.test(YD1j7, NoRTLM1j7, paired =TRUE)

TTres.1a8<- t.test(YD1a8, NoRTLM1a8, paired =TRUE)
TTres.1b8<-t.test(YD1b8, NoRTLM1b8, paired =TRUE)
TTres.1c8<-t.test(YD1c8, NoRTLM1c8, paired =TRUE)
TTres.1d8<-t.test(YD1d8, NoRTLM1d8, paired =TRUE)
TTres.1e8<-t.test(YD1e8, NoRTLM1e8, paired =TRUE)
TTres.1f8<-t.test(YD1f8, NoRTLM1f8, paired =TRUE)
TTres.1g8<-t.test(YD1g8, NoRTLM1g8, paired =TRUE)
TTres.1h8<-t.test(YD1h8, NoRTLM1h8, paired =TRUE)
TTres.1i8<-t.test(YD1i8, NoRTLM1i8, paired =TRUE)
TTres.1j8<-t.test(YD1j8, NoRTLM1j8, paired =TRUE)

TTres.1a9<- t.test(YD1a9, NoRTLM1a9, paired =TRUE)
TTres.1b9<-t.test(YD1b9, NoRTLM1b9, paired =TRUE)
TTres.1c9<-t.test(YD1c9, NoRTLM1c9, paired =TRUE)
TTres.1d9<-t.test(YD1d9, NoRTLM1d9, paired =TRUE)
TTres.1e9<-t.test(YD1e9, NoRTLM1e9, paired =TRUE)
TTres.1f9<-t.test(YD1f9, NoRTLM1f9, paired =TRUE)
TTres.1g9<-t.test(YD1g9, NoRTLM1g9, paired =TRUE)
TTres.1h9<-t.test(YD1h9, NoRTLM1h9, paired =TRUE)
TTres.1i9<-t.test(YD1i9, NoRTLM1i9, paired =TRUE)
TTres.1j9<-t.test(YD1j9, NoRTLM1j9, paired =TRUE)

TTres.1a10<- t.test(YD1a10, NoRTLM1a10, paired =TRUE)
TTres.1b10<-t.test(YD1b10, NoRTLM1b10, paired =TRUE)
TTres.1c10<-t.test(YD1c10, NoRTLM1c10, paired =TRUE)
TTres.1d10<-t.test(YD1d10, NoRTLM1d10, paired =TRUE)
TTres.1e10<-t.test(YD1e10, NoRTLM1e10, paired =TRUE)
TTres.1f10<-t.test(YD1f10, NoRTLM1f10, paired =TRUE)
TTres.1g10<-t.test(YD1g10, NoRTLM1g10, paired =TRUE)
TTres.1h10<-t.test(YD1h10, NoRTLM1h10, paired =TRUE)
TTres.1i10<-t.test(YD1i10, NoRTLM1i10, paired =TRUE)
TTres.1j10<-t.test(YD1j10, NoRTLM1j10, paired =TRUE)


# individual violence

TTres.2a1<- t.test(YD2a1, NoRTLM2a1, paired =TRUE)
TTres.2b1<-t.test(YD2b1, NoRTLM2b1, paired =TRUE)
TTres.2c1<-t.test(YD2c1, NoRTLM2c1, paired =TRUE)
TTres.2d1<-t.test(YD2d1, NoRTLM2d1, paired =TRUE)
TTres.2e1<-t.test(YD2e1, NoRTLM2e1, paired =TRUE)
TTres.2f1<-t.test(YD2f1, NoRTLM2f1, paired =TRUE)
TTres.2g1<-t.test(YD2g1, NoRTLM2g1, paired =TRUE)
TTres.2h1<-t.test(YD2h1, NoRTLM2h1, paired =TRUE)
TTres.2i1<-t.test(YD2i1, NoRTLM2i1, paired =TRUE)
TTres.2j1<-t.test(YD2j1, NoRTLM2j1, paired =TRUE)

TTres.2a2<- t.test(YD2a2, NoRTLM2a2, paired =TRUE)
TTres.2b2<-t.test(YD2b2, NoRTLM2b2, paired =TRUE)
TTres.2c2<-t.test(YD2c2, NoRTLM2c2, paired =TRUE)
TTres.2d2<-t.test(YD2d2, NoRTLM2d2, paired =TRUE)
TTres.2e2<-t.test(YD2e2, NoRTLM2e2, paired =TRUE)
TTres.2f2<-t.test(YD2f2, NoRTLM2f2, paired =TRUE)
TTres.2g2<-t.test(YD2g2, NoRTLM2g2, paired =TRUE)
TTres.2h2<-t.test(YD2h2, NoRTLM2h2, paired =TRUE)
TTres.2i2<-t.test(YD2i2, NoRTLM2i2, paired =TRUE)
TTres.2j2<-t.test(YD2j2, NoRTLM2j2, paired =TRUE)

TTres.2a3<- t.test(YD2a3, NoRTLM2a3, paired =TRUE)
TTres.2b3<-t.test(YD2b3, NoRTLM2b3, paired =TRUE)
TTres.2c3<-t.test(YD2c3, NoRTLM2c3, paired =TRUE)
TTres.2d3<-t.test(YD2d3, NoRTLM2d3, paired =TRUE)
TTres.2e3<-t.test(YD2e3, NoRTLM2e3, paired =TRUE)
TTres.2f3<-t.test(YD2f3, NoRTLM2f3, paired =TRUE)
TTres.2g3<-t.test(YD2g3, NoRTLM2g3, paired =TRUE)
TTres.2h3<-t.test(YD2h3, NoRTLM2h3, paired =TRUE)
TTres.2i3<-t.test(YD2i3, NoRTLM2i3, paired =TRUE)
TTres.2j3<-t.test(YD2j3, NoRTLM2j3, paired =TRUE)

TTres.2a4<- t.test(YD2a4, NoRTLM2a4, paired =TRUE)
TTres.2b4<-t.test(YD2b4, NoRTLM2b4, paired =TRUE)
TTres.2c4<-t.test(YD2c4, NoRTLM2c4, paired =TRUE)
TTres.2d4<-t.test(YD2d4, NoRTLM2d4, paired =TRUE)
TTres.2e4<-t.test(YD2e4, NoRTLM2e4, paired =TRUE)
TTres.2f4<-t.test(YD2f4, NoRTLM2f4, paired =TRUE)
TTres.2g4<-t.test(YD2g4, NoRTLM2g4, paired =TRUE)
TTres.2h4<-t.test(YD2h4, NoRTLM2h4, paired =TRUE)
TTres.2i4<-t.test(YD2i4, NoRTLM2i4, paired =TRUE)
TTres.2j4<-t.test(YD2j4, NoRTLM2j4, paired =TRUE)

TTres.2a5<- t.test(YD2a5, NoRTLM2a5, paired =TRUE)
TTres.2b5<-t.test(YD2b5, NoRTLM2b5, paired =TRUE)
TTres.2c5<-t.test(YD2c5, NoRTLM2c5, paired =TRUE)
TTres.2d5<-t.test(YD2d5, NoRTLM2d5, paired =TRUE)
TTres.2e5<-t.test(YD2e5, NoRTLM2e5, paired =TRUE)
TTres.2f5<-t.test(YD2f5, NoRTLM2f5, paired =TRUE)
TTres.2g5<-t.test(YD2g5, NoRTLM2g5, paired =TRUE)
TTres.2h5<-t.test(YD2h5, NoRTLM2h5, paired =TRUE)
TTres.2i5<-t.test(YD2i5, NoRTLM2i5, paired =TRUE)
TTres.2j5<-t.test(YD2j5, NoRTLM2j5, paired =TRUE)

TTres.2a6<- t.test(YD2a6, NoRTLM2a6, paired =TRUE)
TTres.2b6<-t.test(YD2b6, NoRTLM2b6, paired =TRUE)
TTres.2c6<-t.test(YD2c6, NoRTLM2c6, paired =TRUE)
TTres.2d6<-t.test(YD2d6, NoRTLM2d6, paired =TRUE)
TTres.2e6<-t.test(YD2e6, NoRTLM2e6, paired =TRUE)
TTres.2f6<-t.test(YD2f6, NoRTLM2f6, paired =TRUE)
TTres.2g6<-t.test(YD2g6, NoRTLM2g6, paired =TRUE)
TTres.2h6<-t.test(YD2h6, NoRTLM2h6, paired =TRUE)
TTres.2i6<-t.test(YD2i6, NoRTLM2i6, paired =TRUE)
TTres.2j6<-t.test(YD2j6, NoRTLM2j6, paired =TRUE)

TTres.2a7<- t.test(YD2a7, NoRTLM2a7, paired =TRUE)
TTres.2b7<-t.test(YD2b7, NoRTLM2b7, paired =TRUE)
TTres.2c7<-t.test(YD2c7, NoRTLM2c7, paired =TRUE)
TTres.2d7<-t.test(YD2d7, NoRTLM2d7, paired =TRUE)
TTres.2e7<-t.test(YD2e7, NoRTLM2e7, paired =TRUE)
TTres.2f7<-t.test(YD2f7, NoRTLM2f7, paired =TRUE)
TTres.2g7<-t.test(YD2g7, NoRTLM2g7, paired =TRUE)
TTres.2h7<-t.test(YD2h7, NoRTLM2h7, paired =TRUE)
TTres.2i7<-t.test(YD2i7, NoRTLM2i7, paired =TRUE)
TTres.2j7<-t.test(YD2j7, NoRTLM2j7, paired =TRUE)

TTres.2a8<- t.test(YD2a8, NoRTLM2a8, paired =TRUE)
TTres.2b8<-t.test(YD2b8, NoRTLM2b8, paired =TRUE)
TTres.2c8<-t.test(YD2c8, NoRTLM2c8, paired =TRUE)
TTres.2d8<-t.test(YD2d8, NoRTLM2d8, paired =TRUE)
TTres.2e8<-t.test(YD2e8, NoRTLM2e8, paired =TRUE)
TTres.2f8<-t.test(YD2f8, NoRTLM2f8, paired =TRUE)
TTres.2g8<-t.test(YD2g8, NoRTLM2g8, paired =TRUE)
TTres.2h8<-t.test(YD2h8, NoRTLM2h8, paired =TRUE)
TTres.2i8<-t.test(YD2i8, NoRTLM2i8, paired =TRUE)
TTres.2j8<-t.test(YD2j8, NoRTLM2j8, paired =TRUE)

TTres.2a9<- t.test(YD2a9, NoRTLM2a9, paired =TRUE)
TTres.2b9<-t.test(YD2b9, NoRTLM2b9, paired =TRUE)
TTres.2c9<-t.test(YD2c9, NoRTLM2c9, paired =TRUE)
TTres.2d9<-t.test(YD2d9, NoRTLM2d9, paired =TRUE)
TTres.2e9<-t.test(YD2e9, NoRTLM2e9, paired =TRUE)
TTres.2f9<-t.test(YD2f9, NoRTLM2f9, paired =TRUE)
TTres.2g9<-t.test(YD2g9, NoRTLM2g9, paired =TRUE)
TTres.2h9<-t.test(YD2h9, NoRTLM2h9, paired =TRUE)
TTres.2i9<-t.test(YD2i9, NoRTLM2i9, paired =TRUE)
TTres.2j9<-t.test(YD2j9, NoRTLM2j9, paired =TRUE)

TTres.2a10<- t.test(YD2a10, NoRTLM2a10, paired =TRUE)
TTres.2b10<-t.test(YD2b10, NoRTLM2b10, paired =TRUE)
TTres.2c10<-t.test(YD2c10, NoRTLM2c10, paired =TRUE)
TTres.2d10<-t.test(YD2d10, NoRTLM2d10, paired =TRUE)
TTres.2e10<-t.test(YD2e10, NoRTLM2e10, paired =TRUE)
TTres.2f10<-t.test(YD2f10, NoRTLM2f10, paired =TRUE)
TTres.2g10<-t.test(YD2g10, NoRTLM2g10, paired =TRUE)
TTres.2h10<-t.test(YD2h10, NoRTLM2h10, paired =TRUE)
TTres.2i10<-t.test(YD2i10, NoRTLM2i10, paired =TRUE)
TTres.2j10<-t.test(YD2j10, NoRTLM2j10, paired =TRUE)



#calculate means

# all violence

TTpval.12 <- mean(c(TTres.12a1$p.value, TTres.12b1$p.value, TTres.12c1$p.value, TTres.12d1$p.value, TTres.12e1$p.value, TTres.12f1$p.value, TTres.12g1$p.value, TTres.12h1$p.value, TTres.12i1$p.value, TTres.12j1$p.value, TTres.12a2$p.value, TTres.12b2$p.value, TTres.12c2$p.value, TTres.12d2$p.value, TTres.12e2$p.value, TTres.12f2$p.value, TTres.12g2$p.value, TTres.12h2$p.value, TTres.12i2$p.value, TTres.12j2$p.value, TTres.12a3$p.value, TTres.12b3$p.value, TTres.12c3$p.value, TTres.12d3$p.value, TTres.12e3$p.value, TTres.12f3$p.value, TTres.12g3$p.value, TTres.12h3$p.value, TTres.12i3$p.value, TTres.12j3$p.value, TTres.12a4$p.value, TTres.12b4$p.value, TTres.12c4$p.value, TTres.12d4$p.value, TTres.12e4$p.value, TTres.12f4$p.value, TTres.12g4$p.value, TTres.12h4$p.value, TTres.12i4$p.value, TTres.12j4$p.value, TTres.12a5$p.value, TTres.12b5$p.value, TTres.12c5$p.value, TTres.12d5$p.value, TTres.12e5$p.value, TTres.12f5$p.value, TTres.12g5$p.value, TTres.12h5$p.value, TTres.12i5$p.value, TTres.12j5$p.value, TTres.12a6$p.value, TTres.12b6$p.value, TTres.12c6$p.value, TTres.12d6$p.value, TTres.12e6$p.value, TTres.12f6$p.value, TTres.12g6$p.value, TTres.12h6$p.value, TTres.12i6$p.value, TTres.12j6$p.value, TTres.12a7$p.value, TTres.12b7$p.value, TTres.12c7$p.value, TTres.12d7$p.value, TTres.12e7$p.value, TTres.12f7$p.value, TTres.12g7$p.value, TTres.12h7$p.value, TTres.12i7$p.value, TTres.12j7$p.value, TTres.12a8$p.value, TTres.12b8$p.value, TTres.12c8$p.value, TTres.12d8$p.value, TTres.12e8$p.value, TTres.12f8$p.value, TTres.12g8$p.value, TTres.12h8$p.value, TTres.12i8$p.value, TTres.12j8$p.value, TTres.12a9$p.value, TTres.12b9$p.value, TTres.12c9$p.value, TTres.12d9$p.value, TTres.12e9$p.value, TTres.12f9$p.value, TTres.12g9$p.value, TTres.12h9$p.value, TTres.12i9$p.value, TTres.12j9$p.value, TTres.12a10$p.value, TTres.12b10$p.value, TTres.12c10$p.value, TTres.12d10$p.value, TTres.12e10$p.value, TTres.12f10$p.value, TTres.12g10$p.value, TTres.12h10$p.value, TTres.12i10$p.value, TTres.12j10$p.value))


# militia violence

TTpval.1 <- mean(c(TTres.1a1$p.value, TTres.1b1$p.value, TTres.1c1$p.value, TTres.1d1$p.value, TTres.1e1$p.value, TTres.1f1$p.value, TTres.1g1$p.value, TTres.1h1$p.value, TTres.1i1$p.value, TTres.1j1$p.value, TTres.1a2$p.value, TTres.1b2$p.value, TTres.1c2$p.value, TTres.1d2$p.value, TTres.1e2$p.value, TTres.1f2$p.value, TTres.1g2$p.value, TTres.1h2$p.value, TTres.1i2$p.value, TTres.1j2$p.value, TTres.1a3$p.value, TTres.1b3$p.value, TTres.1c3$p.value, TTres.1d3$p.value, TTres.1e3$p.value, TTres.1f3$p.value, TTres.1g3$p.value, TTres.1h3$p.value, TTres.1i3$p.value, TTres.1j3$p.value, TTres.1a4$p.value, TTres.1b4$p.value, TTres.1c4$p.value, TTres.1d4$p.value, TTres.1e4$p.value, TTres.1f4$p.value, TTres.1g4$p.value, TTres.1h4$p.value, TTres.1i4$p.value, TTres.1j4$p.value, TTres.1a5$p.value, TTres.1b5$p.value, TTres.1c5$p.value, TTres.1d5$p.value, TTres.1e5$p.value, TTres.1f5$p.value, TTres.1g5$p.value, TTres.1h5$p.value, TTres.1i5$p.value, TTres.1j5$p.value, TTres.1a6$p.value, TTres.1b6$p.value, TTres.1c6$p.value, TTres.1d6$p.value, TTres.1e6$p.value, TTres.1f6$p.value, TTres.1g6$p.value, TTres.1h6$p.value, TTres.1i6$p.value, TTres.1j6$p.value, TTres.1a7$p.value, TTres.1b7$p.value, TTres.1c7$p.value, TTres.1d7$p.value, TTres.1e7$p.value, TTres.1f7$p.value, TTres.1g7$p.value, TTres.1h7$p.value, TTres.1i7$p.value, TTres.1j7$p.value, TTres.1a8$p.value, TTres.1b8$p.value, TTres.1c8$p.value, TTres.1d8$p.value, TTres.1e8$p.value, TTres.1f8$p.value, TTres.1g8$p.value, TTres.1h8$p.value, TTres.1i8$p.value, TTres.1j8$p.value, TTres.1a9$p.value, TTres.1b9$p.value, TTres.1c9$p.value, TTres.1d9$p.value, TTres.1e9$p.value, TTres.1f9$p.value, TTres.1g9$p.value, TTres.1h9$p.value, TTres.1i9$p.value, TTres.1j9$p.value, TTres.1a10$p.value, TTres.1b10$p.value, TTres.1c10$p.value, TTres.1d10$p.value, TTres.1e10$p.value, TTres.1f10$p.value, TTres.1g10$p.value, TTres.1h10$p.value, TTres.1i10$p.value, TTres.1j10$p.value))


#individual violence

TTpval.2 <- mean(c(TTres.2a1$p.value, TTres.2b1$p.value, TTres.2c1$p.value, TTres.2d1$p.value, TTres.2e1$p.value, TTres.2f1$p.value, TTres.2g1$p.value, TTres.2h1$p.value, TTres.2i1$p.value, TTres.2j1$p.value, TTres.2a2$p.value, TTres.2b2$p.value, TTres.2c2$p.value, TTres.2d2$p.value, TTres.2e2$p.value, TTres.2f2$p.value, TTres.2g2$p.value, TTres.2h2$p.value, TTres.2i2$p.value, TTres.2j2$p.value, TTres.2a3$p.value, TTres.2b3$p.value, TTres.2c3$p.value, TTres.2d3$p.value, TTres.2e3$p.value, TTres.2f3$p.value, TTres.2g3$p.value, TTres.2h3$p.value, TTres.2i3$p.value, TTres.2j3$p.value, TTres.2a4$p.value, TTres.2b4$p.value, TTres.2c4$p.value, TTres.2d4$p.value, TTres.2e4$p.value, TTres.2f4$p.value, TTres.2g4$p.value, TTres.2h4$p.value, TTres.2i4$p.value, TTres.2j4$p.value, TTres.2a5$p.value, TTres.2b5$p.value, TTres.2c5$p.value, TTres.2d5$p.value, TTres.2e5$p.value, TTres.2f5$p.value, TTres.2g5$p.value, TTres.2h5$p.value, TTres.2i5$p.value, TTres.2j5$p.value, TTres.2a6$p.value, TTres.2b6$p.value, TTres.2c6$p.value, TTres.2d6$p.value, TTres.2e6$p.value, TTres.2f6$p.value, TTres.2g6$p.value, TTres.2h6$p.value, TTres.2i6$p.value, TTres.2j6$p.value, TTres.2a7$p.value, TTres.2b7$p.value, TTres.2c7$p.value, TTres.2d7$p.value, TTres.2e7$p.value, TTres.2f7$p.value, TTres.2g7$p.value, TTres.2h7$p.value, TTres.2i7$p.value, TTres.2j7$p.value, TTres.2a8$p.value, TTres.2b8$p.value, TTres.2c8$p.value, TTres.2d8$p.value, TTres.2e8$p.value, TTres.2f8$p.value, TTres.2g8$p.value, TTres.2h8$p.value, TTres.2i8$p.value, TTres.2j8$p.value, TTres.2a9$p.value, TTres.2b9$p.value, TTres.2c9$p.value, TTres.2d9$p.value, TTres.2e9$p.value, TTres.2f9$p.value, TTres.2g9$p.value, TTres.2h9$p.value, TTres.2i9$p.value, TTres.2j9$p.value, TTres.2a10$p.value, TTres.2b10$p.value, TTres.2c10$p.value, TTres.2d10$p.value, TTres.2e10$p.value, TTres.2f10$p.value, TTres.2g10$p.value, TTres.2h10$p.value, TTres.2i10$p.value, TTres.2j10$p.value)) 


# means of t-test results #

TTpval.12
TTpval.1
TTpval.2

#  means of results of contROC meaaure # 

colMeans(Rw12ALL)
colMeans(Rw1ALL)
colMeans(Rw2ALL)




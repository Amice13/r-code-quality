quick_barplot <- function(dat_var, main_title="", relative=T, mai_setting=c(1.9, 0.82, 0.32, 0.42), ...) {
  par(mai=mai_setting)
  if(relative==T) {
    p <- barplot(round(table(dat_var,  useNA = c("ifany"))/sum(table(dat_var,  useNA = c("ifany"))),4)*100, 
                 main=main_title, las=3, border=F, beside=T, ylab="Percentage", ...)
    text(p, 3, round(table(dat_var,  useNA = c("ifany"))/sum(table(dat_var,  useNA = c("ifany"))),4)*100)
  } else {
    p <- barplot(table(dat_var,  useNA = c("ifany")), 
                 main=main_title, las=3, border=F, beside=T, ylab="Abs. frequency", ...)
    text(p, 50, table(dat_var,  useNA = c("ifany")))
  }
}
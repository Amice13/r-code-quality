package <- function(package, dependencies=TRUE, ...){
  
  package <- as.character(substitute(package))
  if (!(package %in% .packages(all.available=TRUE)))
    install.packages(package, dependencies=dependencies)
  library(package, character.only=TRUE, ...)     }


package(hesim)
package(mnormt)
package(compositions)
package(data.table)
package(stringr)

clipboard<- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=col.names,...)
}



############################################## 


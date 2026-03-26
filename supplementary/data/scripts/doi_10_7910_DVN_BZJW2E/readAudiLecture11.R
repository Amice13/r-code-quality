AudiA4Raw <- scan("rawCars.txt", what = "a", sep = "\n" )

theTag <- substr(AudiA4Raw,6,12 )
CarsIndexStart <- which(theTag == "Audi A4")
#CarsIndexStart<- grep("[0-9][0-9][0-9][0-9] Audi", AudiA4Raw)
CarsIndexEnd <- CarsIndexStart[2:length(CarsIndexStart)] - 1
# add last line
CarsIndexEnd <- c(CarsIndexEnd, length( AudiA4Raw) )

#  156 cars found
length( CarsIndexStart)

# So for example CarsIndexStart[10] CarsIndexEnd10] would 
# be the lines for the 10th car
# -- maybe barring strangeness in the records!

#   list all first lines of records  AudiA4Raw[ CarsIndexStart]
#   check for strange records 
table(CarsIndexEnd -  CarsIndexStart )
# note that the number of lines in the record varies for cars 
# but wow! one big one
 which(CarsIndexEnd -  CarsIndexStart > 200)
# this is the last car -- just lots of garbage after the cars record.
 
# now we can process these data car by car  

nCars <- length(CarsIndexStart)
# make space to hold each variable for each car 

check <- year <- price <- mileage <- distance <- rep( NA, nCars)

for(k in 1:nCars){
	 start <- CarsIndexStart[k]
	 end   <- CarsIndexEnd[k]
	 cat("Car:", k, start, end - start , fill=TRUE )
	 
	 work<- AudiA4Raw[start:end]
# here we use the scan function but just scan the first line of the 
# car record -- not from a file instead we give it the 
#  line we have already read in	 
	 temp <- scan(text = work[1], what = "a", sep = " ", quiet = TRUE)
	 year[k]  <- temp[1]
	 check[k] <- temp[3] # better be "A4" !
	 
# price
	 ind <- grep("$", work, fixed = TRUE) 
	 temp <- scan(text = work[ind], what = "a", sep=" ", quiet = TRUE)
	 price[k] <- temp[1]	
	 
# mileage
	ind3 <- grep( 'mi.', work, fixed = TRUE)
	guessLine <- ind3[1]
	temp <- scan(text = work[guessLine], what = "a", sep = " ", quiet = TRUE)
	mileage[k] <- temp[1]
	
#	distance
	ind <- grep('80305', work) 
	if(length(ind) != 0){
	  temp <- scan(text = work[ind], what= "a", quiet = TRUE)
	  ind2 <- grep("mi.", temp, fixed = TRUE)
	  distance[k] <- temp[ind2-1]
	}
}

# sanity check on finding each cars record
if(any(check !="A4" )){
  stop("strange first line: not an A4")
}

# build data frame

distance <- as.numeric(distance)
year <- as.numeric(year)

# remove the $ and the comma in the prices
#  "" means nothing so replace with nothing ...
price <-as.numeric(gsub('[$,]', '', price) )
# remove the  comma 
mileage <- as.numeric(gsub(",", "", mileage))

AudiA4 <- data.frame(year, price, mileage, distance)
#AudiA4<- na.omit( AudiA4)

save(AudiA4, file="AudiA4.rda")

library( fields)

plot( AudiA4$price,
      AudiA4$mileage,
      col=color.scale(AudiA4$year),
      pch=16, cex=2 )
points(AudiA4$price,
       AudiA4$mileage, cex=2, col="grey20" )




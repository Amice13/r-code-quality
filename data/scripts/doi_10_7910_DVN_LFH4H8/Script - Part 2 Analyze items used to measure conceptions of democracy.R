

# Script for Conceptualizing and Measuring Citizens' Preferences for Democracy - Taking Stock of Three Decades of Research in a Fragmented Field

# Part 2: Inspect items used in the reviewed studies to measure conceptions of democracy

# Version: 08 November 2021


#### read data ####

library(readxl)

df <- read_excel("List of items for measuring conceptions of democracy.xlsx")
df.full <- data.frame(df)



#### Total number of items with unique ID ####

length( unique(df.full$ID) )

table(df.full$Construct...dimension) # by construct




#### Items by construct ####

# Liberal democracy
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[3] ,  ] 

length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# Direct democracy
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[5] ,  ]  

length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# Substantive democracy
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[9] ,  ] 

length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# Populist democracy
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[2] ,  ] 

length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# Stealth democracy
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[1] ,  ]  

length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# Authoritarian democracy
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[7] ,  ] 
 
length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# None predefined
tmp <- df.full[df.full$Construct...dimension == unique(df.full$Construct...dimension)[10] ,  ]  

length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )


# Bipolar items
tmp <- df.full[df.full$Construct...dimension %in% unique(df.full$Construct...dimension)[c(4,6,8,11)] ,  ]  


length( unique(tmp$ID) ) # unique IDs

tmp.frame <- data.frame(  table(tmp$ID)   )
head(tmp.frame[order(-tmp.frame$Freq) , ], 10  )






#### Identify items with overlap ####

itemvec <- vector()

for (i in 1:max(df.full$ID)) {

tmp <- df.full[df.full$ID == i & 
                 df.full$Construct...dimension != "None predefined / Only inductively related to dimensions" , ] 

if ( length(  unique(tmp$Construct...dimension)  ) > 1 ) itemvec <- rbind(itemvec, i)

}

itemvec

######################################################################
## A series of nested functions to clean texts and convert to TFM   ##
## ---------------------------------------------------------------- ##
## ADW: Presidents, Policy Compromise and Legislative Success. JOP. ##
## June 2016                                                        ##
## Implemented in R 3.2.3 "Wooden Christmas-Tree"                   ##
######################################################################



## ================================================= ##
## 1 Make lists without the accents in the stopwords ##
## ================================================= ##


# Make a list with the replaced stopwords
sww.sp <- sort(stopwords(kind='spanish'))

# get these also out of the system
swwout.sp <- gsub('ñ', 'n',sww.sp)                          # gets rid of ñ
swwout.sp <- gsub('ã', 'a',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('õ', 'o',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('á', 'a',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('é', 'e',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('í', 'i',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('ó', 'o',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('ú', 'u',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('â', 'a',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('ê', 'e',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('î', 'i',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('ô', 'o',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('û', 'u',swwout.sp)                       # gets rid of special characters
swwout.sp <- gsub('ç', 'c',swwout.sp)                       # gets rid of special characters			

numberstems.sp <- c('diez','veinte','treint','quarent','cincuent','sesent','setent','ochent','novent','cient','doscient','trescient','cuatrocient','cincocient','sescient','setecient','ochocient','novecient', 'mil', 'million')



# Make a list with the replaced stopwords
sww.po <- sort(stopwords(kind='portuguese'))

# get these also out of the system
swwout.po <- gsub('ñ', 'n',sww.po)                       # gets rid of ñ
swwout.po <- gsub('ã', 'a',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('õ', 'o',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('á', 'a',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('é', 'e',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('í', 'i',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('ó', 'o',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('ú', 'u',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('â', 'a',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('ê', 'e',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('î', 'i',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('ô', 'o',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('û', 'u',swwout.po)                       # gets rid of special characters
swwout.po <- gsub('ç', 'c',swwout.po)                       # gets rid of special characters			

numberstems.po <- c('dez','vinte','trint','quarent','cinquent','sessent','setent','oitent','novent','cem','duzent','trezent','quatrocent','quinhent','seiscent','setecent','oitocent','novecent', 'mil', 'milhao', 'milho','bilho')



## =============================================================================== ##
## 2 Function which reads in the texts, cleans the speeches and writes out results ##
## =============================================================================== ##
corp.maker <- function(textpath, language = 'spanish'){
	# Filter to check for the correct entry of the language. Gives error if strings for language are not correct (below)
	if (is.element(language, c('portuguese','spanish'))){
		# First statement is to executive function for all spanish texts
		if (language == 'spanish') {
			## 1 Read in stuff
			sourcefile <-textpath
			# Makes Text Corpus
			corp <- Corpus(
				DirSource(sourcefile, recursive = TRUE)
				  ,readerControl = list(
				    reader = readPlain
				    ,language = "spanish"
				  )
			)
			names.corp <- names(corp)
			## 2 get corpus in shape
			for (i in 1:(length(corp))){
			    corp[[i]] <- iconv(corp[[i]], to = 'utf-8', sub="")                 # just for safety reasons...
			}
			# remove numbers
			corp <- tm_map(corp, removeNumbers)
			# remove punctuation
			corp<-tm_map(corp,removePunctuation)
			# remove all breaks and stuff
			corp<-tm_map(corp,PlainTextDocument)
			# strip whitespace
			corp<-tm_map(corp,stripWhitespace)			
			#
			for (i in 1:(length(corp))){
			    corp[[i]] <- PlainTextDocument(gsub('ñ', 'n',corp[[i]]))                       # gets rid of ñ
			    corp[[i]] <- PlainTextDocument(gsub('ã', 'a',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('õ', 'o',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('á', 'a',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('é', 'e',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('í', 'i',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('ó', 'o',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('ú', 'u',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('â', 'a',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('ê', 'e',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('î', 'i',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('ô', 'o',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('û', 'u',corp[[i]]))                       # gets rid of special characters
			    corp[[i]] <- PlainTextDocument(gsub('ç', 'c',corp[[i]]))                       # gets rid of special characters			
			    corp[[i]] <- PlainTextDocument(gsub('aplausos', ' ',corp[[i]]))                       # gets rid of aplause
			    corp[[i]] <- PlainTextDocument(gsub('aplauso', ' ',corp[[i]]))                       # gets rid of aplause			
				# page numbers until 30 in roman
				corp[[i]] <- removeWords(corp[[i]], c("i","ii","iii","iv","v","vii","viii","ix","x","xi","xii","xiii","xiv","xv","xvi","xvii","xviii","xix","xx","xxi","xxii","xxiii","xxiv","xxv","xxvi","xxvii","xxviii","xxix","xxx", "xxxiv"))
				corp[[i]] <- removeWords(corp[[i]], stopwords('spanish'))
				corp[[i]] <- removeWords(corp[[i]], swwout.sp)				# removes the stopwords without the accents
				corp[[i]] <- stemDocument(corp[[i]], language = 'spanish')	# This line takes account of the accents on the words
				corp[[i]] <- removeWords(corp[[i]], numberstems.sp)				# removes the stopwords without the accents								
			}
			# to lowercase
			#corp <- tm_map(corp, tolower)		
			corp <- tm_map(corp, content_transformer(tolower))				
			names(corp) <- names.corp
			## Now: get the frequency matrices 
			# create TermDocMAtrix
			corp_wordfreqTM <-suppressWarnings(TermDocumentMatrix(corp))
			return(corp)
		} else {
		# Second case is for the portuguese version	
			## 1 Read in stuff
			sourcefile <- textpath
			# Makes Text Corpus
			corp <- Corpus(
				DirSource(sourcefile, recursive = TRUE)
				  ,readerControl = list(
				    reader = readPlain
				    ,language = "portuguese"
				  )
			)
			names.corp <- names(corp)			
			## 2 get corpus in shape
			for (i in 1:(length(corp))){
			    corp[[i]] <- iconv(corp[[i]], to = 'utf-8', sub="")                 # just for safety reasons...
			}			
			# remove numbers
			corp <- tm_map(corp, removeNumbers)
			# remove punctuation
			corp<-tm_map(corp,removePunctuation)
			# remove all breaks and stuff
			corp<-tm_map(corp,PlainTextDocument)
			# strip whitespace
			corp<-tm_map(corp,stripWhitespace)
			#
			for (i in 1:(length(corp))){
			    corp[[i]] <- PlainTextDocument(gsub('ã', 'a',corp[[i]]))                     # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('õ', 'o',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('á', 'a',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('é', 'e',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('í', 'i',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('ó', 'o',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('ú', 'u',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('â', 'a',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('ê', 'e',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('î', 'i',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('ô', 'o',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('û', 'u',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('ç', 'c',corp[[i]]))                       # gets rid of special characters
  		    	corp[[i]] <- PlainTextDocument(gsub('aplausos', ' ',corp[[i]]))                # gets rid of applause in transcripts
  		    	corp[[i]] <- PlainTextDocument(gsub('aplauso', ' ',corp[[i]]))               # gets rid of applause
  			# page numbers until 30 in roman
				corp[[i]] <- removeWords(corp[[i]], c("i","ii","iii","iv","v","vii","viii","ix","x","xi","xii","xiii","xiv","xv","xvi","xvii","xviii","xix","xx","xxi","xxii","xxiii","xxiv","xxv","xxvi","xxvii","xxviii","xxix","xxx"))	
				corp[[i]] <- removeWords(corp[[i]], stopwords('portuguese'))
				corp[[i]] <- removeWords(corp[[i]], swwout.po)				# removes the stopwords without the accents				
				corp[[i]] <- stemDocument(corp[[i]], language = 'portuguese')	
				corp[[i]] <- removeWords(corp[[i]], numberstems.po)				# removes the stopwords without the accents												
			}
			#corp <- tm_map(corp, tolower)		
			corp <- tm_map(corp, content_transformer(tolower))				
			names(corp) <- names.corp			
			## Now: get the frequency matrices 
			# create TermDocMAtrix
			return(corp)
		}
	# This is the error message if the string values for the language are wrong
	} else {
		cat("Please insert the correct string values for the language (either 'spanish' or 'portuguese')")
	}
}


#Takes the corpus and makes a tm package term document matrix
tdm.maker <- function(textpath, language){
	corp <- corp.maker(textpath, language)
	corp_wordfreqTM <-suppressWarnings(TermDocumentMatrix(corp))
	return(corp_wordfreqTM)
}

# takes a tm package term document matrix and makes a word frequenct data matrix
wfm.maker <- function(textpath,wfmfile, language = 'spanish'){
		# This makes a nice term document matrix from the tm package
			tdm <- tdm.maker(textpath, language)
		# Take tdm matrix
			corp_wordfreq <- as.matrix(tdm)
		# Get the colnames (not elegant...)
			corp <- Corpus(
				DirSource(textpath, recursive = TRUE)
				  ,readerControl = list(
				    reader = readPlain
				    ,language = language
				  )
			)
			colnames(corp_wordfreq) <- names(corp[])
		# Write results
			cat(paste("I just finished",wfmfile,"\n"))
			write.csv(corp_wordfreq, wfmfile)	
}
    

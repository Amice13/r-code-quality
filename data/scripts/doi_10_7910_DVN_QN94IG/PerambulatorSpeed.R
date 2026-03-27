#' \code{FilterAllCols} applies the provided filter function to a matrix with
#' three columns.
#'
#' @param m matrix with three columns
#' @param f filter to use
#'
#' @return a matrix with three columns containing the filtered data of the
#'         data.frame provided
#'
#' @examples
#' \dontrun{
#'  r <- Rawdata("dummy.bin")
#'  hf <- CreateFilter("human")
#'  filtered <- FilterAllCols(r[, 1:3],hf)
#' }
FilterAllCols <- function(m, f) {
  return(cbind(signal::filtfilt(f,m[, 1]),
               signal::filtfilt(f,m[, 2]),
               signal::filtfilt(f,m[, 3])))
}

#' Creates a predefined actibelt filter
#'
#' @return the created filter
CreatePerambulatorFilter <- function(){
		# use a Chebyshev filter, as we are interessted only in the frequency and not in amplitude
		return(signal::cheby1(signal::cheb1ord(c(0.0055, 0.05), c(0.003, 0.1), 1.2, 14)))
}

#' PolePeaks
#'
#' Get the peaks of all rotating poles
#'
#' @param raw acceleration data
#' @param raw.filter filter to be applied to the raw signal before peak detection
#' @param nups findpeaks param
#' @param minpeakheight findpeaks param
#' @param minpeakdistance findpeaks param
#' @param threshold findpeaks param
#'
#' @return a data frame with three columns: the index of the peak, the type of pole(x,-x,y,-y) and the status (status indicates how many of the neighbouring peaks are in the expected ordering)
#' @export
#'
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' speed <- PolesToSpeed(FilterPoles(PolePeaks(raw)))
#' }
PolePeaks <-
	function(raw,
		raw.filter = CreatePerambulatorFilter(),
		nups = 25,
		minpeakheight = 120,
		minpeakdistance = 30,
		threshold = 30) {
		# filter with band pass
		f <- FilterAllCols(raw, raw.filter)
		actibeltpeaks <- function(data) {
			pracma::findpeaks(
				data,
				nups = nups,
				minpeakheight	= minpeakheight,
				minpeakdistance = minpeakdistance,
				threshold = threshold
			)[, 2]
		}
		# detect poles for min and max on x and y
		peaks <- list()
		peaks[[1]] <-	actibeltpeaks(f[, 1])
		peaks[[2]] <-	actibeltpeaks(-f[, 1])
		peaks[[3]] <-	actibeltpeaks(f[, 2])
		peaks[[4]] <- actibeltpeaks(-f[, 2])

		df <- data.frame(peaks = peaks[[1]], type = 1)
		df <- rbind(df, data.frame(peaks = peaks[[2]], type = 3))
		df <- rbind(df, data.frame(peaks = peaks[[3]], type = 4))
		df <- rbind(df, data.frame(peaks = peaks[[4]], type = 2))

		df <- df %>% dplyr::arrange(peaks)

		df
	}

#' Filter and prepare detected peaks
#'
#' @param poles detected peak poles
#' @param inter.threshold max delta between poles
#' @param intra.threshold max delta between poles of same type
#'
#' @return a data.frame containing the filtered poles with context information per peak
#' @export
#'
FilterPoles <- function(poles, inter.threshold = 120, intra.threshold = 360) {

	# calculate the "status", indicating if the peak is in sync with its neighbours
	poles <- poles %>%
		dplyr::mutate(inter = c(diff(peaks),0)) %>%
		dplyr::group_by(type) %>%
		dplyr::mutate(intra = c(diff(peaks),0)) %>%
		dplyr::ungroup()

	intra <- poles$intra[1:(nrow(poles)-1)]
	inter <- poles$inter[1:(nrow(poles)-1)]

	cw.neighbour.count <- ValidNeighbourCount(poles$type, inter, inter.threshold, T)
	ccw.neighbour.count <- ValidNeighbourCount(poles$type, inter, inter.threshold, F)

	neighbour.count <- if (sum(cw.neighbour.count) > sum(ccw.neighbour.count)) {
		cw.neighbour.count
	} else {
		ccw.neighbour.count
	}

	poles <- poles %>%
		dplyr::mutate(neighbours = neighbour.count) %>%
		dplyr::mutate(intra = ifelse(intra > intra.threshold & inter < inter.threshold, inter * 4, intra)) %>%
		dplyr::filter(intra < intra.threshold & neighbours > 0)


	poles
}

#' Calculate valid neighbour count per peak
#'
#' Assigns the number of valid neighbour per peak. Valid neighbour means that the preceding
#' or succeeding peak follows the rotation ordering and is within the specified of threshold
#'
#'
#' @param peak.types vector of peak types
#' @param inter vector of durations between peaks
#' @param inter.threshold threshold below inter neighbours counts as valid
#' @param clockwise logical T if clockwise F if counter clockwise should be calculated
#' @param threshold number i
#'
#' @return vector of length of peak.types. Each value corresponds to the number of
#'    accepted neighbours (0,1,2)
#'
ValidNeighbourCount <- function(peak.types, inter, inter.threshold = 120, clockwise = T){

	stopifnot(length(peak.types) == (length(inter) + 1) )

	d <- diff(peak.types)
	neighbours = if (clockwise){
		 (d == 1 | d == -3) & (inter < inter.threshold)
	} else {
		 (d == -1 | d == 3) & (inter < inter.threshold)
	}
	c(neighbours, utils::tail(neighbours, 1)) + c(utils::head(neighbours, 1), neighbours)

}


#' Calculate speed vector
#'
#' @param poles data frame as created by FilterPoles
#'
#' @return a vector of length x, where x is the last calculated speed value != 0
#' @export
#'
#' @examples
#' \dontrun{
#' speed <- PolesToSpeed(FilterPoles(PolePeaks(raw)))
#' }
PolesToSpeed <- function(poles) {
	speed <- rep(0, max(poles$peaks + poles$intra))

	# for each of the 4 different poles determine the speed, and add to overall speed
	for (i in 1:nrow(poles)) {
		duration <- poles[i, ]$intra
		r <- poles[i, ]$peaks:(poles[i, ]$peaks + duration - 1 )
		speed[r] <- speed[r] + 0.25 / (duration * 0.01)
	}

	speed
}

#' ContiguousDistances
#'
#' Groups contiguous speed samples into distances.
#'
#' @param speed vector of speed samples
#'
#' @return dataframe of start, end and distance for each contiguous group
#' @export
#'
ContiguousDistances <- function(speed) {
	speed.rle <- rle(speed > 0)
	speed.groups <- data.frame()
	for (i in 2:length(speed.rle$values)) {
		if (speed.rle$values[i]) {
			start <- sum(speed.rle$lengths[1:i - 1])
			end <- start + speed.rle$lengths[i]
			speed.groups <-
				rbind(speed.groups, c(start, end, sum(speed[start:end]) / 100))
		}
	}
	names(speed.groups) <- c("Start", "End", "Distance")
	speed.groups
}





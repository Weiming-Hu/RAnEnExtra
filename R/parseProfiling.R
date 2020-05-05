# "`-''-/").___..--''"`-._
#  (`6_ 6  )   `-.  (     ).`-.__.`)   WE ARE ...
#  (_Y_.)'  ._   )  `._ `. ``-..-'    PENN STATE!
#    _ ..`--'_..-_/  /--'_.' ,'
#  (il),-''  (li),'  ((!.-'
#
# Author: Weiming Hu <weiming@psu.edu>
#         Geoinformatics and Earth Observation Laboratory (http://geolab.psu.edu)
#         Department of Geography and Institute for CyberScience
#         The Pennsylvania State University
#

#' RAnEnExtra::parseProfiling
#'
#' RAnEnExtra::parseProfiling is used to parse standard output messages generated
#' from the commandline utilities, e.g. `anen_grib` and `anen_grib_mpi`
#' ([What are these](https://github.com/Weiming-Hu/AnalogsEnsemble/tree/master/apps)? 
#' [How to install these](https://weiming-hu.github.io/AnalogsEnsemble/doc#canen)?).
#'
#' @author Weiming Hu \email{weiming@@psu.edu}
#'
#' @param file The file to parse
#' @param first_up Convert the first letter in the name column to uppercase.
#' @return A data frame with parsed messages as columns
#' 
#' @md
#' @export
parseProfiling <- function(file, first_up = T) {
	
	stopifnot(requireNamespace('stringr', quietly = T))
	stopifnot(requireNamespace('chron', quietly = T))
	
	# Check whether file exists
	stopifnot(file.exists(file))
	
	# Read all lines from the file
	file <- file(file, open = 'r')
	lines <- readLines(con = file)
	close(file)
	
	# Anchor the start and the end lines of the profiling information
	pattern_start <- '\\* +Profiler Summary +\\*'
	pattern_end <- '\\* +End of Profiler Summary +\\*'
	
	line_start <- grep(pattern = pattern_start, x = lines)
	line_end <- grep(pattern = pattern_end, x = lines)
	
	if (length(line_start) != 1 || length(line_end) != 1) {
		msg <- paste0('None or multiple anchor points have been found using\n',
									pattern_start, ' for the start and ',
									pattern_end, ' for the end.\n',
									'Please check your log file or file an issue report.')
		stop(msg)
	}
	
	lines <- lines[(line_start + 1):(line_end - 1)]
	
	names <- stringr::str_match(pattern = '^ *(\\w+.*?):.*', string = lines)
	cpu_time <- stringr::str_match(pattern = '.*processor time \\((.*?),.*', string = lines)
	cpu_time_ratio <- stringr::str_match(pattern = '.*processor time \\(.*?, +(.*?)%\\).*', string = lines)
	wall_time <- stringr::str_match(pattern = '.*wall time \\((.*?),.*', string = lines)
	wall_time_ratio <- stringr::str_match(pattern = '.*wall time \\(.*?, +(.*?)%\\).*', string = lines)
	peak_heap <- stringr::str_match(pattern = '.*peak memory \\((.*?) bytes.*', string = lines)
	
	df <- data.frame(
		name = factor(1:nrow(names), labels = names[, 2]),
		cpu_time = chron::chron(times. = cpu_time[, 2]),
		cpu_time_ratio = as.numeric(cpu_time_ratio[, 2]),
		wall_time = chron::chron(times. = wall_time[, 2]),
		wall_time_ratio = as.numeric(wall_time_ratio[, 2]),
		peak_heap = as.numeric(peak_heap[, 2]))
	
	if (first_up) {
		df <- stringr::str_to_title(df$name)
	}
	
	return(df)
}
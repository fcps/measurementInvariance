library(magrittr)
library(doMC)
doMC::registerDoMC(cores = 8)

# These are the school year values available
years <- c(1993:2019)

# Scrape all of the data from 1992-1993 to 2018-2019 seasons
overallStats <- plyr::llply(years, .parallel = TRUE, .fun = function(x) { 
	
	# Column headers for the table that gets scraped
	headers <- c("rank", "schnm", "games", "wins", "losses", "wlpct", 
			 "srs", "sos", "confwins", "conflosses", 
			 "homewins", "homelosses", "roadwins", "roadlosses", 
			 "totpts", "totptsagainst", "filler", "minutes", "fieldgoals",
			 "goalattempts", "goalpct", "tpgoals", "tpattempts", "tppct",
			 "ft", "ftattempts", "ftpct", "offrebounds", "totrebounds",
			 "assists", "steals", "blocks", "turnovers", "fouls")

	# Creates an academic year string
	schoolYear <- paste(x - 1, x, sep = "-")
	
	# Generate URL
	url <- paste0("https://www.sports-reference.com/cbb/seasons/", 
	   				x, "-school-stats.html")	
	# Get the website HTML
	table <- xml2::read_html(url) %>% rvest::html_nodes("#basic_school_stats")

	# Identify column headers that appear in the body of the table for removal
	toRemoveInline <- table %>% rvest::html_nodes(".thead")
	
	# Remove the multispan column headers at the top of the table
	toRemove <- table %>% rvest::html_nodes(".over_header")
	
	# Removes the header rows identified in the lines above
	xml2::xml_remove(toRemoveInline); xml2::xml_remove(toRemove)
	
	# Get the table of data
	data <- rvest::html_table(table, fill = TRUE)[[1]] 
	
	# Rename the columns
	names(data) <- headers
	
	# Adds a column with the school year to the data frame
	data$schyr <- schoolYear
	
	# Adds a column to indicate if the school made it to the NCAA tournament
	data %<>% dplyr::mutate(tourney = grepl("NCAA", data$schnm))
	
	# Now remove the NCAA value from the school name and any trailing white
	# spaces left behind
	data$schnm <- stringr::str_replace(data$schnm, "NCAA", "") %>% 
				  stringr::str_replace("\\s+$", "")
	
	# Returns the data frame without the row number values
	return(data[, -1])
	
}) %>% dplyr::bind_rows()

# Scrapes all of the KY basketball schedules for the same period
kySchedules <- plyr::llply(years, .parallel = TRUE, .fun = function(x) { 
	
	# Column headers for the table that gets scraped
	headers <- c("g", "gamedate", "gametime", "type", "locationtype", "opponent", 
				 "conference", "outcome", "points", "pointsagainst",
				 "ot", "wins", "losses", "streak", "arena")
	# Creates an academic year string
	schoolYear <- paste(x - 1, x, sep = "-")
	
	# Generate URL
	url <- paste0("https://www.sports-reference.com/cbb/schools/kentucky/", 
	   				x, "-schedule.html")
	
	# Get the website HTML
	table <- xml2::read_html(url) %>% rvest::html_nodes("#schedule")

	# Identify column headers that appear in the body of the table for removal
	toRemoveInline <- table %>% rvest::html_nodes(".thead")
	
	# Removes the header rows identified in the lines above
	xml2::xml_remove(toRemoveInline)
	
	# Get the table of data
	data <- rvest::html_table(table, fill = TRUE)[[1]] 
	
	# Test and adjust data frame for the variation in the table structures
	if (length(names(data)) == 14) data %<>% tibble::add_column(time = "", .after = "Date")
			
	# Rename the columns
	names(data) <- headers
	
	# Adds a column with the school year to the data frame
	data$schyr <- schoolYear; data$schnm <- "Kentucky"
	
	# Adds a column to indicate if the school made it to the NCAA tournament
	data %<>% dplyr::mutate(ranked = grepl("\\(\\d+\\)", data$opponent))
	
	# Now remove the NCAA value from the school name and any trailing white
	# spaces left behind
	data$opponent <- stringr::str_replace(data$opponent, "\\(\\d+\\)", "") %>% 
				  stringr::str_replace("\\s+$", "")
	
	# Returns the data frame without the row number values
	return(data[, -1])
	
}) %>% dplyr::bind_rows()

# the kySchedules data frame could be used to create aggregate statistics about 
# the team's opponents.  Feel free to modify line 96 to put the numeric ranking
# in the column instead of a boolean if you'd like.  

# Data frame to use for examples related to Beau's email
data <- dplyr::filter(overallStats, schyr == "1995-1996" | schyr == "2011-2012")


################################################################################################################
# Invoking the pipebind operator
Sys.setenv("_R_USE_PIPEBIND_" = "true") # Invoking the pipebind operator

# Findaing Percentage of missing Values ####
# Percentage of Missing Values for each year conditioned on Met Stations ####
`NA_%`  <- function(vec = "") {
	((as.character(is.na(vec)) %>% 
	  	.[. == "TRUE"] %>% 
	  	length * 100) / length(vec)) %>% 
		round(digits = 2) %>% 
		paste0("%")
}

# Building a function which calls `NA_%` to loop over the vector elements of a list
`NA_%_1` <- function(list = "") lapply(list, `NA_%`) |> . => do.call(rbind, .)





# Summary Statistics for all Indices ####
# Function accepts a list
# Function to create Summary Statistics for climatic index ####
summary_stats <- function(data = "", Station = "", var = ""){
	# Looping an ananymous function of summary Statistics of the period, mean, max, min and sd of a station
	lapply(
		Station, # names of elements of the list 
		\(vec = ""){
			data.frame(
				Met_Station = vec,
				# Stations in this ananymous function is the daily data for the station
				Period = format(range(Stations[[vec]][ ,"Date"]), format = "%Y") |> . =>
					paste(.[1], .[2], sep = "-"),
				mean = mean(data[[vec]][ ,var], na.rm = TRUE),
				min = min(data[[vec]][ ,var], na.rm = TRUE),
				max = max(data[[vec]][ ,var], na.rm = TRUE),
				sd = sd(data[[vec]][ ,var], na.rm = TRUE)
			)
		}
	)
	
}





# Longest Dry Spell Per year for All Stations ####
# Run length encoding -- function to run length of zeroes in a vector
runs <- function(vec = ""){
	rle(vec) |> . => 
		data.frame(v = .$value, l = .$lengths) %>% 
		.[.$v == 0.0, ] %>% 
		.[.$l == max(.$l), ]
}

# Building a function which calls runs to loop over the vector elements of a list
runs_1 <- function(list = "") lapply(list, runs) |> . => do.call(rbind, .)







# Consecutive wet days of the season ####
# Run length encoding -- function to run length of zeroes in a vector
runs_wet <- function(vec = ""){
	ifelse(vec < 0.85, FALSE, TRUE) |>
		rle() |> . => 
		data.frame(v = .$value, l = .$lengths) |> . =>
		.[!.[ "v"] == FALSE, ] |>  . =>
		.[.[, "l"] == max(.[ ,"l"]), ]
}

# Building a function which calls runs to loop over the vector elements of a list
runs_1_wet <- function(list = "") lapply(list, runs_wet) |> . => do.call(rbind, .)













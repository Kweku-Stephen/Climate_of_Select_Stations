#############################################################################################################
# Sorucing Functions Script ####
source("Functions Script.R")

# Creating Directories ####
for ( i in c("Data_outputs", "Plots_outputs", "Outputs")){
	if (!dir.exists(i)) dir.create(i) else message("Already Created")
}

# loading magrittr 
require(magrittr)

# Introducing the pipebind operator
Sys.setenv("_R_USE_PIPEBIND_" = "true") # Invoking the pipebind operator






# DATA IMPORT AND LIL RESHAPING ####
# Importing all three Stations
dir(path = "~/Climate_of_Select_Stations/Rainfall", pattern = ".csv$", full.names = TRUE) |> # Reading in all datasets with names "Koforidu.csv, Kintampo.csv, navrong.csv"
	lapply(
		read.csv, # Reading in output of "dir"
		na.strings = -99.9
	) |> 
	lapply(
		# Looping the function below on each element of the list returned from above
		# to create a date object with columns 1, 2 and 3 of each dataset
		\(data = "") {
			data.frame(
				Date = as.Date(paste(data[ ,1], data[ ,2], data[ ,3], sep = "-"), format = "%Y-%m-%d"),
				Rain = data[ ,"Rain"]
			) |> . =>
				.[.[ ,"Date"] >= "1980-01-01", ]
		}
	) |> 
	setNames(c("Kintampo", "Koforidua", "Navrongo")) -> Stations # Renaming the list/output above

# for efficiency, use rio::import_list instead of combining "lapply" and "read.csv" to read the datasets into R

# Changing class of Koforidua Rain variable to "numeric"
#Stations[["Koforidua"]][ ,"Rain"] <- as.numeric(Stations[["Koforidua"]][ ,"Rain"])



# calling `NA_%_1` to loop over the elements of the list "Stations"
# Stations is a two level nested list
lapply(
	Stations,
	\(data = "") split(data[ ,"Rain"], as.factor(format(data[ ,"Date"], "%Y"))) 
) |> 
	lapply(
		`NA_%_1`
	) #-> perc_NA





# Annual Rainfall Total ####
lapply(
	Stations,
	\(data = "") {
		aggregate(
			Rain ~ format(data[ ,"Date"], format = "%Y"), 
			data = data, 
			FUN = sum, 
			na.rm = TRUE
		)
	}
) -> Rainfall_Total


# Summary Stats for Rainfall Total ####
sink("Data_outputs/summary statisitcs.txt")

list(
	`Annual Rainfall Total` = summary_stats(
		data = Rainfall_Total,
		Station = names(Rainfall_Total),
		var = "Rain"
	) |> . =>
		do.call(rbind, .) 
)

sink(file = NULL)




# Mean Monthly Raifall / climatology of the Stations ####
# Computing mean monthly Rainfall for all Stations of the list/data "Stations"
lapply(
	Stations,
	\(data = "") {
		aggregate(
			Rain ~ format(data[ ,"Date"], format = "%B") + format(data[ ,"Date"], format = "%Y"), 
			data = data, 
			FUN = sum, 
			na.rm = TRUE
		) |> . =>
			aggregate(
				Rain ~ `format(data[, "Date"], format = "%B")`,
				data = .,
				FUN = mean,
				na.rm = TRUE
			)
	}
) |> 
	# Ordering Months of each dataframe
	lapply(
		\(data = "") data[match(month.name, data[ ,1]), ]
	) -> mean_monthly_Rainfall


# Plotting mean monthly Rainfall ####
require(ggplot2, quietly = TRUE)
# require(patchwork)

Date <- seq(as.Date("1990-01-01"), by = "month", length.out = 12)

# Plots for mean monthly Rainfall ####
# Kintampo mean monthly Rainfall
ggplot(data = mean_monthly_Rainfall[["Kintampo"]], aes(x = Date, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	scale_x_date(date_labels = "%b", date_breaks = "1 month") +
	ggtitle("Mean Monthly Rainfall of KIntampo \n 1944 - 2021") +
	xlab("Month") +
	ylab("Rainfall (mm)") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Kintampo



# Koforidua mean monthly Rainfall
ggplot(data = mean_monthly_Rainfall[["Koforidua"]], aes(x = Date, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	scale_x_date(date_labels = "%b", date_breaks = "1 month") +
	ggtitle("Mean Monthly Rainfall of Koforidua \n 1965 - 2021") +
	xlab("Month") +
	ylab("Rainfall (mm)") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Koforidua

Navrongo <- ggplot(data = mean_monthly_Rainfall[["Navrongo"]], aes(x = Date, y = Rain)) +
	geom_line(lwd = 2, col = "darkblue") +
	scale_x_date(date_labels = "%b", date_breaks = "1 month") +
	ggtitle("Mean Monthly Rainfall of Navrongo \n 1946 - 2021") +
	xlab("Month") +
	ylab("Rainfall (mm)") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20))


# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Mean Monthly Rainfall.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Statistics for Mean Monthly Rainfall ####
 sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	`Mean Monthly Rainfall` = summary_stats(
		data = mean_monthly_Rainfall,
		Station = names(mean_monthly_Rainfall),
		var = "Rain"
	) |> . =>
		do.call(rbind, .)
)

sink(file = NULL)




# Annual Rainfall Anomaly ####
Annual_Rainfall_Anomaly <- lapply(
	Stations,
	\(data = "") aggregate(
		Rain ~ format(data[ ,"Date"], format = "%Y"), 
		data = data, 
		FUN = sum, 
		na.rm = TRUE
	) |> 
		setNames(c("Year", "Rain")) |> . =>
		transform(., Year = as.numeric(.[ ,1]))
) |> 
	# Computing annual Deviations from the long term mean
	lapply(
		\(data = "") {
			data.frame(
				Year = data[ ,1], 
				Deviations = (data[ ,"Rain"] - mean(data[ ,"Rain"], na.rm = T)) / sd(data[ ,"Rain"], na.rm = TRUE)
			)
		}
	)


# Plots for Rainfall Anomaly ####
# Kintampo Rainfall Anomaly
Kintaampo_rr_anomaly <- ggplot(data = Annual_Rainfall_Anomaly[["Kintampo"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
	geom_linerange(data = Annual_Rainfall_Anomaly[["kintampo"]], aes(
		colour = ifelse(Deviations > 0, "Positive", "Negative")),
		stat = "identity", position = "identity", size = 4) +
	geom_hline(yintercept = 0) +
	labs(title = "Rainfall Anomaly of Kintampo \n 1944 - 2021", x = "Year", y = "Rainfall (mm)") +
	scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
	theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
		 axis.title = element_text(size = 28, face = "bold"),
		 axis.text = element_text(size = 26),
		 legend.title = element_blank(),
		 legend.text = element_text(size = 23))



# Koforidua Rainfall Anomaly
Koforidua_rr_anomaly <- ggplot(data = Annual_Rainfall_Anomaly[["Koforidua"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
	geom_linerange(data = Annual_Rainfall_Anomaly[["koforidua"]], aes(
		colour = ifelse(Deviations > 0, "Positive", "Negative")),
		stat = "identity", position = "identity", size = 4) +
	geom_hline(yintercept = 0) +
	labs(title = "Rainfall Anomaly of Koforidua \n 1944 - 2021", x = "Year", y = "Rainfall (mm)") +
	scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 7)) +
	theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
		 axis.title = element_text(size = 28, face = "bold"),
		 axis.text = element_text(size = 26),
		 legend.title = element_blank(),
		 legend.text = element_text(size = 23))


# Navrongo Rainfall Anomaly
Navrongo_rr_anomaly <- ggplot(data = Annual_Rainfall_Anomaly[["Navrongo"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
	geom_linerange(data = Annual_Rainfall_Anomaly[["Navrongo"]], aes(
		colour = ifelse(Deviations > 0, "Positive", "Negative")),
		stat = "identity", position = "identity", size = 4) +
	geom_hline(yintercept = 0) +
	labs(title = "Rainfall Anomaly of Navrongo \n 1980 - 2021", x = "Year", y = "Rainfall (mm)") +
	scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
	theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
		 axis.title = element_text(size = 28, face = "bold"),
		 axis.text = element_text(size = 26),
		 legend.title = element_blank(),
		 legend.text = element_text(size = 23))


# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Rainfall Total Anomaly.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Statistics for Rainfall Total Anomaly ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	`Annual Rainfall Anomaly` = summary_stats(
		data = Annual_Rainfall_Anomaly,
		Station = names(Annual_Rainfall_Anomaly),
		var = "Deviations"
	) |> . =>
		do.call(rbind, .)
)

sink(file = NULL)





# Rainy Days ">=" 20mm
lapply(
	Stations,
	\(data = "") (
		dplyr::filter(data, Rain >= 20) |> . =>
			aggregate(
				Rain ~ format(.[ ,"Date"], format = "%Y"),
				data = .,
				FUN = length
			) |> . =>
			setNames(., c("Year", "Rain")) |> . =>
			transform(., Year = as.numeric(.[, "Year"]))
	)
) -> Annual_heavy_events


# Plots for Annual heavy events for all Stations ####
# Kintampo Annual heavy events
ggplot(data = Annual_heavy_events[["Kintampo"]], aes(x = Year, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 2000, y = 54, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Kintampo"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
	ggtitle("Number of heavy Rainfall events in Kintampo \n 1944 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Kintampo_heavy_events



# Koforidua annual heavy events
ggplot(data = Annual_heavy_events[["Koforidua"]], aes(x = Year, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	scale_x_continuous(breaks = seq(1965, 2021, by = 5)) +
	annotate("text", x = 1990, y = 35, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Koforidua"]][ ,2])))["sl"], 2)),
		size = 10) +
	ggtitle("Number of heavy Rainfall events in Koforidua \n 1965 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Koforidua_heavy_events



# Navrongo annual heavy evens
ggplot(data = Annual_heavy_events[["Navrongo"]], aes(x = Year, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 1998, y = 28, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Navrongo"]][ ,2])))["sl"], 2)),
		size = 10) +
	#scale_x_continuous(breaks = seq(1946, 2021, by = 9)) +
	ggtitle("Number of heavy Rainfall events in Navrongo \n 1980 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Navrongo_heavy_events


# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Heavy Rainall events.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Statistics for Heavy Rainfall Events ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	`Annual heavy Ranfall Events` = summary_stats(
		data = Annual_heavy_events,
		Station = names(Annual_heavy_events),
		var = "Rain"
	) |> . =>
		do.call(rbind, .)
)

sink(file = NULL)





# Annual Rainy Days ####
lapply(
	Stations,
	\(data = "") (
		dplyr::filter(data, Rain >= 0.85) |> . =>
			aggregate(
				Rain ~ format(.[ ,"Date"], format = "%Y"),
				data = .,
				FUN = length
			) |> . =>
			setNames(., c("Year", "Rain")) |> . =>
			transform(., Year = as.numeric(.[, "Year"]))
	)
) -> Annual_rain_events


# Plots for Annual rainy days for all Stations ####
# Kintampo Annual Rainy Days
ggplot(data = Annual_rain_events[["Kintampo"]], aes(x = Year, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 1999, y = 120, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_rain_events[["Kintampo"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
	ggtitle("Number of Rain events in Kintampo \n 1944 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Kintampo_rain_events



# Koforidua annual heavy events
ggplot(data = Annual_rain_events[["Koforidua"]], aes(x = Year, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 2000, y = 130, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Koforidua"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(1965, 2021, by = 7)) +
	ggtitle("Number of Rain events in Koforidua \n 1965 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Koforidua_rain_events



# Navrongo annual heavy evens
ggplot(data = Annual_rain_events[["Navrongo"]], aes(x = Year, y = Rain)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 1995, y = 80, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Navrongo"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(1946, 2021, by = 9)) +
	ggtitle("Number of Rain events in Navrongo \n 1980 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Navrongo_rain_events


# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Rainall events.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Stats for Rainfall Events ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	Annual_rain_events = summary_stats(
		data = Annual_rain_events,
		Station = names(Annual_rain_events),
		var = "Rain"
	) |> . =>
		do.call(rbind, .)
)

sink(file = NULL)





# Longest Dry Spell Per year for All Stations ####

# calling runs_1 to loop over the elements of the list "Stations"
 # Stations is a two level nested list on the second lapply call of this pipeline
lapply(
	Stations,
	\(data = "") split(data[ ,"Rain"], as.factor(format(data[ ,"Date"], "%Y"))) %>% 
		lapply(\(vec) vec[!is.na(vec)])
) |> 
	lapply(
	runs_1
) -> Longest_Dry_Spells


# Removing duplicated years, thus, years of 2 or more maximum values ####
Longest_Dry_Spells %<>% 
	lapply(
		\(data = ""){
			if (any(grepl("[0-9]+\\.[0-9]+", rownames(data)))){
				
				#splitting, extracting, conversion of the output vector to a dataframe
				strsplit(rownames(data), "\\.") |> 
					sapply(\(vec = "") vec[1]) |> 
					as.numeric() |> . =>
					data.frame(Year = ., l = data[ ,2]) |> 
					subset(!duplicated(Year)) |> . => 
					set_rownames(., .[ ,1]) -> rs
				
				# setting column 1 of rs to `0` and renaming variables
				(rs[ ,1] <- c(l = 0)); setNames(rs, c("v", "l"))
				
			} else data
		}
	)


# Plotting Longest Dry Spell ####
# Kintampo Annual Rainy Days
ggplot(data = Longest_Dry_Spells[["Kintampo"]], 
	  aes(x = as.numeric(rownames(Longest_Dry_Spells[["Kintampo"]])), y = l)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 1999, y = 80, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Longest_Dry_Spells[["Kintampo"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(from = 1980, to = 2021, by = 8)) +
	ggtitle("Longest Dry Spell of Kintampo \n 1980 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Kintampo_Longest_Dryspell



# Koforidua annual heavy events
ggplot(data = Longest_Dry_Spells[["Koforidua"]], 
	  aes(x = as.numeric(rownames(Longest_Dry_Spells[["Koforidua"]])), y = l)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 2000, y = 60, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Longest_Dry_Spells[["Koforidua"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(1980, 2021, by = 8)) +
	ggtitle("Longest Dry Spell of Koforidua \n 1980 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Koforidua_Longest_DrySpell



# Navrongo annual heavy evens
ggplot(data = Longest_Dry_Spells[["Navrongo"]], 
	  aes(x = as.numeric(rownames(Longest_Dry_Spells[["Navrongo"]])), y = l)) + 
	geom_line(lwd = 2, col = "darkblue") +
	geom_smooth(method = "lm", col = "red") +
	annotate("text", x = 1995, y = 110, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Longest_Dry_Spells[["Navrongo"]][ ,2])))["sl"], 2)),
		size = 10) +
	#scale_x_continuous(breaks = seq(1980, 2021, by = 9)) +
	ggtitle("Longest Dry Spell of Navrongo \n 1980 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Navrongo_Longest_DrySpell

# Summary Stats for Longest Dry Spell for the entire year
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	`Longest_Dry_Spells` = 
		summary_stats(
			data = Longest_Dry_Spells,
			Station = names(Longest_Dry_Spells),
			var = "l"
		) |> . =>
		do.call(rbind, .)
)

sink(file = NULL)



# Longest  Dry Spell for the Rainy Season April to October ####
Stations |> 
	lapply(
		# Ananymous function to subset the months April to October only for each year
		\(data = "", Date = "") {
			data[
				format(data[ ,Date], "%b") %in% month.abb[4:10] & !is.na(data[ ,Date]) & !is.na(data[ ,"Rain"]), 
			]
		},
		Date = "Date"
	) |> 
	lapply(\(data = "") split(data[ ,"Rain"], as.factor(format(data[ ,"Date"], format = "%Y")))) |> 
	lapply(runs_1) -> Longest_Dry_Spells_Seasons


# # Removing duplicated years, thus, years of 2 or more maximum values ####
Longest_Dry_Spells_Seasons %<>% 
	lapply(
		\(data = ""){
			if (any(grepl("[0-9]+\\.[0-9]+", rownames(data)))){
				
				#splitting, extracting, conversion of the output vector to a dataframe
				strsplit(rownames(data), "\\.") |> 
					sapply(\(vec = "") vec[1]) |> 
					as.numeric() |> . =>
					data.frame(Year = ., l = data[ ,2]) |> 
					subset(!duplicated(Year)) |> . => 
					set_rownames(., .[ ,1]) -> rs
				
				# setting column 1 of rs to `0` and renaming variables
				(rs[ ,1] <- c(l = 0)); setNames(rs, c("v", "l"))
				
			} else data
		}
	)


# Summary Stats for Longest Dry spell of the Season ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	`Longest_Dry_spell_ofthe_season` = summary_stats(
		data = Longest_Dry_Spells_Seasons,
		Station = names(Longest_Dry_Spells_Seasons),
		var = "l"
	) |> . =>
		do.call(rbind, .)
)

sink()




# Consecutive wet days of the season ####

# calling runs_1 to loop over the elements of the list "Stations"
# Stations is a two level nested list on the second vectorised lapply call on this pipeline
Stations |> 
	lapply(
		# Ananymous function to subset the months April to October only for each year
		\(data = "", Date = "") {
			data[
				format(data[ ,Date], "%b") %in% month.abb[4:10] & !is.na(data[ ,Date]) & !is.na(data[ ,"Rain"]), 
			]
		},
		Date = "Date"
	) |> 
	lapply(\(data = "") split(data[ ,"Rain"], as.factor(format(data[ ,"Date"], "%Y"))) ) |> 
	lapply(runs_1_wet) -> Longest_wet_spell


# Removing duplicated years, thus, years of 2 or more maximum values ####
Longest_wet_spell %<>% 
	lapply(
		\(data = ""){
			if (any(grepl("[0-9]+\\.[0-9]+", rownames(data)))){
				
				#splitting, extracting, conversion of the output vector to a dataframe
				strsplit(rownames(data), "\\.") |> 
					sapply(\(vec = "") as.numeric(vec[1])) |> . => 
					data.frame(Year = ., l = data[ ,2]) |> 
					subset(!duplicated(Year)) |> . => 
					set_rownames(., .[ ,1]) -> rs
				
				# setting column 1 of rs to `0` and renaming variables
				(rs[ ,1] <- c(l = "wet")); setNames(rs, c("v", "l"))
				
			} else data
		}
	)

# Summary Stats for Longest wet spell in the season
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
	`Longest_Wet_Spell_of_theseason` = summary_stats(
		data = Longest_wet_spell,
		Station = names(Longest_wet_spell),
		var = "l"
	) |> . =>
		do.call(rbind, .)
)
 
sink(file = NULL)

#############################################################################################################
# Creating Directories ####
for ( i in c("Data_outputs", "Plots_outputs")){
	if (!dir.exists(i)) dir.create(i) else message("Already Created")
}

# loading magrittr 
require(magrittr)

# Introducing the pipebind operator
Sys.setenv("_R_USE_PIPEBIND_" = "true") # Invoking the pipebind operator

# DATA IMPORT AND LIL RESHAPING
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


# Percentage of Missing Values for each year conditioned on Met Stations ####
`NA_%`  <- function(vec = "") {
	((as.character(is.na(vec)) %>% .[. == "TRUE"] %>% length * 100) / length(vec)) %>% 
		round(digits = 2) %>% 
		paste0("%")
}

# Building a function which calls `NA_%` to loop over the vector elements of a list
`NA_%_1` <- function(list = "") lapply(list, `NA_%`) |> . => do.call(rbind, .)

# calling `NA_%_1` to loop over the elements of the list "Stations"
# Stations is a two level nested list
lapply(
	Stations,
	\(data = "") split(data[ ,"Rain"], as.factor(format(data[ ,"Date"], "%Y"))) 
) |> 
	lapply(
		`NA_%_1`
	) -> perc_NA



# Mean Monthly Raifall / climatology of the Stations ####
# Computing mean monthly Rainfall for all Stations of the list/data "Stations"
lapply(
	Stations,
	\(data = "") {
		aggregate(
			Rain ~ format(data[ ,"Date"], format = "%B") + format(data[ ,"Date"], format = "%Y"), 
			data = data, 
			FUN = sum, 
			na.rm = T
		) |> . =>
			aggregate(
				Rain ~ `format(data[, "Date"], format = "%B")`,
				data = .,
				FUN = mean,
				na.rm = T
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

# Composites of Mean Monthly Rainfall
# Kintampo / Koforidua / Navrongo
gridExtra::grid.arrange(
	Kintampo, Koforidua, Navrongo,
	ncol = 2, nrow = 2
)


# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Mean Monthly Rainfall.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Statistics for Mean Monthly Rainfall ####
data.frame(
	Period = {
		a = format(range(Stations[["Kintampo"]][, "Date"]), format = "%Y")
		paste(a[1], a[2], sep = "-")
	},
	mean = mean(mean_monthly_Rainfall[["Kintampo"]][ ,"Rain"], na.rm = T),
	min = min(mean_monthly_Rainfall[["Kintampo"]][ ,"Rain"], na.rm = T),
	max = max(mean_monthly_Rainfall[["Kintampo"]][ ,"Rain"], na.rm = T),
	sd = sd(mean_monthly_Rainfall[["Kintampo"]][ ,"Rain"], na.rm = T)
) |>
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Koforidua"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(mean_monthly_Rainfall[["Koforidua"]][ ,"Rain"], na.rm = T),
			min = min(mean_monthly_Rainfall[["Koforidua"]][ ,"Rain"], na.rm = T),
			max = max(mean_monthly_Rainfall[["Koforidua"]][ ,"Rain"], na.rm = T),
			sd = sd(mean_monthly_Rainfall[["Koforidua"]][ ,"Rain"], na.rm = T)
		)
	) |> 
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Navrongo"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(mean_monthly_Rainfall[["Navrongo"]][ ,"Rain"], na.rm = T),
			min = min(mean_monthly_Rainfall[["Navrongo"]][ ,"Rain"], na.rm = T),
			max = max(mean_monthly_Rainfall[["Navrongo"]][ ,"Rain"], na.rm = T),
			sd = sd(mean_monthly_Rainfall[["Navrongo"]][ ,"Rain"], na.rm = T)
		)
	) |> 
	write.csv(
		file = "outputs/Mean Monthly Rainfall summ_Stats.csv",
		row.names = FALSE
	)



# Annual Rainfall Anomaly ####
Annual_Rainfall_Anomaly <- lapply(
	Stations,
	\(data = "") aggregate(
		Rain ~ format(data[ ,"Date"], format = "%Y"), 
		data = data, 
		FUN = sum, 
		na.rm = T
	) |> 
		setNames(c("Year", "Rain")) |> . =>
		transform(., Year = as.numeric(.[ ,1]))
) |> 
	# Computing annual Deviations from the long term mean
	lapply(
		\(data = "") {
			data.frame(
				Year = data[ ,1], 
				Deviations = (data[ ,"Rain"] - mean(data[ ,"Rain"], na.rm = T)) / sd(data[ ,"Rain"], na.rm = T)
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
	labs(title = "Rainfall Anomaly of Navrongo \n 1944 - 2021", x = "Year", y = "Rainfall (mm)") +
	scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
	theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
		 axis.title = element_text(size = 28, face = "bold"),
		 axis.text = element_text(size = 26),
		 legend.title = element_blank(),
		 legend.text = element_text(size = 23))

# Composites of Raifnall Anomaly
gridExtra::grid.arrange(
	Kintaampo_rr_anomaly, Koforidua_rr_anomaly, Navrongo_rr_anomaly,
	ncol = 2, nrow = 2
)

# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Rainfall Total Anomaly.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Statistics for Rainfall Total Anomaly ####
data.frame(
	Period = {
		a = format(range(Stations[["Kintampo"]][, "Date"]), format = "%Y")
		paste(a[1], a[2], sep = "-")
	},
	mean = mean(Annual_Rainfall_Anomaly[["Kintampo"]][ ,"Deviations"], na.rm = T),
	min = min(Annual_Rainfall_Anomaly[["Kintampo"]][ ,"Deviations"], na.rm = T),
	max = max(Annual_Rainfall_Anomaly[["Kintampo"]][ ,"Deviations"], na.rm = T),
	sd = sd(Annual_Rainfall_Anomaly[["Kintampo"]][ ,"Deviations"], na.rm = T)
) |>
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Koforidua"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(Annual_Rainfall_Anomaly[["Koforidua"]][ ,"Deviations"], na.rm = T),
			min = min(Annual_Rainfall_Anomaly[["Koforidua"]][ ,"Deviations"], na.rm = T),
			max = max(Annual_Rainfall_Anomaly[["Koforidua"]][ ,"Deviations"], na.rm = T),
			sd = sd(Annual_Rainfall_Anomaly[["Koforidua"]][ ,"Deviations"], na.rm = T)
		)
	) |> 
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Navrongo"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(Annual_Rainfall_Anomaly[["Navrongo"]][ ,"Deviations"], na.rm = T),
			min = min(Annual_Rainfall_Anomaly[["Navrongo"]][ ,"Deviations"], na.rm = T),
			max = max(Annual_Rainfall_Anomaly[["Navrongo"]][ ,"Deviations"], na.rm = T),
			sd = sd(Annual_Rainfall_Anomaly[["Navrongo"]][ ,"Deviations"], na.rm = T)
		)
	) |> 
	write.csv(
		file = "outputs/Rainfall Total Anomaly summ_Stats.csv",
		row.names = FALSE
	)


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
	annotate("text", x = 1998, y = 40, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Navrongo"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(1946, 2021, by = 9)) +
	ggtitle("Number of heavy Rainfall events in Navrongo \n 1946 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Navrongo_heavy_events

# Composites of heavy Raifnall events
gridExtra::grid.arrange(
	Kintampo_heavy_events, Koforidua_heavy_events, Navrongo_heavy_events,
	ncol = 2, nrow = 2
)
# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Heavy Rainall events.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Statistics for Heavy Rainfall Events ####
data.frame(
	Period = {
		a = format(range(Stations[["Kintampo"]][, "Date"]), format = "%Y")
		paste(a[1], a[2], sep = "-")
	},
	mean = mean(Annual_heavy_events[["Kintampo"]][ ,"Rain"], na.rm = T),
	min = min(Annual_heavy_events[["Kintampo"]][ ,"Rain"], na.rm = T),
	max = max(Annual_heavy_events[["Kintampo"]][ ,"Rain"], na.rm = T),
	sd = sd(Annual_heavy_events[["Kintampo"]][ ,"Rain"], na.rm = T)
) |>
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Koforidua"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(Annual_heavy_events[["Koforidua"]][ ,"Rain"], na.rm = T),
			min = min(Annual_heavy_events[["Koforidua"]][ ,"Rain"], na.rm = T),
			max = max(Annual_heavy_events[["Koforidua"]][ ,"Rain"], na.rm = T),
			sd = sd(Annual_heavy_events[["Koforidua"]][ ,"Rain"], na.rm = T)
		)
	) |> 
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Navrongo"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(Annual_heavy_events[["Navrongo"]][ ,"Rain"], na.rm = T),
			min = min(Annual_heavy_events[["Navrongo"]][ ,"Rain"], na.rm = T),
			max = max(Annual_heavy_events[["Navrongo"]][ ,"Rain"], na.rm = T),
			sd = sd(Annual_heavy_events[["Navrongo"]][ ,"Rain"], na.rm = T)
		)
	) |> 
	write.csv(
		file = "outputs/Heavy Rainfall events summ_Stats.csv",
		row.names = FALSE
	)



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
	annotate("text", x = 1995, y = 85, label = paste(
		"p-value = ", 
		round((unlist(Kendall::MannKendall(Annual_heavy_events[["Navrongo"]][ ,2])))["sl"], 2)),
		size = 10) +
	scale_x_continuous(breaks = seq(1946, 2021, by = 9)) +
	ggtitle("Number of Rain events in Navrongo \n 1946 - 2021") +
	xlab("Year") +
	ylab("Number of Days") +
	theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
		 axis.title = element_text(size = 22, face = "bold"),
		 axis.text = element_text(size = 20)) -> Navrongo_rain_events


# Composites of Raifnall Anomaly
gridExtra::grid.arrange(
	Kintampo_rain_events, Koforidua_rain_events, Navrongo_rain_events,
	ncol = 2, nrow = 2
)

# Saving above plot to disk
dev.copy(
	png, 
	filename = "Plots_outputs/Rainall events.png",
	width = 1450,
	height = 850
)
dev.off()

# Summary Stats for Rainfall Events ####
data.frame(
	Period = {
		a = format(range(Stations[["Kintampo"]][, "Date"]), format = "%Y")
		paste(a[1], a[2], sep = "-")
	},
	mean = mean(Annual_rain_events[["Kintampo"]][ ,"Rain"], na.rm = T),
	min = min(Annual_rain_events[["Kintampo"]][ ,"Rain"], na.rm = T),
	max = max(Annual_rain_events[["Kintampo"]][ ,"Rain"], na.rm = T),
	sd = sd(Annual_rain_events[["Kintampo"]][ ,"Rain"], na.rm = T)
) |>
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Koforidua"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(Annual_rain_events[["Koforidua"]][ ,"Rain"], na.rm = T),
			min = min(Annual_rain_events[["Koforidua"]][ ,"Rain"], na.rm = T),
			max = max(Annual_rain_events[["Koforidua"]][ ,"Rain"], na.rm = T),
			sd = sd(Annual_rain_events[["Koforidua"]][ ,"Rain"], na.rm = T)
		)
	) |> 
	rbind(
		data.frame(
			Period = {
				a = format(range(Stations[["Navrongo"]][, "Date"]), format = "%Y")
				paste(a[1], a[2], sep = "-")
			},
			mean = mean(Annual_rain_events[["Navrongo"]][ ,"Rain"], na.rm = T),
			min = min(Annual_rain_events[["Navrongo"]][ ,"Rain"], na.rm = T),
			max = max(Annual_rain_events[["Navrongo"]][ ,"Rain"], na.rm = T),
			sd = sd(Annual_rain_events[["Navrongo"]][ ,"Rain"], na.rm = T)
		)
	) |> 
	write.csv(
		file = "outputs/Rainfall events summ_Stats.csv",
		row.names = FALSE
	)




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

# calling runs_1 to loop over the elements of the list "Stations"
 # Stations is a two level nested list
lapply(
	Stations,
	\(data = "") split(data[ ,"Rain"], as.factor(format(data[ ,"Date"], "%Y"))) %>% 
		lapply(\(vec) vec[!is.na(vec)])
) |> 
	lapply(
	runs_1
) -> Longest_Dry_Spells

# Removing duplicated years, thus, years of 2 or more maximum values ####
Longest_Dry_Spells |> 
	lapply(
		\(data = ""){
			if (!duplicated(strsplit(rownames(data), "\\.") |> sapply(\(vec = "") vec[1]) |> as.numeric())){
				
			}
		}
	)


# Onset of the Season ####

strsplit(rownames(kof), "\\.") |> sapply(\(vec = "") vec[1]) |> as.numeric() -> a
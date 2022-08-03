# Creating Directories ####
for ( i in c("Data_outputs", "Plots_outputs")){
  if (!dir.exists(i)) dir.create(i) else message("Already Created")
}

# Introducing the pipebind operator
Sys.setenv("_R_USE_PIPEBIND_" = "true") # Invoking the pipebind operator

# checking and loading packages #### 
pkgs <- c("magrittr", "ggplot2")
sapply(setdiff(pkgs, (.packages())), require, character.only = TRUE)

# c("magrittr", "ggplot2") |> 
#   setdiff((.packages())) |> 
#   sapply(require, character.only = TRUE)


# c("magrittr", "ggplot2") |> . => 
#   paste("package", ., sep = ":") |> 
#   setdiff(search()) |> 
#   strsplit(":") |> 
#   sapply(\(vec = "") vec[2]) |> 
#   sapply(require, character.only = TRUE)





# DATA IMPORT AND LIL RESHAPING ####
# Importing all three Stations #
dir(path = "~/Climate_of_Select_Stations/Min_Tmp", pattern = ".csv$", full.names = TRUE) |> # Reading in all datasets with names "Koforidu.csv, Kintampo.csv, navrong.csv"
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
        Tmin = data[ ,"tmin"]
      ) |> . =>
        .[.[ ,"Date"] >= "1980-01-01", ]
    }
  ) |> 
  setNames(c("Koforidua", "Navrongo")) -> Stations_Tmin # Renaming the list/output above

# for efficiency, use rio::import_list instead of combining "lapply" and "read.csv" to read the datasets into R

# Changing class of Koforidua Rain variable to "numeric"
#Stations[["Koforidua"]][ ,"Rain"] <- as.numeric(Stations[["Koforidua"]][ ,"Rain"])


# Percentage of Missing Values for each year conditioned on Met Stations ####
# calling `NA_%_1` to loop over the elements of the list "Stations"
# Stations is a two level nested list
lapply(
  Stations_Tmin,
  \(data = "") split(data[ ,"Tmin"], as.factor(format(data[ ,"Date"], "%Y"))) 
) |> 
  lapply(
    `NA_%_1`
  ) #-> perc_NA_tmain




# Computing Annual Minimum Temperature ####
lapply(
  Stations_Tmin,
  \(data = "") {
    aggregate(
      Tmin ~ format(data[ ,"Date"], format = "%Y"),
      data = data,
      FUN = mean,
      na.rm = TRUE
    ) |> 
      transform(`format(data[, "Date"], format = "%Y")` = as.numeric(`format(data[, "Date"], format = "%Y")`)) |> 
      setNames(c("Year", "Tmin"))
  }
) -> Annual_Min_Temp

# Plotting Annual Min Temp

# Plotting Annual Min Temp
ggplot(data = Annual_Min_Tmp[["Koforidua"]], aes(x = Year, y = Tmin)) + 
  geom_line(lwd = 2, col = "firebrick") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  ggtitle("Minimum Temperature of Koforidua \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) -> Koforidua_anntmin

ggplot(data = Annual_min_Tmp[["Navrongo"]], aes(x = Year, y = Tmin)) +
  geom_line(lwd = 2, col = "firebrick") +
  scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  ggtitle("Monthly Minimum Temperature of Navrongo \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) -> Navrongo_anntmin

# Composites of Mean Monthly Rainfall
# Kintampo / Koforidua / Navrongo
gridExtra::grid.arrange(
  
  ncol = 2, nrow = 2
)

 
# Summary Statistics of Maximum Temperature ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Annual_Minimum_Temperature` = summary_stats(
    data = Annual_Min_Temp,
    Station = names(Annual_Min_Temp),
    var = "Tmin"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)








# Monthly Min Temp / climatology of the Stations ####
# Computing mean monthly Max Temp Stations of the list/data "Stations"
lapply(
  Stations_Tmin,
  \(data = "") {
    aggregate(
      Tmin ~ format(data[ ,"Date"], format = "%B"), 
      data = data, 
      FUN = mean, 
      na.rm = TRUE
    ) #|> . =>
    # aggregate(
    #   Rain ~ `format(data[, "Date"], format = "%B")`,
    #   data = .,
    #   FUN = mean,
    #   na.rm = T
    # )
  }
) |> 
  # Ordering Months of each dataframe
  lapply(
    \(data = "") data[match(month.name, data[ ,1]), ]
  ) -> Monthly_Tmin


# Date for x-axis
Date <- seq(as.Date("1990-01-01"), as.Date("1990-12-31"), by = "month")

# Koforidua mean monthly min tmp
ggplot(data = Monthly_Tmin[["Koforidua"]], aes(x = Date, y = Tmin)) + 
  geom_line(lwd = 2, col = "firebrick") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ggtitle("Monthly Min Tmp of Koforidua \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) 

ggplot(data = Monthly_Tmin[["Navrongo"]], aes(x = Date, y = Tmin)) +
  geom_line(lwd = 2, col = "darkblue") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ggtitle("Mean Monthly Rainfall of Navrongo \n 1946 - 2021") +
  xlab("Month") +
  ylab("Rainfall (mm)") +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20))


# Summary Statistics for Mean monthly min temp
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Monthly Minimum Temperature` = summary_stats(
    data = Monthly_Tmin,
    Station = names(Monthly_Tmin),
    var = "Tmin"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)








# Annual Minimum Tmp Anomaly ####
# Maximum Temperature Anomaly ####
Annual_MinTmp_Anomaly <- lapply(
  Stations_Tmin,
  \(data = "") aggregate(
    Tmin ~ format(data[ ,"Date"], format = "%Y"), 
    data = data, 
    FUN = mean, 
    na.rm = TRUE
  ) %>% 
    setNames(c("Year", "Tmin")) %>%
    transform(Year = as.numeric(.[ ,"Year"]))
) |> 
  # Computing annual Deviations from the long term mean
  lapply(
    \(data = "") {
      data.frame(
        Year = data[ ,1], 
        Deviations = (data[ ,"Tmin"] - mean(data[ ,"Tmin"], na.rm = TRUE)) / sd(data[ ,"Tmin"], na.rm = TRUE)
      )
    }
  )


ggplot(data = Annual_MinTmp_Anomaly[["Koforidua"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
  geom_linerange(data = Annual_MinTmp_Anomaly[["koforidua"]], aes(
    colour = ifelse(Deviations > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Min Tmp Anomaly of Koforidua \n 1980 - 2021", x = "Year", y = expression("Temp ("~degree*C*")")) +
  #scale_x_continuous(breaks = seq(from = 1980, to = 2021, by = 7)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.text = element_text(size = 23))

# Summay Statistics for min tmp anomaly
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Annual Minimum Temperature Anomaly` = summary_stats(
    data = Annual_MinTmp_Anomaly,
    Station = names(Annual_MinTmp_Anomaly),
    var = "Deviations"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)








# Maximum and Minimum of Minimum Temperature ####
lapply(
  Stations_Tmin, # Data/List to loop over
  
  # aninymous function to extract max temps in the 90th percentile
  \(data = "") {
    aggregate(
      Tmin ~ format(data[ ,"Date"], format = "%Y"),
      data = data,
      FUN = \(vec = "") length(vec[vec > quantile(vec, probs = 0.9, na.rm = TRUE) & !is.na(vec)])
    ) |> 
      setNames(c("Year", "Tmin")) |> 
      transform(Year = as.numeric(Year))
  }
) -> max_minimum_temp


# Plotting 
ggplot(data = max_minimum_temp[["Koforidua"]], aes(x = Year, y = Tmin)) +
  geom_line(col = "firebrick", lwd = 2) +
  #scale_x_continuous(breaks = seq(1980, 2021, by = 5)) +
  labs(
    title = "Number of Days with Min Tmp in the 90th percentile \n 1980 - 2021", 
    x = "Year", 
    y = expression("Temperature("~degree*C*")")
  ) + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20))

# summary Statistics of max of minimum temperature
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `minimum of minimum Temperature` = summary_stats(
    data = max_minimum_temp,
    Station = names(max_minimum_temp),
    var = "Tmin"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)








# Maximum and Minimum of Maximum Temperature ####
lapply(
  Stations_Tmin, # Data/List to loop over
  
  # aninymous function to extract max temps in the 90th percentile
  \(data = "") {
    aggregate(
      Tmin ~ format(data[ ,"Date"], format = "%Y"),
      data = data,
      FUN = \(vec = "") length(vec[vec < quantile(vec, probs = 0.1, na.rm = TRUE) & !is.na(vec)])
    ) |> 
      setNames(c("Year", "Tmin")) |> 
      transform(Year = as.numeric(Year))
  }
) -> min_minimum_temp

# Plotting 
ggplot(data = min_minimum_temp[["Koforidua"]], aes(x = Year, y = Tmin)) +
  geom_line(col = "firebrick", lwd = 2) +
  #scale_x_continuous(breaks = seq(1980, 2021, by = 5)) +
  labs(
    title = "Number of Days with Min Tmp in the 10th percentile \n 1980 - 2021", 
    x = "Year", 
    y = expression("Temperature("~degree*C*")")
  ) + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20))


sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Minimum of minimum temperature` = summary_stats(
    data = min_minimum_temp,
    Station = names(min_minimum_temp),
    var = "Tmin"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)





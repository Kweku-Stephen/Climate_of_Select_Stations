#############################################################################################################
# Creating Directories ####
for ( i in c("Data_outputs", "Plots_outputs")){
  if (!dir.exists(i)) dir.create(i) else message("Already Created")
}

# Introducing the pipebind operator
Sys.setenv("_R_USE_PIPEBIND_" = "true") # Invoking the pipebind operator

# checking and loading packages ####
pkgs <- c("magrittr", "ggplot2") # packages to be loaded
sapply(setdiff(pkgs, (.packages())), require, character.only = TRUE) # loading unloaded packages

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
# Importing all three Stations
dir(path = "~/Climate_of_Select_Stations/Max_Tmp", pattern = ".csv$", full.names = TRUE) |> # Reading in all datasets with names "Koforidu.csv, Kintampo.csv, navrong.csv"
  lapply(
    read.csv, # Reading in output of "dir"
    na.strings = -99.9 # setting missing values coded as -99.9 to NA
  ) |> # Piping the above output into the loop below
  lapply(
    # Looping the ananymous function below on each element of the list returned from above
    # to create a date object using columns "Year", "month" and "day" of each dataset
    \(data = "") {
      # Creating a dataframe 
      data.frame(
        Date = as.Date(paste(data[ ,1], data[ ,2], data[ ,3], sep = "-"), format = "%Y-%m-%d"),
        Tmax = data[ ,"tmax"]
      ) |> . => # Piping the output from above into the expression below 
        .[.[ ,"Date"] >= "1980-01-01", ] # subseeting years >= 1980
    }
  ) |> 
  # Renaming the list/output above (column names of each data in the list piped form the expression above)
  setNames(c("Koforidua", "Navrongo")) -> Stations_Tmax # 

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
  Stations_Tmax,
  # Lopping the ananymous function below on the elements of the list "stations_Tmax"
  \(data = "") split(data[ ,"Tmax"], as.factor(format(data[ ,"Date"], "%Y"))) 
) |> # Piping the output from above as input to the `NA_%_1` function in the looping construct below 
  lapply(
    `NA_%_1`
  ) #-> perc_NA_tmax




# Computing Annual Minimum Temperature ####
lapply(
  Stations_Tmax,
  # Ananymous function to compute mean annual max temp as a function of year
  \(data = "") {
    aggregate(
      Tmax ~ format(data[ ,"Date"], format = "%Y"),
      data = data,
      FUN = mean,
      na.rm = TRUE
    ) |> 
      # Transforming the year variable of the dataframe returned from above into a numeric vector
      transform(`format(data[, "Date"], format = "%Y")` = as.numeric(`format(data[, "Date"], format = "%Y")`)) |> 
      # Renaming column of the data returned from above
      setNames(c("Year", "Tmax"))
  }
) -> Annual_Max_Temp # Name of the returned list form the looping construct above

# Setting annual mean temp values for the year 2007 and 2008 from Koforidua because missing data is above 50%
Annual_Max_Temp[["Koforidua"]][27:28, 2] <- NA

# Imputing missing values the kalman filter 
Annual_Max_Temp[["Koforidua"]][ ,"Tmax"] <- imputeTS::na_kalman(
  Annual_Max_Temp[["Koforidua"]][ ,"Tmax"], 
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)


# Plotting Annual Min Temp
ggplot(data = Annual_Max_Temp[["Koforidua"]], aes(x = Year, y = Tmax)) + 
  geom_line(lwd = 2, col = "firebrick") +
  #scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  ggtitle("Mean Maximum Temperature of Koforidua \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) -> Koforidua_anntmax



# Koforidua mean monthly Rainfall
# ggplot(data = Monthly_Tmax[["Koforidua"]], aes(x = Date, y = Rain)) + 
#   geom_line(lwd = 2, col = "darkblue") +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#   ggtitle("Mean Monthly Rainfall of Koforidua \n 1965 - 2021") +
#   xlab("Month") +
#   ylab("Rainfall (mm)") +
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20)) -> Koforidua

ggplot(data = Annual_Max_Temp[["Navrongo"]], aes(x = Year, y = Tmax)) +
  geom_line(lwd = 2, col = "firebrick") +
  #scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
  ggtitle("Mean Maximum Temperature of Navrongo \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) -> Navrongo_anntmax

# Composites of Mean Monthly Rainfall
# Kintampo / Koforidua / Navrongo
gridExtra::grid.arrange(
  
  ncol = 2, nrow = 2
)



# Computing mean monthly Max Temp Stations of the list/data "Stations" ####
lapply(
  Stations_Tmax,
  \(data = "") {
    aggregate(
      Tmax ~ format(data[ ,"Date"], format = "%B"), 
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
  ) -> Monthly_Tmax


# Plotting mean monthly Tmax
ggplot(data = Monthly_Tmax[["Koforidua"]], aes(x = Date, y = Tmax)) + 
  geom_line(lwd = 2, col = "firebrick") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ggtitle("Monthly Maximum Temperature of Koforidua \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) -> Koforidua_mnthlytmax

# Koforidua mean monthly Rainfall
# ggplot(data = Monthly_Tmax[["Koforidua"]], aes(x = Date, y = Rain)) + 
#   geom_line(lwd = 2, col = "darkblue") +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#   ggtitle("Mean Monthly Rainfall of Koforidua \n 1965 - 2021") +
#   xlab("Month") +
#   ylab("Rainfall (mm)") +
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20)) -> Koforidua

ggplot(data = Monthly_Tmax[["Navrongo"]], aes(x = Date, y = Tmax)) +
  geom_line(lwd = 2, col = "firebrick") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ggtitle("Monthly Maximum Temperature of Navrongo \n 1980 - 2021") +
  xlab("Month") +
  ylab(expression("Temp ("~degree*C*")")) +
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20)) -> Navrongo_mnthlytmax

# Composites of Mean Monthly Rainfall
# Kintampo / Koforidua / Navrongo
gridExtra::grid.arrange(
  
  ncol = 2, nrow = 2
)



# Maximum Temperature Anomaly ####
Annual_MaxTmp_Anomaly <- lapply(
  Stations_Tmax,
  \(data = "") aggregate(
    Tmax ~ format(data[ ,"Date"], format = "%Y"), 
    data = data, 
    FUN = mean, 
    na.rm = TRUE
  ) |>  
    setNames(c("Year", "Tmax")) %>%
    transform(Year = as.numeric(.[ ,"Year"]))
) |> 
  # Computing annual Deviations from the long term mean
  lapply(
    \(data = "") {
      data.frame(
        Year = data[ ,1], 
        Deviations = (data[ ,"Tmax"] - mean(data[ ,"Tmax"], na.rm = TRUE)) / sd(data[ ,"Tmax"], na.rm = TRUE)
      )
    }
  )


# Plots for Rainfall Anomaly ####
# Kintampo Rainfall Anomaly
# Kintaampo_rr_anomaly <- ggplot(data = Annual_MaxTmp_Anomaly[["Kintampo"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
#   geom_linerange(data = Annual_MaxTmp_Anomaly[["kintampo"]], aes(
#     colour = ifelse(Deviations > 0, "Positive", "Negative")),
#     stat = "identity", position = "identity", size = 4) +
#   geom_hline(yintercept = 0) +
#   labs(title = "Rainfall Anomaly of Kintampo \n 1944 - 2021", x = "Year", y = "Rainfall (mm)") +
#   scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
#   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
#         axis.title = element_text(size = 28, face = "bold"),
#         axis.text = element_text(size = 26),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 23))



# Koforidua Rainfall Anomaly
Koforidua_rr_anomaly <- ggplot(data = Annual_MaxTmp_Anomaly[["Koforidua"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
  geom_linerange(data = Annual_MaxTmp_Anomaly[["koforidua"]], aes(
    colour = ifelse(Deviations > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Max Tmp Anomaly of Koforidua \n 1980 - 2021", x = "Year", y = expression("Temp ("~degree*C*")")) +
  scale_x_continuous(breaks = seq(from = 1980, to = 2021, by = 7)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
        axis.title = element_text(size = 28, face = "bold"),
        axis.text = element_text(size = 26),
        legend.title = element_blank(),
        legend.text = element_text(size = 23))


# Navrongo Rainfall Anomaly
Navrongo_rr_anomaly <- ggplot(data = Annual_MaxTmp_Anomaly[["Navrongo"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
  geom_linerange(data = Annual_MaxTmp_Anomaly[["Navrongo"]], aes(
    colour = ifelse(Deviations > 0, "Positive", "Negative")),
    stat = "identity", position = "identity", size = 4) +
  geom_hline(yintercept = 0) +
  labs(title = "Max Tmp Anomaly of Navrongo \n 1980 - 2021", x = "Year", y = expression("Temp ("~degree*C*")")) +
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
  filename = "outputs/Rainfall Total Anomaly.png",
  width = 1450,
  height = 850
)
dev.off()



# Maximum of Maximum Temperature ####
lapply(
  Stations_Tmax, # Data/List to loop over
  # aninymous function to extract max temps in the 90th percentile
  \(data = "") {
    aggregate(
      Tmax ~ format(data[ ,"Date"], format = "%Y"),
      data = data,
      FUN = \(vec = "") length(vec[vec > quantile(vec, probs = 0.9, na.rm = TRUE)])
    ) |> 
      setNames(c("Year", "Tmax")) |> 
      transform(Year = as.numeric(Year))
  }
) -> max_maximum_temp

# Replacing the year 2007's Count with NA
max_maximum_temp[["Koforidua"]][max_maximum_temp[["Koforidua"]][ ,"Year"] == 2007, "Tmax"] <- NA

# Imputing missing values in the Koforidua max tmp dataframe with the max_maximum_temp list
max_maximum_temp[["Koforidua"]][ ,"Tmax"] <- imputeTS::na_kalman(
    max_maximum_temp[["Koforidua"]][ ,"Tmax"],
    model = "StructTS", 
    smooth = TRUE,
    nit = -1
  )


# Plotting 
ggplot(data = max_maximum_temp[["Koforidua"]], aes(x = Year, y = Tmax)) +
  geom_line(col = "firebrick", lwd = 2) +
  scale_x_continuous(breaks = seq(1980, 2021, by = 5)) +
  labs(
    title = "Number of Days with Max Tmp in the 90th percentile \n 1980 - 2021", 
    x = "Year", 
    y = "Number of Days"
  ) + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20))


# Minimum of Maximum Temperature ####
lapply(
  Stations_Tmax, # Data/List to loop over
  
  # aninymous function to extract max temps in the 90th percentile
  \(data = "") {
    aggregate(
      Tmax ~ format(data[ ,"Date"], format = "%Y"),
      data = data,
      FUN = \(vec = "") length(vec[vec < quantile(vec, probs = 0.1, na.rm = TRUE)])
    ) |> 
      setNames(c("Year", "Tmax")) |> 
      transform(Year = as.numeric(Year))
  }
) -> min_maximum_temp

# Replacing the year 2007's Count with NA
min_maximum_temp[["Koforidua"]][min_maximum_temp[["Koforidua"]][ ,"Year"] == 2007, "Tmax"] <- NA

# Imputing missing values in the Koforidua max tmp dataframe with the max_maximum_temp list
min_maximum_temp[["Koforidua"]][ ,"Tmax"] <- imputeTS::na_kalman(
  min_maximum_temp[["Koforidua"]][ ,"Tmax"],
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)

# Plotting 
ggplot(data = min_maximum_temp[["Koforidua"]], aes(x = Year, y = Tmax)) +
  geom_line(col = "firebrick", lwd = 2) +
  scale_x_continuous(breaks = seq(1980, 2021, by = 5)) +
  labs(
    title = "Number of Days with Max Tmp in the 10th percentile \n 1980 - 2021", 
    x = "Year", 
    y = "Number of Days"
  ) + 
  theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20))











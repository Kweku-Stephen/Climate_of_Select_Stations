#############################################################################################################
# Sourcing Functions Script.R
source("Functions Script.R")


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
# Importing all three Stations #
dir(path = "~/Climate_of_Select_Stations/Max_Tmp", pattern = ".csv$", full.names = TRUE) |> # Reading in all datasets with names "Koforidu.csv, Kintampo.csv, navrong.csv"
  lapply(
    read.csv, # Reading in output of "dir"
    na.strings = c("-99.9", "-99", "9988", "-9988", "9999", "-9999"), # setting missing values coded as -99.9 to NA
    colClasses = c("tmax" = "numeric")
  ) |> # Piping the above output into the loop below
  lapply(
    # Looping the ananymous function below on each element of the list returned from above
    # to create a date object using columns "Year", "month" and "day" of each dataset
    \(data = "") {
      data.frame(
        Date = as.Date(
          paste(
            data[ ,grep("[Yy][Ee][Aa][Rr]", names(data), value = TRUE)],
            data[ ,grep("[Mm][Oo][Nn][Tt]", names(data), value = TRUE)],
            data[ ,grep("[Dd][Aa][Yy]", names(data), value = TRUE)],
            sep = "-"
          ),
          format = "%Y-%m-%e"
        ),
      Tmax = data[ ,"tmax"]
      ) |> . =>
        .[.[ ,"Date"] >= "1980-01-01", ]
    }
  ) |> 
  # Renaming the list/output above (column names of each data in the list piped form the expression above)
  setNames(dir(path = "~/Climate_of_Select_Stations/Max_Tmp", pattern = ".csv$")) -> Stations_Tmax # 


# # Creating a dataframe 
# data.frame(
#   Date = as.Date(
#     paste(
#       data[ ,grep("[Yy][Ee][Aa][Rr]", names(data), value = TRUE)], 
#       data[ ,grep("[Mm][Oo][Nn][Tt][Hh]", names(data), value = TRUE)], 
#       data[ ,grep("[Dd][Aa][Yy][Rr]", names(data), value = TRUE)], 
#       sep = "-"
#     ),
#     format = "%Y-%m-%e"
#   ),
#   Tmax = data[ ,"tmax"]
# ) |> . => # Piping the output from above into the expression below 
#   .[.[ ,"Date"] >= "1980-01-01", ] # subseeting years >= 1980


# for efficiency, use rio::import_list instead of combining "lapply" and "read.csv" to read the datasets into R

# # Converting Cape coast Tmax observations to numeric class
# Stations_Tmax[["capecoast_tmax.csv"]][ ,"Tmax"] <- as.numeric(
#   Stations_Tmax[["capecoast_tmax.csv"]][ ,"Tmax"]
# )


# Percentage of Missing Values for each year conditioned on Met Stations ####

# calling `NA_%_1` to loop over the elements of the list "Stations" (Stations is a two level nested list)
lapply(
  Stations_Tmax,
  # Lopping the ananymous function below on the elements of the list "stations_Tmax"
  \(data = "") split(data[ ,"Tmax"], as.factor(format(data[ ,"Date"], "%Y"))) 
) |> # Piping the output from above as input to the `NA_%_1` function in the looping construct below 
  lapply(
    `NA_%_1`
  ) #-> perc_NA_tmax

require(magrittr)


# Computing Annual Maximum Temperature ####
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


#Insering missing year of 2008
dplyr::full_join(Annual_Max_Temp$koforid_tmax.csv, data.frame(
  Year = 1980:2021
)) %>%
  .[order(.[, "Year"]), ] -> Annual_Max_Temp$koforid_tmax.csv

# Setting annual mean temp values for the year 2007 and 2008 from Koforidua because missing data is above 50%
Annual_Max_Temp[["koforid_tmax.csv"]][27:28, 2] <- NA
# Cape Coast
Annual_Max_Temp[["capecoast_tmax.csv"]][c(33, 34, 36), "Tmax"] <- NA

# Imputing missing values the kalman filter for Koforidua
Annual_Max_Temp[["koforid_tmax.csv"]][ ,"Tmax"] <- imputeTS::na_kalman(
  Annual_Max_Temp[["koforid_tmax.csv"]][ ,"Tmax"], 
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)

# Imputing missing values for Cape Coast using kalman filter
Annual_Max_Temp[["capecoast_tmax.csv"]][ ,"Tmax"] <- imputeTS::na_kalman(
  Annual_Max_Temp[["capecoast_tmax.csv"]][ ,"Tmax"], 
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)


# # Plotting Annual Min Temp
# ggplot(data = Annual_Max_Temp[["Koforidua"]], aes(x = Year, y = Tmax)) + 
#   geom_line(lwd = 2, col = "firebrick") +
#   #scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
#   ggtitle("Mean Maximum Temperature of Koforidua \n 1980 - 2021") +
#   xlab("Month") +
#   ylab(expression("Temp ("~degree*C*")")) +
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20)) -> Koforidua_anntmax
# 
# ggplot(data = Annual_Max_Temp[["Navrongo"]], aes(x = Year, y = Tmax)) +
#   geom_line(lwd = 2, col = "firebrick") +
#   #scale_x_date(date_labels = "%Y", date_breaks = "4 years") +
#   ggtitle("Mean Maximum Temperature of Navrongo \n 1980 - 2021") +
#   xlab("Month") +
#   ylab(expression("Temp ("~degree*C*")")) +
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20)) -> Navrongo_anntmax


# Summary Stats for Annual Max Temp 
# Diverting ouput from console to text file 
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Annual Maximum Temperature` =  summary_stats(
    data = Annual_Max_Temp,
    Station = names(Annual_Max_Temp),
    var = "Tmax"
  ) |> . =>
    do.call(rbind, .) 
)

# Returning ouputs to console 
sink()






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
# ggplot(data = Monthly_Tmax[["Koforidua"]], aes(x = Date, y = Tmax)) + 
#   geom_line(lwd = 2, col = "firebrick") +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#   ggtitle("Monthly Maximum Temperature of Koforidua \n 1980 - 2021") +
#   xlab("Month") +
#   ylab(expression("Temp ("~degree*C*")")) +
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20)) -> Koforidua_mnthlytmax
# 
# ggplot(data = Monthly_Tmax[["Navrongo"]], aes(x = Date, y = Tmax)) +
#   geom_line(lwd = 2, col = "firebrick") +
#   scale_x_date(date_labels = "%b", date_breaks = "1 month") +
#   ggtitle("Monthly Maximum Temperature of Navrongo \n 1980 - 2021") +
#   xlab("Month") +
#   ylab(expression("Temp ("~degree*C*")")) +
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20)) -> Navrongo_mnthlytmax

# Summary Stats for mean monthly max temp
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Monthly Maximum Temperature` = summary_stats(
    data = Monthly_Tmax,
    Station = names(Monthly_Tmax),
    var = "Tmax"
  ) |> . =>
    do.call(rbind, .) 
  
)

# Returning ouputs to console 
sink()







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

# Inseting missing year of 2008 for koforidua
Annual_MaxTmp_Anomaly[["koforid_tmax.csv"]] <- 
  dplyr::full_join(
    Annual_MaxTmp_Anomaly[["koforid_tmax.csv"]],
    data.frame(Year = 1980:2021)
) |> . =>
  .[order(.[ ,"Year"]), ] |> . =>
  transform(
    .,
    Deviations = imputeTS::na_kalman(
      .[, "Deviations"],
      smooth = TRUE, 
      nit = -1
    )
  )

# # Koforidua Rainfall Anomaly
# ggplot(data = Annual_MaxTmp_Anomaly[["Koforidua"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
#   geom_linerange(data = Annual_MaxTmp_Anomaly[["koforidua"]], aes(
#     colour = ifelse(Deviations > 0, "Positive", "Negative")),
#     stat = "identity", position = "identity", size = 4) +
#   geom_hline(yintercept = 0) +
#   labs(title = "Max Tmp Anomaly of Koforidua \n 1980 - 2021", x = "Year", y = expression("Temp ("~degree*C*")")) +
#   scale_x_continuous(breaks = seq(from = 1980, to = 2021, by = 7)) +
#   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
#         axis.title = element_text(size = 28, face = "bold"),
#         axis.text = element_text(size = 26),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 23))
# 
# 
# # Navrongo Rainfall Anomaly
# ggplot(data = Annual_MaxTmp_Anomaly[["Navrongo"]], aes(x = Year, y = Deviations, ymin = 0, ymax = Deviations)) +
#   geom_linerange(data = Annual_MaxTmp_Anomaly[["Navrongo"]], aes(
#     colour = ifelse(Deviations > 0, "Positive", "Negative")),
#     stat = "identity", position = "identity", size = 4) +
#   geom_hline(yintercept = 0) +
#   labs(title = "Max Tmp Anomaly of Navrongo \n 1980 - 2021", x = "Year", y = expression("Temp ("~degree*C*")")) +
#   scale_x_continuous(breaks = seq(from = 1944, to = 2021, by = 11)) +
#   theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 30),
#         axis.title = element_text(size = 28, face = "bold"),
#         axis.text = element_text(size = 26),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 23))

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

# Summary Statistics of Maximum Temperature ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Anuual Maximum Temperature Anomaly` = summary_stats(
    data = Annual_MaxTmp_Anomaly,
    Station = names(Annual_MaxTmp_Anomaly),
    var = "Deviations"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)




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
max_maximum_temp[["koforid_tmax.csv"]][
  max_maximum_temp[["koforid_tmax.csv"]][ ,"Year"] == 2007, "Tmax"
] <- NA

# Replacing the years 2021, 2013 an 2015 with NAs
max_maximum_temp[["capecoast_tmax.csv"]][
  max_maximum_temp[["capecoast_tmax.csv"]][ ,"Year"] %in% c(2012, 2013, 2015), "Tmax"
] <- NA

# Imputing missing values in the Koforidua max tmp dataframe with the max_maximum_temp list
max_maximum_temp[["koforid_tmax.csv"]][ ,"Tmax"] <- imputeTS::na_kalman(
    max_maximum_temp[["koforid_tmax.csv"]][ ,"Tmax"],
    model = "StructTS", 
    smooth = TRUE,
    nit = -1
  )

# Imputing missing values in the capecoast max tmp dataframe with the max_maximum_temp list
max_maximum_temp[["capecoast_tmax.csv"]][ ,"Tmax"] <- imputeTS::na_kalman(
  max_maximum_temp[["capecoast_tmax.csv"]][ ,"Tmax"],
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)



# # Plotting 
# ggplot(data = max_maximum_temp[["Koforidua"]], aes(x = Year, y = Tmax)) +
#   geom_line(col = "firebrick", lwd = 2) +
#   scale_x_continuous(breaks = seq(1980, 2021, by = 5)) +
#   labs(
#     title = "Number of Days with Max Tmp in the 90th percentile \n 1980 - 2021", 
#     x = "Year", 
#     y = "Number of Days"
#   ) + 
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20))

# Summary Statistics of Maximum Temperature ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Maximum of Daily Temperature` = summary_stats(
    data = max_maximum_temp,
    Station = names(max_maximum_temp),
    var = "Tmax"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)






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
min_maximum_temp[["koforid_tmax.csv"]][
  min_maximum_temp[["koforid_tmax.csv"]][ ,"Year"] == 2007, "Tmax"
] <- NA

# Replacing the years 2012, 2013, and 2015 Tmaxs with NA
min_maximum_temp[["capecoast_tmax.csv"]][
  min_maximum_temp[["capecoast_tmax.csv"]][ ,"Year"] %in% c(2012, 2013, 2015), "Tmax"
] <- NA

# Imputing missing values in the Koforidua max tmp dataframe with the max_maximum_temp list
min_maximum_temp[["koforid_tmax.csv"]][ ,"Tmax"] <- imputeTS::na_kalman(
  min_maximum_temp[["koforid_tmax.csv"]][ ,"Tmax"],
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)

# Imputing missing values in the cape coast max tmp dataframe with the max_maximum_temp list
min_maximum_temp[["capecoast_tmax.csv"]][ ,"Tmax"] <- imputeTS::na_kalman(
  min_maximum_temp[["capecoast_tmax.csv"]][ ,"Tmax"],
  model = "StructTS", 
  smooth = TRUE,
  nit = -1
)


# Plotting 
# ggplot(data = min_maximum_temp[["Koforidua"]], aes(x = Year, y = Tmax)) +
#   geom_line(col = "firebrick", lwd = 2) +
#   scale_x_continuous(breaks = seq(1980, 2021, by = 5)) +
#   labs(
#     title = "Number of Days with Max Tmp in the 10th percentile \n 1980 - 2021", 
#     x = "Year", 
#     y = "Number of Days"
#   ) + 
#   theme(plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
#         axis.title = element_text(size = 22, face = "bold"),
#         axis.text = element_text(size = 20))


# Summary Statistics of Maximum Temperature ####
sink("Data_outputs/summary statisitcs.txt", append = TRUE, split = TRUE)

list(
  `Minimum of Daily Temperature` = summary_stats(
    data = min_maximum_temp,
    Station = names(min_maximum_temp),
    var = "Tmax"
  ) |> . =>
    do.call(rbind, .)
)

sink(file = NULL)









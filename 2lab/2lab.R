# install.packages("ggplot2", repos = "https://cloud.r-project.org/")
library(gplots)

lab2 <- function () {
  # Getting dataset
  travel_data <- read.csv("Лучшие имена.csv", fileEncoding = "windows-1251", header=TRUE, sep=";")
  
  # Select columns 3 to 12 of the DataFrame
  selected_columns <- travel_data[, 3:12]
  
  min_values <- apply(selected_columns, 2, min, na.rm=TRUE)
  max_values <- apply(selected_columns, 2, max, na.rm=TRUE)
  mean_values <- apply(selected_columns, 2, mean, na.rm=TRUE)
  print("min")
  print(min_values)
  print("max")
  print(max_values)
  print("mean")
  print(mean_values)
  
  
  # Count number of rows where mean value is greater than 7 out of 10
  
  moreThan7 <- sum(apply(selected_columns, 1, mean, na.rm=TRUE) > 0.7 * 10)
  # Count number of rows where mean value is less than 3 out of 10
  lessThan3 <- sum(apply(selected_columns, 1, mean, na.rm=TRUE) < 0.3 * 10)
  print("more_then_7")
  print(moreThan7)
  print("less_then_3")
  print(lessThan3)
  
  sorted_cols <- names(selected_columns)[order(-mean_values)]
  print("Raitng")
  print(sorted_cols)
  

  # Calculate the mean rating and standard deviation for each column
  mean_ratings <- sort(apply(selected_columns, 2, mean), decreasing = TRUE)
  sd_ratings <- apply(selected_columns, 2, sd)
  
  # Define the color scale
  my_palette <- colorRampPalette(c("green", "yellow", "red"))
  
  # Create a bar plot with the color scale and error bars
  barplot2(mean_ratings, main = "Рейтинг имён", 
           xlab = "Имена", ylab = "Рейтинг",
           col=my_palette(length(mean_ratings)))
  
  # Add error bars to the plot
  arrows(x0=seq(0.7, 12, by = 1.2), y0=mean_ratings - sd_ratings, 
         x1=seq(0.7, 12, by = 1.2), y1=mean_ratings , 
         code=1, length=0.1, angle=90, lwd=1)
  
}

lab2()
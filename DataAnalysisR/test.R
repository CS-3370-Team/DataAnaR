library(tibble)
library(ggplot2)
library(plotly)
library(readr)

install.packages("readr")

plot_3d = function(df, x_col, y_col, z_col) {
  # Retrieve the data (assuming it's a tibble)
  #df = private$df$get_data()  # This should return a tibble

  # Ensure the specified columns exist in the tibble
  if (!(all(c(x_col, y_col, z_col) %in% colnames(df)))) {
    stop("One or more specified columns do not exist in the dataframe.")
  }

  # Print the column names for debugging
  print(x_col)
  print(y_col)
  print(z_col)

  # Create the 3D scatter plot using tibble columns dynamically
  p = plotly::plot_ly(
    data = df,
    x = ~df[[x_col]],  # Dynamically reference tibble columns with [[]]
    y = ~df[[y_col]],
    z = ~df[[z_col]],
    type = "scatter3d",
    mode = "markers"
  )

  message("✅ 3D scatter plot created.")
  return(p)
}

library(DataAnalysisR)

# Create a new DataFrame object
test_df <- DataFrame$new()

# Run tests on the DataFrame object
test_df$test()

# Import data from a CSV file into the DataFrame
file_path <- "~/Desktop/CS-3370/Project Space With R/R-Project-3370/energy_dataset.csv"
test_df$import_from_file(file_path)


df = test_df$get_data()
check_df = read_csv("~/Desktop/CS-3370/Project Space With R/R-Project-3370/energy_dataset.csv")
check_df$`generation biomass`

plot_3d(df, x_col="generation biomass", y_col="generation fossil brown coal/lignite", z_col="generation fossil gas")

class(check_df)

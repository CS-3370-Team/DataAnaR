# DataAnaR

DataAnaR is an R package designed for Data Analysis, developed for the CS-3370 course at Hanoi University of Science and Technology (HUST). This package offers a variety of tools to efficiently handle, visualize, and connect to data from various sources. Currently, it includes four core modules:

## 1. DataFrame Module (Completed)

The DataFrame module allows users to work with tabular data in a flexible and intuitive way. It includes features such as creating, manipulating, and filtering data stored in DataFrame objects. Users can easily transform data, add or remove columns, perform basic filtering, and export the data to various formats like CSV, JSON, and more.

## 2. BeautifulTable Module (Completed)

The BeautifulTable module will help users create aesthetically pleasing tables. This module will enable customization of table appearance, including alignment, formatting, and color schemes, for enhanced readability and presentation.

## 3. Visualizer Module (Completed)

The Visualization module will provide interactive data visualizations, helping users to easily interpret their data through charts, graphs, and other visual representations. This module will support various types of visualizations and allow users to interact with the charts to explore the data in more depth.

## 4. DatabaseConnect Module (Completed)

The DataBaseUtil module will facilitate easy database connectivity. Users will be able to connect to different databases (include MySQL and Azure SQL, PostGresSQL), fetch data, and perform various operations like querying and storing data, making it easier to work with large datasets directly from a database.


## Contributing

We welcome contributions! If you’d like to improve the package, you can fork the repository, make your changes, and submit a pull request. Feel free to open issues if you encounter any bugs or have suggestions.

Contributors:

	•	Dat Tran Tien (Daves Tran) – Team Leader & Contributor for the DataFrame Module
 
	•	Nguyen Hai An – Contributor for the Visualization Module
 
	•	Nguyen Kim Tuan Cuong – Contributor for the BeautifulTable and DataBaseUtil Modules
 
	•	Nguyen Khanh Viet Dung – Contributor for the DataBaseUtil Module

## How to use the package

## 🔧 Installation

1. **Install `devtools`** (if it's not already installed):

```r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
```
2.	Install the package from your local .tar.gz file:
```r
install.packages("path/to/DataAnalysisR_0.0.0.9000.tar.gz", repos = NULL, type = "source")
```
3.  Load the Package 
```r
library(DataAnalysisR)
```
Now you’re ready to use the tools provided by DataAnalysisR!

4.  Example on how to use the package
```r

# Create a new DataFrame object
test_df <- DataFrame$new()

# Run tests on the DataFrame object
test_df$test()

# Import data from a CSV file into the DataFrame
file_path <- ".../weather_features.csv" # Replace with your actual CSV file
test_df$import_from_file(file_path)

# Display basic statistics about the DataFrame
test_df$describe()

# Show data preview (first few rows)
test_df$show_dp()

# Create a BeautifulTable object and print the table in various formats
beautiful_tab <- BeautifulTable$new(test_df)
beautiful_tab$print_the_table("simple")
beautiful_tab$print_the_table("formattable")
beautiful_tab$print_the_table("pander")
beautiful_tab$print_the_table("markdown")

# Create a Visualizer object and generate visualizations
visual <- Visualizer$new(test_df)

# Plot interactive visualization
visual$plot_interactive()

# Plot a scatter plot with specified columns
visual$plot_scatter(x_col = "temp", y_col = "wind_speed")
visual$plot_line(x_col = "temp", y_col = "wind_speed")
visual$plot_box( y_col = "temp")
visual$plot_line(x_col = "temp", y_col = "humidity")
visual$plot_histogram(x_col="wind_speed")

# Plot a 3D scatter plot with specified columns
visual$plot_3d(x_col = "temp", y_col = "humidity", z_col = "wind_speed")

visual$plot_interactive()
visual$plot_heatmap(x_col = "generation biomass", y_col = "generation nuclear", value_col = "generation nuclear")

# Initialize the DatabaseConnector
db <- DatabaseConnector$new()

# Connect to a MySQL database
db$connect_db("mysql", host = "localhost", port = 3306, dbname = "test_db", user = "root", password = "password")

# Fetch a table as a tibble
users <- db$fetch_table_as_dataframe("users")
print(users$get_data())

# Add a new record to the table
db$add_record("users", list(name = "John Doe", age = 30, email = "john.doe@example.com"))

# Remove a record from the table
db$remove_record("users", "name", "John Doe")

# Disconnect from the database
db$disconnect()

```


## License

This project is licensed under the MIT License - see the LICENSE file for details.



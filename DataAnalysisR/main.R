
devtools::clean_dll()
devtools::document()
devtools::build()
devtools::install()
devtools::load_all() # must run this
cat("\014") #- clean the console

df = DataFrame$new(
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 35, 40, 45),
  Score = c(88, 92, 95, 78, 85)
)

df$test()

visualizer_test = function(test_type = "all") {
  message("🔎 Initializing DataFrame...")
  df = DataFrame$new(
    Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
    Age = c(25, 30, 35, 40, 45),
    Score = c(88, 92, 95, 78, 85)
  )
  message("✅ DataFrame initialized successfully.\n")

  # Visualizer object
  message("🔎 Creating Visualizer object...")
  visualizer = Visualizer$new(df)
  message("✅ Visualizer object created.\n")

  # Define possible test cases
  switch(test_type,

         "scatter" = {
           message("🔎 Testing Scatter Plot (Age vs Score)...")
           result = visualizer$plot_scatter(x_col = "Age", y_col = "Score")
           print(result)
           if (!is.null(result)) message("✅ Scatter plot test passed.\n") else message("❌ Scatter plot test failed.\n")
         },

         "line" = {
           message("🔎 Testing Line Plot (Age vs Score)...")
           result = visualizer$plot_line(x_col = "Age", y_col = "Score")
           print(result)
           if (!is.null(result)) message("✅ Line plot test passed.\n") else message("❌ Line plot test failed.\n")
         },

         "bar" = {
           message("🔎 Testing Bar Plot (Name vs Score)...")
           result = visualizer$plot_bar(x_col = "Name", y_col = "Score")
           print(result)
           if (!is.null(result)) message("✅ Bar plot test passed.\n") else message("❌ Bar plot test failed.\n")
         },

         "histogram" = {
           message("🔎 Testing Histogram (Age)...")
           result = visualizer$plot_histogram(x_col = "Age", bins = 5)
           print(result)
           if (!is.null(result)) message("✅ Histogram test passed.\n") else message("❌ Histogram test failed.\n")
         },

         "box" = {
           message("🔎 Testing Box Plot (Score)...")
           result = visualizer$plot_box(y_col = "Score")
           print(result)
           if (!is.null(result)) message("✅ Box plot test passed.\n") else message("❌ Box plot test failed.\n")
         },

         "heatmap" = {
           message("🔎 Testing Heatmap (Name, Age, Score)...")
           result = visualizer$plot_heatmap(x_col = "Name", y_col = "Age", value_col = "Score")
           print(result)
           if (!is.null(result)) message("✅ Heatmap test passed.\n") else message("❌ Heatmap test failed.\n")
         },

         "multiple" = {
           message("🔎 Creating two plots for multiple plot test...")
           p1 = visualizer$plot_line(x_col = "Age", y_col = "Score")
           p2 = visualizer$plot_bar(x_col = "Name", y_col = "Score")
           message("🔎 Testing Multiple Plot Combination...")
           result = visualizer$plot_multiple(plot_list = list(p1, p2))
           print(result)
           if (!is.null(result)) message("✅ Multiple plot combination test passed.\n") else message("❌ Multiple plot combination test failed.\n")
         },

         "3d" = {
           message("🔎 Testing 3D Scatter Plot (Age, Score, Name)...")
           result = visualizer$plot_3d(x_col = "Age", y_col = "Score", z_col = "Name")
           print(result)
           if (!is.null(result)) message("✅ 3D scatter plot test passed.\n") else message("❌ 3D scatter plot test failed.\n")
         },

         "export" = {
           message("🔎 Testing Plot Export (Line plot to PNG)...")
           p = visualizer$plot_line(x_col = "Age", y_col = "Score")
           visualizer$export_plot(plot = p, filename = "line_plot", format = "png")
           message("✅ Plot export test completed (check the working directory for 'line_plot.png').\n")
         },

         "all" = {
           message("🔎 Running all plot tests...\n")
           visualizer_test("scatter")
           visualizer_test("line")
           visualizer_test("bar")
           visualizer_test("histogram")
           visualizer_test("box")
           visualizer_test("heatmap")
           visualizer_test("multiple")
           visualizer_test("3d")
           visualizer_test("export")
           message("🎉 All tests completed!")
         },

         { # Default case
           message("❌ Unknown test type: ", test_type, ". Please use one of: scatter, line, bar, histogram, box, heatmap, multiple, 3d, export, all.\n")
         }
  )
}

"""
Please use one of: scatter, line, bar, histogram, box, heatmap, multiple, 3d, export, all

"""
visualizer_test(test_type ="export")


# Create a DataFrame with mixed data types
df <- DataFrame$new(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(88, 92, 75),
  Pass = c(TRUE, TRUE, FALSE)
)

# Create a BeautifulTable object
bt <- BeautifulTable$new(df)

# Print the table with formattable styling
message("=== Printing Formatted Table ===")
bt$print_the_table(style = "formattable", numeric_color = "lightblue", bold_headers = TRUE)

# Export to CSV and JSON
message("=== Exporting to CSV and JSON ===")
bt$export_to_csv_and_json(base_filename = "beautiful_table")

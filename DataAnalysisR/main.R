
devtools::document()  # Updates the NAMESPACE
devtools::install()   # Reinstalls the package
devtools::install(".")
# cat("\014") - clean the console

devtools::load_all()
library(DataAnalysisR)
df =  DataFrame$new(
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 35, 40, 45),
  Score = c(88, 92, 95, 78, 85)
)

df$import_from_file("export.csv")

df$export_to_file(format="feather", filename= "export")

df$get_data()

filtering = df$filter(query=  "Age > 30 & Score > 88")
filtering$show_dp()
df$describe()

df$test()


test_tibble =  tibble(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(88, 92, 95),
  ID = c(0,1,2)
)


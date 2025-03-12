
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

df$test()

sol = df$delete(delRow = FALSE, col_names = c("Name","Score"), row_indexes = c(1,2))
print(sol)

df$get_data()


test_tibble =  tibble(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35),
  Score = c(88, 92, 95),
  ID = c(0,1,2)
)

test_tibble

test_tibble <- test_tibble %>% mutate(col_name = NULL)
print(test_tibble)

test_tibble = test_tibble %>% slice(-2) # Delete a row

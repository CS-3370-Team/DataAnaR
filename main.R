# Example Usage
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages("languageserver")

install.packages("DataAnalysisR", repos = NULL, type = "source")
library("DataAnalysisR")
# Example Usage
df <- DataFrame$new(c(10, 20, 30), c("A", "B", "C"))
df$show_dp()

data_frame = data.frame(c(10, 20, 30))

print(data_frame)

ls()
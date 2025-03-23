DatabaseConnector <- R6Class("DatabaseConnector",
                             public = list(
                               con = NULL,  # Connection object

                               # Constructor
                               initialize = function() {
                                 self$con <- NULL
                               },

                               # Connect to a database with parameters
                               connect_db = function(db_type, host, port, dbname, user, password, sid = NULL) {
                                 tryCatch({
                                   if (db_type == "mysql") {
                                     self$con <- dbConnect(MariaDB(),
                                                           host = host,
                                                           port = port,
                                                           dbname = dbname,
                                                           user = user,
                                                           password = password)
                                     cat("MySQL connection successful!\n")
                                   } else if (db_type == "oracle") {
                                     drv <- Oracle()
                                     self$con <- dbConnect(drv,
                                                           username = user,
                                                           password = password,
                                                           dbname = paste0("(DESCRIPTION=(ADDRESS_LIST=(ADDRESS=(PROTOCOL=TCP)(HOST=", host, ")(PORT=", port, "))) (CONNECT_DATA=(SID=", sid, ")))"))
                                     cat("Oracle SQL connection successful!\n")
                                   } else if (db_type == "azure") {
                                     driver <- "ODBC Driver 17 for SQL Server"
                                     self$con <- dbConnect(odbc::odbc(),
                                                           Driver = driver,
                                                           Server = host,
                                                           Database = dbname,
                                                           UID = user,
                                                           PWD = password)
                                     cat("Azure SQL connection successful!\n")
                                   } else {
                                     stop("Unsupported database type.")
                                   }
                                   return(TRUE)
                                 }, error = function(e) {
                                   cat("Error connecting to database: ", e$message, "\n")
                                   return(FALSE)
                                 })
                               },

                               fetch_table_as_dataframe = function(table_name) {
                                 if (is.null(self$con)) {
                                   stop("No active database connection.")
                                 }

                                 query <- paste("SELECT * FROM", table_name)

                                 tryCatch({
                                   data <- dbGetQuery(self$con, query)  # Fetch data from the database

                                   # Ensure the data has valid column names
                                   if (is.null(colnames(data)) || any(colnames(data) == "")) {
                                     stop("Error: The fetched table does not contain valid column headers. Please check the database schema.")
                                   }

                                   # Convert to tibble (ensures compatibility with DataFrame)
                                   data_tibble <- as_tibble(data)

                                   # Convert tibble to DataFrame
                                   df <- DataFrame$new(input_table = data_tibble)
                                   return(df)
                                 }, error = function(e) {
                                   cat("Error fetching table: ", e$message, "\n")
                                   return(NULL)
                                 })
                               },

                               add_record = function(table_name, data_list) {
                                 if (is.null(self$con)) {
                                   stop("No active database connection.")
                                 }

                                 # Ensure column names and values are properly formatted
                                 columns <- paste(names(data_list), collapse = ", ")
                                 values <- paste(sapply(data_list, function(x) if (is.numeric(x)) x else paste0("'", x, "'")), collapse = ", ")

                                 query <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table_name, columns, values)

                                 tryCatch({
                                   rows_affected <- dbExecute(self$con, query)
                                   if (rows_affected > 0) {
                                     cat("Record added successfully to", table_name, "\n")
                                   } else {
                                     cat("Failed to add record to", table_name, "\n")
                                   }
                                 }, error = function(e) {
                                   cat("Error inserting record: ", e$message, "\n")
                                 })
                               },

                               remove_record = function(table_name, col, value) {
                                 if (is.null(self$con)) {
                                   stop("No active database connection.")
                                 }

                                 # Properly format the query based on data type
                                 if (is.numeric(value)) {
                                   query <- sprintf("DELETE FROM %s WHERE %s = %s", table_name, col, value)
                                 } else {
                                   query <- sprintf("DELETE FROM %s WHERE %s = '%s'", table_name, col, value)
                                 }

                                 tryCatch({
                                   rows_affected <- dbExecute(self$con, query)

                                   if (rows_affected > 0) {
                                     cat("Record removed successfully from", table_name, "\n")
                                   } else {
                                     cat("No matching record found in", table_name, "\n")
                                   }

                                 }, error = function(e) {
                                   cat("Error deleting record: ", e$message, "\n")
                                 })
                               },


                               # Disconnect from the database
                               disconnect = function() {
                                 if (!is.null(self$con)) {
                                   dbDisconnect(self$con)
                                   self$con <- NULL
                                   cat("Disconnected from database.\n")
                                 } else {
                                   cat("No active database connection to disconnect.\n")
                                 }
                               }
                             )
)

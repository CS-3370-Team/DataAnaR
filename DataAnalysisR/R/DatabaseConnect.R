#' @title DatabaseConnector
#' @description R6 Class for connecting and interacting with various databases (MySQL,Azure SQL).
#' @field con Database connection object (NULL if disconnected)
#' @importFrom R6 R6Class
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbExecute
#' @importFrom RMariaDB MariaDB
#' @importFrom odbc odbc
#' @importFrom tibble as_tibble
#' @importFrom RPostgres Postgres
#' @export
DatabaseConnector <- R6::R6Class("DatabaseConnector",
                             public = list(
                               con = NULL,  # Connection object

                               #' @description Initialize the connector (no active connection)
                               initialize = function() {
                                 self$con <- NULL
                               },

                               #' @description Connect to the database
                               #' @param db_type Type of database: "mysql", "azure", "PostGres"
                               #' @param host Host address
                               #' @param port Port number (optional)
                               #' @param dbname Database name
                               #' @param user Username
                               #' @param password Password
                               #' @param in_drv Input Driver File (for Azure only)
                               #' @return TRUE if connection successful, FALSE otherwise
                               connect_db = function(db_type, host, port, dbname, user, password, in_drv = NULL) {
                                 tryCatch({
                                   if (db_type == "mysql") {
                                     self$con <- DBI::dbConnect(MariaDB::MariaDB(),
                                                           host = host,
                                                           port = port,
                                                           dbname = dbname,
                                                           user = user,
                                                           password = password)
                                     cat("MySQL connection successful!\n")
                                   } else if (db_type == "azure") {
                                     if (is.null(in_drv)) {
                                       driver <- "ODBC Driver 18 for SQL Server"
                                     } else {
                                       driver = in_drv
                                     }
                                     self$con <- DBI::dbConnect(odbc::odbc(),
                                                                Driver = driver,
                                                                Server = host,
                                                                Database = dbname,
                                                                UID = user,
                                                                PWD = password,
                                                                Encrypt = "yes",
                                                                TrustServerCertificate = "no",
                                                                ConnectionTimeout = 30)
                                     cat("Azure SQL connection successful!\n")
                                   }
                                   else if(db_type == "PostGres") {
                                         self$con <- DBI::dbConnect(
                                           RPostgres::Postgres(),
                                           dbname = dbname,
                                           host = host,
                                           port = port,
                                           user = user,
                                           password = password
                                         )
                                        cat("PostgresSQL connection successful!\n")
                                   } else {
                                     stop("Unsupported database type.")
                                   }
                                   return(TRUE)
                                 }, error = function(e) {
                                   cat("Error connecting to database: ", e$message, "\n")
                                   return(FALSE)
                                 })
                               },

                               #' @description Fetch a table as a DataFrame (tibble)
                               #' @param table_name Name of the table
                               #' @return tibble with data
                               fetch_table_as_dataframe = function(table_name) {
                                 if (is.null(self$con)) {
                                   stop("No active database connection.")
                                 }

                                 query <- paste("SELECT * FROM", table_name)

                                 tryCatch({
                                   data <- DBI::dbGetQuery(self$con, query)  # Fetch data from the database

                                   # Ensure the data has valid column names
                                   if (is.null(colnames(data)) || any(colnames(data) == "")) {
                                     stop("Error: The fetched table does not contain valid column headers. Please check the database schema.")
                                   }

                                   # Convert to tibble (ensures compatibility with DataFrame)
                                   data_tibble <- tibble::as_tibble(data)

                                   # Convert tibble to DataFrame
                                   df <- DataFrame$new(input_table = data_tibble)
                                   return(df)
                                 }, error = function(e) {
                                   cat("Error fetching table: ", e$message, "\n")
                                   return(NULL)
                                 })
                               },

                               #' @description Add a record to a table
                               #' @param table_name Name of the table
                               #' @param data_list Named list of column = value pairs
                               add_record = function(table_name, data_list) {
                                 if (is.null(self$con)) {
                                   stop("No active database connection.")
                                 }

                                 # Ensure column names and values are properly formatted
                                 columns <- paste(names(data_list), collapse = ", ")
                                 values <- paste(sapply(data_list, function(x) if (is.numeric(x)) x else paste0("'", x, "'")), collapse = ", ")

                                 query <- sprintf("INSERT INTO %s (%s) VALUES (%s)", table_name, columns, values)

                                 tryCatch({
                                   rows_affected <- DBI::dbExecute(self$con, query)
                                   if (rows_affected > 0) {
                                     cat("Record added successfully to", table_name, "\n")
                                   } else {
                                     cat("Failed to add record to", table_name, "\n")
                                   }
                                 }, error = function(e) {
                                   cat("Error inserting record: ", e$message, "\n")
                                 })
                               },

                               #' @description Remove a record from a table
                               #' @param table_name Name of the table
                               #' @param col Column name for condition
                               #' @param value Value to match
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
                                   rows_affected <- DBI::dbExecute(self$con, query)

                                   if (rows_affected > 0) {
                                     cat("Record removed successfully from", table_name, "\n")
                                   } else {
                                     cat("No matching record found in", table_name, "\n")
                                   }

                                 }, error = function(e) {
                                   cat("Error deleting record: ", e$message, "\n")
                                 })
                               },

                               #' @description Disconnect from the database
                               disconnect = function() {
                                 if (!is.null(self$con)) {
                                   DBI::dbDisconnect(self$con)
                                   self$con <- NULL
                                   cat("Disconnected from database.\n")
                                 } else {
                                   cat("No active database connection to disconnect.\n")
                                 }
                               }
                             )
)

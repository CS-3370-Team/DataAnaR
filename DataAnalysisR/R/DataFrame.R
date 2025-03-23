#' @description A custom R6 class for handling tabular data using tibbles.
#' It provides methods for data manipulation, filtering, exporting, and more.
#'
#' @importFrom data.table data.table
#' @importFrom tibble as_tibble is_tibble
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate select all_of cur_column bind_rows bind_cols intersect
#' @importFrom magrittr %>%
#' @importFrom rlang sym type_of
#' @importFrom skimr skim
#' @importFrom readr write_csv write_tsv
#' @importFrom writexl write_xlsx
#' @importFrom jsonlite write_json
#' @importFrom arrow write_parquet write_feather
#' @importFrom tools file_ext
#'
#' @export
DataFrame = R6::R6Class("DataFrame",
                         private = list(
                           data = NULL
                         ),

                         public = list(
                                #' @description Initializes the DataFrame with named vectors as columns.
                                #' @param ... Named vectors that form the columns of the DataFrame.
                                #' @export

                                initialize = function(...) {
                                   input_list <- list(...)  # Capture all arguments into a list
                                   private$data <- tibble::as_tibble(input_list)  # Convert to tibble
                                 },

                                #' @description Displays the stored tibble.
                                #' @export
                                show_dp = function() {
                                 print(private$data)  # Print the stored tibble
                               },

                                #' @description Retrieves the entire stored tibble.
                                #' @return A tibble containing all the stored data.
                                #' @export
                                get_data = function() {
                                 return(private$data)
                               },
                                 #' @description
                                 #' Retrieves the column names from the private `data` field.
                                 #'
                                 #' @return
                                 #' A character vector containing the names of the columns in the `data` tibble.
                                 #'
                                 #' @examples
                                 #' \dontrun{
                                 #'   obj <- MyClass$new(data = tibble::tibble(a = 1, b = 2))
                                 #'   obj$get_col_names()
                                 #' }
                                get_col_names = function() {
                                 return(colnames(private$data))
                                },
                                #' @description the setter of the data
                                #' @return NULL
                                #' @param input_table - a tibble or any object can convert to tibble
                                #' @export
                                set_data = function(input_table) {
                                  if (!tibble::is_tibble(input_table)) {
                                    input_table = tibble::as_tibble(input_table)  # Ensure it's a tibble
                                  }
                                  private$data = input_table
                                  return(NULL)
                                },

                               #' @description Retrieves one or more columns by name.
                               #' @param colName A string or a vector of strings specifying the column(s) to retrieve.
                               #' @return A tibble containing the selected columns.
                               #' throws an Error if `colName` is not a character or if a specified column does not exist.
                               #' @export
                               get_col = function(colName) {
                                  if (!is.character(colName)) {
                                    stop("colName must be a string or a list of strings")
                                  }
                                  if (!all(colName %in% colnames(private$data))) {
                                    stop("One or more columns do not exist in private$data")
                                  }

                                  # Return selected columns
                                  return(private$data[, colName, drop = FALSE])
                              },

                              #' @description Retrieves the dimensions of the table.
                              #' @return A numeric vector containing the number of rows and columns.
                              #' @details Also prints a message with the table size.
                              #' @export
                              get_size = function() {
                              rows = nrow(private$data)
                              cols = ncol(private$data)
                              message(sprintf("Table has %d rows and %d columns.", rows, cols))
                              return(c(rows, cols))  # Correctly return a numeric vector
                             },


                              #' @description Selects specific rows and columns from the DataFrame.
                              #' @param id_row A numeric vector of row indices to select. Defaults to all rows if NULL.
                              #' @param id_col A numeric vector of column indices to select. Defaults to all columns if NULL.
                              #' @return A tibble subset of `private$data`.
                              #' throws an  Error if row or column indices exceed table dimensions.
                              select = function(id_row = NULL, id_col = NULL) {
                               table_size = self$get_size()  # Get table dimensions

                               # If id_row is NULL, select all rows
                               if (is.null(id_row)) {
                                 id_row = seq_len(table_size[1])  # Select all row indices
                               }

                               # If id_col is NULL, select all columns
                               if (is.null(id_col)) {
                                 id_col = seq_len(table_size[2])  # Select all column indices
                               }

                               # Ensure id_row and id_col are valid
                               if (any(id_row > table_size[1])) {
                                 stop("Exceed the number of rows in the table")
                               }
                               if (any(id_col > table_size[2])) {
                                 stop("Exceed the number of columns in the table")
                               }

                               # Select the data
                               return(private$data[id_row, id_col, drop = FALSE])

                             },

                              #' @description Delete a row or column in the DataFrame
                              #' @param delRow Logical. If TRUE, delete rows. If FALSE, delete columns.
                              #' @param col_names Character vector. Column names to delete (used if `delRow = FALSE`).
                              #' @param row_indexes Integer vector. Row indices to delete (used if `delRow = TRUE`).
                              #' @return the modified table
                              #' @export
                              delete = function(delRow = FALSE, col_names = NULL, row_indexes = NULL) {
                              # Delete columns if delRow is FALSE
                              if (!delRow) {
                                if (!is.null(col_names)) {
                                  if (!all(col_names %in% colnames(private$data))) {
                                    stop("Error: One or more specified columns do not exist in the table.")
                                  }
                                  private$data = dplyr::select(private$data, -dplyr::all_of(col_names))
                                  message(sprintf("✅ Successfully deleted column(s): %s", toString(col_names)))
                                } else {
                                  message("⚠️ Warning: `col_names` is NULL. No columns were deleted.")
                                }
                              }
                              # Delete rows if delRow is TRUE
                              else {
                                if (!is.null(row_indexes)) {
                                  if (any(row_indexes > nrow(private$data) | row_indexes < 1)) {
                                    stop("Error: One or more row indexes are out of bounds.")
                                  }
                                  private$data <- private$data[-row_indexes, , drop = FALSE]
                                  message(sprintf("✅ Successfully deleted row(s): %s", toString(row_indexes)))
                                }
                                else {
                                  stop("Error: `row_indexes` is NULL. No rows were deleted.")
                                }
                              }
                              return(private$data)
                            },


                              #' @description Adds a new row or column to the DataFrame.
                              #'
                              #' @param addRow Logical. If TRUE, a new row is added; if FALSE, a new column is added.
                              #' @param added_row A named vector representing the row to be added. The names should match column names in the DataFrame.
                              #' @param added_col A vector representing the column to be added. Must have a length equal to the number of rows in the DataFrame.
                              #' @param new_col_name A string specifying the name of the new column to be added.
                              #'
                              #' @details
                              #' - If `addRow = TRUE` and `added_row` is provided, the function adds a new row with matching column names.
                              #' - If column names are missing in `added_row`, they are automatically filled with `NA` values.
                              #' - The function ensures that new row values match the data types of the existing columns.
                              #' - If `added_row` is NULL, an empty row (filled with `NA`) is added.
                              #'
                              #' - If `addRow = FALSE`, the function attempts to add a new column.
                              #' - `added_col` must be a vector with a length equal to the number of existing rows.
                              #' - The function ensures the column is correctly named and matches existing data structures.
                              #'
                              #' @return The modified DataFrame with the new row or column added.
                              #'
                              #' throws
                              #' - If `added_row` is not a vector.
                              #' - If `added_col` has an incorrect length.
                              #' - If `new_col_name` is missing when adding a new column.
                              #'
                              #' @examples
                              #' df$add_a_new_entry(addRow = TRUE, added_row = c(Name = "Emy", Age = 45, Score = 65))
                              #' df$add_a_new_entry(addRow = FALSE, new_col_name = "Check", added_col = c(TRUE, FALSE, TRUE))
                              #' @export
                              add_a_new_entry = function(addRow = FALSE, added_row = NULL, added_col = NULL, new_col_name = NULL) {
                              if (addRow) { # add the row
                                if (!is.null(added_row)) {  # Check if the added row is not NULL
                                    if (is.vector(added_row)) { # Add row - flexibly - if the added_row is not null

                                      # Ensure the new row has the same column names
                                      added_row <- as.list(added_row)  # Convert to named list
                                      missing_cols <- setdiff(colnames(private$data), names(added_row))

                                      # Fill missing columns with NA
                                      for (col in missing_cols) {
                                        added_row[[col]] <- NA
                                      }

                                      # Convert to tibble and match column types
                                      added_row = tibble::as_tibble(added_row)
                                      added_row = dplyr::mutate(
                                        added_row,
                                        dplyr::across(
                                          colnames(private$data),
                                          ~get(paste0("as.", class(private$data[[dplyr::cur_column()]])))(.)
                                        )
                                      )

                                      # Bind rows
                                      private$data <- dplyr::bind_rows(private$data, added_row)
                                      message("✅ Successfully added a new row!")
                                    }
                                  }
                                else {
                                  empty_row = tibble::tibble(!!!setNames(rep(NA, ncol(private$data)), colnames(private$data)))
                                  private$data = dplyr::bind_rows(private$data, empty_row)
                                  message("✅ Successfully added a new empty row!")
                                      }
                              }
                              else { # Add the column
                                if (!is.null(added_col) && !is.null(new_col_name)) {  # Check for column addition
                                  if (is.vector(added_col) && length(added_col) == nrow(self$get_data())) {
                                    # Ensure it's a vector and matches row count
                                    private$data <- dplyr::mutate(private$data, !!rlang::sym(new_col_name) := added_col)
                                    message("✅ Successfully added a new column: ", new_col_name)
                                    }
                                  else {
                                    stop("❌ Error: The column must be a vector with length = ", nrow(self$get_data()))
                                    }
                                }
                                else {
                                  stop("❌ Error: Cannot fill the column with empty name or not declared value")
                                }

                              }
                            },

                            #' @title Add Multiple Rows or Columns to the Data Table
                            #'
                            #' @description This function allows adding multiple rows or multiple columns to the data table.
                            #' It ensures that new rows match the existing column names and structure, and new columns are
                            #' provided in a valid tibble format.
                            #'
                            #' @param add_rows Logical; if `TRUE`, new rows are added. If `FALSE`, new columns are added.
                            #' @param new_cols A tibble containing new columns to be added to the data table. Must be a valid tibble.
                            #' @param new_rows A tibble containing new rows to be added to the data table. The column names must match
                            #' the existing data structure.
                            #'
                            #' @details
                            #' - If `add_rows = FALSE`, `new_cols` must be a non-null tibble with new columns to be added.
                            #' - If `add_rows = TRUE`, `new_rows` must be a non-null tibble with columns (that must-contains all the columns) of the existing data.
                            #' - The function ensures data integrity by verifying types and column consistency before adding new rows or columns.
                            #'
                            #' @return NULL (modifies `private$data` in place)
                            #'
                            #' @examples
                            #' df = DataFrame$new(Name = c("Alice", "Bob"), Age = c(25, 30))
                            #' new_columns = tibble::tibble(Score = c(88, 92))
                            #' df$add_multi_entries(add_rows = FALSE, new_cols = new_columns) # Adds a new column
                            #'
                            #' new_rows = tibble::tibble(Name = c("Charlie"), Age = c(35))
                            #' df$add_multi_entries(add_rows = TRUE, new_rows = new_rows) # Adds a new row
                            #'
                            #' @export
                            add_multi_entries = function(add_rows = FALSE, new_cols = NULL, new_rows = NULL) {
                                if(!add_rows) { # if adding the column
                                    if (is.null(new_cols)) { # check if null
                                      stop("❌ Error:Cannot adding the  column that is NULL - consider passing an emptry tibble instead")
                                    }
                                    if (!tibble::is_tibble(new_cols)) { # check if new_cols is not a tibble (data frame)
                                        stop(paste0("❌ Error: new_cols should be a tibble. Given type: ", class(new_cols),
                                                    ". Consider converting it using tibble::as_tibble()."))
                                    }
                                    private$data = dplyr::bind_cols(private$data, new_cols)
                                    message("✅ Successfully added new columns!")
                                  }
                                else { # if adding the rows

                                  if (is.null(new_rows)) { # check if null
                                    stop("❌ Error:Cannot adding the rows that is NULL - consider passing an emptry tibble instead")
                                  }

                                  if (!tibble::is_tibble(new_rows)) { # check if new_rows is not a tibble (data frame)
                                    stop(paste0("❌ Error: new_rows should be a tibble. Given type: ", class(new_cols),
                                                ". Consider converting it using tibble::as_tibble()."))
                                  }

                                  # Check if column names matches
                                  missing_cols = setdiff(colnames(private$data), colnames(new_rows))
                                  extra_cols = setdiff(colnames(new_rows), colnames(private$data))


                                  if (length(missing_cols) > 0) {
                                    stop("❌ Error: ", length(missing_cols), " required columns are missing: ",
                                         paste(head(missing_cols, 5), collapse = ", "),
                                         if (length(missing_cols) > 5) "..." else "")
                                  }

                                  if (length(extra_cols) > 0) {
                                    message("!!! Warning: ", length(extra_cols), " extra columns won't be added: ",
                                            paste(head(extra_cols, 5), collapse = ", "),
                                            if (length(extra_cols) > 5) "..." else "")
                                  }

                                  # Ensure correct column order
                                  new_rows = new_rows[, colnames(private$data), drop = FALSE]


                                  # Bind new rows to existing data
                                  private$data = dplyr::bind_rows(private$data, new_rows)
                                  message("✅ Successfully added new rows!")
                                }
                              return(NULL)
                            },


                            #' @description Provides a detailed summary of the DataFrame, including
                            #'              statistics, missing values, and distribution of numerical
                            #'              and categorical columns.
                            #'
                            #' @details This method uses the `skimr::skim()` function to generate
                            #'          an enhanced summary of the dataset. It provides insights such as
                            #'          mean, median, standard deviation, and missing values for each column.
                            #'
                            #' @return NULL (The summary is printed to the console)
                            #'
                            #' @examples
                            #' df$describe()  # Display summary statistics for the DataFrame
                            #' @export
                            describe = function() {
                               print("Description of the DataFrame")
                               print(skimr::skim(private$data))
                               return(NULL)
                            },

                            #' @description Filters the DataFrame based on a specified query string.
                            #' This function allows users to filter rows in a tibble using R's logical syntax.
                            #' The query must reference column names exactly as they appear in the data.
                            #'
                            #' @param query A character string representing the filtering condition.
                            #'              - Use column names exactly as they appear in the DataFrame.
                            #'              - Logical operators allowed:
                            #'                - `&` (AND), `|` (OR), `==` (equal), `!=` (not equal),
                            #'                  `>` `<` `>=` `<=` (comparisons).
                            #'              - String values must be enclosed in single quotes (e.g., `"Name == 'Alice'"`).
                            #'              - Multiple conditions can be combined with `&` and `|` (e.g., `"Age > 30 & Score > 80"`).
                            #'              - Parentheses `()` can be used to group conditions for clarity.
                            #'
                            #' @return A new `DataFrame` object containing the filtered rows.
                            #'         If no rows match, a warning is displayed, and an empty DataFrame is returned.
                            #'
                            #' @examples
                            #' df$filter(query = "Age > 30")                # Returns rows where Age > 30
                            #' df$filter(query = "Score >= 85 & Age < 40")  # Returns rows where both conditions are met
                            #' df$filter(query = "Name == 'Alice'")         # Filters for Alice
                            #' df$filter(query = "Name != 'Eve'")           # Excludes Eve
                            #' df$filter(query = "Score > 90 | Age < 25")   # Returns rows where Score > 90 OR Age < 25
                            #'
                            #' throws Error if the query is not a character string.
                            #' throws Error if the query is empty (`NULL`).
                            #' throws Error if the query references columns that do not exist in the DataFrame.
                            filter = function(query = NULL) {
                              if (is.null(query)) {
                                stop("❌ Error: Cannot filter with an empty query")
                              }

                              if (!is.character(query)) {
                                stop("❌ Error: Query must be a string (character)")
                              }

                              # Ensure private$data is a tibble
                              if (!tibble::is_tibble(private$data)) {
                                private$data = tibble::as_tibble(private$data)
                              }

                              # Try filtering with safer handling
                              tryCatch({
                                print(query)
                                filtered_dp = dplyr::filter(private$data, eval(parse(text = query)))

                                # Ensure filtered data is valid
                                if (nrow(filtered_dp) == 0) {
                                  warning("⚠ Warning: No rows match the query.")
                                }

                                # Ensure filtered tibble has column names
                                if (any(colnames(filtered_dp) == "")) {
                                  stop("❌ Error: Filtered data contains unnamed columns.")
                                }

                                filtered = DataFrame$new()
                                filtered$set_data(filtered_dp)
                                message("✅ Filtering successful")
                                return(filtered)
                              }, error = function(e) {
                                stop("❌ Error: Invalid query - ", e$message)
                              })
                            },




                            #' @description Exports the current DataFrame to a file in the specified format.
                            #' The function automatically detects if the filename contains an extension.
                            #' If no extension is provided, it appends the appropriate extension based on the selected format.
                            #'
                            #' @param filename A string representing the name of the output file. Defaults to `"result"`.
                            #'                 If no extension is provided, the function will append the correct extension.
                            #' @param format A string specifying the file format. Must be one of `"csv"`, `"tsv"`, `"xlsx"`, `"json"`, `"rds"`, `"parquet"`, or `"feather"`. Defaults to `"csv"`.
                            #'
                            #' @details
                            #' This function exports the `private$data` tibble to a file using appropriate export methods
                            #' based on the chosen format. If the filename already contains a valid extension, it will not
                            #' append another one. Otherwise, it appends the correct extension.
                            #'
                            #' @return The function does not return a value; it saves the data to a file and displays a success message.
                            #'
                            #' @examples
                            #' df$export_to_file("my_data", "csv")  # Exports as "my_data.csv"
                            #' df$export_to_file("output.xlsx", "xlsx")  # Keeps "output.xlsx"
                            #' df$export_to_file("dataset", "json")  # Saves as "dataset.json"
                            #'
                            #' @export
                            export_to_file = function(filename = "result", format = "csv") {
                              tryCatch({
                                # Ensure format is valid
                                valid_formats = c("csv", "tsv", "xlsx", "json", "rds", "parquet", "feather")
                                if (!(format %in% valid_formats)) {
                                  stop("❌ Error: Unsupported format. Choose from: ", paste(valid_formats, collapse = ", "))
                                }

                                # Extract file extension (if exists)
                                ext = tools::file_ext(filename)

                                # Append extension if missing
                                if (ext == "") {
                                  filename = paste0(filename, ".", format)
                                }

                                # Export based on format
                                if (format == "csv") {
                                  readr::write_csv(private$data, filename)
                                } else if (format == "tsv") {
                                  readr::write_tsv(private$data, filename)
                                } else if (format == "xlsx") {
                                  writexl::write_xlsx(private$data, filename)
                                } else if (format == "json") {
                                  jsonlite::write_json(private$data, filename, pretty = TRUE)
                                } else if (format == "rds") {
                                  saveRDS(private$data, filename)
                                } else if (format == "parquet") {
                                  arrow::write_parquet(private$data, filename)
                                } else if (format == "feather") {
                                  arrow::write_feather(private$data, filename)
                                }

                                message("✅ Successfully exported data to ", filename)
                              }, error = function(e) {
                                stop("❌ Error: Failed to export file - ", e$message)
                              })
                            },


                            #' @description
                            #' Automatically imports data from a file into a tibble based on its format.
                            #'
                            #' @param filename The name of the file to import (must include an extension).
                            #' @return A tibble containing the imported data.
                            #' @examples
                            #' df = import_from_file("data.csv")   # Imports CSV file
                            #' df = import_from_file("data.json")  # Imports JSON file as tibble
                            #' df = import_from_file("data.xlsx")  # Imports Excel file
                            import_from_file = function(filename) {
                              tryCatch({
                                # Check if the file exists
                                if (!file.exists(filename)) {
                                  stop("❌ Error: File not found - ", filename)
                                }

                                # Extract file extension
                                format = tools::file_ext(filename)
                                if (format == "") {
                                  stop("❌ Error: Could not determine file format. Ensure the file has an extension.")
                                }

                                # Read file based on format
                                df = switch(format,
                                            "csv" = readr::read_csv(filename),
                                            "tsv" = readr::read_tsv(filename),
                                            "xlsx" = readxl::read_excel(filename),
                                            "json" = tibble::as_tibble(jsonlite::fromJSON(filename)),
                                            "rds" = tibble::as_tibble(readRDS(filename)),
                                            "parquet" = tibble::as_tibble(arrow::read_parquet(filename)),
                                            "feather" = tibble::as_tibble(arrow::read_feather(filename)),
                                            stop("❌ Error: Unsupported format. Supported formats: csv, tsv, xlsx, json, rds, parquet, feather.")
                                )

                                # Set the data
                                self$set_data(df)
                                message("✅ Successfully imported data from ", filename)

                                return(df)
                              }, error = function(e) {
                                stop("❌ Error: Failed to import file - ", e$message)
                              })
                            },

                            #' @description For testing purpose
                            test = function() {

                              # Initialize DataFrame
                              print("Initialize the test with a DataFrame object")
                              df = DataFrame$new(
                                Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
                                Age = c(25, 30, 35, 40, 45),
                                Score = c(88, 92, 95, 78, 85)
                              )

                              # Display initial data
                              print("Initial Data:")
                              print(df$get_data())

                              # Test: Delete specific columns
                              #print("Test: Deleting specific columns ('Name' and 'Score')")
                              #sol = df$delete(delRow = FALSE, col_names = c("Name", "Score"), row_indexes = c(1,2))
                              #print("Result after deletion:")
                              #print(sol)

                              # Check updated DataFrame after deletion
                              #print("Updated DataFrame after deletion:")
                              #print(df$get_data())

                              # Test: Adding a new row
                              print("Test: Adding a new row (Name = 'Hella', Age = '20')")
                              df$add_a_new_entry(addRow = TRUE, added_row = c(Name = "Hella", Age = "20"))
                              print("Updated DataFrame after adding row:")
                              print(df$get_data())

                              # Test: Adding a new column with NA values
                              print("Test: Adding a new column ('Check') with NA values")
                              df$add_a_new_entry(addRow = FALSE, new_col_name = "Check", added_col = rep(NA, nrow(df$get_data())))
                              print("Updated DataFrame after adding column:")
                              print(df$get_data())

                              # Test: Cloning DataFrame
                              print("Test: Cloning the DataFrame")
                              cloned_df = df$clone()
                              cloned_df$show_dp()

                              # Test: Delete in cloned dataframe
                              #print("Test: Deleting ('Name' and 'Check') in cloned DataFrame")
                              #cloned_df$delete(delRow = FALSE, col_names = c("Name", "Check"), row_indexes = c(1,2))
                              #print("Updated Cloned DataFrame after deletion:")
                              #print(cloned_df$get_data())

                              # Test: Adding multiple columns
                              print("Test: Adding multiple columns ('New_Col1' and 'New_Col2')")
                              new_columns = tibble::tibble(New_Col1 = c(1, 2, 3, 4, 5, 6), New_Col2 = c("A", "B", "C", "D", "E", "F"))
                              df$add_multi_entries(add_rows = FALSE, new_cols = new_columns)
                              print("Updated DataFrame after adding multiple columns:")
                              print(df$get_data())

                              # Test: Adding multiple rows
                              print("Test: Adding multiple rows")
                              new_rows = tibble::tibble(Name = c("Frank", "Grace"), Age = c(50, 55), Score = c(90, 87), Check = c(NA, NA), New_Col1 = c(7, 8), New_Col2 = c("G", "H"), New_Col3 = c("6", "7"))
                              df$add_multi_entries(add_rows = TRUE, new_rows = new_rows)
                              print("Updated DataFrame after adding multiple rows:")
                              print(df$get_data())


                              # Test: Viewing the description of the DataFrame
                              print("Test: Viewing the description of the DataFrame")
                              df$describe()


                              # Test: Doing the Filtering
                              print("Test doing the Filtering")
                              filtering = df$filter(query=  "Age > 30 & Score > 88")
                              filtering$show_dp()


                              # Test: Doing the Export Data Frame to File:
                              print("Test: Test doing the Export the DataFrame to external file")
                              df$export_to_file(format="csv", filename= "export")


                              # Test: Doing the Import from file
                              print("Test: Test doing the Import from external file")
                              df$import_from_file("export.csv")

                              print("✅ All tests completed successfully!")

                              return(NULL)
                            }

              )
)

#' @importFrom data.table data.table
#' @importFrom tibble tibble
#' @importFrom R6 R6Class
#' @importFrom dplyr mutate
#' @export
#' @title DataFrame Class
#' @description A custom R6 class that provides an interface for handling tabular data using tibbles.
#' It includes methods for initializing data, retrieving subsets, and performing basic table operations.
DataFrame <- R6::R6Class("DataFrame",
                         private = list(
                           data = NULL
                         ),

                         public = list(
                           #' @description Initializes the DataFrame with named vectors as columns.
                           #' @param ... Named vectors that form the columns of the DataFrame.
                           initialize = function(...) {
                             input_list <- list(...)  # Capture all arguments into a list
                             private$data <- tibble::as_tibble(input_list)  # Convert to tibble
                           },

                           #' @description Displays the stored tibble.
                           show_dp = function() {
                             print(private$data)  # Print the stored tibble
                           },

                           #' @description Retrieves the entire stored tibble.
                           #' @return A tibble containing all the stored data.
                           get_data = function() {
                             return(private$data)
                           },

                           #' @description Retrieves one or more columns by name.
                           #' @param colName A string or a vector of strings specifying the column(s) to retrieve.
                           #' @return A tibble containing the selected columns.
                           #' throws an Error if `colName` is not a character or if a specified column does not exist.
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
                            #' @param delRow: check if want delete row or not,
                            #' col_name - define the colummn names, row_index - define the index of rows
                            #' @return the modified table

                          delete = function(delRow = FALSE, col_names = NULL, row_indexes = NULL) {
                            # Delete columns if delRow is FALSE
                            if (!delRow) {
                              if (!is.null(col_names)) {
                                if (!all(col_names %in% colnames(private$data))) {
                                  stop("Error: One or more specified columns do not exist in the table.")
                                }
                                private$data <- private$data %>% select(-all_of(col_names))
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
                              } else {
                                stop("Error: `row_indexes` is NULL. No rows were deleted.")
                              }
                            }
                            return(private$data)
                          },


                            #' @description For testing purpose
                            test = function() {
                              print(self$get_size())
                              return
                           }

              )
)

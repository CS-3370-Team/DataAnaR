#' @title BeautifulTable Class
#' @description An R6 class for formatting and exporting a DataFrame object.
#'
#' @importFrom R6 R6Class
#' @importFrom knitr kable
#' @importFrom formattable formattable color_tile formatter
#' @importFrom pander pandoc.table
#'
#' @export
BeautifulTable <- R6::R6Class("BeautifulTable",
                              private = list(
                                data_frame = NULL  # Store the DataFrame object
                              ),

                              public = list(
                                #' @description Initializes the BeautifulTable with a DataFrame object.
                                #' @param data_frame A DataFrame object (R6 class) containing the data to format.
                                #' @export
                                initialize = function(data_frame) {
                                  if (!inherits(data_frame, "DataFrame")) {
                                    stop("❌ Error: Input must be a DataFrame object.")
                                  }
                                  private$data_frame <- data_frame
                                },

                                #' @description Prints the DataFrame in a formatted table.
                                #' @param style Character. The formatting style: "simple" (knitr simple table), "markdown" (knitr markdown table), "formattable" (basic formatting with formattable), "pander" (console-friendly table using pander).
                                #' @param numeric_color Character. The color for numeric column gradients when style is "formattable" (default: "lightblue").
                                #' @param bold_headers Logical. If TRUE, makes table headers bold when style is "formattable" (default: TRUE).
                                #' @return NULL (prints the table to the console).
                                #' @export
                                print_the_table = function(style = "formattable", numeric_color = "lightblue", bold_headers = TRUE) {
                                  # Validate style parameter
                                  valid_styles <- c("simple", "markdown", "formattable", "pander")
                                  if (!(style %in% valid_styles)) {
                                    stop("❌ Error: Unsupported style. Choose from: ", paste(valid_styles, collapse = ", "))
                                  }

                                  df <- private$data_frame$get_data()

                                  if (style == "simple") {
                                    print(knitr::kable(df, format = "simple"))
                                  } else if (style == "markdown") {
                                    print(knitr::kable(df, format = "markdown"))
                                  } else if (style == "pander") {
                                    # Use pander to render a console-friendly table
                                    pander::pandoc.table(df, style = "grid")
                                  } else if (style == "formattable") {
                                    # Basic formattable formatting
                                    format_rules <- list()

                                    # Apply a light gradient to numeric columns
                                    numeric_cols <- sapply(df, is.numeric)
                                    if (any(numeric_cols)) {
                                      for (col in names(df)[numeric_cols]) {
                                        format_rules[[col]] <- color_tile("white", numeric_color)
                                      }
                                    }

                                    # Apply checkmarks/crosses to logical columns
                                    logical_cols <- sapply(df, is.logical)
                                    if (any(logical_cols)) {
                                      for (col in names(df)[logical_cols]) {
                                        format_rules[[col]] <- formatter("span",
                                                                         style = x ~ ifelse(x, "color: green", "color: red"),
                                                                         x ~ ifelse(x, "✔", "✖"))
                                      }
                                    }

                                    # Apply italic style to character columns
                                    character_cols <- sapply(df, is.character)
                                    if (any(character_cols)) {
                                      for (col in names(df)[character_cols]) {
                                        format_rules[[col]] <- formatter("span", style = "font-style: italic")
                                      }
                                    }

                                    formatted_table <- formattable(df, format_rules,
                                                                   table.attr = if (bold_headers) 'style="font-weight: bold;"' else NULL)
                                    print(formatted_table)
                                  }

                                  return(NULL)
                                },

                                #' @description Exports the DataFrame to both CSV and JSON files.
                                #' @param base_filename A string specifying the base name for the output files (default: "beautiful_table").
                                #' @return NULL (exports the data to files).
                                #' @export
                                export_to_csv_and_json = function(base_filename = "beautiful_table") {
                                  # Export to CSV
                                  private$data_frame$export_to_file(filename = paste0(base_filename, ".csv"), format = "csv")

                                  # Export to JSON
                                  private$data_frame$export_to_file(filename = paste0(base_filename, ".json"), format = "json")

                                  return(NULL)
                                }
                              )
)



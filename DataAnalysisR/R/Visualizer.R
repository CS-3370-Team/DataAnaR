#' @title Visualizer Class
#' @description A custom R6 class for visualizing data from a DataFrame object.
#' It provides methods for creating various types of plots using ggplot2, plotly, and GWalkR.
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_line geom_bar geom_histogram geom_boxplot labs theme_minimal ggsave scale_fill_brewer
#' @importFrom plotly plot_ly
#' @importFrom patchwork wrap_plots
#' @importFrom GWalkR gwalkr
#' @export
Visualizer = R6::R6Class("Visualizer",
                         private = list(
                           df = NULL,

                           #' @description Check if required columns exist in the DataFrame.
                           #' @param required_cols Number of columns required.
                           #' @param col_names Character vector of column names to check.
                           #' @return Logical TRUE if valid, FALSE otherwise.
                           check_columns = function(required_cols, col_names) {
                             data_cols = private$df$get_col_names()

                             if (length(data_cols) < required_cols) {
                               message(sprintf("❌ Not enough columns. Required: %d, Available: %d", required_cols, length(data_cols)))
                               return(FALSE)
                             }

                             if (!all(col_names %in% data_cols)) {
                               missing = col_names[!col_names %in% data_cols]
                               message(sprintf("❌ Missing columns: %s", paste(missing, collapse = ", ")))
                               return(FALSE)
                             }
                             return(TRUE)
                           }
                         ),

                         public = list(

                           #' @description Initialize the Visualizer with a DataFrame object.
                           #' @param df A DataFrame object containing the data to visualize.
                           initialize = function(df) {
                             if (!inherits(df, "DataFrame")) {
                               stop("❌ Input must be a DataFrame object.")
                             }
                             private$df = df
                           },

                           #' @description Create an interactive plot using GWalkR.
                           #' @return Launches the GWalkR interface.
                           #' @export
                           plot_interactive = function() {
                             if (!requireNamespace("GWalkR", quietly = TRUE)) {
                               stop("❌ Package 'GWalkR' is required but not installed. Please install it first.")
                             }
                             message("🚀 Launching GWalkR interface...\n",
                                     "1. Select chart type (e.g., Scatter).\n",
                                     "2. Drag desired columns.\n",
                                     "3. Adjust settings as needed.\n",
                                     "🔔 Stop the app manually to continue your R session.")
                             GWalkR::gwalkr(private$df$get_data())
                           },

                           #' @description Create a scatter plot using ggplot2.
                           #' @param x_col Column name for x-axis.
                           #' @param y_col Column name for y-axis.
                           #' @return ggplot object or NULL.
                           #' @export
                           plot_scatter = function(x_col, y_col) {
                             if (!is.function(private$check_columns)) {
                               stop("Method check_columns is not recognized as a function.")
                             }
                             if (!private$check_columns(2, c(x_col, y_col))) {
                               message("❌ Columns not valid.")
                               return(NULL)
                             }
                             p = ggplot2::ggplot(private$df$get_data(), ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
                               ggplot2::geom_point() +
                               ggplot2::labs(title = paste("Scatter Plot:", x_col, "vs", y_col), x = x_col, y = y_col) +
                               ggplot2::theme_minimal()
                             message("✅ Scatter plot created.")
                             return(p)
                           },

                           #' @description Create a line plot using ggplot2.
                           #' @param x_col Column name for x-axis.
                           #' @param y_col Column name for y-axis.
                           #' @return ggplot object or NULL.
                           #' @export
                           plot_line = function(x_col, y_col) {
                             print("Yes")
                             is_enough_col = private$check_columns(2, c(x_col, y_col))
                             if (!is_enough_col) return(NULL)

                             p = ggplot2::ggplot(private$df$get_data(), ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
                               ggplot2::geom_line() +
                               ggplot2::labs(title = paste("Line Plot:", x_col, "vs", y_col), x = x_col, y = y_col) +
                               ggplot2::theme_minimal()
                             message("✅ Line plot created.")
                             return(p)
                           },

                           #' @description Create a bar plot using ggplot2.
                           #' @param x_col Categorical column for x-axis.
                           #' @param y_col Numeric column for y-axis.
                           #' @return ggplot object or NULL.
                           #' @export
                           plot_bar = function(x_col, y_col) {
                             if (!private$check_columns(2, c(x_col, y_col))) return(NULL)

                             p = ggplot2::ggplot(private$df$get_data(), ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]])) +
                               ggplot2::geom_bar(stat = "identity") +
                               ggplot2::labs(title = paste("Bar Plot:", x_col, "vs", y_col), x = x_col, y = y_col) +
                               ggplot2::theme_minimal()
                             message("✅ Bar plot created.")
                             return(p)
                           },

                           #' @description Create a histogram using ggplot2.
                           #' @param x_col Column for histogram.
                           #' @param bins Number of bins.
                           #' @return ggplot object or NULL.
                           #' @export
                           plot_histogram = function(x_col, bins = 30) {
                             if (!private$check_columns(1, x_col)) return(NULL)

                             p = ggplot2::ggplot(private$df$get_data(), ggplot2::aes(x = .data[[x_col]])) +
                               ggplot2::geom_histogram(bins = bins, fill = "blue", color = "black") +
                               ggplot2::labs(title = paste("Histogram of", x_col), x = x_col, y = "Count") +
                               ggplot2::theme_minimal()
                             message("✅ Histogram created.")
                             return(p)
                           },

                           #' @description Create a box plot using ggplot2.
                           #' @param y_col Column for y-axis.
                           #' @return ggplot object or NULL.
                           #' @export
                           plot_box = function(y_col) {
                             if (!private$check_columns(1, y_col)) return(NULL)

                             p = ggplot2::ggplot(private$df$get_data(), ggplot2::aes(y = .data[[y_col]])) +
                               ggplot2::geom_boxplot(fill = "lightblue") +
                               ggplot2::labs(title = paste("Box Plot of", y_col), y = y_col) +
                               ggplot2::theme_minimal()
                             message("✅ Box plot created.")
                             return(p)
                           },

                           #' @description Create a heatmap using ggplot2.
                           #' @param x_col Column for x-axis - should be categorical value
                           #' @param y_col Column for y-axis.
                           #' @param value_col Column for fill values.
                           #' @return ggplot object or NULL.
                           #' @export
                           plot_heatmap = function(x_col, y_col, value_col) {
                             if (!private$check_columns(3, c(x_col, y_col, value_col))) return(NULL)

                             p = ggplot2::ggplot(private$df$get_data(), ggplot2::aes(x = .data[[x_col]], y = .data[[y_col]], fill = .data[[value_col]])) +
                               ggplot2::geom_tile() +
                               ggplot2::scale_fill_gradient(low = "lightblue", high = "darkblue") +  # FIXED
                               ggplot2::labs(title = paste("Heatmap:", value_col, "by", x_col, "and", y_col),
                                             x = x_col, y = y_col, fill = value_col) +
                               ggplot2::theme_minimal()
                             message("✅ Heatmap created.")
                             return(p)
                           },

                           #' @description Combine multiple ggplot plots.
                           #' @param plot_list List of ggplot objects.
                           #' @return Combined patchwork plot or NULL.
                           #' @export
                           plot_multiple = function(plot_list) {
                             if (!is.list(plot_list) || length(plot_list) == 0) {
                               message("❌ Provide a non-empty list of ggplot objects.")
                               return(NULL)
                             }
                             if (!all(sapply(plot_list, function(p) inherits(p, "ggplot")))) {
                               message("❌ All elements must be ggplot objects.")
                               return(NULL)
                             }

                             p = patchwork::wrap_plots(plot_list)
                             message("✅ Multiple plots combined.")
                             return(p)
                           },

                           #' @description Create a 3D scatter plot using plotly.
                           #' @param x_col Column for x-axis.
                           #' @param y_col Column for y-axis.
                           #' @param z_col Column for z-axis.
                           #' @return plotly object or NULL.
                           #' @export
                           plot_3d = function(x_col, y_col, z_col) {
                             if (!private$check_columns(3, c(x_col, y_col, z_col))) return(NULL)

                             p = plotly::plot_ly(
                               data = private$df$get_data(),
                               x = as.formula(paste0("~", x_col)),
                               y = as.formula(paste0("~", y_col)),
                               z = as.formula(paste0("~", z_col)),
                               type = "scatter3d",
                               mode = "markers"
                             )
                             message("✅ 3D scatter plot created.")
                             return(p)
                           },

                           #' @description Export a plot to a file.
                           #' @param plot Plot object (ggplot or plotly).
                           #' @param filename Output filename without extension.
                           #' @param format File format ("png", "jpg", "pdf", "html").
                           #' @param width Width in pixels (default 800).
                           #' @param height Height in pixels (default 600).
                           #' @return NULL.
                           #' @export
                           export_plot = function(plot, filename, format = "png", width = 800, height = 600) {
                             if (is.null(plot)) {
                               message("❌ Plot is NULL, export aborted.")
                               return(NULL)
                             }

                             if (inherits(plot, "ggplot")) {
                               ggplot2::ggsave(
                                 filename = paste0(filename, ".", format),
                                 plot = plot,
                                 device = format,
                                 width = width / 72,
                                 height = height / 72,
                                 units = "in"
                               )
                               message(sprintf("✅ ggplot exported to %s.%s", filename, format))
                             } else if (inherits(plot, "plotly")) {
                               htmlwidgets::saveWidget(plot, paste0(filename, ".html"))
                               message(sprintf("✅ plotly plot exported to %s.html", filename))
                             } else {
                               message("❌ Unsupported plot type.")
                             }
                           }
                         )
)

#' Create a Sankey Diagram for Treatment Sequences
#'Author: Yayehirad A Melsew
#' This function generates a Sankey diagram to visualize patient treatment sequences
#' across different lines of therapy (LOTs). It requires pre-prepared data with 
#' specific column names that must be specified by the user.
#'
#' @param data A data frame containing the treatment sequence data. Default is NULL.
#' @param id_col Character string specifying the column name for patient IDs. 
#'   Default is "id".
#' @param lot_cols Character vector specifying the column names for lines of therapy 
#'   in the order they should appear in the diagram. Default is 
#'   c("LOT1", "LOT2", "LOT3", "LOT4").
#' @param file_path Character string specifying the path to an Excel file 
#'   containing the data. Default is NULL.
#' @param show_legend Logical value indicating whether to show the legend. 
#'   Default is TRUE.
#' @param show_numbers Logical value indicating whether to show frequency numbers 
#'   alongside node labels (e.g., "PI only = 50"). Default is FALSE.
#'
#' @return A ggplot object representing the Sankey diagram.
#'
#' @details
#' The function performs the following steps:
#' 1. Loads required R packages (ggalluvial, dplyr, ggplot2, readxl, ggsankey, RColorBrewer)
#' 2. Loads data from either a provided data frame or Excel file
#' 3. Validates that all specified columns exist in the data
#' 4. Selects only the specified ID and LOT columns
#' 5. Filters out rows where the first LOT is missing
#' 6. Aggregates data by unique treatment sequences
#' 7. Prepares data in long format for Sankey visualization
#' 8. Calculates node frequencies for each LOT if show_numbers is TRUE
#' 9. Generates a distinct color scheme for all unique nodes using RColorBrewer
#' 10. Creates and returns the Sankey diagram with links colored by source nodes
#'     and optional frequency labels
#'
#' @note
#' - Users must prepare their data beforehand, ensuring column names match those 
#'   specified in id_col and lot_cols parameters
#' - The data should contain an ID column and at least one LOT column
#' - Missing values (NA) in the first LOT column will be filtered out
#' - Colors are dynamically assigned using RColorBrewer's Paired palette for distinctness
#' - Links inherit colors from their source nodes
#' - If show_numbers = TRUE, node labels will include frequencies (e.g., "PI only = 50")
#'
#' @examples
#' # Example 1: Using two LOTs with legend and numbers
#' plot12 <- create_sankey_diagram(
#'   file_path = "LOT1234_PtLevel.xlsx",
#'   id_col = "Unique patient ID",
#'   lot_cols = c("L1S", "L2S"),
#'   show_legend = TRUE,
#'   show_numbers = TRUE
#' )
#' print(plot12)
#'
#' # Example 2: Using three LOTs without legend or numbers
#' plot123 <- create_sankey_diagram(
#'   file_path = "LOT1234_PtLevel.xlsx",
#'   id_col = "Unique patient ID",
#'   lot_cols = c("L1S", "L2S", "L3S"),
#'   show_legend = FALSE,
#'   show_numbers = FALSE
#' )
#' print(plot123)
#'
#' @export
create_sankey_diagram <- function(data = NULL,
                                  id_col = "id",
                                  lot_cols = c("LOT1", "LOT2", "LOT3", "LOT4"),
                                  file_path = NULL,
                                  show_legend = TRUE,
                                  show_numbers = FALSE) {
  # Step 1: Load required libraries
  library(ggalluvial)  # For alluvial/Sankey plot functionality
  library(dplyr)       # For data manipulation
  library(ggplot2)     # For plotting
  library(readxl)      # For reading Excel files
  library(ggsankey)    # For Sankey diagram specific features
  library(RColorBrewer) # For distinct color palettes
  
  # Step 2: Load data
  if (!is.null(file_path)) {
    df <- readxl::read_xlsx(file_path)
  } else if (!is.null(data)) {
    df <- data
  } else {
    stop("Either 'data' or 'file_path' must be provided")
  }
  
  # Step 3: Validate inputs
  if (length(lot_cols) < 1) {
    stop("At least one LOT column must be specified")
  }
  required_cols <- c(id_col, lot_cols)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(paste("The following required columns are missing from the data:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Step 4: Select specified columns
  df <- df %>%
    select(all_of(required_cols))
  
  # Step 5: Filter out rows where first LOT is missing
  df <- df %>%
    filter(!is.na(.data[[lot_cols[1]]]))
  
  # Step 6: Aggregate data
  df_agg <- df %>%
    group_by(across(all_of(lot_cols))) %>%
    summarise(Freq = n(), .groups = "drop")
  
  # Step 7: Prepare Sankey data
  df_sankey <- df_agg %>%
    make_long(!!!syms(lot_cols), value = "Freq") %>%
    filter(!is.na(node))
  
  # Step 8: Calculate node frequencies for each LOT (if show_numbers = TRUE)
  if (show_numbers) {
    # Calculate frequencies for each LOT
    node_freq_list <- lapply(lot_cols, function(lot) {
      df_agg %>%
        group_by(!!sym(lot)) %>%
        summarise(total_freq = sum(Freq), .groups = "drop") %>%
        rename(node = !!sym(lot)) %>%
        mutate(x = lot)
    })
    node_freq <- bind_rows(node_freq_list)
    
    # Join frequencies with df_sankey
    df_sankey <- df_sankey %>%
      left_join(node_freq, by = c("x", "node"))
    
    # Create label with node name and frequency
    df_sankey <- df_sankey %>%
      mutate(label_with_freq = paste0(node, " = ", total_freq))
  }
  
  # Step 9: Generate distinct color scheme
  unique_nodes <- unique(df_sankey$node)
  n_nodes <- length(unique_nodes)
  
  if (n_nodes <= 12) {
    my_colors <- brewer.pal(max(3, n_nodes), "Paired")
    my_colors <- my_colors[1:n_nodes]
  } else {
    paired_colors <- brewer.pal(12, "Paired")
    extra_needed <- n_nodes - 12
    extra_colors <- brewer.pal(max(3, extra_needed), "Set3")
    extra_colors <- extra_colors[1:extra_needed]
    my_colors <- c(paired_colors, extra_colors)
  }
  names(my_colors) <- unique_nodes
  
  # Step 10: Create Sankey diagram
  p <- ggplot(df_sankey,
              aes(x = x,
                  next_x = next_x,
                  node = node,
                  next_node = next_node,
                  fill = node,
                  label = if (show_numbers) label_with_freq else node,
                  value = value)) +
    geom_sankey(flow.alpha = 0.7, node.color = "black") +
    geom_sankey_label(color = "black", size = 3) +
    scale_fill_manual(values = my_colors) +
    theme_sankey(base_size = 14) +
    labs(title = "Sankey Diagram of Treatment Sequences",
         x = "Line of Therapy",
         y = "Number of Patients")
  
  # Step 11: Conditionally show or hide legend
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  return(p)
}
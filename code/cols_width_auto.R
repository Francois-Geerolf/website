# Auto-width columns in a gt table based on average string length
# - Computes mean(nchar()) per selected column
# - Converts to pixels via px_per_char, then clamps to [min_px, max_px]
# - Optionally strips Markdown links so "[Text](url)" counts as "Text"
#
# Usage:
#   gt_tbl %>% cols_width_auto(data = df)
#   gt_tbl %>% cols_width_auto(px_per_char = 7, min_px = 90, max_px = 600)

cols_width_auto <- function(gt_tbl,
                            data = NULL,
                            cols = tidyselect::everything(),
                            px_per_char = 8,
                            min_px = 80,
                            max_px = 520,
                            strip_md_links = TRUE) {
  stopifnot(inherits(gt_tbl, "gt_tbl"))
  
  # Try to retrieve data from gt_tbl if not provided (best-effort)
  if (is.null(data)) {
    # gt stores the input data in gt_tbl[["_data"]] (not a hard API guarantee, but common)
    data <- tryCatch(gt_tbl[["_data"]], error = function(e) NULL)
    if (is.null(data)) {
      warning("Provide `data=` (the data.frame used to build the gt table) for reliable width computation.")
      return(gt_tbl)
    }
  }
  
  # Only consider columns that are actually in the gt table
  gt_cols <- names(data)
  if (is.null(gt_cols) || !length(gt_cols)) return(gt_tbl)
  
  # Tidyselect resolution of `cols`
  sel <- tidyselect::eval_select(rlang::enquo(cols), data = data)
  sel_names <- names(sel)
  if (!length(sel_names)) return(gt_tbl)
  
  # Helper: turn vectors into character for nchar; handle list-cols
  to_chr <- function(x) {
    if (is.list(x)) {
      # e.g., list of strings -> paste (or lengths)
      return(vapply(x, function(e) paste(as.character(e), collapse = " "), character(1)))
    }
    as.character(x)
  }
  
  # Optional: strip Markdown link targets so "[Label](url)" counts as "Label"
  strip_links <- function(x) {
    if (!strip_md_links) return(x)
    gsub("\\[(.*?)\\]\\([^)]*\\)", "\\1", x, perl = TRUE)
  }
  
  # Compute average char length per selected column
  avg_chars <- vapply(
    sel_names,
    function(col) {
      v <- to_chr(data[[col]])
      v <- strip_links(v)
      mean(nchar(v[!is.na(v)]), na.rm = TRUE)
    },
    numeric(1)
  )
  
  # Convert to px and clamp
  widths_px <- pmin(pmax(round(avg_chars * px_per_char), min_px), max_px)
  
  # Build mapping for cols_width(): list(col1 = px(...), col2 = px(...))
  width_map <- stats::setNames(lapply(widths_px, gt::px), sel_names)
  
  # Apply
  do.call(gt::cols_width, c(list(gt_tbl), width_map))
}


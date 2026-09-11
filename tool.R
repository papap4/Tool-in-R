# ============================================================================
# Required packages: shiny, DT, readxl, readr, writexl
# ============================================================================

library(shiny)
library(DT)
library(readxl)
library(readr)
library(writexl)
library(plotly)

MAX_LEVEL <- 8  # ΓΕΩΓΡΑΦΙΚΟ ΕΠΙΠΕΔΟ ranges 0 (country) .. 8 (settlement)

# Folder containing the ELSTAT
DATA_FOLDER <- "data"

MANUAL_ALIASES <- c(
  "Σύνολο" = "Total",
  "Πληθυσμός" = "Population",
  "Άγαμοι" = "Single",
  "Έγγαμοι ή με σύμφωνο συμβίωσης ή σε διάσταση" = "Married",
  "Χήροι" = "Widowed",
  "Διαζευγμένοι" = "Divorced",
  "Άρρενες" = "Male",
  "Θήλεις" = "Female",
  "Και των δύο φύλων" = "Both sexes"
)

# ---------------------------------------------------------------------------
# Helper functions
# ---------------------------------------------------------------------------

parse_elstat_number <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "-", ":", "..", "...", "N/A", "n/a")] <- NA
  has_comma_decimal <- grepl(",\\d{1,2}$", x)
  x <- ifelse(has_comma_decimal,
              gsub("\\.", "", x),
              gsub("(?<=\\d)\\.(?=\\d{3}(\\D|$))", "", x, perl = TRUE))
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Combine every known alias for a value column
build_column_aliases <- function(value_cols, dims, short_names, short_titles) {
  n <- length(value_cols)
  out <- character(n)
  for (i in seq_len(n)) {
    parts <- value_cols[i]
    if (!is.na(short_names[i])  && nzchar(short_names[i]))  parts <- c(parts, short_names[i])
    if (!is.na(short_titles[i]) && nzchar(short_titles[i])) parts <- c(parts, short_titles[i])
    if (!is.null(dims) && dims$n_levels > 0) {
      for (L in seq_len(dims$n_levels)) {
        col <- dims$col_level_map[[paste0("L", L)]]
        if (is.null(col)) next
        v <- col[i]
        if (!is.na(v)) {
          parts <- c(parts, v)
          if (!is.na(MANUAL_ALIASES[v])) parts <- c(parts, unname(MANUAL_ALIASES[v]))
        }
      }
    }
    out[i] <- paste(unique(parts), collapse = " · ")
  }
  out
}

detect_layout <- function(mat) {
  col1 <- trimws(mat[, 1])
  col3 <- trimws(mat[, 3])
  # The country total row is the anchor for where data starts. Most ELSTAT
  # exports mark it with ΓΕΩΓΡΑΦΙΚΟ ΕΠΙΠΕΔΟ = "0", but some leave that cell
  # blank — so also recognize it by its fixed description "ΣΥΝΟΛΟ ΧΩΡΑΣ".
  data_start <- which(col1 == "0" | col3 == "ΣΥΝΟΛΟ ΧΩΡΑΣ")[1]
  if (is.na(data_start)) {
    return(list(header_start = 1L, header_end = 1L, data_start = 2L, ok = FALSE,
                short_name_row = NA_integer_, short_title_row = NA_integer_))
  }
  header_end <- data_start - 1L
  
  short_name_row  <- NA_integer_
  short_title_row <- NA_integer_
  if (header_end >= 1) {
    for (r in seq_len(header_end)) {
      row_txt <- tolower(trimws(mat[r, ]))
      if (is.na(short_name_row)  && any(row_txt == "short name",  na.rm = TRUE)) short_name_row  <- r
      if (is.na(short_title_row) && any(row_txt == "short title", na.rm = TRUE)) short_title_row <- r
    }
  }
  
  if (!is.na(short_name_row) || !is.na(short_title_row)) {
    # dimension header 
    header_start <- max(c(short_name_row, short_title_row), na.rm = TRUE) + 1L
  } else {
    r <- header_end
    while (r > 1) {
      row_vals <- mat[r - 1, ]
      if (all(is.na(row_vals) | trimws(row_vals) == "")) break
      r <- r - 1
    }
    header_start <- max(1L, r)
  }
  if (header_end < header_start) header_end <- header_start
  
  # Trimming
  while (header_end > header_start) {
    row_vals <- mat[header_end, ]
    if (all(is.na(row_vals) | trimws(row_vals) == "")) header_end <- header_end - 1L else break
  }
  
  list(header_start = header_start, header_end = header_end,
       data_start = data_start, ok = TRUE,
       short_name_row = short_name_row, short_title_row = short_title_row)
}

build_header_structure <- function(mat, header_start, header_end) {
  nc <- ncol(mat)
  raw <- mat[header_start:header_end, , drop = FALSE]
  n_levels <- nrow(raw)
  for (r in seq_len(n_levels)) {
    for (c in seq_len(nc)) {
      v <- raw[r, c]
      raw[r, c] <- if (!is.na(v) && trimws(v) != "") trimws(v) else NA_character_
    }
  }
  
  # Horizontal fill-forward, but boundary-aware: a merged-cell group at a
  # deeper header row must NOT bleed across a column where a SHALLOWER row
  # just started a new group of its own
  boundary <- matrix(FALSE, n_levels, nc)
  for (r in seq_len(n_levels)) {
    last <- NA_character_
    for (c in seq_len(nc)) {
      shallow_boundary <- if (r == 1) FALSE else boundary[r - 1, c]
      if (shallow_boundary) last <- NA_character_
      if (!is.na(raw[r, c])) last <- raw[r, c]
      filled[r, c] <- last
      if (c == 1) {
        boundary[r, c] <- TRUE
      } else {
        prev <- filled[r, c - 1]
        changed <- is.na(filled[r, c]) != is.na(prev) ||
          (!is.na(filled[r, c]) && !is.na(prev) && filled[r, c] != prev)
        boundary[r, c] <- shallow_boundary || changed
      }
    }
  }
  
  flat_names <- character(nc)
  for (c in seq_len(nc)) {
    vals <- filled[, c]
    vals <- unique(vals[!is.na(vals) & vals != ""])
    flat_names[c] <- if (length(vals) == 0) paste0("Στήλη_", c) else paste(vals, collapse = " - ")
  }
  flat_names <- make.unique(flat_names, sep = "_")
  list(flat_names = flat_names, level_matrix = filled, n_levels = n_levels)
}

parse_table <- function(mat, header_start, header_end, data_start,
                        short_name_row = NA_integer_, short_title_row = NA_integer_) {
  hdr_info <- build_header_structure(mat, header_start, header_end)
  nms <- hdr_info$flat_names
  data_rows <- mat[data_start:nrow(mat), , drop = FALSE]
  df <- as.data.frame(data_rows, stringsAsFactors = FALSE)
  colnames(df) <- nms
  blank_row <- apply(df, 1, function(r) all(is.na(r) | trimws(r) == ""))
  df <- df[!blank_row, , drop = FALSE]
  
  colnames(df)[1] <- "level"
  colnames(df)[2] <- "code"
  colnames(df)[3] <- "name"
  
  df$level <- suppressWarnings(as.integer(trimws(df$level)))
  df$code  <- trimws(df$code)
  df$name  <- trimws(df$name)
 
  if (nrow(df) > 0 && is.na(df$level[1]) && identical(df$name[1], "ΣΥΝΟΛΟ ΧΩΡΑΣ")) {
    df$level[1] <- 0L
  }
  df <- df[!is.na(df$level), , drop = FALSE]
  
  value_cols <- setdiff(colnames(df), c("level", "code", "name"))
  for (vc in value_cols) df[[vc]] <- parse_elstat_number(df[[vc]])
  
  rownames(df) <- NULL
  df$row_id <- seq_len(nrow(df))
  
  nc_total <- ncol(mat)
  n_levels <- hdr_info$n_levels
  level_names <- paste0("Πεδίο ", seq_len(n_levels))
  level_values <- vector("list", n_levels)
  col_level_map <- data.frame(value_col = value_cols, stringsAsFactors = FALSE)
  short_names  <- rep(NA_character_, length(value_cols))
  short_titles <- rep(NA_character_, length(value_cols))
  if (nc_total >= 4 && length(value_cols) > 0) {
    val_idx <- 4:nc_total
    for (L in seq_len(n_levels)) {
      lv <- hdr_info$level_matrix[L, val_idx]
      col_level_map[[paste0("L", L)]] <- lv
      level_values[[L]] <- sort(unique(lv[!is.na(lv)]))
    }
    if (!is.na(short_name_row)) {
      sv <- trimws(mat[short_name_row, val_idx])
      sv[sv == ""] <- NA_character_
      short_names <- sv
    }
    if (!is.na(short_title_row)) {
      sv <- trimws(mat[short_title_row, val_idx])
      sv[sv == ""] <- NA_character_
      short_titles <- sv
    }
  }
  dims <- list(n_levels = n_levels, level_names = level_names,
               level_values = level_values, col_level_map = col_level_map)
  col_aliases <- build_column_aliases(value_cols, dims, short_names, short_titles)
  
  list(df = df, value_cols = value_cols, dims = dims,
       short_names = short_names, short_titles = short_titles, col_aliases = col_aliases)
}

resolve_cols_from_fields <- function(input, id_prefix, dims) {
  map <- dims$col_level_map
  if (dims$n_levels == 0 || nrow(map) == 0) return(character(0))
  keep <- rep(TRUE, nrow(map))
  for (L in seq_len(dims$n_levels)) {
    all_vals <- dims$level_values[[L]]
    if (length(all_vals) == 0) next
    sel <- input[[paste0(id_prefix, "_lvl_", L)]]
    col_vals <- map[[paste0("L", L)]]
    if (is.null(sel) || length(sel) == 0 || setequal(sel, all_vals)) {
      next  # no effective narrowing at this level
    }
    keep <- keep & (col_vals %in% sel)
  }
  map$value_col[keep]
}


render_field_select_ui <- function(id_prefix, dims) {
  if (is.null(dims) || dims$n_levels == 0) return(NULL)
  widgets <- lapply(seq_len(dims$n_levels), function(L) {
    vals <- dims$level_values[[L]]
    if (length(vals) == 0) return(NULL)
    selectizeInput(paste0(id_prefix, "_lvl_", L), label = dims$level_names[L],
                   choices = vals, selected = vals, multiple = TRUE,
                   options = list(plugins = list("remove_button"), placeholder = "Όλες οι τιμές"))
  })
  do.call(tagList, widgets)
}

attach_hierarchy <- function(df) {
  n <- nrow(df)
  parent <- rep(NA_integer_, n)
  path   <- character(n)
  stack_lvl <- integer(0); stack_id <- integer(0); stack_path <- character(0)
  for (i in seq_len(n)) {
    lvl <- df$level[i]
    while (length(stack_lvl) > 0 && stack_lvl[length(stack_lvl)] >= lvl) {
      stack_lvl  <- stack_lvl[-length(stack_lvl)]
      stack_id   <- stack_id[-length(stack_id)]
      stack_path <- stack_path[-length(stack_path)]
    }
    parent[i] <- if (length(stack_id) == 0) NA_integer_ else stack_id[length(stack_id)]
    parent_path <- if (length(stack_path) == 0) "" else stack_path[length(stack_path)]
    cur_path <- if (parent_path == "") df$name[i] else paste0(parent_path, " » ", df$name[i])
    path[i] <- cur_path
    stack_lvl  <- c(stack_lvl, lvl)
    stack_id   <- c(stack_id, df$row_id[i])
    stack_path <- c(stack_path, cur_path)
  }
  df$parent_id <- parent
  df$path <- path
  children_map <- split(df$row_id, df$parent_id)
  root_id <- df$row_id[which(is.na(df$parent_id))[1]]
  if (is.na(root_id)) root_id <- df$row_id[which.min(df$level)][1]
  list(df = df, children_map = children_map, root_id = root_id)
}


# variable columns.
bind_rows_base <- function(dfs) {
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0) return(NULL)
  all_cols <- unique(unlist(lapply(dfs, colnames)))
  dfs2 <- lapply(dfs, function(d) {
    missing <- setdiff(all_cols, colnames(d))
    for (m in missing) d[[m]] <- NA
    d[, all_cols, drop = FALSE]
  })
  do.call(rbind, dfs2)
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { background-color: #f4f6f8; }
    .app-title { color: #0b3d61; font-weight: 700; margin-bottom: 2px; }
    .app-subtitle { color: #55707f; margin-bottom: 18px; }
    .well { background-color: #ffffff; border: 1px solid #dce3e8; }
    .breadcrumb-box {
      background: #0b3d61; color: #ffffff; padding: 10px 14px; border-radius: 4px;
      margin-bottom: 12px; font-size: 14px;
    }
    .breadcrumb-box b { color: #ffd166; }
    table.dataTable thead th { background-color: #0b3d61 !important; color: #fff !important; }
    .node-summary { background:#eef4f8; border-left:4px solid #0b3d61; padding:10px 14px; margin-bottom:12px; }
    .level-select-block { border-top: 1px dashed #dce3e8; padding-top: 8px; margin-top: 8px; }
    .add-to-custom-box { background:#fff7e6; border:1px solid #ffe0a3; border-radius:4px; padding:10px 14px; margin-bottom: 12px; }
    .loaded-file-row { font-size: 12px; padding: 2px 0; }
  "))),
  
  div(class = "app-title", h2("Γεωγραφική Ιεραρχία ΕΛΣΤΑΤ — Viewer")),
  div(class = "app-subtitle", "Φορτώστε ένα ή περισσότερα αρχεία .xlsx/.csv της ΕΛΣΤΑΤ από φάκελο, εναλλάσσεστε ανάμεσά τους ακαριαία, και εξερευνήστε τα ανά ΓΕΩΓΡΑΦΙΚΟ ΕΠΙΠΕΔΟ (0 = Χώρα ... 8 = Οικισμός), όπως στο PxWeb."),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h5("Αρχεία"),
      helpText(sprintf("Βάλτε τα αρχεία .xlsx/.xls/.csv στον φάκελο «%s» δίπλα στο app.R.", DATA_FOLDER)),
      fluidRow(
        column(9, uiOutput("folder_file_choices_ui")),
        column(3, actionButton("refresh_folder", "↻", title = "Ανανέωση λίστας", width = "100%"))
      ),
      
      tags$details(
        tags$summary("Ρυθμίσεις ανάγνωσης (προχωρημένο, καθολικές)"),
        checkboxInput("manual_rows",
                      "Χειροκίνητη ρύθμιση γραμμών επικεφαλίδας/δεδομένων (εφαρμόζεται στα ίδια σε όλα τα αρχεία της παρτίδας — αλλιώς γίνεται αυτόματος εντοπισμός ξεχωριστά για κάθε αρχείο)",
                      FALSE),
        conditionalPanel(
          condition = "input.manual_rows == true",
          numericInput("header_start", "Πρώτη γραμμή επικεφαλίδας", value = 4, min = 1),
          numericInput("header_end",   "Τελευταία γραμμή επικεφαλίδας", value = 6, min = 1),
          numericInput("data_start",   "Πρώτη γραμμή δεδομένων", value = 7, min = 1)
        ),
        selectInput("csv_delim", "Διαχωριστικό CSV", choices = c("," = ",", ";" = ";", "Tab" = "\t"), selected = ";"),
        selectInput("csv_encoding", "Κωδικοποίηση CSV", choices = c("UTF-8", "WINDOWS-1253", "ISO-8859-7"), selected = "UTF-8")
      ),
      actionButton("load_selected_files", "📂 Φόρτωση επιλεγμένων αρχείων", class = "btn-primary", width = "100%"),
      br(), br(),
      
      conditionalPanel(
        condition = "output.has_datasets == true",
        h5("Φορτωμένα αρχεία"),
        uiOutput("loaded_files_ui"),
        actionButton("remove_unchecked_btn", "🗑 Αφαίρεση αποεπιλεγμένων", width = "100%"),
        hr(),
        uiOutput("active_dataset_ui"),
        tags$div(class = "level-select-block",
                 h5("Πλοήγηση Ιεραρχίας (ΓΕΩΓΡΑΦΙΚΟ ΕΠΙΠΕΔΟ)"),
                 uiOutput("level_selector_ui"),
                 actionButton("reset_path", "↺ Επαναφορά στην αρχή (Χώρα)", width = "100%")
        ),
        hr(),
        tags$div(class = "level-select-block",
                 h5("Φίλτρο Στηλών (Πεδία & Τιμές)"),
                 helpText("Επιλέξτε τιμές ανά πεδίο (π.χ. Φύλο, Ομάδα ηλικιών) — μόνο οι στήλες που ταιριάζουν σε ΟΛΑ τα πεδία εμφανίζονται στους πίνακες παρακάτω."),
                 uiOutput("column_filter_ui"),
                 tags$details(
                   tags$summary("🔎 Αναζήτηση με σύντομα ονόματα / τίτλους (π.χ. Pop, Σύνολο, Total)"),
                   helpText("Αναζητήστε με το σύντομο όνομα ή τίτλο του αρχείου (π.χ. \"Pop\", \"Πληθυσμός\"), ή με όρους από το χειροκίνητο λεξικό (π.χ. \"Total\"). Αν επιλέξετε στήλες εδώ, αυτές υπερισχύουν του παραπάνω φίλτρου πεδίων. Δείτε την καρτέλα «📋 Μεταβλητές» για όλα τα διαθέσιμα ψευδώνυμα ανά στήλη."),
                   uiOutput("manual_column_filter_ui")
                 )
        ),
        hr(),
        checkboxInput("show_leaf_descendants", "Εμφάνιση όλων των τελικών απογόνων (φύλλα) αντί των άμεσων παιδιών", FALSE),
        downloadButton("download_csv", "Λήψη τρέχοντος πίνακα (CSV)", width = "100%")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Πίνακας",
                 br(),
                 uiOutput("breadcrumb_ui"),
                 uiOutput("node_summary_ui"),
                 DTOutput("data_table"),
                 tags$div(class = "add-to-custom-box",
                          h5("➕ Προσθήκη στον Προσαρμοσμένο Πίνακα"),
                          helpText("Οι γραμμές μπορούν να προστεθούν ως έχουν, με όλες τις μεταβλητές, χωρίς να διαλέξετε τίποτα εδώ. Αν θέλετε μόνο συγκεκριμένες μεταβλητές, επιλέξτε τες παρακάτω. Αν κάνετε κλικ σε συγκεκριμένες γραμμές του πίνακα παραπάνω, θα προστεθούν μόνο αυτές — διαφορετικά προστίθενται όλες οι εμφανιζόμενες γραμμές."),
                          uiOutput("vars_to_add_hier_ui"),
                          actionButton("add_to_custom_hier", "➕ Προσθήκη επιλεγμένων/όλων των γραμμών", class = "btn-primary")
                 )
        ),
        tabPanel("Φίλτρο ανά Επίπεδο (Όλη η Χώρα)",
                 br(),
                 helpText("Δείτε ΟΛΕΣ τις εγγραφές ενός συγκεκριμένου ΓΕΩΓΡΑΦΙΚΟΥ ΕΠΙΠΕΔΟΥ σε όλη τη χώρα ταυτόχρονα, ανεξαρτήτως περιοχής. Δεν επηρεάζει την πλοήγηση στην καρτέλα «Πίνακας»."),
                 uiOutput("flat_level_ui"),
                 downloadButton("download_flat_csv", "Λήψη πίνακα επιπέδου (CSV)"),
                 br(), br(),
                 DTOutput("flat_level_table"),
                 tags$div(class = "add-to-custom-box",
                          h5("➕ Προσθήκη στον Προσαρμοσμένο Πίνακα"),
                          helpText("Οι γραμμές μπορούν να προστεθούν ως έχουν, με όλες τις μεταβλητές, χωρίς να διαλέξετε τίποτα εδώ. Αν θέλετε μόνο συγκεκριμένες μεταβλητές, επιλέξτε τες παρακάτω. Αν κάνετε κλικ σε συγκεκριμένες γραμμές του πίνακα παραπάνω, θα προστεθούν μόνο αυτές — διαφορετικά προστίθενται όλες οι εμφανιζόμενες γραμμές."),
                          uiOutput("vars_to_add_flat_ui"),
                          actionButton("add_to_custom_flat", "➕ Προσθήκη επιλεγμένων/όλων των γραμμών", class = "btn-primary")
                 )
        ),
        tabPanel("🧩 Προσαρμοσμένος Πίνακας",
                 br(),
                 helpText("Εδώ συγκεντρώνονται οι γραμμές/μεταβλητές που προσθέσατε από τις καρτέλες «Πίνακας» και «Φίλτρο ανά Επίπεδο» — ακόμη κι από διαφορετικά φορτωμένα αρχεία."),
                 fluidRow(
                   column(3, actionButton("remove_selected_custom_btn", "❌ Αφαίρεση επιλεγμένων", width = "100%")),
                   column(3, actionButton("clear_custom_btn", "🗑 Εκκαθάριση Πίνακα", width = "100%")),
                   column(3, downloadButton("download_custom_csv", "⬇ CSV", width = "100%")),
                   column(3, downloadButton("download_custom_xlsx", "⬇ XLSX", width = "100%"))
                 ),
                 helpText("Κάντε κλικ σε μία ή περισσότερες γραμμές του πίνακα για να τις επιλέξετε, μετά πατήστε «❌ Αφαίρεση επιλεγμένων» για να τις διαγράψετε — μία-μία ή περισσότερες μαζί."),
                 br(),
                 DTOutput("custom_table"),
                 hr(),
                 h4("📊 Γράφημα"),
                 helpText("Το γράφημα χρησιμοποιεί τις γραμμές που έχετε επιλέξει (κλικ) στον πίνακα παραπάνω — αν καμία δεν είναι επιλεγμένη, χρησιμοποιούνται όλες οι γραμμές."),
                 fluidRow(
                   column(6, uiOutput("chart_vars_ui")),
                   column(3, radioButtons("chart_type", "Τύπος γραφήματος",
                                          choices = c("Ράβδοι" = "bar", "Πίτα" = "pie"), selected = "bar")),
                   column(3, checkboxInput("chart_percent", "Ως ποσοστά (%) ανά γραμμή", FALSE))
                 ),
                 helpText("Πίτα: είτε επιλέξτε 1 γραμμή με πολλές μεταβλητές (κατανομή αυτής της περιοχής), είτε 1 μεταβλητή με πολλές γραμμές (σύγκριση περιοχών)."),
                 plotlyOutput("custom_chart", height = "450px")
        ),
        tabPanel("📋 Μεταβλητές",
                 br(),
                 helpText("Τα πεδία (μεταβλητές) που εντοπίστηκαν στο ενεργό αρχείο και οι πιθανές τιμές τους — αυτά τα ίδια πεδία χρησιμοποιούνται στα φίλτρα και στην προσθήκη γραμμών στον προσαρμοσμένο πίνακα."),
                 DTOutput("variables_overview_table"),
                 hr(),
                 h4("Στήλες & Ψευδώνυμα"),
                 helpText("Κάθε στήλη δεδομένων με το σύντομο όνομα/τίτλο της (αν το αρχείο τα παρέχει) και όλα τα ψευδώνυμα με τα οποία μπορείτε να την αναζητήσετε στο «🔎 Αναζήτηση με σύντομα ονόματα» στο πλαϊνό μενού."),
                 DTOutput("column_aliases_table")
        ),
        tabPanel("Προεπισκόπηση δομής",
                 br(),
                 helpText("Όλες οι γραμμές του ενεργού αρχείου όπως αναγνωρίστηκαν, με το επίπεδο, κωδικό και όνομα κάθε γραμμής."),
                 DTOutput("preview_table")
        ),
        tabPanel("Οδηγίες",
                 br(),
                 HTML(sprintf("
            <h4>Πώς λειτουργεί</h4>
            <ol>
              <li>Βάλτε τα αρχεία <b>.xlsx</b> / <b>.csv</b> της ΕΛΣΤΑΤ στον φάκελο <code>%s</code> δίπλα στο app.R (αλλάξτε τη σταθερά <code>DATA_FOLDER</code> στην αρχή του κώδικα αν θέλετε άλλη διαδρομή).</li>
              <li>Επιλέξτε ένα ή περισσότερα αρχεία (ή, για αρχεία με πολλαπλά φύλλα, συγκεκριμένα φύλλα) από τη λίστα και πατήστε <b>«📂 Φόρτωση επιλεγμένων αρχείων»</b>. Όλα φορτώνονται ταυτόχρονα στη μνήμη — δεν χρειάζεται να τα ξαναφορτώνετε για να εναλλάσσεστε ανάμεσά τους.</li>
              <li>Χρησιμοποιήστε το <b>«Ενεργό αρχείο προβολής»</b> για να επιλέξετε ποιο από τα φορτωμένα αρχεία/φύλλα εμφανίζεται αυτή τη στιγμή στις καρτέλες — η εναλλαγή είναι ακαριαία.</li>
              <li>Πλοηγηθείτε στην ιεραρχία με τα αναδυόμενα drop-down μενού, ή δείτε ένα ολόκληρο επίπεδο (π.χ. όλους τους Δήμους) στην καρτέλα «Φίλτρο ανά Επίπεδο».</li>
              <li>Το <b>«Φίλτρο Στηλών (Πεδία &amp; Τιμές)»</b> στο πλαϊνό μενού αναλύει αυτόματα τις μεταβλητές του αρχείου σε ξεχωριστά <b>πεδία</b> (π.χ. Φύλο, Οικογενειακή κατάσταση) αντί για μία τεράστια λίστα συνδυασμών — επιλέξτε τιμές ανά πεδίο για να φιλτράρετε ποιες στήλες εμφανίζονται.</li>
              <li>Αν το αρχείο της ΕΛΣΤΑΤ έχει δικές του γραμμές <b>«Short name»/«Short title»</b> (σύντομα ονόματα/τίτλοι ανά στήλη, π.χ. \"Pop\" / \"Πληθυσμός\"), αναγνωρίζονται αυτόματα. Χρησιμοποιήστε το <b>«🔎 Αναζήτηση με σύντομα ονόματα»</b> στο πλαϊνό μενού για να βρείτε στήλες γράφοντας είτε το σύντομο όνομα (π.χ. \"Pop\"), είτε τον ελληνικό όρο (π.χ. \"Σύνολο\"), είτε μια μετάφραση από το χειροκίνητο λεξικό <code>MANUAL_ALIASES</code> στην αρχή του κώδικα (π.χ. \"Total\") — αυτό το λεξικό δουλεύει ακόμη και σε αρχεία χωρίς δικές τους γραμμές Short name/title, και μπορείτε να προσθέσετε τις δικές σας μεταφράσεις εκεί. Δείτε την καρτέλα <b>«📋 Μεταβλητές»</b> για όλα τα εντοπισμένα πεδία, τιμές, και ψευδώνυμα ανά στήλη.</li>
              <li>Σε οποιαδήποτε από τις δύο καρτέλες δεδομένων, επιλέξτε τιμές πεδίων (προαιρετικό — αν δεν επιλέξετε τίποτα, προστίθενται όλες οι μεταβλητές) και πατήστε <b>«➕ Προσθήκη επιλεγμένων/όλων των γραμμών»</b> για να προσθέσετε στον <b>Προσαρμοσμένο Πίνακα</b> — μπορείτε να συνδυάσετε προσθήκες από διαφορετικά αρχεία, επίπεδα, ή περιοχές.</li>
              <li>Στην καρτέλα «🧩 Προσαρμοσμένος Πίνακας», αφαιρέστε γραμμές μία-μία (κλικ + «❌ Αφαίρεση επιλεγμένων»), κατεβάστε ό,τι έχετε συγκεντρώσει σε <b>CSV</b> ή <b>XLSX</b>, φτιάξτε ράβδους/πίτα στο «📊 Γράφημα», ή πατήστε «Εκκαθάριση» για να ξεκινήσετε από την αρχή.</li>
            </ol>
            <h4>Σημείωση για τη μορφή αρχείου</h4>
            <p>Η πρώτη στήλη πρέπει να είναι το <b>Γεωγραφικό Επίπεδο</b> (0-8, ή κενό για τη γραμμή ΣΥΝΟΛΟ ΧΩΡΑΣ — αναγνωρίζεται ως επίπεδο 0 αυτόματα), η δεύτερη ο <b>Γεωγραφικός Κωδικός</b>, η τρίτη η <b>Περιγραφή</b>, και οι υπόλοιπες οι μεταβλητές/μετρήσεις, με τις γραμμές γεωγραφικά ταξινομημένες όπως τις εξάγει η ΕΛΣΤΑΤ. Κάθε γραμμή επικεφαλίδας αντιστοιχίζεται αυτόματα σε ένα ξεχωριστό «πεδίο» για το φίλτρο· τυχόν γραμμές Short name/Short title και κενές γραμμές-διαχωριστικά εντοπίζονται και εξαιρούνται αυτόματα. Ο αυτόματος εντοπισμός γίνεται ξεχωριστά για κάθε αρχείο/φύλλο της παρτίδας, εκτός αν ενεργοποιήσετε τη χειροκίνητη ρύθμιση.</p>
          ", DATA_FOLDER))
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  
  datasets   <- reactiveValues(store = list())  # filename -> list(df, value_cols, children_map, root_id)
  rv         <- reactiveValues(path = list())
  custom_df  <- reactiveVal(NULL)               # the custom table itself, as one editable data.frame
  
  # ---- Folder listing — enumerates individual sheets for xlsx workbooks ----
  ITEM_SEP <- "\u241F"  # separator unlikely to appear in filenames/sheet names
  
  list_loadable_items <- function(folder) {
    if (is.null(folder) || !nzchar(folder) || !dir.exists(folder)) return(character(0))
    files <- sort(list.files(folder, pattern = "\\.(xlsx|xls|csv)$", ignore.case = TRUE, full.names = FALSE))
    items <- c()
    for (f in files) {
      ext <- tolower(tools::file_ext(f))
      if (ext %in% c("xlsx", "xls")) {
        sheets <- tryCatch(readxl::excel_sheets(file.path(folder, f)), error = function(e) character(0))
        if (length(sheets) <= 1) {
          items <- c(items, setNames(f, f))
        } else {
          labels <- paste0(f, "  —  ", sheets)
          values <- paste0(f, ITEM_SEP, sheets)
          items <- c(items, setNames(values, labels))
        }
      } else {
        items <- c(items, setNames(f, f))
      }
    }
    items
  }
  
  output$folder_file_choices_ui <- renderUI({
    input$refresh_folder
    items <- list_loadable_items(DATA_FOLDER)
    if (length(items) == 0) {
      return(helpText(sprintf("Δεν βρέθηκαν αρχεία .xlsx/.xls/.csv στον φάκελο «%s».", DATA_FOLDER)))
    }
    selectizeInput("files_to_load", label = NULL, choices = items, multiple = TRUE,
                   options = list(plugins = list("remove_button"),
                                  placeholder = "Επιλέξτε ένα ή περισσότερα αρχεία (ή φύλλα)..."))
  })
  
  # ---- Load one file into a parsed dataset object (or an error) ----
  load_one <- function(path, ext, sheet = NULL) {
    mat <- tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        sh <- sheet
        if (is.null(sh)) {
          sh <- tryCatch(readxl::excel_sheets(path)[1], error = function(e) NULL)
          if (is.null(sh)) sh <- 1
        }
        as.matrix(readxl::read_excel(path, sheet = sh, col_names = FALSE,
                                     col_types = "text", .name_repair = "minimal"))
      } else {
        as.matrix(readr::read_delim(path, delim = input$csv_delim, col_names = FALSE,
                                    col_types = readr::cols(.default = "c"),
                                    locale = readr::locale(encoding = input$csv_encoding),
                                    trim_ws = FALSE, progress = FALSE))
      }
    }, error = function(e) NULL)
    if (is.null(mat)) return(list(ok = FALSE, msg = "αποτυχία ανάγνωσης αρχείου"))
    
    short_name_row <- NA_integer_; short_title_row <- NA_integer_
    if (isTRUE(input$manual_rows)) {
      hs <- input$header_start; he <- input$header_end; ds <- input$data_start
      bad <- is.null(hs) || is.null(he) || is.null(ds) || is.na(hs) || is.na(he) || is.na(ds) ||
        !(hs >= 1) || !(he >= hs) || !(ds > he)
      if (isTRUE(bad)) return(list(ok = FALSE, msg = "μη έγκυρες χειροκίνητες γραμμές επικεφαλίδας/δεδομένων"))
    } else {
      layout <- detect_layout(mat)
      if (!isTRUE(layout$ok)) return(list(ok = FALSE, msg = "δεν εντοπίστηκε αυτόματα γραμμή επιπέδου 0"))
      hs <- layout$header_start; he <- layout$header_end; ds <- layout$data_start
      short_name_row <- layout$short_name_row; short_title_row <- layout$short_title_row
    }
    
    parsed <- tryCatch(parse_table(mat, hs, he, ds, short_name_row, short_title_row), error = function(e) NULL)
    if (is.null(parsed) || nrow(parsed$df) == 0) return(list(ok = FALSE, msg = "δεν βρέθηκαν έγκυρες γραμμές δεδομένων"))
    
    built <- attach_hierarchy(parsed$df)
    list(ok = TRUE, data = list(df = built$df, value_cols = parsed$value_cols,
                                children_map = built$children_map, root_id = built$root_id,
                                dims = parsed$dims, short_names = parsed$short_names,
                                short_titles = parsed$short_titles, col_aliases = parsed$col_aliases))
  }
  
  observeEvent(input$load_selected_files, {
    req(input$files_to_load)
    ok_names <- c(); fail_msgs <- c()
    for (item in input$files_to_load) {
      parts <- strsplit(item, ITEM_SEP, fixed = TRUE)[[1]]
      fname <- parts[1]
      sheet <- if (length(parts) > 1) parts[2] else NULL
      label <- if (is.null(sheet)) fname else paste0(fname, "  —  ", sheet)
      path  <- file.path(DATA_FOLDER, fname)
      ext   <- tolower(tools::file_ext(path))
      res   <- load_one(path, ext, sheet)
      if (isTRUE(res$ok)) {
        datasets$store[[label]] <- res$data
        ok_names <- c(ok_names, label)
      } else {
        fail_msgs <- c(fail_msgs, paste0(label, ": ", res$msg))
      }
    }
    if (length(ok_names) > 0) {
      showNotification(sprintf("Φορτώθηκαν %d αρχεία/φύλλα επιτυχώς: %s",
                               length(ok_names), paste(ok_names, collapse = ", ")), type = "message")
    }
    if (length(fail_msgs) > 0) {
      showNotification(paste(fail_msgs, collapse = " | "), type = "error", duration = 12)
    }
  })
  
  output$has_datasets <- reactive({ length(datasets$store) > 0 })
  outputOptions(output, "has_datasets", suspendWhenHidden = FALSE)
  
  # ---- Loaded-files list + removal (single static observer, no dynamic per-row observers) ----
  output$loaded_files_ui <- renderUI({
    nms <- names(datasets$store)
    req(length(nms) > 0)
    checkboxGroupInput("keep_files", label = NULL, choices = nms, selected = nms)
  })
  
  observeEvent(input$remove_unchecked_btn, {
    nms <- names(datasets$store)
    req(length(nms) > 0)
    keep <- input$keep_files
    if (is.null(keep)) keep <- character(0)
    to_remove <- setdiff(nms, keep)
    if (length(to_remove) == 0) {
      showNotification("Δεν υπάρχουν αποεπιλεγμένα αρχεία για αφαίρεση.", type = "warning")
      return(invisible(NULL))
    }
    for (nm in to_remove) datasets$store[[nm]] <- NULL
    showNotification(sprintf("Αφαιρέθηκαν: %s", paste(to_remove, collapse = ", ")), type = "message")
  })
  
  # ---- Active dataset selector ----
  output$active_dataset_ui <- renderUI({
    nms <- names(datasets$store)
    req(length(nms) > 0)
    cur <- isolate(input$active_dataset)
    sel <- if (!is.null(cur) && cur %in% nms) cur else nms[length(nms)]
    selectInput("active_dataset", "Ενεργό αρχείο προβολής", choices = nms, selected = sel)
  })
  
  observeEvent(input$active_dataset, { rv$path <- list() }, ignoreInit = TRUE)
  
  hier_active <- reactive({
    req(input$active_dataset)
    ds <- datasets$store[[input$active_dataset]]
    req(ds)
    ds
  })
  
  output$variables_overview_table <- renderDT({
    ds <- hier_active()
    dims <- ds$dims
    if (is.null(dims) || dims$n_levels == 0 || length(ds$value_cols) == 0) {
      return(datatable(
        data.frame(Μήνυμα = "Δεν εντοπίστηκαν πεδία μεταβλητών σε αυτό το αρχείο.", stringsAsFactors = FALSE),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    overview <- data.frame(
      Πεδίο = dims$level_names,
      `Αριθμός τιμών` = vapply(dims$level_values, length, integer(1)),
      `Πιθανές τιμές` = vapply(dims$level_values, function(v) paste(v, collapse = " · "), character(1)),
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(overview, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$column_aliases_table <- renderDT({
    ds <- hier_active()
    if (length(ds$value_cols) == 0) {
      return(datatable(
        data.frame(Μήνυμα = "Δεν υπάρχουν στήλες δεδομένων σε αυτό το αρχείο.", stringsAsFactors = FALSE),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    sn <- ds$short_names; sn[is.na(sn)] <- ""
    st <- ds$short_titles; st[is.na(st)] <- ""
    tbl <- data.frame(
      `Στήλη (πλήρες όνομα)` = ds$value_cols,
      `Short name` = sn,
      `Short title` = st,
      `Όλα τα ψευδώνυμα` = ds$col_aliases,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    datatable(tbl, rownames = FALSE, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # ---- Reset path ----
  observeEvent(input$reset_path, { rv$path <- list() })
  
  # ---- Cascading level drop-downs (unchanged logic, now scoped to hier_active()) ----
  lapply(seq_len(MAX_LEVEL), function(i) {
    local({
      ii <- i
      observeEvent(input[[paste0("sel_", ii)]], {
        val <- input[[paste0("sel_", ii)]]
        rv$path[[as.character(ii)]] <- val
        if (ii < MAX_LEVEL) {
          for (j in (ii + 1):MAX_LEVEL) rv$path[[as.character(j)]] <- NULL
        }
      }, ignoreInit = TRUE, ignoreNULL = FALSE)
    })
  })
  
  output$level_selector_ui <- renderUI({
    ds <- hier_active()
    df <- ds$df
    widgets <- list()
    parent_id <- ds$root_id
    
    for (i in seq_len(MAX_LEVEL)) {
      kids <- ds$children_map[[as.character(parent_id)]]
      if (is.null(kids) || length(kids) == 0) break
      kids_df <- df[df$row_id %in% kids, ]
      kids_df <- kids_df[order(kids_df$row_id), ]
      lvl_label <- kids_df$level[1]
      
      choice_vals <- as.character(kids_df$row_id)
      choice_labels <- paste0(kids_df$name, "  [", kids_df$code, "]")
      choices <- c(setNames("", "— Όλα (δείξε τα παιδιά στον πίνακα) —"),
                   setNames(choice_vals, choice_labels))
      
      sel_val <- rv$path[[as.character(i)]]
      if (is.null(sel_val)) sel_val <- ""
      
      widgets[[length(widgets) + 1]] <- selectInput(
        paste0("sel_", i),
        label = paste0("Επίπεδο ", lvl_label, " (", nrow(kids_df), " επιλογές)"),
        choices = choices, selected = sel_val
      )
      
      if (sel_val == "") break
      parent_id <- as.integer(sel_val)
    }
    do.call(tagList, widgets)
  })
  
  current_node_id <- reactive({
    ds <- hier_active()
    sel <- ds$root_id
    for (i in seq_len(MAX_LEVEL)) {
      v <- rv$path[[as.character(i)]]
      if (is.null(v) || v == "") break
      sel <- as.integer(v)
    }
    sel
  })
  
  output$breadcrumb_ui <- renderUI({
    ds <- hier_active()
    df <- ds$df
    node <- current_node_id()
    chain <- c()
    cur <- node
    guard <- 0
    while (!is.na(cur) && guard < 20) {
      row <- df[df$row_id == cur, ]
      chain <- c(row$name, chain)
      cur <- row$parent_id
      guard <- guard + 1
    }
    div(class = "breadcrumb-box", HTML(paste(sprintf("<b>%s</b>", chain), collapse = " &raquo; ")))
  })
  
  output$node_summary_ui <- renderUI({
    ds <- hier_active()
    df <- ds$df
    node <- current_node_id()
    row <- df[df$row_id == node, ]
    vc <- selected_value_cols()
    vals <- vapply(vc, function(v) {
      x <- row[[v]]
      if (length(x) == 0 || is.na(x)) "—" else format(round(x, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE)
    }, character(1))
    div(class = "node-summary",
        tags$b(sprintf("%s [%s] — Επίπεδο %d", row$name, row$code, row$level)), tags$br(),
        HTML(paste(sprintf("%s: <b>%s</b>", names(vals), vals), collapse = " &nbsp;|&nbsp; "))
    )
  })
  
  # ---- Column (variable) display filter — field-select by dimension ----
  output$column_filter_ui <- renderUI({
    ds <- hier_active()
    ui_fields <- render_field_select_ui("colfilter", ds$dims)
    if (is.null(ui_fields)) helpText("Δεν εντοπίστηκαν πεδία μεταβλητών σε αυτό το αρχείο.") else ui_fields
  })
  
  output$manual_column_filter_ui <- renderUI({
    ds <- hier_active()
    labels <- if (!is.null(ds$col_aliases) && length(ds$col_aliases) == length(ds$value_cols)) {
      ds$col_aliases
    } else {
      ds$value_cols
    }
    choices <- setNames(ds$value_cols, labels)
    selectizeInput("manual_selected_cols", label = NULL, choices = choices,
                   selected = character(0), multiple = TRUE,
                   options = list(plugins = list("remove_button"),
                                  placeholder = "π.χ. Pop, Σύνολο, Total..."))
  })
  
  selected_value_cols <- reactive({
    ds <- hier_active()
    manual <- input$manual_selected_cols
    if (!is.null(manual) && length(manual) > 0) {
      return(intersect(ds$value_cols, manual))
    }
    if (is.null(ds$dims) || ds$dims$n_levels == 0) return(ds$value_cols)
    resolve_cols_from_fields(input, "colfilter", ds$dims)
  })
  
  # ---- Main hierarchy table: children (or leaf descendants) of current node ----
  display_rows <- reactive({
    ds <- hier_active()
    df <- ds$df
    node <- current_node_id()
    
    if (isTRUE(input$show_leaf_descendants)) {
      collect_leaves <- function(id) {
        kids <- ds$children_map[[as.character(id)]]
        if (is.null(kids) || length(kids) == 0) return(id)
        unlist(lapply(kids, collect_leaves))
      }
      ids <- collect_leaves(node)
      if (length(ids) == 1 && ids == node) ids <- node
    } else {
      kids <- ds$children_map[[as.character(node)]]
      ids <- if (is.null(kids) || length(kids) == 0) node else kids
    }
    df[df$row_id %in% ids, , drop = FALSE]
  })
  
  output$data_table <- renderDT({
    vc <- selected_value_cols()
    tbl <- display_rows()[, c("level", "code", "name", vc), drop = FALSE]
    colnames(tbl)[1:3] <- c("Επίπεδο", "Κωδικός", "Περιγραφή")
    dt <- datatable(tbl, rownames = FALSE, filter = "top", selection = "multiple",
                    options = list(pageLength = 15, scrollX = TRUE,
                                   language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/el.json")))
    if (length(vc) > 0) dt <- formatCurrency(dt, vc, currency = "", interval = 3, mark = ".", digits = 0)
    dt
  })
  
  # ---- Flat "whole country, one level" filter ----
  output$flat_level_ui <- renderUI({
    ds <- hier_active()
    lvls <- sort(unique(ds$df$level))
    counts <- vapply(lvls, function(l) sum(ds$df$level == l), integer(1))
    choices <- setNames(as.character(lvls), paste0("Επίπεδο ", lvls, "  (", counts, " εγγραφές)"))
    selectInput("flat_level", "Γεωγραφικό Επίπεδο", choices = choices, selected = as.character(max(lvls)))
  })
  
  flat_rows <- reactive({
    ds <- hier_active()
    req(input$flat_level)
    ds$df[ds$df$level == as.integer(input$flat_level), , drop = FALSE]
  })
  
  output$flat_level_table <- renderDT({
    vc <- selected_value_cols()
    tbl <- flat_rows()[, c("path", "code", "name", vc), drop = FALSE]
    colnames(tbl)[1:3] <- c("Διαδρομή", "Κωδικός", "Περιγραφή")
    dt <- datatable(tbl, rownames = FALSE, filter = "top", selection = "multiple",
                    options = list(pageLength = 20, scrollX = TRUE,
                                   language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/el.json")))
    if (length(vc) > 0) dt <- formatCurrency(dt, vc, currency = "", interval = 3, mark = ".", digits = 0)
    dt
  })
  
  output$download_flat_csv <- downloadHandler(
    filename = function() paste0("elstat_level_", input$flat_level, "_", Sys.Date(), ".csv"),
    content = function(file) {
      vc <- selected_value_cols()
      tbl <- flat_rows()[, c("path", "code", "name", vc), drop = FALSE]
      readr::write_excel_csv(tbl, file)
    }
  )
  
  output$preview_table <- renderDT({
    ds <- hier_active()
    vc <- selected_value_cols()
    tbl <- ds$df[, c("level", "code", "name", vc), drop = FALSE]
    colnames(tbl)[1:3] <- c("Επίπεδο", "Κωδικός", "Περιγραφή")
    datatable(tbl, rownames = FALSE, filter = "top",
              options = list(pageLength = 20, scrollX = TRUE,
                             language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/el.json")))
  })
  
  output$download_csv <- downloadHandler(
    filename = function() paste0("elstat_export_", Sys.Date(), ".csv"),
    content = function(file) {
      vc <- selected_value_cols()
      tbl <- display_rows()[, c("level", "code", "name", vc), drop = FALSE]
      readr::write_excel_csv(tbl, file)
    }
  )
  
  # ---------------------------------------------------------------------
  # Custom table builder
  # ---------------------------------------------------------------------
  
  output$vars_to_add_hier_ui <- renderUI({
    ds <- hier_active()
    ui_fields <- render_field_select_ui("addhier", ds$dims)
    if (is.null(ui_fields)) helpText("Δεν εντοπίστηκαν πεδία μεταβλητών σε αυτό το αρχείο — θα προστεθούν όλες οι στήλες.") else ui_fields
  })
  
  output$vars_to_add_flat_ui <- renderUI({
    ds <- hier_active()
    ui_fields <- render_field_select_ui("addflat", ds$dims)
    if (is.null(ui_fields)) helpText("Δεν εντοπίστηκαν πεδία μεταβλητών σε αυτό το αρχείο — θα προστεθούν όλες οι στήλες.") else ui_fields
  })
  
  # Variable columns 
  add_rows_to_custom <- function(rows_df, vars, source_label, all_vars) {
    if (nrow(rows_df) == 0) return(FALSE)
    use_vars <- if (is.null(vars) || length(vars) == 0) all_vars else vars
    if (length(use_vars) == 0) return(FALSE)
    id_cols <- rows_df[, c("level", "code", "name", "path"), drop = FALSE]
    colnames(id_cols) <- c("Επίπεδο", "Κωδικός", "Περιγραφή", "Διαδρομή")
    var_block <- rows_df[, use_vars, drop = FALSE]
    colnames(var_block) <- paste0(source_label, " — ", use_vars)
    block <- cbind(data.frame(Αρχείο = source_label, stringsAsFactors = FALSE), id_cols, var_block)
    custom_df(bind_rows_base(list(custom_df(), block)))
    TRUE
  }
  
  
  rows_to_add_hier <- reactive({
    all_rows <- display_rows()
    sel <- input$data_table_rows_selected
    if (!is.null(sel) && length(sel) > 0) all_rows[sel, , drop = FALSE] else all_rows
  })
  
  rows_to_add_flat <- reactive({
    all_rows <- flat_rows()
    sel <- input$flat_level_table_rows_selected
    if (!is.null(sel) && length(sel) > 0) all_rows[sel, , drop = FALSE] else all_rows
  })
  
  observeEvent(input$add_to_custom_hier, {
    rows <- rows_to_add_hier()
    ds <- hier_active()
    vars <- if (is.null(ds$dims) || ds$dims$n_levels == 0) ds$value_cols else resolve_cols_from_fields(input, "addhier", ds$dims)
    ok <- add_rows_to_custom(rows, vars, input$active_dataset, ds$value_cols)
    if (isTRUE(ok)) {
      showNotification(sprintf("Προστέθηκαν %d γραμμές στον προσαρμοσμένο πίνακα.", nrow(rows)), type = "message")
    } else {
      showNotification("Δεν υπάρχουν γραμμές ή μεταβλητές προς προσθήκη.", type = "warning")
    }
  })
  
  observeEvent(input$add_to_custom_flat, {
    rows <- rows_to_add_flat()
    ds <- hier_active()
    vars <- if (is.null(ds$dims) || ds$dims$n_levels == 0) ds$value_cols else resolve_cols_from_fields(input, "addflat", ds$dims)
    ok <- add_rows_to_custom(rows, vars, input$active_dataset, ds$value_cols)
    if (isTRUE(ok)) {
      showNotification(sprintf("Προστέθηκαν %d γραμμές στον προσαρμοσμένο πίνακα.", nrow(rows)), type = "message")
    } else {
      showNotification("Δεν υπάρχουν γραμμές ή μεταβλητές προς προσθήκη.", type = "warning")
    }
  })
  
  observeEvent(input$clear_custom_btn, {
    custom_df(NULL)
    showNotification("Ο προσαρμοσμένος πίνακας καθαρίστηκε.", type = "message")
  })
  
  # Row selection
  observeEvent(input$remove_selected_custom_btn, {
    df <- custom_df()
    sel <- input$custom_table_rows_selected
    if (is.null(df) || is.null(sel) || length(sel) == 0) {
      showNotification("Επιλέξτε πρώτα μία ή περισσότερες γραμμές στον πίνακα (κλικ πάνω τους).", type = "warning")
      return(invisible(NULL))
    }
    remaining <- df[-sel, , drop = FALSE]
    custom_df(if (nrow(remaining) == 0) NULL else remaining)
    showNotification(sprintf("Αφαιρέθηκαν %d γραμμές.", length(sel)), type = "message")
  })
  
  output$custom_table <- renderDT({
    df <- custom_df()
    if (is.null(df)) {
      return(datatable(
        data.frame(Μήνυμα = "Δεν έχουν προστεθεί ακόμα δεδομένα. Χρησιμοποιήστε το κουμπί «➕ Προσθήκη» στις καρτέλες «Πίνακας» ή «Φίλτρο ανά Επίπεδο».", stringsAsFactors = FALSE),
        rownames = FALSE, options = list(dom = "t")
      ))
    }
    datatable(df, rownames = FALSE, filter = "top", selection = "multiple",
              options = list(pageLength = 15, scrollX = TRUE,
                             language = list(url = "//cdn.datatables.net/plug-ins/1.13.6/i18n/el.json")))
  })
  
  output$download_custom_csv <- downloadHandler(
    filename = function() paste0("custom_table_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- custom_df()
      req(df)
      readr::write_excel_csv(df, file)
    }
  )
  
  output$download_custom_xlsx <- downloadHandler(
    filename = function() paste0("custom_table_", Sys.Date(), ".xlsx"),
    content = function(file) {
      df <- custom_df()
      req(df)
      writexl::write_xlsx(df, file)
    }
  )
  
  # ---------------------------------------------------------------------
  # Chart builder — works off the custom table, reusing its row selection
  # ---------------------------------------------------------------------
  
  CUSTOM_ID_COLS <- c("Αρχείο", "Επίπεδο", "Κωδικός", "Περιγραφή", "Διαδρομή")
  
  output$chart_vars_ui <- renderUI({
    df <- custom_df()
    if (is.null(df)) return(helpText("Δεν υπάρχουν ακόμα δεδομένα στον προσαρμοσμένο πίνακα."))
    vars <- setdiff(colnames(df), CUSTOM_ID_COLS)
    if (length(vars) == 0) return(helpText("Δεν υπάρχουν αριθμητικές μεταβλητές στον πίνακα."))
    selectizeInput("chart_vars", "Μεταβλητές για το γράφημα", choices = vars, selected = NULL,
                   multiple = TRUE, options = list(plugins = list("remove_button"),
                                                   placeholder = "Επιλέξτε μία ή περισσότερες μεταβλητές..."))
  })
  

  chart_rows <- reactive({
    df <- custom_df()
    req(df)
    sel <- input$custom_table_rows_selected
    if (!is.null(sel) && length(sel) > 0) df[sel, , drop = FALSE] else df
  })
  

  make_unique_labels <- function(x) {
    x <- as.character(x)
    if (anyDuplicated(x) == 0) return(x)
    stats::ave(x, x, FUN = function(v) if (length(v) > 1) paste0(v, " (", seq_along(v), ")") else v)
  }
  
  output$custom_chart <- renderPlotly({
    rows <- chart_rows()
    vars <- input$chart_vars
    validate(need(length(vars) > 0, "Επιλέξτε τουλάχιστον μία μεταβλητή για το γράφημα."))
    validate(need(nrow(rows) > 0, "Δεν υπάρχουν γραμμές για το γράφημα."))
    
    labels <- make_unique_labels(rows[["Περιγραφή"]])
    vals <- rows[, vars, drop = FALSE]
    for (v in vars) vals[[v]] <- suppressWarnings(as.numeric(vals[[v]]))
    
    if (isTRUE(input$chart_percent)) {
      totals <- rowSums(vals, na.rm = TRUE)
      vals <- as.data.frame(lapply(vals, function(col) ifelse(totals > 0, col / totals * 100, NA)),
                            stringsAsFactors = FALSE)
      colnames(vals) <- vars
    }
    
    y_title <- if (isTRUE(input$chart_percent)) "%" else "Τιμή"
    
    if (identical(input$chart_type, "pie")) {
      if (nrow(rows) == 1) {
        plot_ly(labels = vars, values = unlist(vals[1, ], use.names = FALSE), type = "pie",
                textinfo = "label+percent", hoverinfo = "label+value+percent") %>%
          layout(title = labels[1])
      } else if (length(vars) == 1) {
        plot_ly(labels = labels, values = vals[[1]], type = "pie",
                textinfo = "label+percent", hoverinfo = "label+value+percent") %>%
          layout(title = vars[1])
      } else {
        validate("Η πίτα χρειάζεται είτε 1 γραμμή με πολλές μεταβλητές, είτε 1 μεταβλητή με πολλές γραμμές. Δοκιμάστε ράβδους, ή περιορίστε την επιλογή σας.")
      }
    } else {
      p <- plot_ly()
      for (v in vars) {
        p <- add_trace(p, x = labels, y = vals[[v]], type = "bar", name = v)
      }
      p %>% layout(barmode = "group", xaxis = list(title = ""), yaxis = list(title = y_title))
    }
  })
}

shinyApp(ui, server)
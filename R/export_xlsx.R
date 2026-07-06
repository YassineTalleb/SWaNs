xml_escape <- function(text) {
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub("\"", "&quot;", text, fixed = TRUE)
  text <- gsub("'", "&apos;", text, fixed = TRUE)
  text
}

xlsx_column_name <- function(index) {
  letters_out <- character(0)
  while (index > 0) {
    remainder <- (index - 1L) %% 26L
    letters_out <- c(intToUtf8(65L + remainder), letters_out)
    index <- (index - 1L) %/% 26L
  }
  paste0(letters_out, collapse = "")
}

xlsx_cell_xml <- function(value, row_index, col_index) {
  cell_ref <- paste0(xlsx_column_name(col_index), row_index)

  if (is.na(value) || identical(value, "")) {
    return(sprintf('<c r="%s"/>', cell_ref))
  }

  if (is.numeric(value)) {
    return(sprintf('<c r="%s"><v>%s</v></c>', cell_ref, as.character(value)))
  }

  text_value <- as.character(value)
  preserve <- grepl("^\\s|\\s$|\\s{2,}", text_value)
  t_attr <- if (preserve) ' xml:space="preserve"' else ""

  sprintf(
    '<c r="%s" t="inlineStr"><is><t%s>%s</t></is></c>',
    cell_ref,
    t_attr,
    xml_escape(text_value)
  )
}

write_simple_xlsx <- function(path, sheet_name, data) {
  data <- as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  output_path <- if (grepl("^([A-Za-z]:|/|\\\\\\\\)", path)) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(file.path(getwd(), path), winslash = "/", mustWork = FALSE)
  }

  for (column_name in names(data)) {
    if (is.factor(data[[column_name]])) {
      data[[column_name]] <- as.character(data[[column_name]])
    }
  }

  temp_dir <- tempfile("xlsx_export_")
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dir.create(file.path(temp_dir, "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "xl", "_rels"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(temp_dir, "xl", "worksheets"), recursive = TRUE, showWarnings = FALSE)

  sheet_rows <- character(0)

  header_cells <- vapply(
    seq_along(names(data)),
    function(col_index) xlsx_cell_xml(names(data)[[col_index]], 1L, col_index),
    character(1)
  )
  sheet_rows[[1]] <- sprintf('<row r="1">%s</row>', paste(header_cells, collapse = ""))

  if (nrow(data) > 0) {
    for (row_index in seq_len(nrow(data))) {
      xml_cells <- vapply(
        seq_len(ncol(data)),
        function(col_index) {
          xlsx_cell_xml(data[[col_index]][[row_index]], row_index + 1L, col_index)
        },
        character(1)
      )
      sheet_rows[[row_index + 1L]] <- sprintf(
        '<row r="%d">%s</row>',
        row_index + 1L,
        paste(xml_cells, collapse = "")
      )
    }
  }

  worksheet_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<sheetData>',
    paste(sheet_rows, collapse = ""),
    '</sheetData>',
    '</worksheet>'
  )

  workbook_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
    '<sheets>',
    sprintf(
      '<sheet name="%s" sheetId="1" r:id="rId1"/>',
      xml_escape(sheet_name)
    ),
    '</sheets>',
    '</workbook>'
  )

  workbook_rels_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" ',
    'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" ',
    'Target="worksheets/sheet1.xml"/>',
    '<Relationship Id="rId2" ',
    'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles" ',
    'Target="styles.xml"/>',
    '</Relationships>'
  )

  root_rels_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
    '<Relationship Id="rId1" ',
    'Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" ',
    'Target="xl/workbook.xml"/>',
    '</Relationships>'
  )

  styles_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<styleSheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
    '<fonts count="1"><font><sz val="11"/><name val="Calibri"/></font></fonts>',
    '<fills count="1"><fill><patternFill patternType="none"/></fill></fills>',
    '<borders count="1"><border/></borders>',
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
    '<cellXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/></cellXfs>',
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
    '</styleSheet>'
  )

  content_types_xml <- paste0(
    '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ',
    'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
    '<Override PartName="/xl/worksheets/sheet1.xml" ',
    'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
    '<Override PartName="/xl/styles.xml" ',
    'ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml"/>',
    '</Types>'
  )

  writeLines(content_types_xml, file.path(temp_dir, "[Content_Types].xml"), useBytes = TRUE)
  writeLines(root_rels_xml, file.path(temp_dir, "_rels", ".rels"), useBytes = TRUE)
  writeLines(workbook_xml, file.path(temp_dir, "xl", "workbook.xml"), useBytes = TRUE)
  writeLines(workbook_rels_xml, file.path(temp_dir, "xl", "_rels", "workbook.xml.rels"), useBytes = TRUE)
  writeLines(styles_xml, file.path(temp_dir, "xl", "styles.xml"), useBytes = TRUE)
  writeLines(worksheet_xml, file.path(temp_dir, "xl", "worksheets", "sheet1.xml"), useBytes = TRUE)

  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(temp_dir)

  files_to_zip <- c(
    "[Content_Types].xml",
    "_rels/.rels",
    "xl/workbook.xml",
    "xl/_rels/workbook.xml.rels",
    "xl/styles.xml",
    "xl/worksheets/sheet1.xml"
  )

  if (file.exists(output_path)) {
    unlink(output_path, force = TRUE)
  }

  suppressWarnings(
    try(
      utils::zip(
        zipfile = output_path,
        files = files_to_zip,
        flags = "-r9Xq"
      ),
      silent = TRUE
    )
  )

  if (!file.exists(output_path) && .Platform$OS.type == "windows") {
    temp_dir_ps <- gsub("'", "''", normalizePath(temp_dir, winslash = "\\", mustWork = TRUE), fixed = TRUE)
    path_ps <- gsub("'", "''", normalizePath(output_path, winslash = "\\", mustWork = FALSE), fixed = TRUE)
    ps_command <- paste0(
      "Set-Location -LiteralPath '", temp_dir_ps, "'; ",
      "Compress-Archive -Path * -DestinationPath '", path_ps, "' -Force"
    )
    status <- suppressWarnings(
      system2("powershell", c("-NoProfile", "-Command", ps_command))
    )

    if (!identical(status, 0L) || !file.exists(output_path)) {
      stop("The summary table could not be written as an Excel file.", call. = FALSE)
    }
  }

  if (!file.exists(output_path)) {
    stop("The summary table could not be written as an Excel file.", call. = FALSE)
  }
}

format_display_token <- function(token) {
  token <- enc2utf8(as.character(token))

  if (is.na(token) || !nzchar(token)) {
    return("")
  }

  if (grepl("^[A-Z0-9-]+$", token)) {
    return(token)
  }

  if (grepl("^[0-9]+$", token)) {
    return(token)
  }

  if (grepl("^[a-z]+$", token) || grepl("^[a-z]+[0-9-]+$", token)) {
    return(paste0(toupper(substr(token, 1, 1)), substr(token, 2, nchar(token))))
  }

  token
}

format_identifier_label <- function(value) {
  if (length(value) > 1L) {
    return(vapply(value, format_identifier_label, character(1)))
  }

  value <- enc2utf8(as.character(value))

  if (is.na(value) || !nzchar(trimws(value))) {
    return("")
  }

  clean_value <- gsub("_+", " ", trimws(as.character(value)))
  clean_value <- gsub("\\s+", " ", clean_value)
  parts <- strsplit(clean_value, " ", fixed = TRUE)[[1]]

  paste(vapply(parts, format_display_token, character(1)), collapse = " ")
}

format_identifier_choices <- function(values) {
  values <- as.character(values)
  stats::setNames(values, format_identifier_label(values))
}

format_identifier_sequence <- function(value) {
  if (length(value) > 1L) {
    return(vapply(value, format_identifier_sequence, character(1)))
  }

  if (is.na(value) || !nzchar(trimws(value))) {
    return("")
  }

  pieces <- trimws(strsplit(as.character(value), ",", fixed = TRUE)[[1]])
  pieces <- pieces[nzchar(pieces)]

  if (!length(pieces)) {
    return("")
  }

  paste(format_identifier_label(pieces), collapse = ", ")
}

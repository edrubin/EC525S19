# Function to remove '--' from xaringan slides (especially helpful for PDF generation)

unpause <- function(file_name, path = ".", pdf_paused = F, pdf_unpaused = F) {

  # Load packages
  library(pacman)
  p_load(stringr, magrittr)

  # Read .Rmd file's text
  rmd_text <- paste0(path, "/", file_name) %>% readLines()
  # Drop rows with '--' (must start with '--' and cannot be followed by '-')
  new_text <- rmd_text[!str_detect(rmd_text, "^-{2}(?!-)")]
  # Drop rows that reference 'unpause' (don't want infinite loops)
# HACK: Problematic if unpause() call spans multiple lines
# FIXME: Search for closing of parenthesis in unpause() call?
  new_text <- new_text[!str_detect(new_text, "unpause\\(")]

  # New file name
  new_file_name <- str_replace(file_name, "\\.Rmd", "_NoPause.Rmd") %>%
    str_replace("\\.rmd", "_NoPause.rmd") %>%
    paste0(path, "/", .)
  # Find the root of the file names (drop suffix). Helpful for html/pdf naming later.
  new_file_root <- new_file_name %>% str_remove("\\.Rmd$") %>% str_remove("\\.rmd$")
  old_file_root <- paste0(path, "/", file_name) %>%
    str_remove("\\.Rmd$") %>% str_remove("\\.rmd$")

  # Save the updated .Rmd script
  con <- file(new_file_name)
  writeLines(new_text, con)
  close(con)

  # Render unpaused .Rmd script to HTML slides
# HACK: Executing with 'system' due to conflicts with same-name code chunks
  output_format <- "xaringan::moon_reader"
  system(paste0(
    "Rscript --vanilla -e \'rmarkdown::render(",
    paste0("\"", new_file_name, "\""),
    ", ",
    paste0("\"", output_format, "\""),
    ")\'"
  ))

  # Generate PDFs for paused and unpaused slides (if requested)
  # Paused PDF slides
  if (pdf_paused) {
    xaringan::decktape(
      file = paste0(old_file_root, ".html "),
      output = paste0(old_file_root, ".pdf")
    )
  }
  # Unpaused PDF slides
  if (pdf_unpaused) {
    xaringan::decktape(
      file = paste0(new_file_root, ".html "),
      output = paste0(new_file_root, ".pdf")
    )
  }

}

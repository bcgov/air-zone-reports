render_intermediate <- function(file, 
                                ozone_ems_ids = NA_character_, 
                                pm25_ems_ids = NA_character_, 
                                summary_report = FALSE, ...) {
  
  outdir <- "intermediate_reports"
  
  dir.create(outdir, showWarnings = FALSE)
  
  outfile <- gsub(".Rmd", "_intermediate.pdf", file)
  outmd <- gsub("pdf", "md", outfile)
  
  logo_tex <- header_logo_tex()
  on.exit(unlink(logo_tex))
  
  rmarkdown::render(
    file,
    output_dir = outdir, 
    params = if (!summary_report) {
      list(ems_ids_ozone = ozone_ems_ids, 
                  ems_ids_pm25 = pm25_ems_ids)
      },
    output_format = rmarkdown::pdf_document(
      latex_engine = "lualatex",
      keep_md = TRUE,
      fig_caption = TRUE,
      includes =  rmarkdown::includes(
        in_header =  c(if (!summary_report) "components/header.tex", 
                       logo_tex)
      ), 
      md_extensions = "+raw_attribute",
      ...),
    output_file = outfile
  )
  
  message("\n*****\n1. Preview \"", file.path(outdir, outfile), "\".\n",
          "2. Edit \"", file.path(outdir, outmd), "\".\n",
          "3. Finally, run:\n    render_final(\"", 
          file.path(outdir, outmd), "\")\n*****\n")
}

render_final <- function(file, include_header = TRUE, ...) {
  out_dir <- "final_reports"
  
  dir.create(out_dir, showWarnings = FALSE)
  
  outfile <- gsub("intermediate.md", "final.pdf", basename(file))
  
  logo_tex <- header_logo_tex()
  on.exit(unlink(logo_tex))
  
  rmarkdown::render(
    file,
    output_dir = out_dir, 
    intermediates_dir = "final_reports",
    output_format = rmarkdown::pdf_document(
      latex_engine = "lualatex",
      fig_caption = TRUE,
      includes =  rmarkdown::includes(
        in_header =  c(if (include_header) "../components/header.tex", 
                       logo_tex)
      ), 
      ...
    ),
    output_file = file.path(out_dir, outfile)
  )
}

header_logo_tex <- function() {
  path <- normalizePath("components/BC_MoE.png", mustWork = TRUE)
  
  tmp <- tempfile(fileext = ".tex")
    
    writeLines(sprintf(
    "\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\fancypagestyle{firststyle}
{
  \\renewcommand{\\headrulewidth}{0pt}
  \\fancyhf{}
  \\setlength\\headheight{45pt}
 \\fancyhead[R]{\\includegraphics[width=4cm]{%s}}
}", path), 
    con = tmp
  )
    tmp
}

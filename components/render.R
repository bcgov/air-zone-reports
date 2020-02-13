render_intermediate <- function(file, 
                            ozone_ems_ids = NA_character_, 
                            pm25_ems_ids = NA_character_) {
  
  outdir <- "intermediate_reports"
  
  dir.create(outdir, showWarnings = FALSE)
  
  outfile <- gsub(".Rmd", "_intermediate.pdf", file)
  outmd <- gsub("pdf", "md", outfile)
  
  render(
    file,
    output_dir = outdir, 
    params = list(ems_ids_ozone = ozone_ems_ids, 
                  ems_ids_pm25 = pm25_ems_ids),
    output_format = "pdf_document",
    output_file = outfile
  )
  message("\n*****\n1. Preview \"", file.path(outdir, outfile), "\".\n",
          "2. Edit \"", file.path(outdir, outmd), "\".\n",
          "3. Finally, run:\n    render_final(\"", 
          file.path(outdir, outmd), "\")\n*****\n")
}

render_final <- function(file) {
  out_dir <- "final_reports"
  
  dir.create(out_dir, showWarnings = FALSE)
  
  outfile <- gsub("intermediate.md", "final.pdf", basename(file))
  
  render(
    file,
    output_dir = out_dir, 
    knit_root_dir = ".",
    output_format = pdf_document(latex_engine = "lualatex",
                                 fig_caption =  TRUE,
                                 keep_md = FALSE,
                                 includes = includes(in_header = "../components/header.tex")),
    output_file = file.path(out_dir, outfile)
  )
}

#Processes R markdown files
#knit them to html, then tweak to make it compatible to shiny

library(dplyr)
library(rmarkdown)

prev_wd <- getwd()


#define functions-------------
# convert html to R-shiny html. This removes all text in html that is outsive <bod></body>
#' Modify html to remove unneeded part and make it compatible with R Shiny
#' 
fix_shinyhtml <- function(inputhtml, outputhtml) {
  # Set the path to the input and output files
  input_file <-inputhtml
  output_file <- outputhtml
  
  # Read the contents of the input file
  html <- readLines(input_file)
  
  # Find the start and end positions of the <body> tag
  start_pos <- grep("<body", html)
  end_pos <- grep("</body>", html)
  
  # Extract the content between the <body> and </body> tags
  body_content <- html[start_pos:end_pos]
  
  # Write the body content to the output file
  writeLines(body_content, output_file)
  
} 
#end of function--------

# Get the directory where the current R script file is located
script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

filelocation <- script_dir
#knit markdowns to html
setwd(filelocation)
files <- list.files(full.names = TRUE)
files <- list.files()
files <- files[grepl('.Rmd',files)]

for (files_ in files) {
  
  rmd_path <- file.path(files_)
  html_path <-  gsub("\\.Rmd$", ".html", rmd_path)
  
  rmarkdown::render(input = files_,output_file  = html_path,output_format = 'html_document')
}



files <- list.files(full.names = TRUE)

files <- files[grepl('.html',files)]

for (files_ in files) {
  fix_shinyhtml(files_,files_)
}

setwd(prev_wd)

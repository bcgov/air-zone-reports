# -knits the main page and modifed accordingly

list.files()

# -define parameter
file_markdown <- './level_main/00_MainPage.Rmd'
file_html <- './level_main/airzone_report.html'

# -convert the markdown to html
# knitr::knit(file_markdown,output = file_html)
# !Please knit the markdown using the knit function

# -insert the title page
file.rename('./level_main/00_MainPage.html',file_html)
html_content <- readLines(file_html,warn = FALSE)
# Find the position to insert the <title> entry
head_position <- grep("<head>", html_content)
insert_position <- head_position[1] + 1

# Insert the <title> entry
title_entry <- "<title>B.C. Air Zone Report 2019-2021</title>"
html_content <- c(html_content[1:insert_position], title_entry, html_content[(insert_position + 1):length(html_content)])

# Write the modified HTML back to the file
writeLines(html_content, file_html)

cat("Title entry inserted successfully!\n")

cd ~/Dropbox/data/flags

/usr/local/bin/R -f "flagColors_json.R"
/usr/local/bin/R -f "flagColors_data.R"

/usr/local/bin/R -e 'rmarkdown::render("index.Rmd", "bookdown::html_document2")'

sh _to_website_offline.sh
sh _to_website.sh

#THIS FILE IS TO SAVE THE WORKSPACE FOR THE R MARKDOWN.
library(readr)
dataHolidays <- read_csv("~/TFG/CSV/dataHolidays.csv")
airports <- read_csv("~/TFG/CSV/airports.csv")
set.seed("1234")
#Random 500000 rows for more agile plots.
dataReducted <- dataHolidays[sample(nrow(dataHolidays), 500000), ]
save.image("~/TFG/sessionMarkdown.RData")

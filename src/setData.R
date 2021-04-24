library(jsonlite)
source("./src/getData.R")

jsContent <- content(get())

jsDataTable <- as.data.frame(jsContent)

jsHits <- jsDataTable$hits.hits._id.1

print(jsHits)
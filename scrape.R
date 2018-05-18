
source("functions.R")

# another run (+save all)
startDates <- seq(from = as.Date("2018-07-01"), to = as.Date("2018-08-01"), by = "day")
durations <- 13
endDates <- outer(startDates, durations, `+`)
testDF <- data.frame(inb = as.character(startDates), outb = as.character(endDates), stringsAsFactors = FALSE)

testRes2 <- getPriceDF(testDF)
saveRDS(testRes2, "resJ.rds")

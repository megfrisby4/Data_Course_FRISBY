csvs <- list.files(path = "Data", pattern = ".csv", full.names = TRUE)
readLines(csvs[1],n=1)
for (lines in csvs) {print(readLines(lines,1-5
                                    ))
}

# Assignment 2-----------------------------------------------------------

#4
csv_files <- list.files(path = "Data", pattern = ".csv")

#5
length(csv_files)

#6
df <- read.csv("Data/wingspan_vs_mass.csv")

#7
head(csv_files, n = 5)

#8
list.files(recursive = TRUE, 
           path = "Data", 
           pattern = "^b")

#9
b <- list.files(recursive = TRUE, 
                path = "Data",
                full.names = TRUE,
                pattern = "^b")

for(i in 1:length(b)) print(readLines(b[i], n = 1))


#10
b <- list.files(recursive = TRUE, 
                path = "Data",
                full.names = TRUE,
                pattern = ".csv$")

for(i in 1:length(b)) print(readLines(b[i], n = 1))
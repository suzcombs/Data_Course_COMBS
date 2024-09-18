# Saves all the .csv files found in the Data/ directory
csv_files <- list.files(path = "Data/", recursive = FALSE, pattern = ".csv")

# How many .csv files found in Data/ directory
length(csv_files)
  
# Read wingspan_vs_mass.csv and store contents
df <- read.csv("Data/wingspan_vs_mass.csv")

# Inspect the first 5 lines of df
head(df, 5)
  
# Recursively find files in Data/ directory with letter "b"
has_b <- list.files(path = "Data/", recursive = TRUE, pattern = "b", full.names = TRUE)

# For-loop displays first line of each file in has_b
for (i in has_b) {
  first_line <- readLines(i, n = 1)
  print(first_line)
}

# For-loop displays first line of each .csv file
for (j in csv_files) {
  first_line <- readLines(i, n = 1)
  print(first_line)
}

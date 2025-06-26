library(fs)
# Get the file sizes of all files in the directory and its subdirectories
files_info <- dir_info("/Users/cameronnealy/Documents/Desktop/Bchron:Rstudio/Air_Quality/Shiny_DVtool_v2_copy", recurse = TRUE)

# Calculate the total size of all files
total_size <- sum(files_info$size)

# Convert the size to a human-readable format (e.g., MB, GB)
total_size_mb <- total_size / (1024^2)
total_size_gb <- total_size / (1024^3)

# Print the result
cat("Total size in MB:", total_size_mb, "\n")
cat("Total size in GB:", total_size_gb, "\n")

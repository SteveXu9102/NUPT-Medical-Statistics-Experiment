# R script for NUPT Medical Statistics Experiment
# Experiment Name: SAMPLING AND ITS DISTRIBUTION
# Generates uniformly / normally distributed population,
# samples them, each for multiple times;
# calculates statistics data and draws graphs
# of each sample for analysis.
# Author: Steve Hsu
# Email: stevexu9012@gmail.com
# Version 1.0
# Usage: Source this file in R console and get output.

# Statistics values calculation & output
datastats <- function(all_data) {
  # Data type judgement
  if (!is.data.frame(all_data)) {
    stop("Parameter is not a data.frame")
  }
  cat("Mean Value: ")
  cat(mean(all_data$value))
  cat("\nMedian: ")
  cat(median(all_data$value))
  cat("\nSum: ")
  cat(sum(all_data$value))
  cat("\nSummary: \n")
  print(summary(all_data$value))
  cat("Variance: ")
  cat(var(all_data$value))
  cat("\nStandard Deviation: ")
  cat(sd(all_data$value))
  cat("\n")
  rm(all_data)
}

# Sampling and graph generation for uniform population
sa_and_draw_uni <- function(alldata_uni) {
  # Data type judgement
  if (!is.data.frame(alldata_uni)) {
    stop("Parameter is not a data.frame")
  }
  cat("\nHandling uniformly distributed random nums.\n")
  cat("\nPopulation:\n")
  datastats(alldata_uni) # Gets population stat data first
  # Strings used multiple times
  str1 <- "Sample num:"
  str2 <- "./gen_exp1/hist_uniform_num.pdf"
  str3 <- "./gen_exp1/plot_uniform_num.pdf"
  str4 <- "./gen_exp1/samplenum_uniform.txt"
  # Samples 6 times
  for (i in (1:6)) {
    cat("\n", paste(gsub("num", i, str1), "\n", sep = ""))
    # Gets single sample
    sampled_data <- data.frame(sample(alldata_uni$value, 750, replace = TRUE))
    colnames(sampled_data) <- "value" # Renames column
    datastats(sampled_data) # Gets sample stat data
    # Histogram generation
    pdf(gsub("num", i, str2)) # Using gsub() for unique file names gen.
    # Specifies title and x label to beautify histogram
    hist(sampled_data$value, main = bquote("Histogram of Sample " ~ .(i)), xlab = "Sample Values")
    dev.off()
    # Plot generation
    pdf(gsub("num", i, str3))
    # Specifies y label to beautify plot
    plot(sampled_data$value, ylab = bquote("Sample " ~ .(i) ~ " Values"))
    dev.off()
    cat("Plot and Histogram saved.\n")
    # Saves data of each sample
    write.table(sampled_data, gsub("num", i, str4), quote = FALSE, row.names = FALSE, sep = "\t")
    cat("Sample data saved to text.\n")
  }
}

# Sampling and graph generation for normal population
sa_and_draw_norm <- function(alldata_norm) {
  # Data type judgement
  if (!is.data.frame(alldata_norm)) {
    stop("Parameter is not a data.frame")
  }
  cat("\nHandling normally distributed random nums.\n")
  cat("\nPopulation:\n")
  datastats(alldata_norm) # Gets population stat data first
  # Strings used multiple times
  str1 <- "Sample num:"
  str2 <- "./gen_exp1/hist_normal_num.pdf"
  str3 <- "./gen_exp1/plot_normal_num.pdf"
  str4 <- "./gen_exp1/samplenum_normal.txt"
  # Samples 6 times
  for (i in (1:6)) {
    cat("\n", paste(gsub("num", i, str1), "\n", sep = ""))
    # Gets single sample
    sampled_data <- data.frame(sample(alldata_norm$value, 500, replace = TRUE))
    colnames(sampled_data) <- "value" # Renames column
    datastats(sampled_data) # Gets sample stats data
    pdf(gsub("num", i, str2)) # Histogram generation
    # Gets histogram object to get max density
    # No histogram drawn at this point
    hist_obj <- hist(sampled_data$value, plot = FALSE)
    max_den <- max(hist_obj$density)
    ylim_range <- c(0, max_den * 1.1) # Defines y limit using max den.
    # THIS line draws actual histogram
    # Specifies title, y limit and x label to beautify histogram
    hist(sampled_data$value, prob = TRUE, main = expression("Normal " ~ mu == 170 ~ "," ~ sigma == 16), ylim = ylim_range, xlab = "Sample Values")
    # Adds corresponding normal dist. curve for comparison
    curve(dnorm(x, 170, 16), add = TRUE)
    dev.off()
    # Plot generation
    pdf(gsub("num", i, str3))
    # Specifies y label to beautify plot
    plot(sampled_data$value, ylab = bquote("Sample " ~ .(i) ~ " Values"))
    dev.off()
    cat("Plot and Histogram saved.\n")
    # Saves data of each sample
    write.table(sampled_data, gsub("num", i, str4), quote = FALSE, row.names = FALSE, sep = "\t")
    cat("Sample data saved to text.\n")
  }
}

if (!dir.exists("./gen_exp1")) {
  dir.create("./gen_exp1")
}

dataall <- data.frame(runif(10000, 0, 1)) # Generates uniform dist. population
dataall_normal <- data.frame(rnorm(10000, 170, 16)) # Generates normal dist. population
colnames(dataall) <- "value" # Renames column
colnames(dataall_normal) <- "value" # Renames column

# Saves population data as text
write.table(dataall, "./gen_exp1/dataall.txt", quote = FALSE, row.names = FALSE, sep = "\t")
write.table(dataall_normal, "./gen_exp1/dataall_normal.txt", quote = FALSE, row.names = FALSE, sep = "\t")
cat("Populations of uniformly and randomly distribution nums saved to separate text files.\n")

# Starts sampling
sa_and_draw_uni(dataall) # For uniform dist.
sa_and_draw_norm(dataall_normal) # For normal dist.

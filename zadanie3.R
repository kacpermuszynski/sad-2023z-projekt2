library(truncdist)

filename <- 'signal_50MHz.bin'
zz <- file(filename, "rb")

BajtowNaLiczbe = 4
fsize = file.size(filename)
LiczbaLiczb = fsize / BajtowNaLiczbe
v<-readBin(zz, numeric(), size=BajtowNaLiczbe, endian="little", n=LiczbaLiczb)
close(zz)

# Get threshold of impulse values
threshold = quantile(v, 0.9997)

detect_impulses <- function(signal, threshold) {
  impulses <- list()
  impulse_start <- NULL
  last_impulse <- -10
  
  for (i in seq_along(signal)) {
    value <- signal[i]
    
    if (value > threshold) {
      last_impulse <- i
      impulse_start <- ifelse(is.null(impulse_start), i, impulse_start)
    } else if (!is.null(impulse_start) && i > last_impulse + 10) {
      start_with_margin <- max(impulse_start - 30, 1)
      end_with_margin <- min(i - 1 + 50, length(signal))
      impulses <- c(impulses, list(c(start_with_margin, end_with_margin)))
      impulse_start <- NULL
    }
  }
  
  if (!is.null(impulse_start)) {
    impulses <- c(impulses, list(c(impulse_start - 30, length(signal))))
  }
  
  return(impulses)
}

# Detect indices of each start and end of impulse with margin (start-30, end+50)
impulses = detect_impulses(v, threshold)

detect_peaks <- function(impulses){
impulses_exact_indices <- list()
for (impulse in impulses) {
  data <- v[impulse[1]:impulse[2]]
  max_value <- max(data)
  max_index <- which.max(data) + impulse[1] - 1
  impulses_exact_indices <- c(impulses_exact_indices, max_index)
}
impulses_exact_indices <- as.numeric(impulses_exact_indices)
return(impulses_exact_indices)
}

# Detect peaks of each impulse
peaks <- detect_peaks(impulses)

# Calculate distances between impulse peaks
distances <- diff(peaks)

# Calculate lengths of each signal
l <- sapply(impulses, function(inner_list) as.numeric(inner_list[[2]]) - as.numeric(inner_list[[1]]))

# Calculate statistics of signal length
mean(l)
max(l)
min(l)
median(l)

# Estimate lambda with different x_0 values
lambda_mean = length(distances)/(sum(distances) - length(distances)*mean(l))
lambda_max = length(distances)/(sum(distances) - length(distances)*max(l))
lambda_min = length(distances)/(sum(distances) - length(distances)*min(l))
lambda_median = length(distances)/(sum(distances) - length(distances)*median(l))

sprintf("%.3e", lambda_mean)
sprintf("%.3e", lambda_max)
sprintf("%.3e", lambda_min)
sprintf("%.3e", lambda_median)


# Show histogram and estimated density function

hist(distances[distances>0], breaks=100, freq=FALSE, 
     xlab='Odległości', main='Odległości między impulsami', ylab='Gęstość')

x <- seq(from = 0, to = 1000000, by = 100)
lines(x, dexp(x, rate = lambda_max), type = "l",col="red", )

# x <- seq(from = 0, to = 1000000, by = 100)
# lines(x, dexp(x, rate = lambda_max), type = "l",col="green", )
# 
# x <- seq(from =0, to = 1000000, by = 100)
# lines(x, dexp(x, rate = lambda_min), type = "l",col="blue", )
# 
# legend(8e+05, 1e-05, legend=c("Lambda (mean distance)", "Lambda (max distance)", "Lambda (min distance)"),
#        col=c("red", "green", "blue"),lty=1, cex=0.8)

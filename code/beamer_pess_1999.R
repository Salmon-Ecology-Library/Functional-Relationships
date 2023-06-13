
# Relate egg to fry survival and peak flow as described in the references below by extracting data from plots, fitting models, and plotting
# last updated: AH Fullerton, 6/12/2023

# Beamer, E., and G. R. Pess. 1999. Effects of peak flows on chinook (Oncorhynchus tshawytscha) spawning success in two Puget Sound 
  # river basins. Pages 67-70 in Watershed Management to Protect Declining Species. Proceedings American Water Resources Association 
  # 1999 Annual Water Resources Conference, Seattle, WA.  

# Seiler, D., S. Neuhauser, and L. Kishimoto. 2002. 2001 Skagit River wild 0+ chinook production evalutaion. State of Washington 
  # Department of Fish and Wildlife, and Seattle City Light, Annual Report, Seattle, WA.

#--------------------------------------------------------------------------------------------------------------------------------------------

# Read in estimated data
# Empirical data points from Figure 2 were extracted using http://www.graphreader.com/ and saved in the .csv file
td <- read.csv("data/beamer_pess_1999.csv")
plot_name <- "beamer_pess_1999"

# X-variable
x <- td$FRI_y
xlb <- "Flood recurrence interval (years)" # label used in plots

# Y-variable
y <- td$e2f_survival
ylb <- "Egg to migrant survival" # label used in plots

# Convert response from percent to proportion
y <- y / 100

# Create new dataset used for predicting (extending the X-value points from a few to many within the existing range)
xl <- seq(min(x), max(x), (max(x) - min(x)) / 1000)
nd <- as.data.frame(xl); colnames(nd) <- "x" #nls() which is used below needs this to be a dataframe with named variable as expected

# Published equation: y = 0.1285e-0.0446x
p1 <- 0.1285
p2 <- 0.0446
eq <- p1 * exp(-p2 * x)
eq.pred <- p1 * exp(-p2 * nd$x)

# Fit a nonlinear model (exponential); needs approximate starting values estimated from plot of empirical data points
a_start <- 0.15 # param a is the y value when x = 0
b_start <- log(0.15) / (80 * 0.15) # b is the decay rate. k = log(A) / (A (intial) * t)
(m1 <- nls(y ~ a * exp(-b * x), start = list(a = a_start, b = b_start)))
m1.pred <- predict(m1)
cor(y, m1.pred) #test fit to see if this was an appropriate form
m1.pred <- predict(m1, newdata = nd)


# Plot graph (saved)
png(paste0("plots/", plot_name, ".png"), width = 5, height = 4, units = "in", res = 300)
plot(x, y, pch = 19, col = 1, las = 1, ylab = ylb, xlab = xlb)
lines(xl, eq.pred, col = 2, lwd = 2)
lines(xl, m1.pred, col = 4, lwd = 2)
legend("topright", legend = c("Extracted data", "Published equation", "Fitted equation"), col = c(1, 2, 4), lwd = c(NA, 2, 2), pch = c(19, NA, NA), bty = 'n')
dev.off()

# Not used:
#Smooth the published equation's fitted line
#lo <- loess(eq ~ x, span = 1.1)
#eq.lo <- predict(lo, xl)



# Relate egg to fry survival and peak flow as described in the references below by extracting data from plots, fitting models, and plotting
# last updated: Taya Clements 6/20/2023

# Cunjak_et_al_2013
#The complex interaction of ecology and hydrology in a small catchment: a salmon's perspective. Hydrol. Process

#--------------------------------------------------------------------------------------------------------------------------------------------

# Read in estimated data
# Empirical data points from Figure 2 were extracted using http://www.graphreader.com/ and saved in the .csv file
td <- read.csv("data/cunjak_et_al_2013.csv")
plot_name <- "cunjak_et_al_2013"

# X-variable
x <- td$x
xlb <- "Discharge (m3s-1)" # label used in plots

# Y-variable
y <- td$y
ylb <- "Egg survival" # label used in plots


# Create new dataset used for predicting (extending the X-value points from a few to many within the existing range)
xl <- round(seq(min(x), max(x), length.out = 1000))
nd <- as.data.frame(xl); colnames(nd) <- "x" #nls() which is used below needs this to be a dataframe with named variable as expected


# Fit a linear model
(m1 <- lm(y ~ x))
summary(m1)
m1.pred <- predict(m1)
cor(y, m1.pred) #test fit to see if this was an appropriate form
m1.pred <- predict(m1, newdata = nd)


# Plot graph (saved)
png(paste0("plots/", plot_name, ".png"), width = 5, height = 4, units = "in", res = 300)
plot(x, y, pch = 19, col = 1, las = 1, ylab = ylb, xlab = xlb)
lines(xl, m1.pred, col = 4, lwd = 2)
dev.off()
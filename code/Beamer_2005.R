# Relate egg to fry survival and peak flow as described in the references below by extracting data from plots, fitting models, and plotting
# last updated: Taya Clements 6/15/2023

# Beamer et al. 2005 
#Linking Watershed Conditions to Egg-to-Fry Survival of Skagit Chinook Salmon. An appendix to the Skagit River System Cooperative Chinook Recovery Plan. Draft Version, 2.

#--------------------------------------------------------------------------------------------------------------------------------------------

# Read in estimated data
# Empirical data points from Figure 2 were extracted using http://www.graphreader.com/ and saved in the .csv file
td <- read.csv("beamer_et_al_2005.csv")
plot_name <- "beamer_et_al_2005"

# X-variable
x <- td$x
xlb <- "Peak flow during egg incubation (cfs)" # label used in plots

# Y-variable
y <- td$y
ylb <- "Egg to migrant fry survival (percent)" # label used in plots

# Convert x-axis to something we can read on the plot (kcfs)
x <- x / 1000
x <- round(x)

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

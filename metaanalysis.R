getwd()
library(dplyr)
library(ggplot2)
install.packages("metafor")
library(metafor)

meta <- read.delim("meta_analysis.tsv", sep = "\t")
names(meta) <- sub("^X95\\.\\.", "", names(meta))
names(meta)
append_row = c(First.author = "Zornberg,2000",
            Publication.Date = 2000,
            Reference="Zornberg GL, Buka SL, Tsuang MT.
Hypoxic-ischemia-related fetal/neonatal complications and risk
of
schizophrenia and other nonaffective psychoses: a 19-year
longitudinal study. Am J Psychiatry. 2000;157(2):196-202",
            OR = 13.35,
            confidence.interval.lower = 4.79,
            confidence.interval.upper = 47.26,
            Number.of.Psychosis.cases = 20,
            Number.of.Controls = 673)
meta = rbind(meta,append_row)
head(meta, 5)
typeof(meta$First.author)
for (i in names(meta)) {
  print(class(meta[[i]]))
}
meta$Publication.Date<-as.integer(meta$Publication.Date)
meta$OR <- as.numeric(meta$OR)
meta$confidence.interval.lower <- as.numeric(meta$confidence.interval.lower)
meta$confidence.interval.upper <- as.numeric(meta$confidence.interval.upper)
meta$Number.of.Psychosis.cases <- as.numeric(meta$Number.of.Psychosis.cases)
meta$Number.of.Controls <- as.numeric(meta$Number.of.Controls)


# Sort the data frame by 'Publication.Date'
# Convert 'Publication.Date' to Date format (setting month and day to January 1st)
meta$Publication.Date <- as.Date(paste(meta$Publication.Date, "-01-01", sep = ""), format = "%Y-%m-%d")
meta <- meta[order(meta$Publication.Date), ]

meta <- conv.wald(out=OR, ci.lb=confidence.interval.lower, ci.ub=confidence.interval.upper, data=meta, transf=log)
meta_summary <- rma(yi, vi,data=meta, method = "REML", slab=paste(First.author))
#meta_result <- rma(yi, vi, data=meta, weights = meta$Number.of.Psychosis.cases,)
summary(meta_summary)
exp(0.7722)
exp(-.0037)
exp(1.5481)
#summary(meta_result)

custom_xlim <- c(-6, 9)  # Replace 'lower_limit' and 'upper_limit' with your desired values

# Create the forest plot with custom x-axis limits
forest(meta_summary, addfit = TRUE, atransf = exp, xlim = custom_xlim)
funnel(meta_summary, refline = 0.7722, pch = 18,
       xlim = c(-1.5, 4.5),col = "blue",
       main = "Funnel Plot", label=TRUE, level = 95, legend = TRUE, atransf = exp)

# Add additional layers
abline(h = c(1.96, -1.96), lty = 2, col = "red")  # Add lines for significance thresholds
legend("topright", legend = c("Studies", "Significance Threshold"), col = c("blue", "red"), pch = c(16, NA), lty = c(NA, 2))



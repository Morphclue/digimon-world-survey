if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggpubr)

csv_df <- read.csv('data/results.csv')
str(csv_df)

df <- csv_df[, 11:30]
names(df)[1] <- 'difficulty'
names(df)[2] <- 'overall'
names(df)[3] <- 'combat'

for (i in 4:20) {
  names(df)[i] <- sprintf("H%s", i - 3)
}

correlations <- round(cor(df, method = 'spearman'), digits = 4)
correlations[abs(correlations) < 0.5 | correlations == 1] <- ''

for (i in 1:20) {
  df[, i] <- round(jitter(df[, i], factor = 0.3), digits = 3)
}

ggscatter(
  df,
  x = 'H11',
  y = 'H12',
  add = 'reg.line',
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = 'spearman',
) +
  theme_minimal()
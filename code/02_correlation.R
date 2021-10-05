if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggpubr, corrplot)

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

df$H11 <- round(jitter(df$H11, factor = 0.2), digits = 3)
df$H12 <- round(jitter(df$H12, factor = 0.2), digits = 3)

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
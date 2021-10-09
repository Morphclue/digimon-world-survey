if (!require('pacman')) install.packages('pacman')
p_load(pacman, tidyverse, ggpubr, corrplot)

csv_df <- read.csv('data/results.csv')

df <- csv_df[, 14:30]

for (i in 1:17) {
  names(df)[i] <- sprintf('H%s', i)
}

correlations <- round(cor(df, method = 'spearman'), digits = 2)
corrplot(correlations, method = 'circle', type = 'upper')

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

ggscatter(
  df,
  x = 'H13',
  y = 'H14',
  add = 'reg.line',
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = 'spearman',
) +
  theme_minimal()

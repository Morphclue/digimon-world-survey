if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse)

csv_df <- read.csv('data/results.csv')
str(csv_df)

df <- csv_df[, 14:30]
for (i in 1:17) {
  names(df)[i] <- sprintf("H%s", i)
}

df[df == 1] <- -2
df[df == 2] <- -1
df[df == 3] <- 0
df[df == 4] <- 1
df[df == 5] <- 2

df1 <- data.frame(as.matrix(colSums(df)))
df1 <- rownames_to_column(df1, 'Hypothesen')
names(df1)[2] <- 'Auswertung'
df1$Hypothesen <- factor(df1$Hypothesen, levels = df1$Hypothesen[order(-df1$Auswertung)])

p <- ggplot(df1, aes(x = Hypothesen, y = Auswertung))

p +
  geom_bar(
    stat = 'identity',
    width = 0.6,
    alpha = 0.7,
    color = 'black',
    fill = 'steelblue'
  ) +
  theme_minimal() +
  theme(legend.position = 'None') +
  labs(x = 'Auswertung', y = 'Hypothesen') +
  geom_text(
    aes(x = Hypothesen, y = Auswertung, label = Auswertung),
    position = position_dodge(width = 1),
    vjust = -0.5,
    size = 4,
  )

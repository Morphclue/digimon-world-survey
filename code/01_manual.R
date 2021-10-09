if (!require('pacman')) install.packages('pacman')
p_load(pacman, tidyverse, ggplot2)

csv_df <- read.csv('data/results.csv')

df <- data.frame(csv_df[8])
names(df)[1] <- 'manual'
df$manual[df$manual == 'No'] <- 'Nein'
df$manual[df$manual == 'Yes'] <- 'Ja'
df$manual[df$manual == 'I am not sure'] <- 'Unbekannt'

df1 <- df %>%
  select(manual) %>%
  group_by(manual) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

p <- ggplot(df1, aes(x = manual, y = percentage, fill = manual))

p +
  geom_bar(
    stat = 'identity',
    width = 0.6,
    alpha = 0.7,
    color = 'black',
  ) +
  theme_minimal() +
  theme(legend.position = 'None') +
  labs(x = 'Handbuch vor Beginn des Spiels gelesen?', y = 'Relative Häufigkeit in %') +
  geom_text(
    aes(x = manual, y = percentage, label = count),
    position = position_dodge(width = 1),
    vjust = -0.5,
    size = 4,
  )

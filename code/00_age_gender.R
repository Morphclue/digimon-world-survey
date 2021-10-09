if (!require('pacman')) install.packages('pacman')
p_load(pacman, tidyverse, ggplot2)

csv_df <- read.csv('data/results.csv')

df <- data.frame(csv_df[3], csv_df[4])
names(df)[1] <- 'Altersgruppe'
names(df)[2] <- 'Geschlecht'
df$Altersgruppe[df$Altersgruppe == 'Under 15'] <- '0-14'
df$Geschlecht[df$Geschlecht == 'Female'] <- 'Weiblich'
df$Geschlecht[df$Geschlecht == 'Male'] <- 'Männlich'
df$Geschlecht[df$Geschlecht == 'Non-binary'] <- 'Nichtbinär'
df$Geschlecht[df$Geschlecht == 'Prefer not to say'] <- 'Keine Angabe'

df1 <- df %>%
  select(Geschlecht, Altersgruppe) %>%
  group_by(Geschlecht, Altersgruppe) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count))

p <- ggplot(df1, aes(x = Geschlecht, y = percentage, fill = Altersgruppe))

p +
  geom_bar(
    stat = 'identity',
    position = 'fill',
    width = 0.6,
    alpha = 0.7,
    color = 'black',
  ) +
  theme_minimal() +
  labs(x = 'Geschlecht', y = 'Anteil') +
  geom_text(aes(label = count), position = position_fill(vjust = 0.5), size = 4)

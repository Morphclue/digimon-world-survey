if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2)

csv_df <- read.csv('data/results.csv')

df <- data.frame(csv_df[4])
str(df)
names(df)[1] <- 'Geschlecht'
df$Geschlecht[df$Geschlecht == 'Female'] <- "Weiblich"
df$Geschlecht[df$Geschlecht == 'Male'] <- "Männlich"
df$Geschlecht[df$Geschlecht == 'Non-binary'] <- "Nichtbinär"
df$Geschlecht[df$Geschlecht == 'Prefer not to say'] <- "Keine Angabe"

df1 <- df %>%
  select(Geschlecht) %>%
  group_by(Geschlecht) %>%
  summarise(count = n()) %>%
  mutate(Prozent = count / sum(count))

p <- ggplot(df1, aes(x = '', y = Prozent, fill = Geschlecht))

p +
  geom_col(color = 'white') +
  coord_polar(theta = 'y') +
  theme_minimal() +
  geom_text(
    aes(label = sprintf('%s%%', round(Prozent * 100, digits = 2))),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = 'black'
  )

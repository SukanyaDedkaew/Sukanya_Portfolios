install.packages("ggbump")

if(!require(pacman)) install.packages("pacman")
library(ggbump)
pacman::p_load(tidyverse, cowplot, wesanderson)

df <- tibble(country = c("India", "India", "India", "Sweden", "Sweden", "Sweden", "Germany", "Germany", "Germany", "Finland", "Finland", "Finland"),
             year = c(2011, 2012, 2013, 2011, 2012, 2013, 2011, 2012, 2013, 2011, 2012, 2013),
             value = c(492, 246, 246, 369, 123, 492, 246, 369, 123, 123, 492, 369))

knitr::kable(head(df))

df <- df %>% 
  group_by(year) %>% 
  mutate(rank = rank(value, ties.method = "random")) %>% 
  ungroup()

knitr::kable(head(df))

ggplot(df, aes(year, rank, color = country)) +
  geom_bump()

ggplot(df, aes(year, rank, color = country)) +
  geom_point(size = 7) +
  geom_text(data = df %>% filter(year == min(year)),
            aes(x = year - .1, label = country), size = 5, hjust = 1) +
  geom_text(data = df %>% filter(year == max(year)),
            aes(x = year + .1, label = country), size = 5, hjust = 0) +
  geom_bump(size = 2, smooth = 8) +
  scale_x_continuous(limits = c(2010.6, 2013.4),
                     breaks = seq(2011, 2013, 1)) +
  theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank()) +
  labs(y = "RANK",
       x = NULL) +
  scale_y_reverse() +
  scale_color_manual(values = wes_palette(n = 4, name = "GrandBudapest1"))


# Original df
df <- tibble(season = c("Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter", 
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter", 
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter",
                        "Spring", "Pre-season", "Summer", "Season finale", "Autumn", "Winter"),
             rank = c(1, 3, 4, 2, 1, 4,
                      2, 4, 1, 3, 2, 3,
                      4, 1, 2, 4, 4, 1,
                      3, 2, 3, 1, 3, 2),
             player = c(rep("David", 6),
                        rep("Anna", 6),
                        rep("Franz", 6),
                        rep("Ika", 6)))


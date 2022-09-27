library(tidyverse)
library(janitor)
library(here)
library(scales)
library(data.table)
library(tidytext)
library(ggthemes)

# Download and load file
dir.create("data")
download.file("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv",
              here("data", "artists.csv"))

artists <- read_csv(here("data", "artists.csv"))

# Plot number of artists by type
famous_types <- artists %>% 
  group_by(type) %>% 
  summarize(artists = sum(artists_n, na.rm = T)) %>%
  ggplot(aes(x = artists, y = reorder(type, artists))) +
  geom_col(fill = "aquamarine3") +
  scale_x_continuous(label = label_number(), expand = c(0,0)) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = "Number of artists", y = "",
       title = "Number of Artists by type")

famous_types

# States with highest and lowest share of artists
top_rates <- artists %>% 
  group_by(state) %>% 
  summarize(artists = sum(artists_n, na.rm = T),
            workers = sum(all_workers_n, na.rm = T),
            artist_share = artists / workers) %>% 
  arrange(desc(artist_share)) %>% 
  head(5)

top_and_last <- artists %>% 
  group_by(state) %>% 
  summarize(artists = sum(artists_n, na.rm = T),
            workers = sum(all_workers_n, na.rm = T),
            artist_share = artists / workers) %>%
  arrange(desc(artist_share)) %>% 
  tail(5) %>% 
  bind_rows(top_rates)


rates_by_state <- top_and_last %>% 
  ggplot(aes(x = artist_share, y = reorder(state, artist_share),
             fill = artist_share > 0.001)) +
  geom_col(show.legend = F) +
  scale_x_continuous(label = label_percent(), expand = c(0,0)) +
  theme(axis.ticks.y = element_blank()) +
  labs(x = "Share of artists on total workforce", y = "",
       title = "States with highest and lowest art share")

# Top 5 types by state

top_types <- artists %>% 
  group_by(type) %>% 
  summarize(artists = sum(artists_n, na.rm = T)) %>% 
  slice_max(artists, n = 8) %>% 
  pull(type)

artists %>% 
  filter(type %in% top_types) %>% 
  group_by(type, state) %>% 
  summarize(artists = sum(artists_n, na.rm = T),
            workers = sum(all_workers_n, na.rm = T),
            artist_share = artists / workers) %>%
  filter(min_rank(desc(artists)) <= 5) %>%
  ggplot(aes(x = artist_share, y = reorder_within(state, artist_share, type), fill = (state == "New York"))) +
  geom_col(show.legend = F) +
  scale_y_reordered() +
  scale_x_continuous(label = label_percent(), expand = c(0,0)) +
  theme_minimal() +
  labs(x = "Share of artists in total workforce within state", y = "",
       title = "New York has the highest density of Artists",
       subtitle = "Musicians prefer Tennessee (Top 8 art disciplines by workforce reflected)",
       caption = "TidyTuesday 2022/W39. Data: arts.gov") +
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(size=16, face = "bold"),
        plot.margin = margin(0.5,0.9,0.5,0.0, "cm")) +
  facet_wrap(~ type, scales = "free", ncol = 2)

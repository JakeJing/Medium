library(ggplot2)
library(grid)
library(rgeos)
library(rgdal)
library(htmltab)
library(dplyr)
library(magrittr)
library(ggpmisc)

## Coronavirus data
data = read.csv("data-lR5wn.csv", header = T, 
                sep = ",", as.is = T) %>% 
  select(Lon, Lat, Titel, Cases, Deaths) %>% 
  set_colnames(c("long", "lat", "canton", "cases", "death"))
top = data %>% arrange(desc(cases)) %>%
  head(5) %>% select(canton, cases, death) %>%
  mutate(label = paste0(canton, ": ", cases))
death_num = data %>% filter(death > 0) %>% 
  arrange(desc(death)) %>% 
  head(5) %>% select(canton, death) %>% 
  mutate(label = paste0(canton, ": ", death))

## GIS data 
devtools::source_gist("https://gist.github.com/hrbrmstr/33baa3a79c5cfef0f6df") # theme
map = readOGR("readme-swiss.json", "cantons")
map_df = fortify(map)
dat = data.frame(id = 0:(length(map@data$name) - 1), 
                 canton = map@data$name)
map_df_merged = merge(map_df, dat, by = "id") %>% 
  mutate_if(is.factor, as.character)

## official languages in each canton
lang_canton = htmltab("https://en.wikipedia.org/wiki/Cantons_of_Switzerland", 2,
                       rm_nodata_cols = F)
lang_canton_new = lang_canton %>% select("Canton of", "Official languages") %>% 
  set_colnames(c("canton", "lang")) %>% 
  mutate(language = case_when(grepl(",", lang) ~ "Mixed",
                              lang == "German" ~ "German",
                              lang == "French" ~ "French",
                              lang == "Italian" ~ "Italian",
                              lang == "Romansh" ~ "Romansh")) %>% 
  select(canton, language) %>% filter(canton != "Switzerland")

## data recoding 
lang_canton_new$canton = recode(lang_canton_new$canton,
       "Basel-Landschaft" = "Basel Country",
       "Basel-Stadt" = "Basel City",
       "Zürich" = "Zurich", "Graubünden; Grisons" = "Graubünden",
       "Bern/Berne"= "Bern", "Genève" = "Geneva",
       "Luzern" = "Lucerne", "Valais/Wallis" = "Valais")
map_df_merged$canton = recode(map_df_merged$canton,
       "Basel-Landschaft" = "Basel Country",
       "Basel-Stadt" = "Basel City",
       "Zürich" = "Zurich", "Graubünden/Grigioni" = "Graubünden",
       "Bern/Berne"= "Bern", "Genève" = "Geneva",
       "Luzern" = "Lucerne", "Valais/Wallis" = "Valais")
map_df_final = map_df_merged %>% 
  inner_join(., lang_canton_new, by = "canton")

# 2 cantons without case
unobserved = setdiff(unique(map_df_merged$canton), unique(data$canton))
print(paste("Safe region: ", paste(unobserved, collapse = ", ")))

gg = ggplot() + geom_map(data=map_df_final, map=map_df_final,
                    aes(map_id=id, group=group, fill=language),
                    color="#ffffff", size=0.25) + 
  scale_fill_manual(values = c("#f38ea2", "#808fde", 
                              "#67d78f", "#b5b6bc")) +
  geom_point(data = data, aes(x = long, y = lat,  
                 size = cases, alpha = 0.5), 
             shape = 21, color = 'black', 
             fill = "purple", stroke = .2, show.legend = F) +
  scale_size_continuous(range = c(2,13)) +
  geom_text(data=data, aes(label=canton, x=long, y=lat), size=2) + 
  coord_map(xlim = range(map_df$long) + c(-0.5, 0.5), 
            ylim = range(map_df$lat) + c(-0.65, 0.65)) +
  theme_map() + 
  labs(x="", y="", title="Coronavirus (COVID-19) in Switzerland",  
       subtitle = paste("Total confirmed cases:", sum(data$cases))) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = -4, 
                     size = 20, family = "Times", color = "red"), 
        plot.subtitle = element_text(hjust = 0.5, vjust = -7,  
                     size = 13, family = "Times", color = "red"),
        legend.position = c(.1,.03), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 10, family = "Times"),
        legend.title = element_blank(),
        legend.box.background = element_rect(fill = 'white', color = 'white'),
        legend.box.margin = margin(t = 12, r = 16, l = 0, b = 12, unit = "pt"))

# add the ranking
txt_size = 3.5
left_corner = 6
gg = gg + annotate("text", x = rep(left_corner, nrow(top) + 1), 
    y = seq(from = 48.1, to = 47.1, by = -0.2)[1:(nrow(top) + 1)],
    label = c("Rank", top$label), family = "Times",
    fontface = c("bold", rep("plain", nrow(top))))

# add the death number
right_corner = 10.2
gg = gg + annotate("text", x = rep(right_corner, nrow(death_num) + 1), 
     y = seq(from = 48.1, to = 47.1, by = -0.2)[1:(nrow(death_num) + 1)],
     label = c("Death", death_num$label), family = "Times",
     fontface = c("bold", rep("plain", nrow(death_num))))
# save the plot and print it
name = "corona.jpg"
ggsave(name, gg, width=8.5, height=5)



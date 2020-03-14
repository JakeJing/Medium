library(ggplot2)
library(grid)
library(rgeos)
library(rgdal)
library(htmltab)
library(dplyr)
library(magrittr)
library(ggpmisc)
library(tidyr)

## get the longitudinal data from wiki
data = htmltab("https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Switzerland", 3, rm_nodata_cols = F)

colnames_recode = sapply(colnames(data), function(x){
  ifelse(grepl("Canton", x), strsplit(x, " >> ")[[1]][2], x)})
data_rename = data %>% set_colnames(colnames_recode) %>% 
  rename("Zurich" = "Zürich", "Graubünden" = "Grisons",
         "Basel City" = "Basel-Stadt", 
         "Basel Country" = "Basel-Landschaft",
         "Appenzell Ausserrhoden" = "AppenzellAusserrhoden",
         "Appenzell Innerrhoden" = "AppenzellInnerrhoden")

# subset the data
row_ids = 1:(which(data_rename$Date == "no date") - 1)
data_subset = data_rename[row_ids, ] %>% 
  select("Date":"Unknown", "Confirmed cases >> Total", 
         "Deaths >> New", "Deaths >> Total") %>% 
  rename("total_cases" = "Confirmed cases >> Total", 
         "new_death" = "Deaths >> New", 
         "total_deaths" = "Deaths >> Total") %>% 
  select(-Unknown)
  
# add the long and lat to each canton (center)
map = readOGR("readme-swiss.json", "cantons")
center = data.frame(gCentroid(map, byid=TRUE))
dat = data.frame(id=0:(length(map@data$name)-1), canton=map@data$name)
long_lat_canton = center %>% mutate(canton = dat$canton) %>% 
  mutate(canton = as.character(canton)) %>% 
  rename("long" = "x", "lat" = "y")
long_lat_canton$canton = recode(long_lat_canton$canton,
       "Basel-Landschaft" = "Basel Country",
       "Basel-Stadt" = "Basel City",
       "Zürich" = "Zurich", "Graubünden/Grigioni" = "Graubünden",
       "Bern/Berne"= "Bern", "Genève" = "Geneva",
       "Luzern" = "Lucerne", "Valais/Wallis" = "Valais")

# data reshaping and joining
data_reshape = data_subset %>% gather(., key = canton, value = value, 
                       -c("Date", "total_cases", "total_deaths", "new_death"))
data_reshape[is.na(data_reshape)] = 0
data_final = data_reshape %>% inner_join(., long_lat_canton, by = "canton")
df_death_all = data_final %>% separate(., col = "value", 
              into = c("case", "dth"), sep = "\\(",
              fill = "right") %>% 
  separate(., col = "dth", into = c("death", "rt"), sep = "\\)",
           fill = "right")  %>% select(-(rt))
df_death_all[which(df_death_all$case == ""), "case"] = "0"
df_death_all[is.na(df_death_all)] = "0"
data_all = df_death_all %>% 
  mutate(case = as.numeric(case), 
         death = as.numeric(death), 
         total_cases = as.numeric(total_cases),
         new_death = as.numeric(new_death),
         total_deaths = as.numeric(total_deaths))

# we need to have the cumsum for each canton
cantons = data_all %>% pull(canton) %>% unique
data_all$cumcases = NA
data_all$cumdeath = NA
for(i in cantons){
  data_all[which(data_all$canton == i), "cumcases"] = 
    cumsum(data_all[which(data_all$canton == i), "case"])
  data_all[which(data_all$canton == i), "cumdeath"] = 
    cumsum(data_all[which(data_all$canton == i), "death"])
}

dts = data_all %>% pull(Date) %>% unique
for(i in dts){
  data_all %>% filter(Date == i) %>% 
    select(lat, long, canton, cumcases, cumdeath) %>% 
    set_colnames(c("Lat", "Lon", "Titel", "Cases", "Deaths")) %>% 
    write.table(., paste0("./wiki/", i, ".csv"), 
              quote = F, sep = ",", row.names = F, 
              col.names = TRUE, append = F)
}

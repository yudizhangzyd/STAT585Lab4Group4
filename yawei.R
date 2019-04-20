library(tidyverse)
library(ggrepel)

iowaliquor <- read_csv("./data/Iowa_Liquor_Sales-Story.csv")
sampledata <- head(iowaliquor)



# add long and lat to data

iowaliquor <- iowaliquor %>% 
  mutate(longlat = str_extract(`Store Location`, pattern = ".*?\\(.*?\\)"),
         lat = as.numeric(str_replace(longlat, "\\((.*?),\\s*.*\\)", "\\1")),
         long = as.numeric(str_replace(longlat, ".*?,\\s*(.*)\\)", "\\1"))) %>%
  select(-longlat) %>%
  filter(lat < 42.3, long > -93.8) %>%
  filter(lat > 41.8, long < -93.2) %>%
  mutate(City = tolower(City))

sampledata <- head(iowaliquor) 


# Work on story county

# save map data for Story county
story <- map_data("county") %>%
  filter(subregion == "story")

# some summary of the data
table(iowaliquor$County)
table(iowaliquor$City)
table(iowaliquor %>% filter(City == "ames") %>% select(`Store Name`))

# Plot1: Story County Liquor Sales: #observations
theme_set(theme_light())

story %>%
  ggplot() + 
  geom_path(aes(x = long, y = lat, group = group)) + 
  geom_count(data = iowaliquor, aes(x = long, y =lat, color = City), alpha = 0.5) +
  ggtitle("Story County Liquor Sales: #observations") + 
  theme_light()
  
# Plot2: Ames City: #observations
ggplot() +
  geom_count(data = iowaliquor %>% filter(City == "ames"), 
             aes(x = long, y =lat), alpha = 0.5, color = "pink2") +
  ggtitle("Ames City Liquor Sales: #observations")

# Plot3: Ames City: #observations and top five stores
# create summary data for stores
storedata <- iowaliquor %>%
  mutate(`Store Name` = tolower(`Store Name`)) %>%
  group_by(`Store Name`) %>%
  filter(!is.na(`Sale (Dollars)`)) %>%
  summarise(n = n(), 
            volume_liters = sum(`Volume Sold (Liters)`),
            sale_dollars = sum(`Sale (Dollars)`), 
            long = mean(long), 
            lat = mean(lat),
            City = City[1]) %>%
  ungroup() %>%
  arrange(desc(n))
# plot
ggplot(data = iowaliquor %>% filter(City == "ames"),
       aes(x = long, y =lat)) +
  geom_count(alpha = 0.5, color = "pink2") +
  geom_label_repel(data = storedata %>% filter(City == "ames") %>% head(n = 5), 
                   aes(label = `Store Name`), 
                   hjust = 1, vjust = -7) +
  ggtitle("Ames City: #observations and top five stores")

# Plot4: Ames City: Store and Volumes

storedata %>% filter(City == "ames") %>%
  ggplot(aes(x = long, y = lat, size = volume_liters)) + 
  geom_point() + 
  ggtitle("Ames City: Store and Volumes")
  

# Plot5: Non-spacial plot

storedata %>% 
  ggplot(aes(x = volume_liters, y = sale_dollars)) + 
  geom_point() + 
  geom_label_repel(data = storedata %>% filter(volume_liters == max(volume_liters)),
                   aes(label = `Store Name`))

# Plot6: Sams Club is so different

storedata %>% 
  ggplot(aes(x = volume_liters, y = sale_dollars)) + 
  geom_point(aes(size = n)) + 
  geom_label_repel(data = storedata %>% filter(volume_liters == max(volume_liters)),
                   aes(label = `Store Name`)) + 
  ggtitle("Sams Club is so different")

# Plot7: Sams Club is so different

storedata %>% 
  ggplot(aes(x = n, y = sale_dollars)) + 
  geom_point() + 
  geom_label_repel(data = storedata %>% filter(volume_liters == max(volume_liters)),
                   aes(label = `Store Name`)) + 
  ggtitle("Sams Club is so different")



























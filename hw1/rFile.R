library(tidyverse)
trees = read_csv("Street_Tree_List-2022-01-30_FILTERED.csv")
species = trees %>%
  group_by(qSpecies) %>%
  summarise(
    count = n(),
    avgDBH = mean(DBH),
    avgDBHround = round(mean(DBH),0)
  )

caretakerCount = as.data.frame(table(trees$binCaretaker))
caretakerCount = trees %>%
  group_by(binCaretaker) %>%
  summarise(
    count = n(),
    avgDBH = mean(DBH),
    avgLat = mean(Latitude),
    avgLong = mean(Longitude)
  )

trial = species %>%
  filter(avgDBH<=10) %>%
  summarize(
    count0to10 = n()
  )

binsDBH = species %>% 
  mutate(new_bin = cut(avgDBH, breaks=c(0,10,20,30,40,50,60,70,80))) %>%
  group_by(new_bin) %>%
  summarize(
    count = n()
  )

binsDBH2 = species %>% 
  mutate(new_bin = cut(avgDBH, breaks=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80))) %>%
  group_by(new_bin) %>%
  summarize(
    count = n()
  )

binsDBH3 = species %>% 
  mutate(dbh_bin = cut(avgDBH, breaks=20)) %>%
  group_by(dbh_bin) %>%
  summarize(
    count = n()
  )

# dbhs = as.matrix(unique(species$avgDBHround))

dbhs = as.data.frame(table(species$avgDBHround))

write.csv(species, "C:\\Users\\hanna\\Desktop\\2022-2023\\info4310\\hw\\hw1\\species.csv", row.names=FALSE)
write.csv(caretakerCount, "C:\\Users\\hanna\\Desktop\\2022-2023\\info4310\\hw\\hw1\\caretakers.csv", row.names=FALSE)
#write.csv(binsDBH2, "C:\\Users\\hanna\\Desktop\\2022-2023\\info4310\\hw\\hw1\\bins_dbh.csv", row.names=FALSE)
write.csv(dbhs, "C:\\Users\\hanna\\Desktop\\2022-2023\\info4310\\hw\\hw1\\bins_dbh.csv", row.names=FALSE)



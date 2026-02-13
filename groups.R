library(tidyverse)
library(readxl)

g2<-read_xlsx("groups.xlsx",col_names=c("Group","TOPIC"))

g1<-read_xlsx("names.xlsx") %>%
  select(name=`First name`,Group) %>%
  filter(Group!="") %>%
  left_join(g2)


g1 %>%
group_by(Group) %>%
  summarize(names=paste(name,collapse=", "),
            topic=first(TOPIC)) %>%
  write.csv("g4.csv")

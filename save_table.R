
time.avg <- spring %>%
  group_by(CAO) %>%
  summarise(`Average minutes in outlet` = as.numeric(mean(Actual.Time..Minutes., na.rm = T)),
            SD = sd(Actual.Time..Minutes., na.rm = T),
            count = n())

time.total <- spring %>%
  group_by(CAO) %>%
  summarise(total.hrs = as.numeric(sum(Actual.Time..Minutes., na.rm = T)),
            count = n()) %>%
  mutate(total.hrs = total.hrs / 60)

time.dc.avg <- spring %>%
  group_by(DC, CAO) %>%
  summarise(`Average minutes in outlet` = as.numeric(mean(Actual.Time..Minutes., na.rm = T)),
            SD = sd(Actual.Time..Minutes., na.rm = T),
            count = n())

time.dc.total <- spring %>%
  group_by(DC, CAO) %>%
  summarise(total.hrs = as.numeric(sum(Actual.Time..Minutes., na.rm = T)),
            count = n()) %>%
  mutate(total.hrs = total.hrs / 60)

tot.times <- time.avg %>%
  left_join(time.total, by = c("CAO", "count")) %>%
  select(CAO, count, total.hrs, `Average minutes in outlet`, SD) %>%
  rename(`Visit Count` = count,
         `Total Hours` = total.hrs)

dc.times <- time.dc.avg %>%
  left_join(time.dc.total, by = c("CAO", "DC", "count")) %>%
  select(DC, CAO, count, total.hrs, `Average minutes in outlet`, SD) %>%
  rename(`Visit Count` = count,
         `Total Hours` = total.hrs)

library(xlsx)
write.xlsx(as.data.frame(tot.times), file="deliverables/racetrac_perf.xlsx", sheetName="sheet1", row.names=FALSE)
write.xlsx(as.data.frame(dc.times), file="deliverables/racetrac_perf.xlsx", sheetName="sheet2", append=TRUE, row.names=FALSE)

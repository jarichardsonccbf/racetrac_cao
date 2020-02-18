library(patchwork)

time.avg.p <- spring %>%
  group_by(CAO) %>%
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(`Average minutes in outlet`), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average Time in Outlet (minutes)") +
  theme_bw() +
  geom_text(aes(label = count, y = `Average minutes in outlet`), hjust = 5, vjust = 1.01)

time.total.p <- spring %>%
  group_by(CAO) %>%
  summarise(total.hrs = sum(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(total.hrs = total.hrs / 60,
         CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(total.hrs), fill = CAO)) +
  ylim(0, 6000) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total Hours in Outlets") +
  theme_bw() +
  geom_text(aes(label = count, y = total.hrs), hjust = 5, vjust = 1.01)

time.dc.avg.p <- spring %>%
  group_by(DC, CAO) %>%
  summarise(`Average minutes in outlet` = mean(Actual.Time..Minutes., na.rm = T),
            SD = sd(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(`Average minutes in outlet`), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  geom_errorbar(aes(ymin = `Average minutes in outlet` - SD, ymax = `Average minutes in outlet` + SD),
                size=.3,    # Thinner lines
                width=.2,
                position=position_dodge(.9)) +
  xlab("") +
  ylab("Average Time in Outlet (minutes)") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(aes(label = count, y = `Average minutes in outlet`), hjust = 1.6, vjust = 1.01)

time.dc.total.p <- spring %>%
  group_by(DC, CAO) %>%
  summarise(total.hrs = sum(Actual.Time..Minutes., na.rm = T),
            count = n()) %>%
  mutate(total.hrs = total.hrs / 60,
         CAO = factor(CAO),
         CAO = factor(CAO, levels = rev(levels(CAO)))) %>%
  ggplot(aes(x = CAO, y = as.numeric(total.hrs), fill = CAO)) +
  geom_bar(position=position_dodge(), stat="identity",
           colour="black", # Use black outlines,
           size=.3) +      # Thinner lines
  xlab("") +
  ylab("Total Hours in Outlets") +
  facet_wrap(~ DC, scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(aes(label = count, y = total.hrs), hjust = 1.6, vjust = 1.01)

(time.avg.p / time.total.p) | (time.dc.avg.p / time.dc.total.p)

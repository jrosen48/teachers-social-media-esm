
d1 <- d %>% 
  select(RecipientEmail, time_point, Q19_1_1:Q19_1_9) %>% 
  set_names(c("email", "time",
              "Resources", "Sharing1", "Sharing2",
              "Learning1", "Learning2",
              "Connecting",
              "Support",
              "Following1", "Following2")) %>%
  mutate(platform = "twitter") %>% 
  mutate_at(vars(Resources:Following2), replace_na, 0) %>% 
  mutate_at(vars(Resources:Following2), as.integer) %>% 
  mutate(Sharing = Sharing1 + Sharing2,
         Learning = Learning1 + Learning2,
         Following = Following1 + Following2)

# FACEBOOK FEATURES
key$Q21_2_1
key$Q21_2_7

d2 <- d %>% 
  select(RecipientEmail, time_point, Q19_2_1:Q19_2_9) %>% 
  set_names(c("email", "time",
              "Resources", "Sharing1", "Sharing2",
              "Learning1", "Learning2",
              "Connecting",
              "Support",
              "Following1", "Following2")) %>%
  mutate(platform = "facebook") %>% 
  mutate_at(vars(Resources:Following2), replace_na, 0) %>% 
  mutate_at(vars(Resources:Following2), as.integer) %>% 
  mutate(Sharing = Sharing1 + Sharing2,
         Learning = Learning1 + Learning2,
         Following = Following1 + Following2)

key$Q21_4_1 # pinterest
key$Q21_4_7 # pinterest

d3 <- d %>% 
  select(RecipientEmail, time_point, Q19_3_1:Q19_3_9) %>% 
  set_names(c("email", "time",
              "Resources", "Sharing1", "Sharing2",
              "Learning1", "Learning2",
              "Connecting",
              "Support",
              "Following1", "Following2")) %>%
  mutate(platform = "pinterest") %>% 
  mutate_at(vars(Resources:Following2), replace_na, 0) %>% 
  mutate_at(vars(Resources:Following2), as.integer) %>% 
  mutate(Sharing = Sharing1 + Sharing2,
         Learning = Learning1 + Learning2,
         Following = Following1 + Following2)

key$Q21_5_1 # instagram

d4 <- d %>% 
  select(RecipientEmail, time_point, Q19_4_1:Q19_4_9) %>% 
  set_names(c("email", "time",
              "Resources", "Sharing1", "Sharing2",
              "Learning1", "Learning2",
              "Connecting",
              "Support",
              "Following1", "Following2")) %>%
  mutate(platform = "instagram") %>% 
  mutate_at(vars(Resources:Following2), replace_na, 0) %>% 
  mutate_at(vars(Resources:Following2), as.integer) %>% 
  mutate(Sharing = Sharing1 + Sharing2,
         Learning = Learning1 + Learning2,
         Following = Following1 + Following2)

my_func <- function(x) {
  ifelse(is.na(x), 1, NA)
}

to_plot <- bind_rows(d1, d2, d3, d4) %>% 
  select(email, time, platform, Resources, Connections = Connecting, Share = Sharing,
         Learn = Learning, Follow = Following) %>% 
  gather(key, val, -email, -time, -platform) %>%
  mutate(platform = as.factor(platform),
         platform2 = as.integer(platform),
         platform = tools::toTitleCase(as.character(platform))) %>% 
  mutate(val = my_func(val))


to_plot_split <- to_plot %>% 
  mutate(val = if_else(is.na(val), 0, val)) %>% 
  #filter(val == 1) %>% 
  group_by(email, platform, key) %>% 
  summarize(sum_val = sum(val)) %>% 
  spread(key, sum_val, fill = 0)

to_plot_split

to_plot_split %>% 
  ungroup() %>% 
  mutate(email = as.integer(as.factor(email))) %>% 
  filter(email %in% c(14, 6, 4, 13)) %>% 
  mutate(email = str_c("Respondent ", email),
         email = factor(email, levels = c("Respondent 4", 
                                          "Respondent 6",
                                          "Respondent 13",
                                          "Respondent 14"))) %>% 
  ggiraphExtra::ggRadar(aes(group = "platform", facet = "email"), use.label = FALSE) +
  scale_color_discrete(NULL) +
  scale_fill_discrete(NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  theme(legend.position = "top",
        text = element_text(size = 9, family = "Times")) +
  theme(legend.text=element_text(size=14)) +
  theme(strip.text.x = element_text(size = 14)) +
  ggtitle("Why Teachers Use Social Media (via ESM)") +
  theme(plot.title = element_text(size=22, hjust = .5)) +
  labs(caption = "The responses from four select teachers who responded to eight ESM surveys. Scale represents the no. of times each purpose was selected.") +
  theme(plot.caption = element_text(size=12, hjust = .5)) +
  scale_color_brewer(NULL, type = "qual", palette = 2) +
  scale_fill_brewer(NULL, type = "qual", palette = 2)

read_and_label <- function(f) {
  d <- read_csv(f)
  f_after_directory <- str_split(f, "CSVs/")
  f_after_directory <- map_chr(f_after_directory, ~.[2])
  d$time_point <- as.integer(str_sub(f_after_directory, start = 1, end = 1))
  d
}

read_data <- function() {
  f <- list.files("Processed CSVs", full.names = T)
  f <- f[tools::file_ext(f) == "csv"]
  f <- f[!(str_detect(f, "key.csv") | str_detect(f, "Merged data"))]
  d <- map_df(f, read_and_label)
  d
}

# f <- list.files("Teachers Social Media COVID", full.names = T)

# dnew <- f %>% map_df(read_csv, skip=2)


key$Q1_30
key$Q1_27
key$Q19_1_1
key$Q19_1_2
key$Q12
key$Q14
key$Q19_2_1
key$Q21_1_1
key$Q19_9_TEXT
key$Q21_9_TEXT

# TWITTER FEATURES
key$Q21_1_1
key$Q21_1_2
key$Q21_1_3
key$Q21_1_4
key$Q21_1_5
key$Q21_1_6
key$Q21_1_7

d %>% 
  select(Q21_1_1:Q21_1_7)

d1 <- d %>% 
  select(RecipientEmail, time_point, Q21_1_1:Q21_1_7) %>% 
  set_names(c("email", "time",
              "Posting", "Sharing", "Replying", 
              "Bookmarking", "Searching", "Subscribing", 
              "Messaging")) %>% 
  mutate(platform = "twitter")

# FACEBOOK FEATURES
key$Q21_2_1
key$Q21_2_7

d2 <- d %>% 
  select(RecipientEmail, time_point, Q21_2_1:Q21_2_7) %>% 
  set_names(c("email", "time",
              "Posting", "Sharing", "Replying", 
              "Bookmarking", "Searching", "Subscribing", 
              "Messaging")) %>% 
  mutate(platform = "facebook")

key$Q21_4_1 # pinterest
key$Q21_4_7 # pinterest

d3 <- d %>% 
  select(RecipientEmail, time_point, Q21_4_1:Q21_4_7) %>% 
  set_names(c("email", "time",
              "Posting", "Sharing", "Replying", 
              "Bookmarking", "Searching", "Subscribing", 
              "Messaging")) %>% 
  mutate(platform = "pinterest")

key$Q21_5_1 # instagram

d4 <- d %>% 
  select(RecipientEmail, time_point, Q21_5_1:Q21_5_7) %>% 
  set_names(c("email", "time",
              "Posting", "Sharing", "Replying", 
              "Bookmarking", "Searching", "Subscribing", 
              "Messaging")) %>% 
  mutate(platform = "instagram")

my_func <- function(x) {
  ifelse(!is.na(x), 1, NA)
}

to_plot <- bind_rows(d1, d2, d3, d4) %>% 
  mutate_at(vars(Posting:Messaging), my_func) %>% 
  gather(key, val, -email, -time, -platform) %>%
  mutate(platform = as.factor(platform),
         platform2 = as.integer(platform),
         platform = tools::toTitleCase(as.character(platform)))

to_plot %>% 
  filter(!is.na(val)) %>% 
  count(platform, key) %>% 
  mutate(prop = n/96) %>% 
  select(-n) %>% 
  spread(key, prop) %>% 
  write_csv("t1.csv")

to_plot_split <- to_plot %>% 
  mutate(val = if_else(is.na(val), 0, val)) %>% 
  group_by(email, platform, key) %>%
  summarize(sum_val = mean(val)) %>% 
  spread(key, sum_val)

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
  ggtitle("The Purposes For Which Teachers Use Social Media (via ESM)") +
  theme(plot.title = element_text(size=22, hjust = .5)) +
  labs(caption = "The responses from four select teachers who responded to eight ESM surveys. Scale ranges from 1 (always use) to 0 (never use).") +
  theme(plot.caption = element_text(size=12, hjust = .5)) +
  scale_color_brewer(NULL, type = "qual", palette = 2) +
  scale_fill_brewer(NULL, type = "qual", palette = 2)

to_plot_split %>% 
  ungroup() %>% 
  mutate(email = as.integer(as.factor(email))) %>% 
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
  ggtitle("Why Teachers Use Social Media") +
  theme(plot.title = element_text(size=22, hjust = .5)) +
  theme(plot.caption = element_text(size=12, hjust = .5))

ggsave("all-teachers.png", width = 8, height = 10)

to_plot %>% 
  filter(!is.na(val)) %>% 
  mutate(email = as.integer(as.factor(email))) %>% 
  arrange(email) %>% 
  ggplot(aes(x = time, y = platform, color = key)) +
  geom_jitter(height = .1) +
  facet_wrap(~email, nrow = 4) +
  theme_bw() +
  scale_color_discrete("Purpose") +
  theme(text = element_text(size = 16, family = "Times")) +
  labs(y = "Social Media Platform", x = "Time") +
  theme(legend.position = "top") +
  ggtitle("Purposes of Social Media Platform Use Over Time By Respondent")

ggsave("fig2.png", width = 10, height = 10)

key$Q21_6_1 # reddit
key$Q21_7_1 # blogs
key$Q21_4_7 # pinterest

key %>% select(Q1_27, Q1_28, Q1_30, Q1_31, Q1_32, Q1_33)

d %>% select(Q21_1_1:Q21_1_7)

function_chr <- c('creating posts or pages',
                  "'sharing or reposting others' posts",
                  "replying to others' post",
                  'bookmarking or saving posts or pages',
                  'searching (through a search bar or function)',
                  'subscribed to or followed a person, page, or resource',
                  'contacted a page or other user privately')

x <- d %>% 
  select(Q21_2_1:Q21_2_7) 

names <- x[complete.cases(x),][1, ] %>% 
  unlist() %>% 
  as.vector()

rec <- function(x) {
  ifelse(!is.na(x), 1, NA)
}

# Twitter
d %>% 
  select(Q21_1_1:Q21_1_7, RecipientEmail, time_point) %>% 
  mutate_at(vars(Q21_1_1:Q21_1_7), rec) %>%
  set_names(c(function_chr, "email", "time")) %>% 
  gather(key, val, -email, -time) %>% 
  ggplot(aes(x = time, y = val, color = key)) +
  geom_point() +
  facet_wrap(~email)

d %>% 
  select(Q21_2_1:Q21_2_7, RecipientEmail, time_point) %>% 
  mutate_at(vars(Q21_2_1:Q21_2_7), rec) %>%
  set_names(c(names, "email", "time")) %>% 
  gather(key, val, -email, -time) %>% 
  ggplot(aes(x = time, y = val, color = key)) +
  geom_point() +
  facet_wrap(~email)

d %>% 
  select(contains("TEXT"))

d %>% 
  select(Q1_35_TEXT) %>% 
  filter(!is.na(Q1_35_TEXT))

d %>% 
  select(RecipientEmail, Q1_27, Q1_28, Q1_30, Q1_31, Q1_32, Q1_33, time_point) %>% 
  gather(key, val, Q1_27, Q1_28, Q1_30, Q1_31, Q1_32, Q1_33) %>% 
  mutate(val = if_else(!is.na(val), 1, 0)) %>% 
  group_by(RecipientEmail, key) %>% 
  summarize(sd_val = sd(val),
            mean_val = mean(val))  %>% 
  group_by(key) %>% 
  summarize(mean_mean_val = mean(mean_val),
            mean_sd_val = sd(mean_val)) %>% 
  mutate(key = c("Twitter", "Facebook", "Pinterest", "Instagram", "Reddit", "Teacher Blogs")) %>% 
  ggplot(aes(x = reorder(key, mean_mean_val), y = mean_mean_val, fill = key)) +
  geom_col() +
  geom_point() +
  geom_errorbar(aes(ymax = mean_mean_val + mean_sd_val,
                    ymin = mean_mean_val - mean_sd_val)) +
  scale_fill_brewer("", type= "qual") +
  xlab("") +
  ylab("Mean") +
  theme(text = element_text(size = 15)) + 
  theme(legend.position = NULL) +
  theme_bw()

# Are there trends over time?
d %>% 
  select(RecipientEmail, Q1_27, Q1_28, Q1_30, Q1_31, Q1_32, time_point) %>% 
  gather(key, val, Q1_27, Q1_28, Q1_30, Q1_31, Q1_32) %>% 
  mutate(val = if_else(!is.na(val), 1, 0)) %>% 
  group_by(time_point, key) %>% 
  summarize(prop_val = mean(val)) %>% 
  ggplot(aes(x = time_point, y = prop_val, color = key)) +
  geom_point() +
  geom_smooth(se = F) +
  scale_color_discrete("", labels = c("Twitter", "Facebook", "Pinterest", "Instagram", "Teacher Blogs")) +
  theme_bw() +
  ylab("Proportion of Respondents Reporting Use") +
  xlab("Time Point") +
  theme(text = element_text(size = 13, family = "Times")) +
  ggtitle("Proportion of Respondents Reporting Use Over Time By Social Media Platform")

ggsave("fig1.png", width = 8, height = 8)

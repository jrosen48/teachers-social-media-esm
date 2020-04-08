# Custom functions are an important part of a drake workflow.
# This is where you write them.
# Details: https://books.ropensci.org/drake/plans.html#functions

read_data <- function() {
  f <- list.files("Processed CSVs", full.names = T)
  f <- f[tools::file_ext(f) == "csv"]
  f <- f[!(str_detect(f, "key.csv") | str_detect(f, "Merged data"))]
  d <- map_df(f, read_and_label)
  d
}

read_and_label <- function(f) {
  d <- read_csv(f)
  f_after_directory <- str_split(f, "CSVs/")
  f_after_directory <- map_chr(f_after_directory, ~.[2])
  d$time_point <- as.integer(str_sub(f_after_directory, start = 1, end = 1))
  d
}

read_data() %>% 
  select(RecipientEmail, Q1_27, Q1_28, Q1_30, Q1_31, Q1_32, time_point) %>% 
  gather(key, val, Q1_27, Q1_28, Q1_30, Q1_31, Q1_32) %>% 
  mutate(val = if_else(!is.na(val), 1, 0)) %>% 
  group_by(time_point, key) %>% 
  summarize(prop_val = mean(val)) %>% 
  ggplot(aes(x = time_point, y = prop_val, color = key)) +
  geom_point() +
  geom_smooth(se =F) +
  scale_color_brewer("", labels = c("Twitter", "Facebook", "Pinterest", "Instagram", "Teacher Blogs"), type = "qual") +
  theme_bw() +
  ylab("Proportion of Respondents Reporting Use") +
  xlab("Time Point") +
  theme(text = element_text(size = 15))

# key <- read_csv("Processed CSVs/key.csv")
# 
# key$Q1_30
# key$Q1_27
# key$Q19_1_1
# key$Q19_1_2
# key$Q12
# key$Q14
# key$Q19_2_1
# key$Q21_1_1

read_and_label <- function(f, f_short) {
  d <- read_csv(f)
  f_after_directory <- str_split(f, "processed/")
  f_after_directory <- map_chr(f_after_directory, ~.[2])
  d$time_point <- str_sub(f_after_directory, start = 1, end = 1)
  d
}

read_and_slice <- function(f) {
  d <- read_csv(f)
  
  f_after_directory <- str_split(f, "covid/")
  f_after_directory <- map_chr(f_after_directory, ~.[2])
  
  d %>% 
    slice(3:n()) %>% 
    mutate(time_point = str_sub(f_after_directory, start = 22, end = 23)) %>% 
    clean_names()
}

my_func <- function(x) {
  ifelse(!is.na(x), 1, NA)
}

prep_how_sm_to_plot <- function(d) {
  
  d1 <- d %>% 
    dplyr::select(recipient_email, time_point, q21_1_1:q21_1_7) %>% 
    purrr::set_names(c("email", "time",
                       "Posting", "Sharing", "Replying", 
                       "Bookmarking", "Searching", "Subscribing", 
                       "Messaging")) %>% 
    mutate(platform = "twitter")
  
  d2 <- d %>% 
    select(recipient_email, time_point, q21_2_1:q21_2_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "facebook")
  
  d3 <- d %>% 
    select(recipient_email, time_point, q21_4_1:q21_4_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "pinterest")
  
  d4 <- d %>% 
    select(recipient_email, time_point, q21_5_1:q21_5_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "instagram")
  
  d5 <- d %>% 
    select(recipient_email, time_point, q21_7_1:q21_7_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "instagram")
  
  to_plot <- bind_rows(d1, d2, d3, d4, d5) %>% 
    mutate_at(vars(Posting:Messaging), my_func) %>% 
    gather(key, val, -email, -time, -platform) %>%
    mutate(platform = as.factor(platform),
           platform2 = as.integer(platform),
           platform = tools::toTitleCase(as.character(platform)))
  
  to_plot_split <- to_plot %>% 
    mutate(val = if_else(is.na(val), 0, val)) %>% 
    group_by(email, platform, key) %>%
    summarize(sum_val = mean(val)) %>% 
    spread(key, sum_val)
  
  to_plot_split
  
}

plot_subset_of_teachers_how <- function(data_to_plot) {
  
  data_to_plot %>% 
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
    ggtitle("How Teachers Use Social Media") +
    theme(plot.title = element_text(size=22, hjust = .5)) +
    labs(caption = "Scale ranges from 1 (always use) to 0 (never use).") +
    theme(plot.caption = element_text(size=12, hjust = .5)) +
    scale_color_brewer(NULL, type = "qual", palette = 2) +
    scale_fill_brewer(NULL, type = "qual", palette = 2)
  
  ggsave(here("img", "subset-teachers-how.png"))
  
}

plot_all_teachers_how <- function(data_to_plot) {
  
  data_to_plot %>% 
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
    ggtitle("How Teachers Use Social Media") +
    theme(plot.title = element_text(size=22, hjust = .5)) +
    theme(plot.caption = element_text(size=12, hjust = .5))
  
  ggsave(here("img", "all-teachers-how.png"), width = 12, height = 12)
  
}

replace_chars <- function(x) {
  ifelse(is.na(x), 0, 1)
}

calc_total_responses <- function(questions, d, n) {
  vec <- c(((questions - 1) * n):(questions * n))
  dd <- select(d, all_of(vec))
  
  dd %>% 
    mutate_all(replace_chars) %>% 
    summarize_all(sum)
    # gather(key, val) %>%
    # summarize(sum_n = sum(val))
}

all_data %>% 
  select(q19_1_1:q19_7_9) %>% 
  gather(key, val) %>% 
  separate(key, into = c("question", "platform", "item")) %>% 
  mutate(val = replace_chars(val))

summarize_responses_why <- function(all_data) {
  
  d <- select(all_data, contains("q19_"))
  
  out_vec <- 1:9 %>% 
    map(calc_total_responses, d, n = 9) %>% 
    unlist()
  
  out_label <- c(orig_key$Q19_1_1, orig_key$Q19_1_2,
                 orig_key$Q19_1_3, orig_key$Q19_1_4,
                 orig_key$Q19_1_5, orig_key$Q19_1_6,
                 orig_key$Q19_1_7, orig_key$Q19_1_8,
                 orig_key$Q19_1_9) %>% 
    str_sub(start = 82) %>% 
    tools::toTitleCase()
  
  res <- tibble(out_vec, out_label) %>% 
    mutate(var = str_c("Q19_1_", 1:9)) %>% 
    select(var, label = out_label, n = out_vec)
  
  res
  
}

summarize_responses_how <- function(all_data) {
  
  d <- select(all_data, contains("q21_"))
  
  out_vec <- 1:7 %>% 
    map(calc_total_responses, d, n = 7) %>% 
    unlist()
  
  out_label <- c(orig_key$Q21_1_1, orig_key$Q21_1_2,
                 orig_key$Q21_1_3, orig_key$Q21_1_4,
                 orig_key$Q21_1_5, orig_key$Q21_1_6,
                 orig_key$Q21_1_7) %>% 
    str_sub(start = 87) %>% 
    tools::toTitleCase()
  
  res <- tibble(out_vec, out_label) %>% 
    mutate(var = str_c("Q21_1_", 1:7)) %>% 
    select(var, label = out_label, n = out_vec)
  
  res
  
}

summarize_responses_why(all_data)
summarize_responses_how(all_data)

# finding material for class" 30 RESOURCES or LEARN
# > orig_key$Q19_1_2
# sharing my materials" 186 *** SHARE
# > orig_key$Q19_1_3
# sharing my experiences" 12 SHARE
# > orig_key$Q19_1_4
# learning about or reviewing curricular content" 94 *** LEARN
# > orig_key$Q19_1_5
# learning about or reviewing teaching strategies" 75 *** LEARN
# > orig_key$Q19_1_6
# connecting with other educators" 5 CONNECT
# > orig_key$Q19_1_7
# seeking emotional support" 181 *** CONNECT
# > orig_key$Q19_1_8
# following or engaging with specific organizations (e.g., NCTM)" 0 - FOLLOWING
# > orig_key$Q19_1_9
# following or engaging with specific websites (e.g., Teachers Pay Teachers)" 9 - FOLLOWING

prep_why_sm_to_plot <- function(d) {
  
  d1 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_1_1:q19_1_9) %>% 
    purrr::set_names(c("email", "time",
                       "Posting", "Sharing", "Replying", 
                       "Bookmarking", "Searching", "Subscribing", 
                       "Messaging")) %>% 
    mutate(platform = "twitter")
  
  d2 <- d %>% 
    select(recipient_email, time_point, q21_2_1:q21_2_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "facebook")
  
  d3 <- d %>% 
    select(recipient_email, time_point, q21_4_1:q21_4_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "pinterest")
  
  d4 <- d %>% 
    select(recipient_email, time_point, q21_5_1:q21_5_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "instagram")
  
  d5 <- d %>% 
    select(recipient_email, time_point, q21_7_1:q21_7_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "instagram")
  
  to_plot <- bind_rows(d1, d2, d3, d4, d5) %>% 
    mutate_at(vars(Posting:Messaging), my_func) %>% 
    gather(key, val, -email, -time, -platform) %>%
    mutate(platform = as.factor(platform),
           platform2 = as.integer(platform),
           platform = tools::toTitleCase(as.character(platform)))
  
  to_plot_split <- to_plot %>% 
    mutate(val = if_else(is.na(val), 0, val)) %>% 
    group_by(email, platform, key) %>%
    summarize(sum_val = mean(val)) %>% 
    spread(key, sum_val)
  
  to_plot_split
  
}
read_and_label <- function(f) {
  d <- read_csv(f)
  f_after_directory <- str_split(f, "CSVs/")
  f_after_directory <- map_chr(f_after_directory, ~.[2])
  d$time_point <- as.integer(str_sub(f_after_directory, start = 1, end = 1))
  d
}

my_func <- function(x) {
  ifelse(!is.na(x), 1, NA)
}

prep_data_to_plot <- function(d) {
  
  d1 <- d %>% 
    dplyr::select(RecipientEmail, time_point, Q21_1_1:Q21_1_7) %>% 
    purrr::set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "twitter")
  
  d2 <- d %>% 
    select(RecipientEmail, time_point, Q21_2_1:Q21_2_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "facebook")
  
  d3 <- d %>% 
    select(RecipientEmail, time_point, Q21_4_1:Q21_4_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "pinterest")
  
  d4 <- d %>% 
    select(RecipientEmail, time_point, Q21_5_1:Q21_5_7) %>% 
    set_names(c("email", "time",
                "Posting", "Sharing", "Replying", 
                "Bookmarking", "Searching", "Subscribing", 
                "Messaging")) %>% 
    mutate(platform = "instagram")
  
  to_plot <- bind_rows(d1, d2, d3, d4) %>% 
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

plot_subset_of_teachers <- function(data_to_plot) {
  
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
    ggtitle("The Purposes For Which Teachers Use Social Media (via ESM)") +
    theme(plot.title = element_text(size=22, hjust = .5)) +
    labs(caption = "The responses from four select teachers who responded to eight ESM surveys. Scale ranges from 1 (always use) to 0 (never use).") +
    theme(plot.caption = element_text(size=12, hjust = .5)) +
    scale_color_brewer(NULL, type = "qual", palette = 2) +
    scale_fill_brewer(NULL, type = "qual", palette = 2)
  
  ggsave(here("img", "orig-subset-teachers.png"))
  
}

plot_all_teachers <- function(data_to_plot) {
  
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
    ggtitle("Why Teachers Use Social Media") +
    theme(plot.title = element_text(size=22, hjust = .5)) +
    theme(plot.caption = element_text(size=12, hjust = .5))
  
  ggsave(here("img", "orig-all-teachers.png"))

}
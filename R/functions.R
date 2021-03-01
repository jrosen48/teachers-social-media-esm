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
    dplyr::select(recipient_email, time_point, q19_1_1:q19_1_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    mutate(platform = "twitter") %>% 
    mutate_if(is.double, as.character)
  
  d2 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_2_1:q19_2_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    mutate(platform = "facebook") %>% 
    mutate_if(is.double, as.character)
  
  d3 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_3_1:q19_3_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    
    mutate(platform = "linkedin") %>% 
    mutate_if(is.double, as.character)
  
  d4 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_4_1:q19_4_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    
    mutate(platform = "pinterest") %>% 
    mutate_if(is.double, as.character)
  
  d5 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_5_1:q19_5_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    
    mutate(platform = "instagram") %>% 
    mutate_if(is.double, as.character)
  
  d6 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_6_1:q19_6_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    mutate(platform = "reddit") %>% 
    mutate_if(is.double, as.character)
  
  d7 <- d %>% 
    dplyr::select(recipient_email, time_point, q19_7_1:q19_7_5) %>% 
    set_names(c("recipient_email", "platform",
                c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    
    mutate(platform = "blogs") %>% 
    mutate_if(is.double, as.character)
  
  to_plot <- bind_rows(d1, d2, d3, d4, d5, d6, d7) %>% 
    mutate_if(is.numeric, my_func) %>% 
    gather(key, val, -recipient_email, -platform) %>%
    mutate(platform = as.factor(platform),
           platform2 = as.integer(platform),
           platform = tools::toTitleCase(as.character(platform)))
  
  to_plot_split <- to_plot %>% 
    mutate(val = ifelse(is.na(val), 0, 1)) %>% 
    group_by(recipient_email, platform, key) %>%
    summarize(sum_val = mean(val)) %>% 
    spread(key, sum_val)
  
  to_plot_split %>% 
    rename(email = recipient_email)
  
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
  
  # library(ggradar)
  # library(dplyr)
  # library(scales)
  # library(tibble)
  # 
  # mtcars_radar <- mtcars %>% 
  #   as_tibble(rownames = "group") %>% 
  #   mutate_at(vars(-group), rescale) %>% 
  #   tail(4) %>% 
  #   select(1:10)
  # 
  # mtcars_radar %>% 
  #   ggradar()
  
  order_to_plot <- data_to_plot %>% 
    ungroup() %>% 
    rename(Connecting = connecting,
           Finding = finding,
           Following = following,
           Learning = learning,
           Sharing = sharing) %>%
    mutate(email = as.integer(as.factor(email))) %>% 
    rename(group = platform) %>% 
    gather(key, val, -email, -group) %>% 
    group_by(email) %>% 
    summarize(sum_val = sum(val)) %>% 
    arrange(desc(sum_val)) %>% 
    pull(email)
  
  # for GIF
  s <- data_to_plot %>% 
    ungroup() %>% 
    rename(Connecting = connecting,
           Finding = finding,
           Following = following,
           Learning = learning,
           Sharing = sharing) %>%
    mutate(email = as.integer(as.factor(email))) %>% 
    rename(group = platform) %>% 
    group_by(email) %>% 
    group_split() %>% 
    purrr::map(~ select(., -email)) %>% 
    map(ggradar, gridline.mid.colour = "gray",
        grid.label.size = 7,
        axis.label.size = 7)
  
  for (i in 1:length(s)) {
    print(s[[i]] + ggtitle("Educator ", i))
    ggsave(str_c("img/gif/img", i, ".png"), dpi = 150)
  }
  
  ## list file names and read in

  imgs <- list.files("img/gif", full.names = TRUE)
  img_list <- lapply(imgs, image_read)
  
  ## join the images together
  img_joined <- image_join(img_list)
  
  ## animate at 2 frames per second
  img_animated <- image_animate(img_joined, fps = 2)
  
  ## view animated image
  img_animated
  
  ## save to disk
  image_write(image = img_animated,
              path = "teachers-sm.gif")
  
  # for plot
  s <- data_to_plot %>% 
    ungroup() %>% 
    rename(Connecting = connecting,
           Finding = finding,
           Following = following,
           Learning = learning,
           Sharing = sharing) %>%
    mutate(email = as.integer(as.factor(email))) %>% 
    rename(group = platform) %>% 
    group_by(email) %>% 
    group_split() %>% 
    purrr::map(~ select(., -email)) %>% 
    map(ggradar, gridline.mid.colour = "gray",
        grid.label.size = 1,
        group.point.size = 2.25,
        group.line.width = .5,
        axis.label.size = 2.75)
  
  ggarrange(s[[7]], s[[9]], s[[2]], 
            s[[10]], s[[14]], s[[4]], s[[6]], 
            s[[8]], s[[12]], s[[3]], 
            s[[5]], s[[1]], s[[13]], 
            s[[11]],
            ncol=4, nrow=4, common.legend = TRUE, legend="top")
  
  ggsave(here("img", "all-teachers-how-new.png"), width = 10, height = 11)
  
  # data_to_plot %>% 
  #   ungroup() %>% 
  #   mutate(email = as.integer(as.factor(email))) %>% 
  #   gather(key, val, -email, -platform) %>% 
  #   mutate(key = tools::toTitleCase(key)) %>% 
  #   # mutate(val = ifelse(val == 0, NA, val)) %>% 
  #   ggplot(aes(y = val, color = platform, x = key, group = platform)) +
  #   geom_point(size = 2) +
  #   geom_line() +
  #   scale_color_brewer("", type = "qual", palette = 2) +
  #   theme_minimal() +
  #   ylab(NULL) +
  #   xlab(NULL) +
  #   facet_wrap(~email, ncol = 2) +
  #   ylab("Proportion of Time Used") +
  #   theme(text = element_text(size = 14)) +
  #   theme(legend.position = "top",
  #         text = element_text(size = 16))
  # 
  # ggsave(here("img", "all-teachers-how-new.png"), width = 9, height = 11)
  # 
  # data_to_plot %>% 
  #   ungroup() %>% 
  #   mutate(email = as.integer(as.factor(email))) %>% 
  #   rename(Connecting = connecting,
  #          Finding = finding,
  #          Following = following,
  #          Learning = learning,
  #          Sharing = sharing) %>%
  #   ggiraphExtra::ggRadar(aes(group = "platform", facet = "email"), use.label = FALSE) +
  #   scale_color_brewer(NULL, type = "qual", guide = guide_legend(direction = "horizontal")) +
  #   scale_fill_brewer(NULL, type = "qual", guide = guide_legend(direction = "horizontal")) +
  #   theme_bw() +
  #   theme(axis.text.y = element_blank(),
  #         axis.ticks.y = element_blank()) +
  #   theme(legend.position = "top",
  #         text = element_text(size = 16)) +
  #   theme(legend.text=element_text(size=14)) +
  #   theme(strip.text.x = element_text(size = 10)) +
  #   theme(text = element_text(family = "Times")) +
  #   theme(panel.spacing = unit(2, "lines"))
  # 
  # ggsave(here("img", "all-teachers-how.png"), width = 18, height = 15)
  
}

replace_chars <- function(x) {
  ifelse(is.na(x), 0, 1)
}

prep_data_for_modeling <- function(all_data, item_n, stress = NULL) {

  all_data <- all_data %>% 
    select(recipient_email, survey_period, time_point, contains(str_c("q19"))) %>% 
    select(recipient_email, survey_period, time_point, matches(str_c("_", item_n, "$"))) %>% # this grabs the types of responses (how, why)
    mutate_at(4:10, my_func) %>% 
    mutate_all(replace_na, 0) %>% 
    rowwise() %>% 
    mutate(s = sum(c_across(4:10))) %>% 
    mutate(s = ifelse(s >= 1, 1, 0)) %>% 
    mutate(time_point = as.double(time_point)) %>% 
    select(recipient_email, survey_period, time_point, s)
  
  if (!is.null(stress)) {
    all_data <- all_data %>% 
      left_join(stress)
    
    all_data$covid_mean <- as.vector(scale(all_data$covid_mean))
    all_data$covid_mean[is.nan(all_data$covid_mean)]<-NA
    
    return(all_data)
  }
  
  all_data
  
}

find_covid_mean <- function(data_to_scale) {
  # data_to_scale$covid_mean_s <- as.vector(scale(data_to_scale$covid_mean))
  # data_to_scale$covid_mean[is.nan(data_to_scale$covid_mean)]<-NA
  data_to_scale
}

prep_why_sm_to_plot <- function(d) {
  
  d1 <- d %>% 
    dplyr::select(recipient_email, time_point, q21_1_1:q21_1_7) %>% 
    set_names(c("recipient_email", "platform",
                c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    mutate(platform = "twitter")
  
  d2 <- d %>% 
    dplyr::select(recipient_email, time_point, q21_2_1:q21_2_7) %>% 
    set_names(c("recipient_email", "platform",
                c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    
    mutate(platform = "facebook")
  
  d3 <- d %>% 
    dplyr::select(recipient_email, time_point, q21_4_1:q21_4_7) %>% 
    set_names(c("recipient_email", "platform",
                c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    
    mutate(platform = "pinterest")
  
  d4 <- d %>% 
    dplyr::select(recipient_email, time_point, q21_5_1:q21_5_7) %>%  
    set_names(c("recipient_email", "platform",
                c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    
    mutate(platform = "instagram")
  
  d5 <- d %>% 
    dplyr::select(recipient_email, time_point, q21_7_1:q21_7_7) %>% 
    set_names(c("recipient_email", "platform",
                c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    
    mutate(platform = "blogs")
  
  to_plot <- bind_rows(d1, d2, d3, d4, d5) %>% 
    mutate_if(is.numeric, my_func) %>% 
    gather(key, val, -recipient_email, -platform) %>%
    mutate(platform = as.factor(platform),
           platform2 = as.integer(platform),
           platform = tools::toTitleCase(as.character(platform)))
  
  to_plot_split <- to_plot %>% 
    mutate(val = ifelse(is.na(val), 0, 1)) %>% 
    group_by(recipient_email, platform, key) %>%
    summarize(sum_val = mean(val)) %>% 
    spread(key, sum_val)
  
  to_plot_split %>% 
    rename(email = recipient_email)
  
}

plot_subset_of_teachers_why <- function(data_to_plot) {
  
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
    ggtitle("Why Teachers Use Social Media") +
    theme(plot.title = element_text(size=22, hjust = .5)) +
    labs(caption = "Scale ranges from 1 (always use) to 0 (never use).") +
    theme(plot.caption = element_text(size=12, hjust = .5)) +
    scale_color_brewer(NULL, type = "qual", palette = 2) +
    scale_fill_brewer(NULL, type = "qual", palette = 2)
  
  ggsave(here("img", "subset-teachers-why.png"))
  
}

plot_all_teachers_why <- function(data_to_plot) {
  
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
  
  ggsave(here("img", "all-teachers-why.png"), width = 12, height = 12)
  
}

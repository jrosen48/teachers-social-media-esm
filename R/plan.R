# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan = drake_plan(
  
  # original data 
  
  orig_data = list.files(here("data-raw", "orig", "processed"), full.names = T) %>% 
    map_df(read_and_label) %>% 
    clean_names() %>% 
    select(recipient_email, time_point, q1_27:q1_33, q19_1_1:q19_9_9, q21_1_1:q21_9_7) %>% 
    mutate(survey_period = "orig"),
  
  orig_key = read_csv(here("data-raw", "orig", "key.csv")),
  
  # covid data
  
  new_data = list.files(here("data-raw", "covid"), full.names = T) %>% 
    map_df(read_and_slice) %>% 
    select(recipient_email, time_point, q1_27:q1_33, q19_1_1:q19_9_9, q21_1_1:q21_9_7) %>% 
    mutate(survey_period = "covid"),
  
  # timing
  overall_time_point_df = all_data %>% 
    count(time_point, survey_period) %>% 
    arrange(desc(survey_period)) %>% 
    mutate(overall_time_point = 1:nrow(.)),
  
  # combining data
  all_data = bind_rows(orig_data, new_data),
  
  # how sm use - descriptives
  how_df = all_data %>% 
    select(q19_1_1:q19_7_9) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  how_tab = how_df %>% 
    filter(val == 1) %>% 
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% 
    set_names(c("platform", c("finding material for class", "sharing my materials", "sharing my experiences", "learning about or reviewing curricular content", "learning about or reviewing teaching strategies", "connecting with other educators", "seeking emotional support", "following or engaging with specific organizations (e.g., NCTM)", "following or engaging with specific websites (e.g., Teachers Pay Teachers)"))) %>% 
    mutate(platform = c("Twitter", "Facebook", "LinkedIn",
                        "Pinterest", "Instagram", "Reddit", "Blogs")),
  
  how_tab_tot = how_tab %>% rbind(c("Total", as.vector(colSums(how_tab[, 2:8])))),
  
  how_tab_prop = how_tab %>% mutate_if(is_double, ~ ./(nrow(all_data))),
  
  # how
  how_data_to_plot = prep_how_sm_to_plot(all_data),
  how_subset_of_teachers_plot = plot_subset_of_teachers_how(how_data_to_plot),
  how_all_teachers_plot = plot_all_teachers_how(how_data_to_plot),
  
  # ACTUALLY, THIS IS HOW - need to switch these
  # why sm use - descriptives
  why_df = all_data %>% 
    select(q21_1_1:q21_9_7) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  why_tab = why_df %>% 
    filter(val == 1) %>% 
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% 
    set_names(c("platform", c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    filter(platform != 9) %>% # filtering other
    mutate(platform = c("Twitter", "Facebook", "LinkedIn",
                        "Pinterest", "Instagram", "Reddit", "Blogs")),
  
  why_tab_tot = why_tab %>% rbind(c("Total", as.vector(colSums(why_tab[, 2:8])))),
  
  why_tab_prop = why_tab %>% mutate_if(is_double, ~ ./(nrow(all_data))),
  
  # why 
  why_data_to_plot = prep_why_sm_to_plot(all_data),
  why_subset_of_teachers_plot = plot_subset_of_teachers_why(why_data_to_plot),
  why_all_teachers_plot = plot_all_teachers_why(why_data_to_plot),
  
  # modeling platform
  
  m1 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 1), family = "poisson"),
  m2 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 2), family = "poisson"),
  m3 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 3), family = "poisson"),
  m4 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 4), family = "poisson"),
  m5 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 5), family = "poisson"),
  m6 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 6), family = "poisson"),
  m7 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 7), family = "poisson"),
  
  model_list = list(m1, m2, m3, m4, m5, m6, m7),
  
  model_output = model_list %>%
    map(broom.mixed::tidy) %>% 
    map(~ filter(., term == "survey_periodorig")) %>% 
    map_df(~.) %>% 
    mutate(effect = c("Twitter", "Facebook", "LinkedIn",
                      "Pinterest", "Instagram", "Reddit", "Blogs")) %>% 
    select(-group)
  
)


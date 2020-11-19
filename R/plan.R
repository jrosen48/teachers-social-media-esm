plan = drake_plan(
  
  # original data 
  
  orig_data = list.files(here("data-raw", "orig", "processed"), full.names = T) %>% 
    map_df(read_and_label) %>% 
    clean_names() %>% 
    select(recipient_email, time_point, q1_27:q1_33, q19_1_1:q19_9_9, q21_1_1:q21_9_7) %>% 
    mutate(survey_period = "orig"),
  
  orig_key = read_csv(here("data-raw", "orig", "key.csv")),
  
  # covid data
  
  new_data = list.files(here("data-raw", "covid"), full.names = T) %>% # five emails are NA
    map_df(read_and_slice) %>% 
    select(recipient_email, time_point, q1_27:q1_33, q19_1_1:q19_9_9, q21_1_1:q21_9_7) %>% 
    mutate(survey_period = "covid"),
  
  # timing
  overall_time_point_df = all_data %>% 
    count(time_point, survey_period) %>% 
    arrange(desc(survey_period)) %>% 
    mutate(overall_time_point = 1:nrow(.)),
  
  # combining data
  all_data = bind_rows(orig_data, new_data) %>% 
    filter(!is.na(recipient_email)),
  
  # how sm use - descriptives
  # 19 has nine goals
  how_df = all_data %>% 
    select(q19_1_1:q19_9_9) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  how_tab = how_df %>% 
    filter(val == 1) %>% # also no nones!
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% 
    set_names(c("platform", c("finding material for class", "sharing my materials", "sharing my experiences", "learning about or reviewing curricular content", "learning about or reviewing teaching strategies", "connecting with other educators", "seeking emotional support", "following or engaging with specific organizations (e.g., NCTM)", "following or engaging with specific websites (e.g., Teachers Pay Teachers)"))) %>% 
    mutate(platform = c("Twitter", "Facebook", "LinkedIn",
                        "Pinterest", "Instagram", "Reddit", "Blogs", "Other")),
  
  how_tab_tot = how_tab %>% rbind(c("Total", as.vector(colSums(how_tab[, 2:8])))),
  how_tab_prop = how_tab %>% mutate_if(is_double, ~ ./(nrow(all_data))),
  
  # how
  how_data_to_plot = prep_how_sm_to_plot(all_data),
  how_subset_of_teachers_plot = plot_subset_of_teachers_how(how_data_to_plot),
  how_all_teachers_plot = plot_all_teachers_how(how_data_to_plot),
  
  # ACTUALLY, THIS IS HOW - need to switch these
  # why sm use - descriptives
  # 21 has 7
  why_df = all_data %>% 
    select(q21_1_1:q21_9_7) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  why_tab = why_df %>% 
    filter(val == 1) %>% 
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% # wow, no "nones" for why
    set_names(c("platform", c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately"))) %>% 
    mutate(platform = c("Twitter", "Facebook", "LinkedIn",
                        "Pinterest", "Instagram", "Reddit", "Blogs", "Other")),
  
  why_tab_tot = why_tab %>% rbind(c("Total", as.vector(colSums(why_tab[, 2:8])))),
  
  why_tab_prop = why_tab %>% mutate_if(is_double, ~ ./(nrow(all_data))),
  
  # why 
  why_data_to_plot = prep_why_sm_to_plot(all_data),
  why_subset_of_teachers_plot = plot_subset_of_teachers_why(why_data_to_plot),
  why_all_teachers_plot = plot_all_teachers_why(why_data_to_plot),
  
  # modeling features
  # 21 has 7 features of SM
  m1 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 1), family = "poisson"),
  m2 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 2), family = "poisson"),
  m3 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 3), family = "poisson"),
  m4 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 4), family = "poisson"),
  m5 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 5), family = "poisson"),
  m6 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 6), family = "poisson"),
  m7 = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 21, 7), family = "poisson"), # warning
  
  model_list = list(m1, m2, m3, m4, m5, m6, m7),
  
  model_output = model_list %>%
    map(broom.mixed::tidy) %>% 
    map(~ filter(., term == "survey_periodorig")) %>% 
    map_df(~.) %>% 
    mutate(effect = c("creating posts or pages", "sharing or reposting others' posts", "replying to others' posts", "bookmarking or saving posts or pages", "searching (through a search bar or function)", "subscribed to or followed a person, page, or resource", "contacted a page or other user privately")) %>% 
    select(-group) %>% 
    mutate(icc = model_list %>% map(performance::icc) %>% map_dbl(~.$ICC_conditional),
           warning = c(rep(F, 6), T)),
  
  # modeling goals
  # 19 has 9 goals
  
  m1p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 1), family = "poisson"),
  m2p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 2), family = "poisson"),
  m3p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 3), family = "poisson"),
  m4p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 4), family = "poisson"),
  m5p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 5), family = "poisson"),
  m6p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 6), family = "poisson"),
  m7p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 7), family = "poisson"),
  m8p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 8), family = "poisson"),
  m9p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 9), family = "poisson"),
  
  model_listp = list(m1p, m2p, m3p, m4p, m5p, m6p, m7p, m8p, m9p),
  
  model_outputp = model_listp %>%
    map(broom.mixed::tidy) %>% 
    map(~ filter(., term == "survey_periodorig")) %>% 
    map_df(~.) %>% 
    mutate(effect = c("finding material for class", "sharing my materials", "sharing my experiences", "learning about or reviewing curricular content", "learning about or reviewing teaching strategies", "connecting with other educators", "seeking emotional support", "following or engaging with specific organizations (e.g., NCTM)", "following or engaging with specific websites (e.g., Teachers Pay Teachers)")) %>% 
    select(-group) %>% 
    mutate(icc = model_listp %>% map(performance::icc) %>% map_dbl(~.$ICC_conditional),
           warning = c(rep(F, 9))),
    
  # stress
  
  stress_data = list.files(here("data-raw", "covid"), full.names = T) %>% 
    map_df(read_and_slice) %>% 
    select(recipient_email, time_point, contains("q24")) %>% 
    mutate(survey_period = "covid"),
  
  stress_data_processed = stress_data %>% 
    mutate_at(vars(q24_1:q24_10), replace_na, 0) %>% 
    mutate_at(vars(q24_1:q24_10), as.integer) %>% 
    rowwise() %>% 
    mutate(stress_var = sum(c_across(q24_1:q24_10))) %>% 
    mutate(m = stress_var/10) %>% 
    select(recipient_email, time_point, survey_period, stress_var, m) %>% 
    filter(!is.na(recipient_email)),
  
  stress_data_joined_all = all_data %>% 
    mutate(row_num = 1:nrow(.)) %>% 
    left_join(stress_data_processed) %>% 
    filter(!is.na(recipient_email)),
  
  m1s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 1, stress_data_processed), family = "poisson"),
  m2s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 2, stress_data_processed), family = "poisson"),
  m3s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 3, stress_data_processed), family = "poisson"),
  m4s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 4, stress_data_processed), family = "poisson"),
  m5s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 5, stress_data_processed), family = "poisson"),
  m6s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 6, stress_data_processed), family = "poisson"),
  m7s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 7, stress_data_processed), family = "poisson"),
  m8s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 8, stress_data_processed), family = "poisson"),
  m9s = glmer(s ~ m + (1|recipient_email), data = prep_data_for_modeling(all_data, 19, 9, stress_data_processed), family = "poisson"),
  
  model_lists = list(m1s, m2s, m3s, m4s, m5s, m6s, m7s, m8s, m9s),
  
  model_outputs = model_lists %>%
    map(broom.mixed::tidy) %>% 
    map(~ filter(., term == "m")) %>% 
    map_df(~.) %>% 
    mutate(effect = c("finding material for class", "sharing my materials", "sharing my experiences", "learning about or reviewing curricular content", "learning about or reviewing teaching strategies", "connecting with other educators", "seeking emotional support", "following or engaging with specific organizations (e.g., NCTM)", "following or engaging with specific websites (e.g., Teachers Pay Teachers)")) %>% 
    select(-group) %>% 
    mutate(icc = model_lists %>% map(performance::icc) %>% map_dbl(~.$ICC_conditional),
           warning = c(rep(F, 9))),
  
  # output
  
  output = rmarkdown::render(
    knitr_in("output.Rmd"),
    output_file = file_out("docs/output.html"),
    params = list(overall_time_point_df = overall_time_point_df,
                  purposes = how_tab_prop,
                  actions = why_tab_prop,
                  purposes_m = model_outputp,
                  actions_m = model_output,
                  stress_m = model_outputs
                  )),
  
  rendered_site = target(
    command = rmarkdown::render_site("docs"),
    trigger = trigger(condition = TRUE)
  ),
)


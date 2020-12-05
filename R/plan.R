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
  all_data_raw = bind_rows(orig_data, new_data) %>% 
    filter(!is.na(recipient_email)),
  
  # grouping together purposes
  all_data = all_data_raw %>% 
    mutate(q19_1_1 = q19_1_1,
           q19_1_2 = ifelse(q19_1_2 == "1" | q19_1_3 == "1", 1, 0),
           q19_1_3 = ifelse(q19_1_4 == "1" | q19_1_5 == "1", 1, 0),
           q19_1_4 = ifelse(q19_1_6 == "1" | q19_1_7 == "1", 1, 0),
           q19_1_5 = ifelse(q19_1_8 == "1" | q19_1_9 == "1", 1, 0)) %>% 
    select(-q19_1_6,-q19_1_7,-q19_1_8,-q19_1_9) %>% 
    mutate(q19_2_1 = q19_2_1,
           q19_2_2 = ifelse(q19_2_2 == "1" | q19_2_3 == "1", 1, 0),
           q19_2_3 = ifelse(q19_2_4 == "1" | q19_2_5 == "1", 1, 0),
           q19_2_4 = ifelse(q19_2_6 == "1" | q19_2_7 == "1", 1, 0),
           q19_2_5 = ifelse(q19_2_8 == "1" | q19_2_9 == "1", 1, 0)) %>% 
    select(-q19_2_6,-q19_2_7,-q19_2_8,-q19_2_9) %>% 
    mutate(q19_4_1 = q19_4_1,
           q19_4_2 = ifelse(q19_4_2 == "1" | q19_4_3 == "1", 1, 0),
           q19_4_3 = ifelse(q19_4_4 == "1" | q19_4_5 == "1", 1, 0),
           q19_4_4 = ifelse(q19_4_6 == "1" | q19_4_7 == "1", 1, 0),
           q19_4_5 = ifelse(q19_4_8 == "1" | q19_4_9 == "1", 1, 0)) %>% 
    select(-q19_4_6,-q19_4_7,-q19_4_8,-q19_4_9) %>% 
    mutate(q19_5_1 = q19_5_1,
           q19_5_2 = ifelse(q19_5_2 == "1" | q19_5_3 == "1", 1, 0),
           q19_5_3 = ifelse(q19_5_4 == "1" | q19_5_5 == "1", 1, 0),
           q19_5_4 = ifelse(q19_5_6 == "1" | q19_5_7 == "1", 1, 0),
           q19_5_5 = ifelse(q19_5_8 == "1" | q19_5_9 == "1", 1, 0)) %>% 
    select(-q19_5_6,-q19_5_7,-q19_5_8,-q19_5_9) %>% 
    mutate(q19_7_1 = q19_7_1,
           q19_7_2 = ifelse(q19_7_2 == "1" | q19_7_3 == "1", 1, 0),
           q19_7_3 = ifelse(q19_7_4 == "1" | q19_7_5 == "1", 1, 0),
           q19_7_4 = ifelse(q19_7_6 == "1" | q19_7_7 == "1", 1, 0),
           q19_7_5 = ifelse(q19_7_8 == "1" | q19_7_9 == "1", 1, 0)) %>% 
    select(-q19_7_6,-q19_7_7,-q19_7_8,-q19_7_9),
  
  # how sm use - descriptives
  # 19 has nine goals
  how_df = all_data %>% 
    select(q19_1_1:q19_1_5,
           q19_2_1:q19_2_5,
           q19_4_1:q19_4_5,
           q19_5_1:q19_5_5,
           q19_7_1:q19_7_5) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  how_tab = how_df %>% 
    filter(val == 1) %>% # also no nones!
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% 
    set_names(c("platform", c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    mutate(platform = c("Twitter", "Facebook", 
                        "Pinterest", "Instagram", "Blogs")),
  
  how_tab_tot = how_tab %>% rbind(c("Total", as.vector(colSums(how_tab[, 2:6])))),
  how_tab_prop = how_tab_tot %>% mutate_at(vars(2:6), as.double) %>%  mutate_if(is_double, ~ ./(nrow(all_data))),
  
  # how
  how_data_to_plot = prep_how_sm_to_plot(all_data),
  how_subset_of_teachers_plot = plot_subset_of_teachers_how(how_data_to_plot),
  how_all_teachers_plot = plot_all_teachers_how(how_data_to_plot),
  
  # JUST FOR ORIG
  how_dfo = all_data %>% 
    filter(survey_period == "orig") %>% 
    select(q19_1_1:q19_1_5,
           q19_2_1:q19_2_5,
           q19_4_1:q19_4_5,
           q19_5_1:q19_5_5,
           q19_7_1:q19_7_5) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  how_tabo = how_dfo %>% 
    filter(val == 1) %>% # also no nones!
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% 
    set_names(c("platform", c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    mutate(platform = c("Twitter", "Facebook", 
                        "Pinterest", "Instagram", "Blogs")),
  
  how_tab_toto = how_tabo %>% rbind(c("Total", as.vector(colSums(how_tabo[, 2:6])))),
  how_tab_propo = how_tab_toto %>% mutate_at(vars(2:6), as.double) %>%  mutate_if(is_double, ~ ./(nrow(all_data))),
  
  # JUST FOR COVID
  how_dfc = all_data %>% 
    filter(survey_period == "covid") %>% 
    select(q19_1_1:q19_1_5,
           q19_2_1:q19_2_5,
           q19_4_1:q19_4_5,
           q19_5_1:q19_5_5,
           q19_7_1:q19_7_5) %>% 
    gather(key, val) %>% 
    separate(key, into = c("question", "platform", "item")) %>% 
    mutate(val = replace_chars(val)),
  
  how_tabc = how_dfc %>% 
    filter(val == 1) %>% # also no nones!
    select(-val) %>% 
    count(platform, item) %>% 
    spread(item, n, fill = 0) %>% 
    set_names(c("platform", c("finding", "sharing", "learning", "connecting", "following"))) %>% 
    mutate(platform = c("Twitter", "Facebook", 
                        "Pinterest", "Instagram", "Blogs")),
  
  how_tab_totc = how_tabc %>% rbind(c("Total", as.vector(colSums(how_tabc[, 2:6])))),
  how_tab_propc = how_tab_totc %>% mutate_at(vars(2:6), as.double) %>%  mutate_if(is_double, ~ ./(nrow(all_data))),

  # modeling goals
  # 19 has 9 goals
  
  m1p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 1), family = "binomial"),
  m2p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 2), family = "binomial"),
  m3p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 3), family = "binomial"),
  m4p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 4), family = "binomial"),
  m5p = glmer(s ~ survey_period + (1|recipient_email), data = prep_data_for_modeling(all_data, 5), family = "binomial"),
  
  # sanity check
  sanity_check = prep_data_for_modeling(all_data, 3) %>% 
    group_by(survey_period) %>% 
    summarize(ssum = sum(s)/n()),
  
  model_listp = list(m1p, m2p, m3p, m4p, m5p),
  
  model_outputp = model_listp %>%
    map(broom.mixed::tidy) %>% 
    map(~ filter(., term == "survey_periodorig")) %>% 
    map_df(~.) %>% 
    mutate(effect = c("finding", "sharing", "learning", "connecting", "following")) %>% 
    select(-group) %>% 
    mutate(icc = model_listp %>% map(performance::icc) %>% map_dbl(~.$ICC_conditional),
           ame = model_listp %>% map(margins::margins) %>% map(summary) %>% map(pluck(2)) %>% unlist(),
           warning = c(rep(F, 5)),
           irr = exp(estimate)) %>% 
    select(effect, term, estimate, irr, ame, icc, everything()),
  
  # stress
  
  stress_data = list.files(here("data-raw", "covid"), full.names = T) %>% 
    map_df(read_and_slice) %>% 
    select(recipient_email, time_point, contains("q24"), contains("q25")) %>% 
    mutate(survey_period = "covid"),
  
  stress_data_processed_odd_numbers = stress_data %>% 
    filter(!is.na(recipient_email)) %>% 
    mutate_at(vars(q24_1:q25_8), as.integer) %>% 
    rowwise() %>% 
    mutate(relationships_mean = mean(c_across(q24_1:q24_5), na.rm = TRUE),
           health_mean = mean(c_across(q24_6:q24_7), na.rm = TRUE),
           covid_mean = mean(c_across(q24_8:q24_10), na.rm = TRUE),
           tech_mean = mean(c_across(q25_1:q25_8), na.rm = TRUE),
           all_mean = mean(c_across(q24_1:q25_8), na.rm = TRUE)) %>% 
    select(recipient_email, time_point, survey_period, contains("_mean")) %>% 
    mutate(time_point = as.integer(time_point)) %>% 
    filter(time_point %in% seq(1, 22, by = 2)),
  
  stress_data_processed_even_numbers = stress_data_processed_odd_numbers %>% 
    mutate(time_point = time_point - 1) %>% 
    filter(time_point > 0),
  
  stress_data_processed_raw = bind_rows(stress_data_processed_odd_numbers, 
                                        stress_data_processed_even_numbers) %>% 
    arrange(time_point),
  
  all_data_tm = mutate(all_data, time_point = as.double(time_point)),
  
  m1so = glmer(s ~ all_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 1, stress_data_processed_raw), family = "binomial"),
  m2so = glmer(s ~ all_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 2, stress_data_processed_raw), family = "binomial"),
  m3so = glmer(s ~ all_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 3, stress_data_processed_raw), family = "binomial"),
  m4so = glmer(s ~ all_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 4, stress_data_processed_raw), family = "binomial"),
  m5so = glmer(s ~ all_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 5, stress_data_processed_raw), family = "binomial"),
  
  model_lists_overall = list(m1so, m2so, m3so, m4so, m5so),
  
  m1s = glmer(s ~ relationships_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 1, stress_data_processed_raw), family = "binomial"),
  m2s = glmer(s ~ relationships_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 2, stress_data_processed_raw), family = "binomial"),
  m3s = glmer(s ~ relationships_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 3, stress_data_processed_raw), family = "binomial"),
  m4s = glmer(s ~ relationships_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 4, stress_data_processed_raw), family = "binomial"),
  m5s = glmer(s ~ relationships_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 5, stress_data_processed_raw), family = "binomial"),
  
  model_lists_relationships = list(m1s, m2s, m3s, m4s, m5s),
  
  m1s1 = glmer(s ~ health_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 1, stress_data_processed_raw), family = "binomial"),
  m2s1 = glmer(s ~ health_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 2, stress_data_processed_raw), family = "binomial"),
  m3s1 = glmer(s ~ health_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 3, stress_data_processed_raw), family = "binomial"),
  m4s1 = glmer(s ~ health_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 4, stress_data_processed_raw), family = "binomial"),
  m5s1 = glmer(s ~ health_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 5, stress_data_processed_raw), family = "binomial"),
  
  model_lists_health = list(m1s1, m2s1, m3s1, m4s1, m5s1),
  
  m1s2 = glmer(s ~ covid_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 1, stress_data_processed_raw), family = "binomial"),
  m2s2 = glmer(s ~ covid_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 2, stress_data_processed_raw), family = "binomial"),
  m3s2 = glmer(s ~ covid_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 3, stress_data_processed_raw), family = "binomial"),
  m4s2 = glmer(s ~ covid_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 4, stress_data_processed_raw), family = "binomial"),
  m5s2 = glmer(s ~ covid_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 5, stress_data_processed_raw), family = "binomial"),
  
  model_lists_covid = list(m1s2, m2s2, m3s2, m4s2, m5s2),
  
  m1s3 = glmer(s ~ tech_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 1, stress_data_processed_raw), family = "binomial"),
  m2s3 = glmer(s ~ tech_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 2, stress_data_processed_raw), family = "binomial"),
  m3s3 = glmer(s ~ tech_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 3, stress_data_processed_raw), family = "binomial"),
  m4s3 = glmer(s ~ tech_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 4, stress_data_processed_raw), family = "binomial"),
  m5s3 = glmer(s ~ tech_mean + (1|recipient_email), data = prep_data_for_modeling(all_data_tm, 5, stress_data_processed_raw), family = "binomial"),
  
  model_lists_tech = list(m1s3, m2s3, m3s3, m4s3, m5s3),
  
  combined_lists = list(model_lists_overall, model_lists_relationships, model_lists_health, model_lists_covid, model_lists_tech) %>% unlist(),
  
  model_outputs = combined_lists %>%
    map(broom.mixed::tidy) %>%
    map(~ filter(., str_detect(term, "_mean"))) %>%
    map_df(~.) %>%
    mutate(effect = rep(c("finding", "sharing", "learning", "connecting", "following"), 5)) %>%
    select(-group) %>%
    mutate(icc = combined_lists %>% map(performance::icc) %>% map_dbl(~.$ICC_conditional),
           ame = combined_lists %>% map(margins::margins) %>% map(summary) %>% map(pluck(2)) %>% unlist(),
           irr = exp(estimate),
           warning = c(rep(F, 25))) %>%
    select(effect, term, estimate, irr, ame, icc, everything()) %>% 
    arrange(term),
  
  # output
  
  output = rmarkdown::render(
    knitr_in("output.Rmd"),
    output_file = file_out("docs/output.html"),
    params = list(overall_time_point_df = overall_time_point_df,
                  purposes = how_tab_prop,
                  purposes_o = how_tab_propo,
                  purposes_c = how_tab_propc,
                  purposes_m = model_outputp,
                  stress_m = model_outputs,
                  stress_data = stress_data_processed_raw
    )),
  
  rendered_site = target(
    command = rmarkdown::render_site("docs"),
    trigger = trigger(condition = TRUE)
  ),
)

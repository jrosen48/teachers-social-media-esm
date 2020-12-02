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
    select(-q19_1_6,-q19_1_7,-q19_1_8,-q19_1_9),
  
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
  
stress_data_processed = stress_data %>% 
  filter(!is.na(recipient_email)) %>% 
  mutate_at(vars(q24_1:q25_8), as.integer) %>% 
  rowwise() %>% 
  #mutate(n_na = sum_na(c_across(q24_1:q25_8))) %>% 
  #mutate(n_valid = 14-n_na) %>% 
  mutate(stress_mean = mean(c_across(q24_1:q25_8), na.rm = TRUE)) %>% 
  select(recipient_email, time_point, survey_period, stress_mean),

stress_data_joined_all = all_data %>% 
  mutate(row_num = 1:nrow(.)) %>% 
  left_join(stress_data_processed) %>% 
  filter(!is.na(recipient_email)),

m1s = glmer(s ~ stress_mean + (1|recipient_email), data = prep_data_for_modeling(all_data, 1, stress_data_processed), family = "binomial"),
m2s = glmer(s ~ stress_mean + (1|recipient_email), data = prep_data_for_modeling(all_data, 2, stress_data_processed), family = "binomial"),
m3s = glmer(s ~ stress_mean + (1|recipient_email), data = prep_data_for_modeling(all_data, 3, stress_data_processed), family = "binomial"),
m4s = glmer(s ~ stress_mean + (1|recipient_email), data = prep_data_for_modeling(all_data, 4, stress_data_processed), family = "binomial"),
m5s = glmer(s ~ stress_mean + (1|recipient_email), data = prep_data_for_modeling(all_data, 5, stress_data_processed), family = "binomial"),

model_lists = list(m1s, m2s, m3s, m4s, m5s),

model_outputs = model_lists %>%
  map(broom.mixed::tidy) %>% 
  map(~ filter(., term == "stress_mean")) %>% 
  map_df(~.) %>% 
  mutate(effect = c("finding", "sharing", "learning", "connecting", "following")) %>% 
  select(-group) %>% 
  mutate(icc = model_lists %>% map(performance::icc) %>% map_dbl(~.$ICC_conditional),
         ame = model_lists %>% map(margins::margins) %>% map(summary) %>% map(pluck(2)) %>% unlist(),
         irr = exp(estimate),
         warning = c(rep(F, 5))) %>% 
  select(effect, term, estimate, irr, ame, icc, everything()),

# output

output = rmarkdown::render(
  knitr_in("output.Rmd"),
  output_file = file_out("docs/output.html"),
  params = list(overall_time_point_df = overall_time_point_df,
                purposes = how_tab_prop,
                purposes_m = model_outputp,
                stress_m = model_outputs,
                stress_data = stress_data
  )),

rendered_site = target(
  command = rmarkdown::render_site("docs"),
  trigger = trigger(condition = TRUE)
),
)


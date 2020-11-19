# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  
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
  
  # combined data
  overall_time_point_df = all_data %>% 
    count(time_point, survey_period) %>% 
    arrange(desc(survey_period)) %>% 
    mutate(overall_time_point = 1:nrow(.)),
  
  all_data = bind_rows(orig_data, new_data),
  
  # overall freq of use
  freq_of_use = all_data %>%
    select(q1_27:q1_33) %>% 
    mutate_all(replace_chars) %>% 
    summarize_all(sum) %>%
    set_names(c("twitter", "facebook", "linkedin", "pinterest", "instagram", "reddit", "blogs")),
  
  # how sm use
  data_to_plot = prep_how_sm_to_plot(all_data),
  subset_of_teachers_plot = plot_subset_of_teachers_how(data_to_plot),
  all_teachers_plot = plot_all_teachers_how(data_to_plot),
  
  # orig_key$Q12
  # orig_data %>% count(q1_35_text)
)


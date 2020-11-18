# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  
  # original data 
  
  orig_data = list.files(here("data-raw", "orig", "processed"), full.names = T) %>% 
    map_df(read_and_label) %>% 
    clean_names(),
  
  orig_key = read_csv(here("data-raw", "orig", "key.csv")),
  
  data_to_plot = prep_data_to_plot(orig_data),
  
  subset_of_teachers_plot = plot_subset_of_teachers(data_to_plot),
  all_teachers_plot = plot_all_teachers(data_to_plot),
  
  # covid data
  
  new_data = list.files(here("data-raw", "covid"), full.names = T) %>% 
    map_df(read_csv, skip = 2) %>%
    clean_names()
  
)


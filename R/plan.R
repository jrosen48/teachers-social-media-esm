# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  
  # original data 
  
  orig_data = read_data(),
  orig_key = read_csv(file_in("Processed CSVs/key.csv")),
  
)


# captalization of employee titles
# https://www.cu.edu/university-relations/capitalization
# rule to capitalize official Job Titles:
# https://grammar.yourdictionary.com/capitalization/capitalization-of-job-titles.html

# Download draft file from sharepoint and save
# title_en <-
#   readr::read_csv("data-raw/emp_desc_en.csv")
#
# hrp_emp_desc <-
#   hr_plus() %>%
#   distinct(emp_code, emp_desc)
#
# kth_employment_title <-
#   title_en %>%
#   left_join(
#     hrp_emp_desc, by = c("emp_desc")
#   ) %>%
#   left_join(
#     ss_employment_title, by = c("emp_code" = "id")
#   ) %>%
#   select(emp_code_hr =  emp_code, emp_desc, title_sv, title_en, title_comment = comment)
#
# readr::write_csv(emp_title_lookup, "data-raw/kth_employment_title.csv", na = "")

kth_employment_title <-
  readr::read_csv("data-raw/kth_employment_title.csv", na = "")

usethis::use_data(kth_employment_title, overwrite = TRUE)

kth_employment_title %>%
  View()

# system("mc cp data-raw/emp_title_lookup.csv kthb/kthcorpus/kth_employment_title.csv")


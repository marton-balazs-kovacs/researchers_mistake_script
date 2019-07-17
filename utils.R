#' Function to not include a vector in another vector
#' Retrieved from https://stackoverflow.com/questions/5831794/opposite-of-in
#' 
`%ni%` <- Negate(`%in%`)

#' Function to convert frequency ratings stored as a character to factor
#' 
frequency_char_to_fact <- as_mapper(~ fct_relevel(.x,
                                                  "Very Low",
                                                  "Low",
                                                  "Moderate",
                                                  "High",
                                                  "Very High"))

#' Function to convert seriousness ratings stored as a character to factor
#' 
serious_char_to_fact <- as_mapper(~ fct_relevel(.x,
                                                "Insignificant",
                                                "Minor",
                                                "Moderate",
                                                "Major",
                                                "Extreme"))

# Function that transfroms frequency rating strings to integer
frequency_char_to_int <- as_mapper(~ case_when(.x == "Very Low" ~ 1L,
                                               .x == "Low" ~ 2L,
                                               .x == "Moderate" ~ 3L,
                                               .x == "High" ~ 4L,
                                               .x == "Very High" ~ 5L,
                                               TRUE ~ NA_integer_))

# Function that transforms seriousness rating strings to integer
serious_char_to_int <- as_mapper(~ case_when(str_detect(.x, "Insignificant") ~ 1L,
                                             str_detect(.x, "Minor") ~ 2L,
                                             str_detect(.x, "Moderate")  ~ 3L,
                                             str_detect(.x, "Major")  ~ 4L,
                                             str_detect(.x, "Extreme")  ~ 5L,
                                             TRUE ~ NA_integer_))
# Load and clean the December 2013 data for inclusion in the time series analysis
# Tyler Peters

# load the necessary packages into the environment
  library(readxl) # load the readxl package
  library(dplyr) # load the dplyr package for improved dataframe manipulation
  library(stringr) # load a friendly string extraction package

# load the data into the environment
  # December 2013 data
  dec_data <- read_excel("Dataset.xlsx", sheet = "December 2013 Data")
  # Heart related condition codes
  hrt_key <- read_excel("Dataset.xlsx", sheet = "Heart-related Condition Codes")

# remove duplicate rows
dec_data <- dec_data[!duplicated(dec_data$`Routing SYSID`), ]
  
# define a function to perform the pattern matching between the Routing
  # SYSIDs and the heart related condition codes
str_match <- function(source,match) 
{
  match <- lapply(match,grep,source)
  return(sort(unique(Reduce(c,match))))
}

# compute the presence of the search pattern in the data
code_match <- str_match(dec_data$`Routing SYSID`, hrt_key$`Condition Code`)

# add a redundant index for efficient comparison
dec_data <- dec_data %>%
  mutate(IDX = 1:n())

# add binary columns (0 = ~Abbeville; 1 = Abbeville)
dec_data <- dec_data %>%
  mutate(Abbeville = +(str_sub(dec_data$`Routing SYSID`,1,4) == "L839" & 
                         (str_sub(dec_data$`Routing SYSID`,-4,-1) == "TGU3" | 
                            str_sub(dec_data$`Routing SYSID`,-4,-1) == "ROV8"))) %>%
  mutate(heartCondition = +(dec_data$IDX %in% code_match))

# compute and print the number of rows which meet the prescribed conditions
dec_data <- dec_data %>%
  filter(dec_data$Abbeville + dec_data$heartCondition == 2)
nrow(dec_data)






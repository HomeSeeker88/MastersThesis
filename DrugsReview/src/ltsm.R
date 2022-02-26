drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")
drugsCom.test <- read_tsv("Data/drugsComTest_raw.tsv")


path <- get_file(
  'nietzsche.txt', 
  origin='https://s3.amazonaws.com/text-datasets/nietzsche.txt'
)

# Load, collapse, and tokenize text
text <- read_lines(path) %>%
  str_to_lower() %>%
  str_c(collapse = "\n") %>% 
  tokenize_characters(strip_non_alphanum = FALSE, simplify = TRUE)

text <- tolower(readChar(path, file.info(path)$size))

drugsCom.train <- drugsCom.train %>% 
  mutate(condition = case_when(
    str_detect(condition, "Cance") ~ case_when(
      !str_detect(condition, "Cancer") ~ str_replace(condition, "Cance", "Cancer"),
      TRUE ~ condition
    ),
    TRUE ~ condition
  )) %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))

text <- paste(drugsCom.train$review, collapse = "\n")

max_length <- 25
step <- 3
text_indices <- seq(1, nchar(text) - max_length, by = 100)
sentences <- str_sub(text, text_indices, text_indices + max_length - 1)
next_chars <- str_sub(text, text_indices + max_length, text_indices + max_length)
length(sentences)
chars <- unique(sort(strsplit(text, "")[[1]]))
length(chars)
char_indices <- 1:length(chars)
names(char_indices) <- chars
x <- array(0L, dim = c(length(sentences), max_length, length(chars)))
y <- array(0L, dim = c(length(sentences), length(chars)))


for (i in 1:length(sequences)){
  
}
# UlyssesLM: a Markov model based on the novel "Ulysses".
# Ryanson Jonathan s2570340
# Joseph Gill s1910643
# Fransiskus Budi Kurnia Agung s2670828

# This code will do two main tasks:
# 1) Preprocess the data from the novel "Ulysses",
# to get the most common words used, including
# the probability of certain words appearing next, and
# 2) Create the Markov model based on data extracted
# from the novel.

# Sets the working directories for the coders.
setwd("/Users/rj/Documents/Codes/StatProg/ulysseslm") # Ryan's path
# setwd("/Users/josephgill/Documents/UlyssesLM") # Joseph's path
# setwd("/Users/fransiskusbudi/ulysseslm") # Frans' path

# Defines constants for the program.
m <- 1000 # How many most common words are we using for the model?
mlag <- 4 # The number of maximum "lag" for model generation
nw <- 50 # How many words will the model generate?

# -------------------------------------------------------
# 1) Data Preprocessing
# The codes below are for preprocessing the data. This includes:
# i) file reading and text cleaning,
# ii) separating punctuations from words,
# iii) finding out most-used words, and
# iv) creating token sequences.

# ***
# Section i) Reading and Cleaning Text

# Reads the file.
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73,
          fileEncoding = "UTF-8")

# Cleans data by removing "_(" from the file.
a <- gsub("_(", "", a, fixed = TRUE)

# ***
# Section ii) Separating Punctuations

split_funct <- function(vec) {
  # Separates punctuations from words.
  # Input: vector of strings
  # Output: vector of strings

  # Gets index of words with punctuations.
  vec_grep <- grep("[,.;!:?]", vec, fixed = FALSE)

  # Creates shifted indices to place the punctuations after the words.
  punct_index <- vec_grep + 1:length(vec_grep)

  # Creates a new vector with length of old vector + number of punctuations.
  new_vec <- rep("", length(vec) + length(punct_index))

  # Extracts punctuations from words with punctuations, then puts them
  # into the new vector.
  new_vec[punct_index] <- substr(vec[vec_grep], nchar(vec[vec_grep]),
                                 nchar(vec[vec_grep]))

  # Places remaining elements of old vector into the new vector.
  new_vec[-punct_index] <- substr(vec, 1, length(vec))

  # Removes punctuations from words with punctuations.
  new_vec[-punct_index] <- gsub("[,.;!:?]", "", new_vec[-punct_index],
                                fixed = FALSE)

  # Returns the new vector.
  return(new_vec)
}

# Separates punctuations from text a.
a_sep <- split_funct(a)

# ***
# Section iii) Finding Most Used Words

# Makes all words lowercase.
a_sep_lower <- tolower(a_sep)

# Finds unique words from a_sep_lower.
a_unique <- unique(a_sep_lower)

# Converts words from the text to indices corresponding to a_unique.
index_match <- match(a_sep_lower, a_unique)

# Counts how many times each unique word appears,
# then filters it to only show the top m words.
freq <- tabulate(index_match)
freq_threshold <- freq[order(freq, decreasing = TRUE)][m]
freq_m <- freq >= freq_threshold

# Stores the top m used words.
a_unique_1000 <- a_unique[freq_m]

# Finds words from text that are in the top m words.
b <- match(a_sep_lower, a_unique_1000)

# Creates a matrix with the total number of words from the novel
# as the row size, and (mlag + 1) as the column size.
M <- matrix(nrow = length(b), ncol = (mlag + 1))

# Fills the matrix with the index of the most common words as the first column,
# a shifted-by-1 index as the second column, a shifted-by-2 index as the third,
# and so on.
for (i in 1:ncol(M)){
  shifted_b <- c(b[i:length(b)], rep(NA, times = i - 1))
  M[, i] <- shifted_b
}

# Cuts the last mlag rows of the matrix, since it's filled with NA values.
M <- M[1:(nrow(M) - mlag), ]


# -------------------------------------------------------
# 2) Markov Model
# The codes below are for creating a Markov model based on the text.
# This includes:
# i) Markov model creation
# ii) Frequency-based model creation
# iii) Case-sensitive Markov model creation (yes, we're going for the
# extra 3 marks.)

# ***
# Section i) Markov Model Creation

# First, we need to generate a random first word.
first_word_index <- sample(b[!is.na(b)], 1)

# Then, we use "cat" to show the first word.
cat("Markov model generation result: ")
cat(a_unique_1000[first_word_index])

# Todo:
# 1. Continue the Markov model generation, using first_word as the basis
# for the next (nw - 1) words.
# Find how to get indices of the previous word, then selecting by random.
# Uncomment the following snippet to continue:

# for (i in 2:nw) {
#   for (j in mlag:1) {
#     if (i > j) {

#     }
#   }
# }


# ***
# Section ii) Frequency-Based Model Creation

# For this model, a sample is directly taken from the whole novel.
# This sample is continuously taken until nw words have been generated.
cat("\nFrequency-based model generation result:")

for (i in 1:nw) {
  word <- sample(a_unique_1000, 1)
  if (length(grep("[,.;!:?]", word, fixed = FALSE)) > 0) {
    cat(word)
  } else {
    cat(paste(" ", word, sep = ""))
  }
}

# ***
# Section iii) Case-Sensitive Markov Model Creation

# Todo: get the initial Markov model going first lmaoooo
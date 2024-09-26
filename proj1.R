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
# setwd("/Users/rj/Documents/Codes/StatProg/ulysseslm") # Ryan's path
# setwd("/Users/josephgill/Documents/UlyssesLM") # Joseph's path
setwd("/Users/fransiskusbudi/ulysseslm") # Frans' path

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
a_unique_1000 <- a_unique[freq_m] # b in the instructions

b <- a_unique_1000

# Finds words from text that are in the top m words.
common_word_match <- match(a_sep_lower, a_unique_1000)

# Creates a matrix with the total number of words from the novel
# as the row size, and (mlag + 1) as the column size.
M <- matrix(nrow = length(common_word_match), ncol = (mlag + 1))

# Fills the matrix with the index of the most common words as the first column,
# a shifted-by-1 index as the second column, a shifted-by-2 index as the third,
# and so on.
for (i in 1:ncol(M)){
  shifted_common_word_match <- c(common_word_match[i:length(common_word_match)], rep(NA, times = i - 1))
  M[, i] <- b[shifted_common_word_match]
}

# Cuts the last mlag rows of the matrix, since it's filled with NA values.
M <- M[1:(nrow(M) - mlag), ]

print(M)

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

# First, we need to generate nw-spaced vector
chain_word <- rep("",nw)
# Generate the first word sample
chain_word[1] <- sample(b[!is.na(b)], 1) # w in instruction

cat("Markov model generation result: ")

# Todo:
# 1. Continue the Markov model generation, using first_word as the basis
# for the next (nw - 1) words.
# Find how to get indices of the previous word, then selecting by random.
# Uncomment the following snippet to continue:

for (i in 2:nw) {
  for (j in mlag:1) {
    if (i > j) {
      word_before <- chain_word[(i-j):(i-1)] # Check the words before
      # print(word_before)
      # print(paste(word_before, collapse=""))
      
      # testing <- grep(word_before,M[,1:length(word_before)])
      # testing <- (which(M[,1:length(word_before)] == word_before, arr.ind = TRUE))
      next_word <- sample(b, 1) # Generate the next word randomly based on the word that have been generated, should be by the word before
      
      # NOTE:
      # we need to use the matrix M in the question 7 as the guidance for the probability on selecting the next work,
      # I managed to find the way to check previous word, but still struggling to find the way to match the word to the M matrix
      # as far as I understand, the sequence of the word is important. we can set up a call thus i can explain on the approach
      
      if (!is.na(next_word)){
        chain_word[i] <- next_word
        break
      }
      
    }
  }
}

cat(chain_word)


# ***
# Section ii) Frequency-Based Model Creation

# For this model, a sample is directly taken from the whole novel.
# This sample is continuously taken until nw words have been generated.
cat("\nFrequency-based model generation result:")

for (i in 1:nw) {
  word <- sample(b, 1)
  if (length(grep("[,.;!:?]", word, fixed = FALSE)) > 0) {
    cat(word)
  } else {
    cat(paste(" ", word, sep = ""))
  }
}

# ***
# Section iii) Case-Sensitive Markov Model Creation

# Todo: get the initial Markov model going first lmaoooo
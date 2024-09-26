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
setwd("/Users/fransiskusbudi/ulysseslm") # Ryan's path
# Feel free to add above for your paths @Joseph @Frans

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
# For this case, m ≈ 1000.
m <- 1000
freq <- tabulate(index_match)
freq_threshold <- freq[order(freq, decreasing = TRUE)][m]
freq_m <- freq >= freq_threshold

# Stores the top m used words.
a_unique_1000 <- a_unique[freq_m] #b in the instruction

# Defines mlag, the maximum lag considered for this model.
# To match instructions, the value 4 is set.
mlag <- 4

# Finds words from text that are in the top m words.
common_word_match <- match(a_sep_lower, a_unique_1000)

# Todo: 
# Code above is until 7a) in practical-1.pdf.
# Continue until 7b) below this text.
# Create function to create the shifted matrix
create_shifted_matrix <- function(j, mlag=4){
  n <- length(j) # Calculate number of rows in the matrix
# Create the M matrix with the criteria
  M <- matrix(nrow = n, ncol = mlag + 1)
  return(M)
}

M<- create_shifted_matrix(common_word_match,mlag)
print(nrow(M))
for (i in 1:ncol(M)){
  shift <- i-1
  shifted_common_word_match <- c(rep(NA,times=shift),common_word_match[1:(length(common_word_match)-shift)])
  M[,i]<-shifted_common_word_match
  #M[,i] <- common_word_match
}

# For 8 and 9, continue under Part 2) Markov Model.

# -------------------------------------------------------
# 2) Markov Model
# The codes below are for creating a Markov model based on the text.

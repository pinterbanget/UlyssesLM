# UlyssesLM: a Markov chain model based on the novel "Ulysses".
# Ryanson Jonathan s2570340
# Joseph Gill s1910643
# Fransiskus Budi Kurnia Agung s2670828

# This code will do two main tasks:
# 1) Preprocess the data from the novel "Ulysses",
# to get the most common words used, including
# the probability of certain words appearing next, and
# 2) Create the Markov chain based on data extracted
# from the novel.

# Sets the working directories for the coders.
setwd("/Users/rj/Documents/Codes/StatProg/ulysseslm") # Ryan's path
# Feel free to add above for your paths @Joseph @Frans

# Reads the file.
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73,
          fileEncoding = "UTF-8")
a <- gsub("_(", "", a, fixed = TRUE) ## remove "_("

split_funct <- function(vec) {
  # Separates punctuations from words.
  # Input: vector
  # Output: vector

  # Gets index of words with punctuations.
  vec_grep <- grep("[,.;!:?]", vec, fixed = FALSE)

  # Creates a shifted index to place the punctuations.
  punct_index <- vec_grep + 1:length(vec_grep)

  # Creates a new vector with length of old vector + number of punctuations.
  new_vec <- rep("", length(vec) + length(punct_index))

  # Places elements of old vector into the new vector.
  # Extracts punctuations from words with punctuations.
  new_vec[punct_index] <- substr(vec[vec_grep], nchar(vec[vec_grep]),
                                 nchar(vec[vec_grep]))
  new_vec[-punct_index] <- substr(vec, 1, length(vec))

  # Removes punctuations from words with punctuations.
  new_vec[-punct_index] <- gsub("[,.;!:?]", "", new_vec[-punct_index],
                                fixed = FALSE)

  # Returns the new vector.
  return(new_vec)
}
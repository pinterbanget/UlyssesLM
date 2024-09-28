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
setwd("/Users/josephgill/Documents/UlyssesLM") # Joseph's path
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
b <- a_unique[freq_m]

# Finds words from text that are in the top m words.
common_word_match <- match(a_sep_lower, b)

# Creates a matrix with the total number of words from the novel
# as the row size, and (mlag + 1) as the column size.
M <- matrix(nrow = length(common_word_match), ncol = (mlag + 1))

# Fills the matrix with the index of the most common words as the first column,
# a shifted-by-1 index as the second column, a shifted-by-2 index as the third,
# and so on.
for (i in 1:ncol(M)){
  shifted_cwm <- c(common_word_match[i:length(common_word_match)],
                   rep(NA, times = i - 1))
  M[, i] <- shifted_cwm
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


cat_punct <- function(word) {
  # Prints ("cat"s) the word with a space before the word
  # if it's a word, and without if it's a punctuation.
  # Input: string
  # Output: none

  # Checks if "word" is a punctuation.
  if (length(grep("[,.;!:?]", word, fixed = FALSE)) > 0) {
    cat(word) # Prints the punctuation directly.
  } else {
    cat(paste(" ", word, sep = "")) # Prints the word with a space beforehand.
  }
}

markov_chain <- function(b){

  # For the Markov model, we need to generate a nw-spaced vector to store
  # the result.
  chain_word <- rep(0, nw)
  
  # Then, generate the first word by sampling a word from the novel
  # (in the form of an index) which is in the top m words.
  chain_word[1] <- sample(common_word_match[!is.na(common_word_match)], 1)
  
  # Prints the title and the first word.
  cat("Markov model generation result: ",sep="\n")
  cat(b[chain_word[1]])
  
  # Loops over from 2 (second word) to nw.
  for (i in 2:nw) {
    for (j in mlag:1) {
      if (i > j) {
        # Takes a sequence from the result.
        w <- chain_word[(i - j):(i - 1)]
        backtracked <- FALSE
        # Loops until the pool of potential next words has more than 1 element.
        while (TRUE) {
          limit <- length(w)
  
          # Finds rows of M that starts with w.
          if (backtracked == TRUE){
            to_select <- previous
            backtracked <-FALSE
          }
          else if (limit > 1) {
            to_select <- which(apply(M[, 1:limit], 1,
                                     function(x) return(all(x == w))))
            previous <- to_select
          } else {
            to_select <- which(M[, 1:limit] == w)
          }
  
          # Takes the value of the (limit + 1)th column to be put
          # in a pool of potential next words.
          next_word_pool <- M[to_select, limit + 1]
  
          # Removes NA values from the pool.
          next_word_pool <- next_word_pool[!is.na(next_word_pool)]
          
          # Checks if the pool contains more than 1 element.
          if (length(next_word_pool) > 1) {
            break # Breaks from the loop.
          } else {
            # If not, remove the first element of w, if length(w) > 1.
            if (limit > 1) {
              w <- w[2:limit]
              backtracked <- TRUE
              
            } else {
              # If w already contains 1 element, put all of the novel
              # (with respect to the top m words) into the pool.
              
              next_word_pool <- common_word_match[!is.na(common_word_match)]
              break
            }
          }
        }
  
        # Picks a word from the pool at random, then puts it onto the result.
        next_word <- sample(next_word_pool, 1)
        chain_word[i] <- next_word
        break
      }
    }
    # Prints the result, with respect to punctuations.
    cat_punct(b[next_word])
  }
}
markov_chain(b)


# ***
# Section ii) Frequency-Based Model Creation

# For this model, a sample is taken from the top m most used words,
# utilising weighting based on the frequency of the words when taking
# a sample.

cat("\n\nFrequency-based model generation result:")

for (i in 1:nw) {
  # Samples a word from the most common words, with the frequency of words
  # as the weights for the sampling.
  word <- sample(b[common_word_match[!is.na(common_word_match)]],1)

  # Checks if the generated word is a punctuation.
  # If so, it's printed without any spaces.
  cat_punct(word)
}

# ***
# Section iii) Case-Sensitive Markov Model Creation
# Modifying B

# Get the frequency of word but this time it is case-sensitive
a_unique_cap <- unique(a_sep)
index_match_cap <- match(a_sep, a_unique_cap)
freq_cap <- tabulate(index_match_cap)

# Initialize modified_b from b
modified_b <- b

# Checking through every word in our modified_b to compare its capitalized vs non-capitalized frequency
for (t in 1:length(modified_b)){
  # Validation to make sure it doesn't return integer(0), if no frequency found then return 0
  if (length(freq_cap[which(a_unique_cap==modified_b[t])])==0){ # checking the frequency of non-capitalized word
    freq_low_cap <- 0
  } else {
    freq_low_cap <-freq_cap[which(a_unique_cap==modified_b[t])] 
  }
  if (length(freq_cap[which(a_unique_cap==gsub(substr(modified_b[t],1,1),toupper(substr(modified_b[t],1,1)),modified_b[t]))])==0){ # Checkin the frequency of the capitalized word
    freq_big_cap <- 0
  } else {
    freq_big_cap <- freq_cap[which(a_unique_cap==gsub(substr(modified_b[t],1,1),toupper(substr(modified_b[t],1,1)),modified_b[t]))] 
  }
  if (freq_big_cap > freq_low_cap){
    modified_b[t] <- gsub(substr(modified_b[t],1,1),toupper(substr(modified_b[t],1,1)),modified_b[t]) # if capitalized frequency occurs more often, then our modified_b is replaced with the capitalized word version
  }
  # if(freq_cap[which(a_unique_cap==modified_b[t])] < freq_cap[which(a_unique_cap==gsub(substr(modified_b[t],1,1),toupper(substr(modified_b[t],1,1)),modified_b[t]))]){
  #   modified_b[t] <- gsub(substr(modified_b[t],1,1),toupper(substr(modified_b[t],1,1)),modified_b[t])
  # }
}

markov_chain(modified_b)

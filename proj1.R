# UlyssesLM: a Markov model based on the novel "Ulysses".
# Ryanson Jonathan (s2570340)
# Joseph Gill (s1910643)
# Fransiskus Budi Kurnia Agung (s2670828)

# Contributions:
# Ryan (40%):
# - split_punct function creation & implementation (q5)
# - vector b of the m most common words creation (q6)
# - M implementation, done together IRL (q7)
# - optimised Markov model implementation (q8)
# - frequency-based model implementation (q9)
# - "pretty-print" punctuation function creation (q10b)

# Joseph (30%):
# - M implementation, done together IRL (q7)
# - Markov model optimisation ideas creation (q8)
# - Code cleaning and commenting

# Frans (30%):
# - M implementation, done together IRL (q7)
# - Markov model starting framework creation (q8)
# - Case-sensitive Markov model creation & implementation (q10a)

# This code will do two main tasks:
# 1) Preprocess the data from the novel "Ulysses", to get
# the most common words used to base the Markov model on, and
# 2) Create the Markov model (along with other models)
# based on data extracted from the novel.

# ***
# Sets the working directories for the coders.
# setwd("/Users/rj/Documents/Codes/StatProg/ulysseslm") # Ryan's path
# setwd("/Users/josephgill/Documents/UlyssesLM") # Joseph's path
# setwd("/Users/fransiskusbudi/uoe/ulysseslm") # Frans' path

# Defines constants for the program.
m <- 1000 # How many most common words are we using for the model?
mlag <- 4 # The number of maximum "lag" for model generation
nw <- 50 # How many words will the model generate?



# -------------------------------------------------------
# 1) Data Preprocessing
# The codes below are for preprocessing the data. This includes:
# i) file reading and text cleaning (q1-3),
# ii) separating punctuations from words (q4-5),
# iii) finding out most-used words (q6), and
# iv) creating token sequences (q7).

# ***
# Section i) Reading and Cleaning Text (Questions 1-3)

# Reads the file.
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73,
          fileEncoding = "UTF-8")

# Cleans data by removing "_(" from the file.
a <- gsub("_(", "", a, fixed = TRUE)


# ***
# Section ii) Separating Punctuations (Questions 4-5)

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
# Section iii) Finding Most Used Words (Question 6)

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


# ***
# Section iv) Creating Token Sequences (Question 7)

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
# i) Markov model creation (q8)
# ii) Frequency-based model creation (q9)
# iii) Case-sensitive Markov model creation (q10)
# (yes, we're going for the extra 3 marks.)

# ***
# Section i) Markov Model Creation (Question 8)

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

markov_chain <- function(b, title) {
  # Simulates a Markov model to generate nw words from the top m used words.
  # Input: vector of indices containing the top m used words
  # Output: none

  # Creates a nw-spaced vector to store the result.
  chain_word <- rep(0, nw)

  # Generates the first word by sampling a word from the novel
  # (in the form of an index) which is in the top m words.
  chain_word[1] <- sample(common_word_match[!is.na(common_word_match)], 1)

  # Prints the title and the first word.
  cat(title)
  cat(b[chain_word[1]])

  # Sets up a flag to tell if the model successfully generated the last word.
  # This is helpful to cache options to pick from.
  # Caching the results of the last word generation saves around 50% of time.
  last_word_success <- FALSE

  # Generates the next (nw - 1) words by looping from 2 to nw.
  for (i in 2:nw) {
    for (j in mlag:1) {
      if (i > j) {
        # Takes a sequence from the result.
        w <- chain_word[(i - j):(i - 1)]

        # Loops until the pool of potential next words has more than 1 element.
        while (TRUE) {
          limit <- length(w)

          # If the last word was successfully generated from the chain,
          # loads the previous options from the matrix rows, where
          # the rows contain the last generated word.
          if (last_word_success == TRUE) {
            cache <- cache[M[cache, limit] == w[limit]]
            to_select <- cache
          } else if (limit > 1) {
            # If not, finds rows of M that starts with w.
            to_select <- which(apply(M[, 1:limit], 1,
                                     function(x) return(all(x == w))))
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
            # Stores the rows of M.
            cache <- to_select

            # Since a next word is guaranteed to be from this pool,
            # sets the success flag to TRUE.
            last_word_success <- TRUE

            # Breaks from the loop; pool generation is over.
            break

          } else {
            # Since the pool isn't sufficient, sets the success flag to FALSE.
            last_word_success <- FALSE

            # Cuts the first element of w, if w has more than 1 element.
            if (limit > 1) {
              w <- w[2:limit]
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
    # Prints the word with respect to punctuations.
    cat_punct(b[next_word])
  }
}

markov_chain(b, "\n\nMarkov model result (q8):\n")


# ***
# Section ii) Frequency-Based Model Creation (Question 9)

# For this model, a sample is taken from the top m most used words,
# utilising weighting based on the frequency of the words when taking
# a sample.

cat("\n\nFrequency-based model result (q9):\n")

for (i in 1:nw) {
  # Samples a word from the most common words, with the frequency of words
  # as the weights for the sampling.
  word <- sample(b, 1, prob = freq[freq >= freq_threshold])

  # Prints the word with respect to punctuations.
  cat_punct(word)
}


# ***
# Section iii) Case-Sensitive Markov Model Creation (Question 10)

# For this section, b needs to be modified to accommodate case-sensitiveness.

# Gets the frequency of words, now with a case-sensitive approach
# (i.e. we don't lowercase the words).
a_unique_cap <- unique(a_sep)
index_match_cap <- match(a_sep, a_unique_cap)
freq_cap <- tabulate(index_match_cap)

# Initialises modified_b from b.
modified_b <- b

# Checks through every word in our modified_b to compare its capitalised
# vs non-capitalised frequency.
for (t in 1:length(b)) {
  # Finds the frequency of lowercase words in the full text
  # by comparing the unique text words vs b.
  a_unique_lower_freq <- freq_cap[which(a_unique_cap == b[t])]

  # Validates to make sure it doesn't return integer(0).
  # If no lowercase words are found, return 0.
  if (length(a_unique_lower_freq) == 0) {
    freq_lower <- 0
  } else {
    freq_lower <- a_unique_lower_freq
  }

  # Finds the frequency of uppercase words in the full text
  # by comparing the unique text words vs b.
  # We capitalise the first letter in the words with sub() and toupper(),
  # using which() to find the capitalised word frequency in the full text.
  a_unique_upper_freq <- freq_cap[which(a_unique_cap == sub(substr(b[t], 1, 1), toupper(substr(b[t], 1, 1)), b[t]))]

  # Validates to make sure it doesn't return integer(0).
  # If no uppercase words are found, return 0.
  if (length(a_unique_upper_freq) == 0) {
    freq_upper <- 0
  } else {
    freq_upper <- a_unique_upper_freq
  }

  # If the capitalised word occurs more often, then b is
  # replaced with the capitalised word version as modified_b.
  if (freq_upper > freq_lower) {
    modified_b[t] <- sub(substr(b[t], 1, 1),
                         toupper(substr(b[t], 1, 1)), b[t])
  }
}

markov_chain(modified_b, "\n\nCase-sensitive Markov model result (q10):\n")

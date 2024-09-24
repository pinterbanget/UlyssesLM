# UlyssesLM: a Markov chain model based on the novel "Ulysses".
# Ryanson Jonathan s2570340
# Joseph Gill s
# Fransiskus Budi Kurnia Agung s2670828

# This code will do two main tasks:
# 1) Preprocess the data from the novel "Ulysses",
# to get the most common words used, including
# the probability of certain words appearing next, and
# 2) Create the Markov chain based on data extracted
# from the novel.

# Reading the file
setwd("put/your/local/repo/location/here") ## comment out of submitted
a <- scan("4300-0.txt", what = "character", skip = 73, nlines = 32858 - 73,
          fileEncoding = "UTF-8")
a <- gsub("_(", "", a, fixed = TRUE) ## remove "_("
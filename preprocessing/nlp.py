from __future__ import print_function
from nltk.stem import *
from nltk.stem.snowball import SnowballStemmer
from nltk.corpus import stopwords
from sys import argv
import enchant

stemmer = SnowballStemmer("english")
stop = set(stopwords.words("english"))
d = enchant.Dict("en_US")
with open(argv[1]) as f:
  for line in f:
     word = line.split()[0]
     if d.check(word):
       stemmed = stemmer.stem(word)
       if stemmed not in stop:
         print(word)
         print(stemmer.stem(word))

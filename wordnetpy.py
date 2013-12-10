from nltk.corpus import wordnet as wn
from sys import argv
delimiter="?-?"
result="not in wordnet"
if len(wn.synsets(argv[1])) >0:
	result=wn.synsets(argv[1])[0].definition
print " "+delimiter+" "+result+" "+delimiter+" "

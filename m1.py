import itertools

word = raw_input("Enter word, palindrome or anagram: ")
string_word = str(word)
word_list = list(word)
var = 0
length = 0
permutations = list(itertools.permutations(word_list))

"""format_word = '(' + word_list[length] + ')'

def length_test(word_list)
 if length > ln(word_list):
 format_word = '(' + word_list[length] + ')'
 length + 1
 else:
  None"""

def reversed_string(string_word):
  return string_word[::-1]

def checkVar(var):
 if var == -1:
  return word + " is a word"
 elif var == 1:
  return word + " is an anagram"
 elif var == 100:
  return word + " is a palindrome"
 else:
  return word + " is gibberish"

def permutation_tester(var):
 if ('h', 'e', 'l', 'l', 'o') in permutations and str(word) != "hello":
  var = 1
  return checkVar(var)
 elif string_word == 'hello':
  var = -1
  return checkVar(var)
 elif string_word == reversed_string(string_word):
  var = 100
  return checkVar(var)
 else:
  return checkVar(var)

print permutation_tester(var)

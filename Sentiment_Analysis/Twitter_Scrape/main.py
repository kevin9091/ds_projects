# Cleaning Text Steps
# 1) Create a text file and take text from it
# 2) Convert the letter into lowercase('Apple' is not equivalent to 'apple')
# 3) Remove punctuations like ,.?! etc (Hi! This is Kevin.)

import string

text=open('read.txt', encoding='utf-8').read()
lower_case = text.lower()
print(text)
print(lower_case)

print(string.punctuation)
"""generateTests.py

Generate the test sets for our F# project. 

"""
import argparse
import sys
import random
from random import randint
import copy

def getRandomLetter(alist):
    alphabet = alist
    randomLetter = random.choice(alphabet)
    return randomLetter

def main():


   for m in range(8, 13): #8,13
        product = 5*m #5
        for g in range(0, product):
            str = []
            for strlen in range(0,m*m):
                str.append('0')
            numPairs = randint(m-3,m)
            letterList = []
            alist = list("abcdefghijklmnopqrstuvwxyz")
            for letters in range(0, numPairs):
                randomL = getRandomLetter(alist)
                if(randomL not in letterList):
                    letterList.append(randomL)
                    alist.remove(randomL)

            for x in range(0, numPairs):

                index = randint(0,m*m - 1)
                pos = str[index]
                while(pos != '0'):
                    index = randint(0,m*m  - 1)
                    pos = str[index]
                str[index] = letterList[x]

                index2 = randint(0,m*m  - 1)
                pos = str[index2]
                while(pos != '0'):
                    index2 = randint(0,m*m  - 1)
                    pos = str[index2]
                str[index2] = letterList[x]
            
            converted = ''.join(str)
            #This is where we print and write to any file we want via the terminal
            print m
            print converted
            print ''


if __name__ == "__main__":
    main()

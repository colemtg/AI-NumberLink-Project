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


    # letterList = []
    # alist = list("abcdefghijklmnopqrstuvwxyz")
    # for letters in range(0, 7):
    #     randomL = getRandomLetter(alist)
    #     letterList.append(randomL)
    #     alist.remove(randomL)
    

    # #letterListPair = copy.deepcopy(letterList)
    # initBoard = []

    # for t in range(0,8): #row
    #     #indexOfLetter = randint(0,4)
    #     #limit = False
    #     for k in range(0,8): #column
    #         #determine = len(initBoard) - 1
    #         #if k == indexOfLetter:
    #             #print letterList
    #         if(len(letterList) > 0):
    #             randomChoiceLL = random.choice(letterList)
    #             #initBoard.count(randomChoiceLL)
    #             print initBoard.count(randomChoiceLL)
    #             if(initBoard.count(randomChoiceLL) < 2):
    #                 initBoard.append(randomChoiceLL)
    #                 #letterList.remove(randomChoiceLL)
    #             else:
    #                 while randomChoiceLL in letterList : letterList.remove(randomChoiceLL)
    #             #elif(initBoard.count(randomChoiceLL) > 2):
    #                 #initBoard.append(randomChoiceLL)
                
                    
    #         else:
    #             initBoard.append('0')
       
    # print(''.join(initBoard))
    # masterList = []

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


    # for m in range(3, 5): #8,13
    #     product = 2*m #5
    #     for g in range(0, product):
    #         n = randint(m-3,m)
    #         letterList = []
    #         for letters in range(0, n):
    #             randomL = getRandomLetter()
    #             if(randomL in letterList):
    #                 letterList.append(randomL)
    #             else:
    #                 randomL2 = getRandomLetter()
    #                 letterList.append(randomL2)

    #         #letterListPair = copy.deepcopy(letterList)
    #         initBoard = []

    #         for t in range(0,m): #row
    #             indexOfLetter = randint(0,m)
    #             #limit = False
    #             for k in range(0,m): #column
    #                 #determine = len(initBoard) - 1
    #                 if k == indexOfLetter:
    #                     #print letterList
    #                     if(len(letterList) > 0):
    #                         randomChoiceLL = random.choice(letterList)
    #                         #initBoard.count(randomChoiceLL)
    #                         if(initBoard.count(randomChoiceLL) < 2):
    #                             initBoard.append(randomChoiceLL)
    #                             #letterList.remove(randomChoiceLL)
                                

    #                         elif(initBoard.count(randomChoiceLL) == 2):
    #                             initBoard.append(randomChoiceLL)
    #                             letterList.remove(randomChoiceLL)
                                
    #                     else:
    #                         initBoard.append('0')
    #                 else:
    #                     initBoard.append('0')
    #         converted = ''.join(initBoard)
    #         sampleInput = (m, converted)
    #         masterList.append(sampleInput)

    # print masterList

if __name__ == "__main__":
    main()
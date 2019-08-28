#Program adapted from the 'SGA' program listed
#in "Genetic Algorithms in search, Optimization and Machine Learning"
#by David E. Goldberg
#Originally writted in Pascal and adapted to Python by Josh Leland
#as a learning experience in Genetic Algorithms
#31/08/2016

from collections import namedtuple
from random import randint
from random import uniform

maxPop = 50
maxStringSize = 20
maxGen = 100
crossProb = 0.6
mutationProb = 0.015

genNum = 0
crossoverNum = 0
mutationNum = 0
sumFitness = 0

Individual = namedtuple("Individual", "chrom x fitness parent1 parent2")
oldPop = []
newPop = []

def CalcFitness(x):
    #Our basic fitness function is finding the largest value
    return pow(x, 2)

def CalcSumFitness(pop):
    global sumFitness
    sumFitness = 0
    for i in pop:
        sumFitness += i.fitness

def Decode(chrom):
    return int(chrom, 2)

def CoinFlip(prob):
    rand = uniform(0, 1)
    if (rand <= prob):
        return True
    else:
        return False

def Initialise():
    global maxPop, maxStringSize, maxGen, crossProb, mutationProb
    #Get values from user
    try:
        maxPop = int(input("Population Size: "))
    except:
        print("Invalid Number Entered, Using default of ", str(maxPop))
    try:
        maxStringSize = int(input("String Size: "))
    except:
        print("Invalid Number Entered, Using default of ", str(maxStringSize))
    try:
        maxGen = int(input("Generation Size: "))
    except:
        print("Invalid Number Entered, Using default of ", str(maxGen))
    try:
        crossProb = float(input("Crossover Probability (Between 0 and 1): "))
    except:
        print("Invalid Number Entered, Using default of ", str(crossProb))
    try:
        mutationProb = float(input("Mutation Probability (Between 0 and 1): "))
    except:
        print("Invalid Number Entered, Using default of ", str(mutationProb))
    print("Initial Generation")
    for i in range(0, maxPop):
        chrom = ""
        for j in range(0, maxStringSize):
            chrom+=str(randint(0, 1))
        x = Decode(chrom)
        fitness = CalcFitness(x)
        ind = Individual(chrom, x, fitness, 0, 0)
        newPop.append(ind)
        print("\t", ind.chrom)
        

def SelectIndividual():
    #Select a single individual via roulette wheel selection
    #sum of all fitness
    sumFitness = 0
    for ind in oldPop:
        sumFitness += ind.fitness

    #Get a random number between 0 and our fitness sum
    rand = randint(0, sumFitness)

    partsum = 0
    for ind in oldPop:
        partsum += ind.fitness
        if (partsum >= rand):
            return ind

    print("Error, no individual found")

def Mutation(allele):
    global mutationNum
    
    if (CoinFlip(mutationProb)):
        #Mutation has occured
        mutationNum += 1
        if (allele == "0"):
            return str(1)
        else:
            return str(0)
    else:
        #Mutation hasn't occured
        return allele

def Crossover(mate1, mate2):
    global crossoverNum
    child1Chrom = ""
    child2Chrom = ""
    
    if (CoinFlip(crossProb)):
        crossoverNum += 1
        crossPoint = randint(0, len(mate1.chrom) - 1)
    else:
        crossPoint = len(mate1.chrom) - 1
    #Add the first halves of each parent to the children
    for i in range(0, crossPoint):
        child1Chrom += str(Mutation(mate1.chrom[i]))
        child2Chrom += str(Mutation(mate2.chrom[i]))
    if (crossPoint != len(mate1.chrom) - 1):
        #Add the second halves of each parent to the children
        for j in range(crossPoint, len(mate1.chrom)):
            child1Chrom += str(Mutation(mate2.chrom[j]))
            child2Chrom += str(Mutation(mate1.chrom[j]))
    else:
        child1Chrom = mate1.chrom
        child2Chrom = mate2.chrom
    #print("Parents: {} {}, Children: {}, {}".format(mate1.chrom, mate2.chrom, child1Chrom, child2Chrom))
    return child1Chrom, child2Chrom
    
def NextGeneration():
    #Creates the next generation of the population using selection, crossover & mutation
    #Assumes an even number popsize
    i = 1
    while (i <= maxPop):
        mate1 = SelectIndividual()
        mate2 = SelectIndividual()
        #while (mate2 == mate1):
        #    #Make sure it doesn't mate with itself
        #    mate2 = SelectIndividual()

        #Crossover and Mutation
        #Mutation is handled inside Crossover
        child1Chrom, child2Chrom = Crossover(mate1, mate2)

        #Decode the children and add to the new population
        #Build child 1
        c1x = Decode(child1Chrom)
        c1Fitness = CalcFitness(c1x)
        child1 = Individual(child1Chrom, c1x, c1Fitness, mate1, mate2)
        #Build child 2
        c2x = Decode(child2Chrom)
        c2Fitness = CalcFitness(c2x)
        child2 = Individual(child2Chrom, c2x, c2Fitness, mate1, mate2)

        #Add to the new Population
        newPop.append(child1)
        newPop.append(child2)

        #Increment the pop counter
        i += 2

def Statistics():
    CalcSumFitness(newPop)
    print("Generation Number: ", genNum)
    print("\tFitness Sum: ", str(sumFitness))
    print("\tCrossovers: ", str(crossoverNum))
    print("\tMutations: ", str(mutationNum))
    
    #Get the now largest value
    largestInd = newPop[0]
    for ind in newPop:
        if (ind.fitness > largestInd.fitness):
            largestInd = ind

    print("\tLargest Individual Chromosome: ", largestInd.chrom)
    print("\tFitness Value: ", largestInd.fitness)
    

def SGA():
    global oldPop
    global newPop
    global genNum
    global mutationNum
    global crossoverNum
    
    genNum = 0
    Initialise()
    while (genNum < maxGen):
        Statistics()
        mutationNum = 0
        crossoverNum = 0
        
        #Increment the generation number
        genNum += 1
        #Swap over the populations, signalling the new generation
        oldPop = newPop
        #Clear the newPop ready to receive the new gen
        newPop = []

        #Generate the new generation
        NextGeneration()

    print("Generation Complete")
    Statistics()
    input("")

SGA()
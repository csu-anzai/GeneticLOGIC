"""
    genetic
    =======
    A library implementing objective function optimization using a simple
    genetic algorithm.
    Author: George Lifchits
    Date: December 7, 2015
"""

import random



import matplotlib
from matplotlib import cm
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D

matplotlib.rcParams['figure.figsize'] = (18.0, 8.0)




MIN = 'MINIMIZE'
MAX = 'MAXIMIZE'


def coin(probability):
    """
    :param probability: {float} 0 ≤ probability ≤ 1
    :returns: {bool} True with probability `probability`
    """
    return random.random() > probability


class Genetic:

    MIN = MIN
    MAX = MAX

    def __init__(self, obj_fun, min_or_max, p_crossover, p_mutation,
            pop_size=200, member_length=10):
        self.objfun = obj_fun
        self.minmax = min_or_max
        self.p_crossover = p_crossover
        self.p_mutation = p_mutation
        self.population = self._generate_sample_population(pop_size,
                member_length)
        self.best_individual = None

    def _random_string(self, length):
        # generates random sequence of booleans (coin toss distribution)
        seq = (coin(0.5) for i in range(length))
        # converts boolean to int (False = 0, True = 1) and joins in a string
        return ''.join(str(int(x)) for x in seq)

    def _generate_sample_population(self, size, member_length):
        """
        :param size: {int} size of the population to generate
        :param member_length: {int} length of each member (members are strings)
        :returns: {List[str]}
        """
        return [self._random_string(member_length) for _ in range(size)]

    def reproduction(self):
        """
        Performs reproduction on the current population
        :returns: {List[str]} new population
        """
        def compute_weight(member):
            """
            :returns: {number} unnormalized roulette weight of the given member
            """
            fitness = self.objfun(member)
            if self.minmax == MIN:
                return 1/(1 + fitness)
            elif self.minmax == MAX:
                return fitness

        # compute weights for each member
        members_weights = [(m, compute_weight(m)) for m in self.population]
        # find the sum of all weights
        sum_weights = sum(w for m, w in members_weights)
        # probability density of each member is its normalized weight
        pdf = [(m, w/sum_weights) for m, w in members_weights]

        # generate new population
        new_population = []

        for i in range(len(self.population)):
            # each iteration selects one member
            rand = random.random()
            cumul = 0
            for member, end_interval in pdf:
                cumul += end_interval
                if rand <= cumul:
                    new_population.append(member)
                    break

        return new_population

    def _individual_crossover(self, parent1, parent2):
        """
        :param parent1: {string}
        :param parent2: {string}
        :returns: {(str, str)}
        """
        index = random.randint(1, len(parent1) - 1)
        head1, tail1 = parent1[:index], parent1[index:]
        head2, tail2 = parent2[:index], parent2[index:]
        return head1+tail2, head2+tail1

    def crossover(self):
        """
        Performs crossover on the current population.
        """
        new_population = []
        while len(self.population) > 2:
            if coin(self.p_crossover): # do crossover
                # pop two parents
                p1 = self.population.pop()
                p2 = self.population.pop()
                # crossover
                m1, m2 = self._individual_crossover(p1, p2)
                # add children to new population
                new_population.append(m1)
                new_population.append(m2)
            else:
                # skip this member and go to the next
                new_population.append(self.population.pop())
        # empty the current population
        while len(self.population) > 0:
            new_population.append(self.population.pop())
        # set population to new_population
        self.population = new_population

    def _individual_mutation(self, member):
        # function which returns the opposite of the given bit
        flipped = lambda x: '1' if x is '0' else '0'
        # list of chars, flipped with probability `self.p_mutation`
        chars = (flipped(c) if coin(self.p_mutation) else c for c in member)
        # return list of chars as a single string
        return ''.join(chars)

    def mutation(self):
        """
        Mutates members in the current population.
        """
        new_pop = [self._individual_mutation(m) for m in self.population]
        self.population = new_pop

    def evolve(self):
        """
        Performs iteration of SGA and maintains the best individual found.
        """
        # SGA steps
        self.reproduction()
        self.crossover()
        self.mutation()
        # now, maintain best individual
        # get the individuals in the population
        individuals = (m for m in self.population)
        # `opt` is a Python built-in function which finds optimum in a sequence
        opt = min if self.minmax == MIN else max
        # `optimum` is a function that calls `opt` with criteria objective func
        optimum = lambda x: opt(x, key=self.objfun)
        best_in_population = optimum(individuals)
        # initialize best_inidividual if necessary
        if self.best_individual is None:
            self.best_individual = best_in_population
        # find and save the overall best individual
        best = optimum([self.best_individual, best_in_population])
        self.best_individual = best

def coin(prob):
    """
    Performs a biased coin toss.
    :param prob: [0 ≤ float ≤ 1]
    :returns: [bool] True with probability `prob` and otherwise False
    """
    # random.random() yields a float between 0 and 1
    return random.random() < prob


def random_string(length):
    """
    :param length: [int] length of random string
    :returns: [string] random string consisting of "0" and "1"
    """
    return ''.join('0' if coin(0.5) else '1' for _ in range(length))
    

def generate_random_population(number, length):
    """
    This function is used to generate the first population.
    This implementation ensure that chromosomes in the initial population
    are uniformly pseudo-random!
    
    :param number: [int] number of strings to return
    :param length: [int] length of the strings to return
    :returns: List[str] list of random binary strings
    """
    return [random_string(length) for _ in range(number)]
    
    
MIN = 0
MAX = 1


def reproduction(population, fitness_func, min_or_max=MAX):
    """
    Produces a new population from biased roulette reproduction of the 
    given population.
    :param population: [List[str]]
    :param fitness_func: [function: number > 0]
    :param min_or_max: {MIN, MAX}
    :returns: [List[str]]
    """
    # First, we define the probability density (roulette weights) for each
    # member in our given population. 
    
    min_fitness = min(fitness_func(m) for m in population)
    
    def compute_weight(m):
        """
        Subroutine which computes the weight of the biased roulette, which 
        is agnostic of the fitness function. In particular, it will invert
        the fitness value if we are seeking a minimum. Member `m` has weight
        that is commensurate with its distance from the member with lowest
        fitness in the population.
        :param m: [str] member
        """
        fitness = fitness_func(m)
        
        if min_or_max == MAX:
            return fitness - min_fitness + 1
        
        elif min_or_max == MIN:
            return 1 / (fitness - min_fitness + 1)
    
    # Here we normalize the weights to be proportions of the total weighting
    weights = [(m, compute_weight(m)) for m in population]
    total_weights = sum(w for m, w in weights)
    pdf = [(m, w/total_weights) for m, w in weights]
    
    # Now we pick members for the new population.
    # We pick the same number of members as the provided population.
    new_population = []
    for i in range(len(population)):
        rand = random.random()
        cumul = 0
        for member, end_interval in pdf:
            cumul += end_interval
            if rand <= cumul:
                new_population.append(member)
                break # generate next member
    
    return new_population

def crossover(string1, string2, index):
    """ Performs crossover on two strings at given index """
    head1, tail1 = string1[:index], string1[index:]
    head2, tail2 = string2[:index], string2[index:]
    return head1+tail2, head2+tail1


def population_crossover(population, crossover_probability):
    """
    Performs crossover on an entire population.
    :param population: List[str]
    :param crossover_probability: [0 ≤ float ≤ 1] 
        chance that any pair will be crossed over
    :returns: List[str] 
        new population with possibly some members crossed over
    """
    pairs = []
    new_population = []
    while len(population) > 1:
        pairs.append((population.pop(), population.pop()))
    if len(population) == 1:
        new_population.append(population.pop())
        
    for s1, s2 in pairs:
        if not coin(crossover_probability): 
            # don't perform crossover, just add the original pair
            new_population += [s1, s2]
            continue
        idx = random.randint(1, len(s1)-1) # select crossover index
        new_s1, new_s2 = crossover(s1, s2, idx)
        new_population.append(new_s1)
        new_population.append(new_s2)
    return new_population

def mutation(string, probability):
    """
    :param string: the binary string to mutate
    :param probability: [0 ≤ float ≤ 1] 
        the probability of any character being flipped
    :returns: [str] 
        just the input string, possibly with some bits flipped
    """
    flipped = lambda x: '1' if x is '0' else '0'
    chars = (flipped(char) if coin(probability) else char for char in string)
    return ''.join(chars)

def mutate_population(population, prob):
    """
    :param population: [List[str]] 
        population of binary strings
    :returns: [List[str]] 
        just the input population with some members possibly mutated
    """
    return [mutation(m, prob) for m in population]
    
def run_genetic_algorithm(obj_fun, decoder, 
                          min_or_max=MAX, num_eras=100, 
                          population_size=20, chromosome_length=12, 
                          crossover_probability=0.4,mutation_probability=0.005):
    
    # define fitness function (decode string, then feed to the OF)
    fitness = lambda coding: obj_fun(*decoder(coding))
    
    # initialize population
    population = generate_random_population(number=population_size, 
                                            length=chromosome_length)
    # data collection
    populations = [population] # initialize with first population
    
    # SGA loop
    for i in range(num_eras):
        population = reproduction(population, fitness, min_or_max)
        population = population_crossover(population, crossover_probability)
        population = mutate_population(population, mutation_probability)
        populations.append(population) # data collection
    
    return populations

def dejong_OF(*x):
    return sum(xi**2 for xi in x)

def rosenbrock_OF(*x):
    irange = range(len(x)-1)
    return sum(100 * (x[i+1] - x[i]**2)**2 + (1-x[i])**2 for i in irange)    

def himmelblau_OF(x, y):
    return (x**2 + y - 11)**2 + (x + y**2 - 7)**2

def esf(*a):
    nov = len(a) # number of variables = length of input vector
    terms = (a[i]*a[j] for i in range(nov) for j in range(i+1, nov))
    return abs(sum(terms))
    
def plot_ga(obj_fun, decoder, ax=None, ga_opts=None, min_or_max=MIN, 
            title="Genetic Algorithm Evolution", legend=True):
    if ga_opts is None:
        ga_opts = {}
        
    ga_opts['min_or_max'] = min_or_max
    # run SGA
    populations = run_genetic_algorithm(obj_fun, decoder, **ga_opts.copy())
    
    # define fitness func
    fitness = lambda c: obj_fun(*decoder(c))
    
    # Find the "global optimum" of all the chromosomes we looked at.
    # A better term for this chromosome is "best individual".
    all_chromosomes = {c for pop in populations for c in pop}
    optimizer = min if min_or_max == MIN else max
    global_optimum = optimizer(all_chromosomes, key=fitness)
    fittest_fitness = fitness(global_optimum)
    
    # Print the optimum to the console
    print("Global optimum:", global_optimum)
    print("Fitness:", fittest_fitness)
    print("Decoded:", decoder(global_optimum))
    
    # Start plotting
    # Define the data ranges
    x_axis = range(len(populations))
    fitnesses = [[fitness(m) for m in population] for population in populations]
    mins = [min(f) for f in fitnesses]
    maxs = [max(f) for f in fitnesses]
    avgs = [sum(f)/len(f) for f in fitnesses]
    optima = [(it, fittest_fitness) for it, pop in enumerate(populations) 
              if fittest_fitness in map(fitness, pop)]
    x_optima, y_optima = zip(*optima) # unzip pairs into two sequences
    
    if ax is None: # if no plotting axes are provided
        # define a set of axes
        fig, ax = plt.subplots(1)
    
    # do the plotting
    l_mins, l_maxs, l_avgs = ax.plot(x_axis, mins, 'r--', maxs, 'b--', avgs, 'g-')
    scatter_ceil = ax.scatter(x_optima, y_optima, c='purple')
    
    # create a legend
    if legend:
        plt.legend(
            (l_mins, l_maxs, l_avgs, scatter_ceil),
            ("min pop fitness", "max pop fitness", "average pop fitness", 
             "occurrences of global optimum"), 
            loc="upper right",
        )
    
    # set parameters for the axes
    ax.set_xlim(0, len(populations))
    ax.set_ylim(0, int(max(maxs) * 1.20))
    ax.set_title(title)
    ax.set_xlabel("era")
    ax.set_ylabel("fitness")
    
    return ax

def plot_ga_minmax(objfun, decoder, min_ga_opts=None, max_ga_opts=None, 
                   title="Genetic Algorithm"):
    fig, (ax1, ax2) = plt.subplots(1, 2)
    for ax, minimax, opts in [(ax1, MIN, min_ga_opts), (ax2, MAX, max_ga_opts)]:
        opts = opts or {} # if none, then use empty dict of options
        minimax_title = "minimization" if minimax == MIN else "maximization"
        print('\n', minimax_title, '\n', '='*len(minimax_title), '\n')
        plot_ga(
            objfun, decoder,
            ax=ax, min_or_max=minimax, 
            ga_opts=opts,
            title="{} ({})".format(title, minimax_title),
            legend=False # no space on the min/max graphs
        )


def plot_esf_minmax(nov, min_ga_opts=None, max_ga_opts=None):
    # initialize option dicts as new dictionaries
    min_opts = {}
    max_opts = {}
    # update with parameter option dicts (if provided)
    min_opts.update(min_ga_opts or {})
    max_opts.update(max_ga_opts or {})
    # update with mandatory chromosome length = NOV
    min_opts['chromosome_length'] = nov
    max_opts['chromosome_length'] = nov
    # plot!
    plot_ga_minmax(esf, esf_decoder, title="ESF ({} vars)".format(nov), 
                   min_ga_opts=min_opts.copy(), max_ga_opts=max_opts.copy()
    )

    
def split_string_into_chunks(string, n):
    """
    Helper function.
    :param string: [str]
    :param n: [int > 0] chunk size
    :returns: List[str] the entire string split into sequential chunks of the 
        given size (plus the remainder)
    
    example:
    
    >>> split_string_into_chunks('12345678', 3)
    ['123', '456', '78']
    
    >>> split_string_into_chunks('12345678', 4)
    ['1234', '5678']
    """
    return [string[i:i+n] for i in range(0, len(string), n)]

def dejong_decoder(coding):
    n = 4
    bits_list = split_string_into_chunks(coding, n)
    # take first bit as the sign, and the remaining bits as integers
    signs_nums = [(-1 if bits[0] == '0' else 1, int(bits[1:], 2)) 
                  for bits in bits_list]
    # use modulo to ensure that the numbers fall within the require interval:
    #   -5.12 ≤ x ≤ 5.12
    xlist = [sign * (num % 5.12) for sign, num in signs_nums]
    return xlist

def rosenbrock_decoder(coding):
    n = 3
    bits_list = split_string_into_chunks(coding, n)
    # take first bit as the sign, and the remaining bits as integers
    signs_nums = [(-1 if bits[0] == '0' else 1, int(bits[1:], 2)) 
                  for bits in bits_list]
    # use modulo to ensure that the numbers fall within the require interval:
    #   -2.048 ≤ x ≤ 2.048
    x = [sign * (num % 2.048) for sign, num in signs_nums]
    return x

def num_in_interval(lo, hi, mult, steps):
    """
    Helper function that takes simple parameters to deterministically
    yield a floating-point number in a given interval.
    
    ex. mult = 6, steps = 10
    
     |---+---+---+---+---+---|---+---+---+---|
    lo                      mult             hi
    
    if low = -10 and hi = 10, then the result will be 
      = -10 + 6*(20/10) 
      = -10 + 12
      = 2

    :param lo: [number] low bound of interval
    :param hi: [number] high bound of interval
    :param mult: [number ≤ divisor] 
    :param steps: [int] the number of steps in the interval
    :returns: [float] a number between `lo` and `hi`
    """
    step_size = (hi - lo)/steps
    return lo + mult*step_size

def himmelblau_decoder(coding):
    mid = int(len(coding)/2)
    # split string into x param and y param
    binx, biny = coding[:mid], coding[mid:]
    # use binary x and y as interval multiplier
    xmult, ymult = int(binx, 2), int(biny, 2)
    # the divisor is the highest possible value x or y could be
    # which is 2**{length of binary string encoding x or y}
    x = num_in_interval(-4, 4, xmult, 2**len(binx))
    y = num_in_interval(-4, 4, ymult, 2**len(biny))
    return x, y

def esf_decoder(coding):
    return [-1 if char == '0' else 1 for char in coding]
    
if __name__ == "__main__":
    
    decoder = dejong_decoder
    obj_fun = dejong_OF

    ga_options = dict(
    num_eras=100, population_size=40, chromosome_length=20, 
    crossover_probability=0.3, mutation_probability=0.05  
    )

    plot_ga(obj_fun, decoder, min_or_max=MIN, 
        ga_opts=ga_options, title="DeJong Function (minimization)")
/**
 * Copyright (c) 2010
 * Author: Aaron Yuen
 * 
 * Date Created: June 29, 2010
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace JaskSudoku.Algorithms
{
    public class Genetic : IAlgorithm
    {
        private class ConstraintBlock
        {
            private HashSet<int> m_permissible;
            private int m_permissibleNum;

            public ConstraintBlock()
            {
                m_permissible = new HashSet<int>();

                m_permissibleNum = 0;
                for (int i = 1; i <= 9; i++)
                {
                    this.AddPermissible(i);
                }
            }

            public void AddPermissible(int value)
            {
                if (m_permissible.Contains(value))
                {
                    System.Console.WriteLine("AddPermissible: {0} already exists in this ConstraintBlock!", value);
                }
                else
                {
                    m_permissibleNum++;
                    m_permissible.Add(value);
                }   
            }

            public void RemovePermissible(int value)
            {
                if (!(m_permissible.Contains(value)))
                {
                    System.Console.WriteLine("RemovePermissible: {0} does not exist in this ConstraintBlock!", value);
                }
                else
                {
                    m_permissibleNum--;
                    m_permissible.Remove(value);
                }
            }

            public HashSet<int> Permissibles
            {
                get { return m_permissible; }
            }

            public int NumOfPermissibles
            {
                get { return m_permissibleNum; }
            }
        }

        /* A private class to represent a population. A population 
         * is a particular set of solutions. 
         */
        private class Population
        {
            private int m_fitness;      // the fitness of this population

            private int[,] m_each_fitness = new int[Sudoku.DIMENSION, Sudoku.DIMENSION];
            
            private bool[,] m_is_given = new bool[Sudoku.DIMENSION, Sudoku.DIMENSION];
            private int[,] m_puzzle = new int[Sudoku.DIMENSION, Sudoku.DIMENSION];

            private ConstraintBlock[] m_squareConstraint = new ConstraintBlock[9];
                
            private const int SUBGRID_DIMENSION = 3;
            private static Random m_random;

            public Population()
            {
                m_random = new Random();
                Initialize();

                for (int i = 0; i < Sudoku.DIMENSION; i++)
                {
                    for (int j = 0; j < Sudoku.DIMENSION; j++)
                    {
                        m_is_given[i, j] = false;
                    }
                }
                GeneratePopulation();
                CalculateFitness();
            }

            public Population(int[,] data, Random rand)
            {
                m_random = rand;
                Initialize();
               
                // Initialize the puzzle
                for (int i = 0; i < SUBGRID_DIMENSION; i++)
                {
                    for (int j = 0; j < SUBGRID_DIMENSION; j++)
                    {
                        for (int x = 0; x < SUBGRID_DIMENSION; x++)
                        {
                            for (int y = 0; y < SUBGRID_DIMENSION; y++)
                            {
                                int x_index = i * SUBGRID_DIMENSION + x;
                                int y_index = j * SUBGRID_DIMENSION + y;

                                if (data[x_index, y_index] != Sudoku.VALUE_EMPTY)
                                {
                                    m_squareConstraint[i * SUBGRID_DIMENSION + j].RemovePermissible(data[x_index, y_index]);
                                    m_each_fitness[x_index, y_index] = -1;
                                }

                                m_is_given[x_index, y_index] = data[x_index, y_index] != Sudoku.VALUE_EMPTY;
                                m_puzzle[x_index, y_index] = data[x_index, y_index];
                            }
                        }

                    }
                }
                
                GeneratePopulation();
                CalculateFitness();
            }

            private void Initialize()
            {
                // initialize square constraint blocks
                for (int i = 0; i < m_squareConstraint.Length; i++)
                {
                    m_squareConstraint[i] = new ConstraintBlock();
                }
            }

            private void GeneratePopulation()
            {
                for (int i = 0; i < Sudoku.DIMENSION; i++)
                {
                    for (int j = 0; j < Sudoku.DIMENSION; j++)
                    {
                        if (!m_is_given[i, j])
                        {
                            // find the square constraint block it belongs to and the possible values it is allowed to have
                            ConstraintBlock currentBlock = m_squareConstraint[(i / 3) * SUBGRID_DIMENSION + (j / 3)];
                            int[] possibleNums = new int[currentBlock.NumOfPermissibles];
                            currentBlock.Permissibles.CopyTo(possibleNums);

                            // Generate a random index of the possible 
                            int random = ((m_random.Next() % currentBlock.NumOfPermissibles));

                            m_puzzle[i, j] = possibleNums[random];
                            currentBlock.RemovePermissible(possibleNums[random]);
                             
                        }
                    }
                }
            }

            public int CalculateFitness()
            {
                m_fitness = 0;
                // Reinitialize m_each_fitness

                for (int i = 0; i < Sudoku.DIMENSION; i++)
                {
                    for (int j = 0; j < Sudoku.DIMENSION; j++)
                    {
                        if (m_is_given[i, j])
                        {
                            m_each_fitness[i, j] = -1;
                        }
                        else
                        {
                            m_each_fitness[i, j] = 0;
                        }
                    }
                }
                

                // Calculates the repetitions in columns
                for (int i = 0; i < Sudoku.DIMENSION; i++)
                {
                    HashSet<int> used_numbers = new HashSet<int>();
                    for (int j = 0; j < Sudoku.DIMENSION; j++)
                    {
                        if (used_numbers.Contains(m_puzzle[i, j]))
                        {
                            m_fitness++;
                            if (m_each_fitness[i, j] != -1)
                            {
                                m_each_fitness[i, j]++;
                            }
                        }
                        else
                        {
                            used_numbers.Add(m_puzzle[i, j]);
                        }

                    }
                }

                // Calculates the repetitions in columns
                for (int i = 0; i < Sudoku.DIMENSION; i++)
                {
                    HashSet<int> used_numbers = new HashSet<int>();
                    for (int j = 0; j < Sudoku.DIMENSION; j++)
                    {
                        if (used_numbers.Contains(m_puzzle[j, i]))
                        {
                            m_fitness++; 
                            m_each_fitness[i, j]++;
                        }
                        else
                        {
                            used_numbers.Add(m_puzzle[j, i]);
                        }

                    }
                }
                return m_fitness;
            }



            public void mutate(int NumOfMutations)
            {
                // Randomly mutate the number of times specified 
                // by the parameter passed in                
                for (int x = 0; x < NumOfMutations; x++)
                {
                    // Randomly choose a 3x3 grid
                    int grid_row = m_random.Next() % SUBGRID_DIMENSION;
                    int grid_col = m_random.Next() % SUBGRID_DIMENSION;


                    // Take the two spots within the 3x3 grid with the highest fitness and swaps them
                    int highest = -1;
                    int second_highest = -1;

                    int[] highest_index = { -1, -1 };
                    int[] second_highest_index = { -1, -1 };

                    for (int i = 0; i < SUBGRID_DIMENSION; i++)
                    {
                        for (int j = 0; j < SUBGRID_DIMENSION; j++)
                        {
                            if (m_each_fitness[grid_row * SUBGRID_DIMENSION + i, grid_col * SUBGRID_DIMENSION + j] > highest)
                            {
                                second_highest = highest;
                                second_highest_index[0] = highest_index[0];
                                second_highest_index[1] = highest_index[1];

                                highest = m_each_fitness[grid_row * SUBGRID_DIMENSION + i, grid_col * SUBGRID_DIMENSION + j];
                                highest_index[0] = (grid_row * SUBGRID_DIMENSION + i);
                                highest_index[1] = (grid_col * SUBGRID_DIMENSION + j);
                            } 
                            else if (m_each_fitness[grid_row * SUBGRID_DIMENSION + i, grid_col * SUBGRID_DIMENSION + j] > second_highest) 
                            {
                                second_highest = m_each_fitness[grid_row * SUBGRID_DIMENSION + i, grid_col * SUBGRID_DIMENSION + j];
                                second_highest_index[0] = (grid_row * SUBGRID_DIMENSION + i);
                                second_highest_index[1] = (grid_col * SUBGRID_DIMENSION + j);
                            }
                        }
                    }

                    // If a second highest is not found, choose a random spot
                    if (second_highest_index[0] == -1 || second_highest_index[1] == -1)
                    {
                        int spot_row2 = (new Random()).Next() % SUBGRID_DIMENSION;
                        int spot_col2 = (new Random()).Next() % SUBGRID_DIMENSION;

                        while (m_is_given[grid_row * SUBGRID_DIMENSION + spot_row2, grid_col * SUBGRID_DIMENSION + spot_col2])
                        {
                            spot_row2 = (new Random()).Next() % SUBGRID_DIMENSION;
                            spot_col2 = (new Random()).Next() % SUBGRID_DIMENSION;
                        }

                        second_highest_index[0] = grid_row * SUBGRID_DIMENSION + spot_row2;
                        second_highest_index[1] = grid_col * SUBGRID_DIMENSION + spot_col2;
                    }

                    // Swaps the two spots                    
                    int temp = m_puzzle[highest_index[0], highest_index[1]];
                    m_puzzle[highest_index[0], highest_index[1]] = m_puzzle[second_highest_index[0], second_highest_index[1]];
                    m_puzzle[second_highest_index[0], second_highest_index[1]] = temp;

                    // updates the fitness
                    CalculateFitness();
                }
            }

            public void printPuzzle()
            {
                for(int i =0; i < Sudoku.DIMENSION; i++ )
                {
                    for(int j=0; j < Sudoku.DIMENSION; j++)
                    {
                        System.Console.Write(" " + m_puzzle[i, j]);
                    }
                    System.Console.WriteLine();
                }

                System.Console.WriteLine();
            }

            public void printFitness()
            {
                for (int i = 0; i < Sudoku.DIMENSION; i++)
                {
                    for (int j = 0; j < Sudoku.DIMENSION; j++)
                    {
                        System.Console.Write(" " + m_each_fitness[i, j]);
                    }
                    System.Console.WriteLine();
                }

                System.Console.WriteLine();
            }

            public int[,] Puzzle
            {
                get { return m_puzzle; }
            }

            public int Fitness
            {
                get { return m_fitness; }
            }
        };

        public Genetic()
        {

        }

        public override string ToString() { return "Genetic Algorithm"; }

        public override bool Solve(int[,] data)
        {
            Random rand = new Random();
            int population_size = 10;
            int fittest_population = population_size + 1;
            int lowest_fitness = int.MaxValue;

            int[,] best_puzzle = new int[Sudoku.DIMENSION, Sudoku.DIMENSION];

            Population[] p = new Population[population_size];
            for (int i = 0; i < p.Length; i++)
            {
                p[i] = new Population(data, rand);
  
                if (p[i].Fitness < lowest_fitness)
                {
                    fittest_population = i;
                    lowest_fitness = p[i].Fitness;
                    for (int a = 0; a < Sudoku.DIMENSION; a++)
                    {
                        for (int b = 0; b < Sudoku.DIMENSION; b++)
                        {
                            best_puzzle[a, b] = p[i].Puzzle[a, b];
                        }
                    }
                }
            }
            
            // Iterate through 200 generations to find the best mutation                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
            int cur_lowest = lowest_fitness;
            for (int x = 0; x < 200; x++)
            {
                for (int i = 0; i < p.Length; i++)
                {
                    p[i].mutate(lowest_fitness / 2);
                    if (p[i].Fitness < cur_lowest)
                    {
                        fittest_population = i;
                        cur_lowest = p[i].Fitness;
                        for (int a = 0; a < Sudoku.DIMENSION; a++)
                        {
                            for (int b = 0; b < Sudoku.DIMENSION; b++)
                            {
                                best_puzzle[a, b] = p[i].Puzzle[a, b];
                            }
                        }
                    }
                }
                lowest_fitness = cur_lowest;
            }

            Console.WriteLine("Fitness: " + lowest_fitness);
            for (int i = 0; i < Sudoku.DIMENSION; i++)
            {
                for (int j = 0; j < Sudoku.DIMENSION; j++)
                {
                    Console.Write(" " + best_puzzle[i, j]);
                }
                Console.WriteLine();
            }

            // send the best mutation (best_puzzle) to Simulated Annealing to find solution

       

            return true;
        }

        private void printData(int[,] data)
        {
            for(int i =0; i < Sudoku.DIMENSION; i++ )
            {
                for(int j=0; j < Sudoku.DIMENSION; j++)
                {
                    System.Console.Write(" " + data[i, j]);
                }
                System.Console.WriteLine();
            }

            System.Console.WriteLine();
        }
    }
}
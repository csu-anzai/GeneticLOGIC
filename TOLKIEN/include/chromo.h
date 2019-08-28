//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __CHROMO_H )
#define        __CHROMO_H

#include        "toarray.h"

//
//
//      A Chromosome instance is an array of TObject instances.
//      This design enables derived classes of class Chromosome
//      to reuse many functions, such as the crossover operator,
//      that are independent of the actual gene type.
//
//

class   Chromosome : public TOArray
{
public:

                                DECLARE_RTTI()

                                Chromosome(OwnerType dt = reference,
                                           const sizeType size =
                                                 DEFAULT_COLLECTION_SIZE);
                                Chromosome(RCChromosome,
                                           OwnerType dt = reference);
                                ~Chromosome();

        virtual void            deepenShallowCopy();
                                _shallowCopy(Chromosome)

        virtual sizeType        length() const;

        virtual void            oddPtCrossover(RCChromosome, RCChromosome, int);
        virtual void            evenPtCrossover(RCChromosome, RCChromosome, int);
        virtual void            uniformCrossover(RCChromosome,
                                                 RCChromosome, float);

        virtual void            printOn( ostream  & out) const;
};

inline  Chromosome::~Chromosome()
{
}

inline  void Chromosome::deepenShallowCopy()
{
        TOArray::deepenShallowCopy();
}

inline  sizeType Chromosome::length() const
{
        return size();
}

#endif


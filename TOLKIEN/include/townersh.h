//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __TOWNERSH_H )
#define  __TOWNERSH_H

enum OwnerType { reference, owner };

class TOwnership
{

public:

    TOwnership( OwnerType dt = reference );
    int         ownsElements() const;
    void        ownsElements( int del );
    OwnerType   getOwnerType() const;

protected:

    int delObj() const;

private:

    OwnerType ownership;


};

inline  TOwnership::TOwnership( OwnerType dt )
{
	ownsElements( dt );
}

inline  int TOwnership::ownsElements() const
{
	return ownership == owner;
}

inline	void TOwnership::ownsElements( int del )
{
	ownership = (del == reference) ? reference : owner;
}

inline	OwnerType TOwnership::getOwnerType() const
{
        return ownership;
}

inline	int     TOwnership::delObj() const
{
        return ownership;
}

#endif


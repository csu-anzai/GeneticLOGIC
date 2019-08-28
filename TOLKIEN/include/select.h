//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined(__SELECT_H)
#define __SELECT_H

#include "populatn.h"

class  SelectionScheme : public TObject
{
public:
				DECLARE_RTTI()

	virtual                 ~SelectionScheme();
        virtual BOOL            isEqual( RCTObject ) const;
        virtual int             compare( RCTObject ) const;
        virtual void            printOn( ostream  & out) const = 0;

        virtual PTObject        shallowCopy() const = 0;
        virtual void            deepenShallowCopy();

	virtual PCTGAObj        select() = 0;
	virtual void            reset(RCCollection src);

protected:
	PCCollection pPop;
				SelectionScheme();

				SelectionScheme(RCCollection src);
        virtual PCTGAObj        member(sizeType index) const;
};

inline  SelectionScheme::SelectionScheme() : pPop(NULL)
{
}

inline  SelectionScheme::~SelectionScheme()
{
}

inline  SelectionScheme::SelectionScheme(RCCollection src)
{
	reset(src);
}

inline void SelectionScheme::reset(RCCollection src)
{
	if (src.size() == 0)
	    (*lib_error_handler)("SelectionScheme", "empty array");
	pPop = &src;
}

inline PCTGAObj SelectionScheme::member(sizeType index) const
{
        return (PCTGAObj) pPop->elem(index);
}

inline BOOL SelectionScheme::isEqual( RCTObject obj ) const
{
        return compare(obj) == 0;
}

inline void  SelectionScheme::deepenShallowCopy()
{
}

#endif


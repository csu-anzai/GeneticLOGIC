/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/
// Generic doubly linked list
// Use the generic class gdlink(TYPE) by:
//
// declare(gdlink,TYPE)
// declare(gdlist,TYPE)
// implement(gdlink,TYPE)
// implement(gdlist,TYPE)
//
// gdlink(TYPE) X;
// gdlist(TYPE) Y(X);

#ifndef DLINK_H
#define DLINK_H
#include <generic.h>

#define gdlink(TYPE) name2(gdlink,TYPE)
#define gdlist(TYPE) name2(gdlist,TYPE)

#define gdlinkdeclare(TYPE)					\
	/* links for the doubly-linked list */			\
	class gdlink(TYPE)					\
	{							\
	public:	/* for debugging only */			\
	   friend class gdlist(TYPE);				\
           friend class fxsortedlist;				\
	   friend void interact();				\
	   gdlink(TYPE) *next;					\
	   gdlink(TYPE) *prev;	/*lsy*/				\
	   TYPE e;						\
								\
	   gdlink(TYPE) (TYPE a,				\
	      gdlink(TYPE) *n = 0,				\
	      gdlink(TYPE) *p = 0)				\
	   { e = a; next = n; prev = p; }			\
								\
	   void insert(gdlink(TYPE) *n);			\
	   void append(gdlink(TYPE) *n);			\
	   void remove();					\
	};

#define gdlistdeclare(TYPE)					\
	/* manage a doubly-linked list */			\
	class gdlist(TYPE)					\
	{							\
	public: /* for debugging only */			\
	   gdlink(TYPE) *last;					\
	   gdlink(TYPE) *curr;					\
	   gdlink(TYPE) *marc;					\
           long kount;						\
								\
/*	public:	*/						\
	   gdlist(TYPE)() { last = curr = 0; kount = 0; } 	\
	   gdlist(TYPE)(TYPE e)					\
	   {							\
	      curr = 0; kount = 1;				\
	      last = new gdlink(TYPE)(e);  /*lsy*/		\
	      last->next = last->prev = last;			\
	   }							\
	   void checklast(const char* s)			\
	   {							\
	      if (last && !(last->next))			\
		 printf("Got one: %s\n",s);			\
	   }							\
	   int isempty() { return last == 0; }  /*lsy*/		\
	   void clear();					\
	   ~gdlist(TYPE)() { clear(); };			\
								\
	   /* set current pointer to 0 */			\
	   void reset() { curr = 0; }  /*lsy*/			\
								\
	   /* mark current pointer position */	/*lsy*/		\
	   void mark() { marc = curr; }  /*lsy*/		\
								\
	   /* mark prev pointer position */	/*lsy*/		\
	   void markprev() /*lsy*/				\
	   {							\
	      if (curr)						\
              {							\
                 marc = curr->prev;				\
                 if (marc == last)				\
                    marc = 0;					\
              }							\
              else						\
                  marc = 0; /* or should it be = last ? */	\
	   }							\
								\
	   /* mark last pointer position */	/*lsy*/		\
	   void marklast() { marc = last; }  /*lsy*/		\
								\
	   /* move current pointer to mark */	/*lsy*/		\
	   void tomark() { curr = marc; }			\
								\
	   /* move current pointer to mark */	/*lsy*/		\
	   void tomark(TYPE &a) { curr = marc; getmark(a); }	\
								\
	   /* return object at current mark */  /*lsy*/		\
	   void getmark(TYPE &a)				\
	   {							\
	      if (marc)						\
	         a = marc->e;					\
	      else						\
	         a = NULL;					\
	   }							\
								\
	   /* YOU'D BETTER KNOW WHAT YOU'RE DOING IF YOU USE */	\
	   /* getcurr() AND setcurr(), AND YOU'D BETTER BE */	\
	   /* CERTAIN THAT THE LIST DOESN'T CHANGE BETWEEN */	\
	   /* A getcurr() AND A SUBSEQUENT setcurr() */		\
	   /* get current pointer */	/*lsy*/			\
	   gdlink(TYPE)* getcurr() { return curr; }		\
								\
	   /* set current pointer */	/*lsy*/			\
	   void setcurr(gdlink(TYPE) *c) { curr = c; }		\
								\
	   /* move around list, leaving links */		\
	   int next(TYPE &e);					\
	   int prev(TYPE &e);					\
	   int current(TYPE &e);				\
								\
	   /* move around list, removing links */		\
	   int getnext(TYPE &e); /*lsy*/			\
	   int getprev(TYPE &e); /*lsy*/			\
								\
	   /* add object/entity to beginning or end of list */	\
	   void insert(const TYPE e); /*lsy*/			\
	   void append(const TYPE e); /*lsy*/			\
	   void add(const TYPE e) { append(e); }  /* synonym */	\
								\
	   /* add link to beginning or end of list */		\
	   void insert(gdlink(TYPE)* e); /*lsy*/		\
	   void append(gdlink(TYPE)* e); /*lsy*/		\
	   void add(gdlink(TYPE)* e) { append(e); } /* synonym */\
								\
	   /* add object/entity before or after current entry */\
	   void inserthere(const TYPE e); /*lsy*/		\
	   void appendhere(const TYPE e); /*lsy*/		\
								\
	   /* add link before or after current entry */		\
	   void inserthere(gdlink(TYPE)* e); /*lsy*/		\
	   void appendhere(gdlink(TYPE)* e); /*lsy*/		\
								\
	   /* remove the current entry */			\
	   void remove();					\
								\
	   /* unlink & return the current entry */		\
	   gdlink(TYPE)* unlink();				\
								\
	   /* remove the entry corresponding to a */		\
	   void remove(const TYPE a);				\
								\
	   /* unlink & return the entry corresponding to a */	\
	   gdlink(TYPE)* unlink(const TYPE a);			\
								\
	   /* unlink & return the entry a */			\
	   gdlink(TYPE)* unlink(gdlink(TYPE)* a);		\
								\
	   /* return the number of elements in the list */	\
           long count() { return kount; }			\
	};

#define gdlinkimplement(TYPE)					\
	/* insert a link in front of this one */		\
	void gdlink(TYPE)::insert(gdlink(TYPE) *newlink)	\
	{							\
	   if (prev)						\
	      prev->next = newlink;				\
	   newlink->next = this;				\
	   newlink->prev = prev;				\
	   prev = newlink;					\
	}							\
								\
	/* append a link after this one */			\
	void gdlink(TYPE)::append(gdlink(TYPE) *newlink)	\
	{							\
	   if (next)						\
	      next->prev = newlink;				\
	   newlink->next = next;				\
	   newlink->prev = this;				\
	   next = newlink;					\
	}							\
								\
	/* remove this link from the list */			\
	void gdlink(TYPE)::remove()				\
	{							\
	   if (prev)						\
	      prev->next = next;				\
	   if (next)						\
	      next->prev = prev;				\
	   next = prev = 0;					\
	}

#define gdlistimplement(TYPE)					\
	/* clear the list */					\
	void gdlist(TYPE)::clear()				\
	{							\
	   gdlink(TYPE) *l = last;				\
	   if (!l) return;					\
	   							\
	   do							\
	   {							\
	      gdlink(TYPE) *ll = l;				\
	      l = l->next;					\
	      delete ll;					\
	   } while (l != last);					\
								\
           kount = 0;						\
	   last = 0;						\
	}							\
								\
	/* get the next entry from list, moving curr forward */	\
	int gdlist(TYPE)::next(TYPE &a)				\
	{							\
	   if (!last) /* is empty */				\
	      return 0;						\
								\
	   /* move curr forward, possibly to beginning or end */\
	   if (curr)						\
	      if (curr == last)					\
	      {							\
		 curr = 0;					\
		 return 0;					\
	      }							\
	      else						\
	         curr = curr->next;				\
	   else							\
	      curr = last->next;				\
								\
	   a = curr->e;						\
	   return 1;						\
	}							\
								\
       /* get previous entry from list, moving curr backwards */\
	int gdlist(TYPE)::prev(TYPE &a)				\
	{							\
	   if (!last)						\
	      return 0;						\
								\
	  /* move curr backward, possibly to beginning or end */\
	   if (curr)						\
	      if (curr == last->next)				\
	      {							\
		 curr = 0;					\
		 return 0;					\
	      }							\
	      else						\
		 curr = curr->prev;				\
	   else							\
	      curr = last;					\
								\
	   a = curr->e;						\
	   return 1;						\
	}							\
								\
	/* get the current entry from the list */		\
	int gdlist(TYPE)::current(TYPE &a)			\
	{							\
	   if (!last || !curr) /* is empty or off end */	\
	      return 0;						\
								\
	   a = curr->e;						\
	   return 1;						\
	}							\
								\
      /* get next entry, removing it after; curr is unchanged */\
	int gdlist(TYPE)::getnext(TYPE &a)			\
	{							\
	   if (!last)						\
	      return 0;						\
								\
	   /* choose the link to remove */			\
	   gdlink(TYPE) *f;					\
	   if (curr)						\
	      if (curr == last)					\
	      {							\
		 curr = 0;					\
		 return 0; /*lsy*/				\
	      }							\
	      else						\
		 f = curr->next;				\
	   else							\
	      f = last->next;					\
								\
	   a = f->e;						\
								\
	   /* remove f from the list */				\
	   if (f == last)					\
	      last = 0;						\
	   else							\
	      f->remove();					\
								\
	   delete f;						\
           kount--;						\
	   return 1;						\
	}							\
								\
       /* get previous entry, remove it after; curr unchanged */\
	int gdlist(TYPE)::getprev(TYPE &a)			\
	{							\
	   if (!last)						\
	      return 0;						\
								\
	   /* choose the link to remove */			\
	   gdlink(TYPE) *f;					\
	   if (curr)						\
	      if (curr == last->prev)				\
	      {							\
		 curr = 0; /*lsy*/				\
		 return 0;					\
	      }							\
	      else						\
		 f = curr->prev;				\
	   else							\
	      curr = last;					\
								\
	   a = f->e;						\
								\
	   /* remove f from the list */				\
	   if (f == last)					\
	      last = 0;						\
	   else							\
	      f->remove();					\
								\
	   delete f;						\
           kount--;						\
	   return 1;						\
	}							\
								\
	/* insert entry at beginning of list */			\
	void gdlist(TYPE)::insert(const TYPE a)			\
	{							\
	   if (last)						\
	      last->next->insert(new gdlink(TYPE)(a));		\
	   else							\
	   {							\
	      last = new gdlink(TYPE)(a);			\
	      last->next = last->prev = last;			\
	   }							\
           kount++;						\
	}							\
								\
	/* insert link at beginning of list */			\
	void gdlist(TYPE)::insert(gdlink(TYPE)* a)		\
	{							\
	   if (last)						\
	      last->next->insert(a);				\
	   else							\
	   {							\
	      last = a;						\
	      last->next = last->prev = last;			\
	   }							\
           kount++;						\
	}							\
								\
	/* append entry to end of list */			\
	/* note this is same as insert except last is adjusted */\
	void gdlist(TYPE)::append(const TYPE a)			\
	{							\
	   this->insert(a);					\
	   last = last->next;					\
	}							\
								\
	/* append link to end of list */			\
	/* note this is same as insert except last is adjusted */\
	void gdlist(TYPE)::append(gdlink(TYPE)* a)		\
	{							\
	   this->insert(a);					\
	   last = last->next;					\
	}							\
								\
	/* insert entry before current location in list */	\
	/* if curr is not set, insert at beginning of list */	\
	void gdlist(TYPE)::inserthere(const TYPE a)		\
	{							\
	   if (curr)						\
	   {							\
	      curr->insert(new gdlink(TYPE)(a));		\
              kount++;						\
	   }							\
	   else							\
	   {							\
	      this->insert(a);					\
	   }							\
	}							\
								\
	/* insert link before current location in list */	\
	/* if curr is not set, insert at beginning of list */	\
	void gdlist(TYPE)::inserthere(gdlink(TYPE)* a)		\
	{							\
	   if (curr)						\
	   {							\
	      curr->insert(a);					\
              kount++;						\
	   }							\
	   else							\
	      this->insert(a);					\
	}							\
								\
	/* append an entry after the current location in list */\
	/* if curr is not set, append at end of list */		\
	void gdlist(TYPE)::appendhere(const TYPE a)		\
	{							\
	   if (curr)						\
	   {							\
	      curr->append(new gdlink(TYPE)(a));		\
              kount++;						\
	   }							\
	   else							\
	      this->append(a);					\
	}							\
								\
	/* append a link after the current location in list */	\
	/* if curr is not set, append at end of list */		\
	void gdlist(TYPE)::appendhere(gdlink(TYPE)* a)		\
	{							\
	   if (curr)						\
	   {							\
	      curr->append(a);					\
              kount++;						\
	   }							\
	   else							\
	      this->append(a);					\
	}							\
								\
	/* remove the current entry */				\
	void gdlist(TYPE)::remove()				\
	{							\
	   gdlink(TYPE) *prevcurr;				\
	   if (isempty())					\
	      return;						\
	   if (curr)						\
	   {							\
	      if (curr == last->next) /* first link */		\
	      {							\
		 curr->remove();				\
		 delete curr;					\
		 curr = 0;					\
		 kount--;					\
	      }							\
	      else if (curr == last) /* last link */		\
	      {							\
		 prevcurr = curr->prev;				\
		 curr->remove();				\
		 delete curr;					\
		 curr = prevcurr;				\
		 last = curr;					\
		 kount--;					\
	      }							\
	      else						\
	      {							\
		 prevcurr = curr->prev;				\
		 curr->remove();				\
		 delete curr;					\
		 curr = prevcurr;				\
		 kount--;					\
	      }							\
	   }							\
	   else	 /* treat curr == 0 like first link? */		\
	   /* it might be much better to do nothing here */	\
	   {							\
	      curr = last->next; /* first link */		\
	      curr->remove();					\
	      delete curr;					\
	      curr = 0;						\
              kount--;						\
	   }							\
	   if (!kount) last = 0;				\
	}							\
								\
	/* unlink & return the current entry */			\
	gdlink(TYPE)* gdlist(TYPE)::unlink()			\
	{							\
	   gdlink(TYPE) *prevcurr;				\
	   gdlink(TYPE) *savecurr;				\
	   if (isempty())					\
	      return NULL;					\
	   if (curr)						\
	   {							\
	      if (curr == last->next) /* first link */		\
	      {							\
		 curr->remove();				\
		 savecurr = curr;				\
		 curr = 0;					\
		 kount--;					\
	      }							\
	      else if (curr == last) /* last link */		\
	      {							\
		 prevcurr = curr->prev;				\
		 curr->remove();				\
		 savecurr = curr;				\
		 curr = prevcurr;				\
		 last = curr;					\
		 kount--;					\
	      }							\
	      else						\
	      {							\
		 prevcurr = curr->prev;				\
		 curr->remove();				\
		 savecurr = curr;				\
		 curr = prevcurr;				\
		 kount--;					\
	      }							\
	   }							\
	   else	 /* treat curr == 0 like first link? */		\
	   /* it might be much better to do nothing here */	\
	   {							\
	      curr = last->next; /* first link */		\
	      curr->remove();					\
	      savecurr = curr;					\
	      curr = 0;						\
              kount--;						\
	   }							\
	   if (!kount) last = 0;				\
	   return savecurr;					\
	}							\
								\
	/* remove the entry corresponding to a */		\
	void gdlist(TYPE)::remove(const TYPE a)			\
	{							\
	   gdlink(TYPE) *savecurr;				\
	   savecurr = curr;					\
	   curr = 0;						\
	   TYPE temp;						\
           while (this->next(temp))				\
	   {							\
              if (temp == a)					\
	      {							\
		 if (savecurr == curr) savecurr = curr->prev;	\
		 remove();					\
		 if (kount)					\
		    curr = savecurr;				\
		 return;					\
	      }							\
	   }							\
	   cerr << "attempt to remove non-existent TYPE node\n";\
	}							\
								\
	/* unlink the entry corresponding to object a */	\
	gdlink(TYPE)* gdlist(TYPE)::unlink(const TYPE a)	\
	{							\
	   gdlink(TYPE) *savecurr;				\
	   gdlink(TYPE) *templink;				\
	   savecurr = curr;					\
	   curr = 0;						\
	   TYPE temp;						\
           while (this->next(temp))				\
	   {							\
              if (temp == a)					\
	      {							\
		 if (savecurr == curr) savecurr = curr->prev;	\
		 templink = unlink();				\
		 if (kount)					\
		    curr = savecurr;				\
		 return templink;				\
	      }							\
	   }							\
	   cerr << "attempt to remove non-existent TYPE node\n";\
	}							\
								\
	/* unlink the link corresponding to link a */		\
	gdlink(TYPE)* gdlist(TYPE)::unlink(gdlink(TYPE)* a)	\
	{							\
	   gdlink(TYPE) *savecurr;				\
	   gdlink(TYPE) *temp;					\
	   savecurr = curr;					\
	   curr = last->next; /* first link */			\
           do							\
	   {							\
              if (curr == a)					\
	      {							\
		 if (curr == savecurr)				\
		    savecurr = curr->prev;			\
		 temp = unlink();				\
		 if (kount)					\
		    curr = savecurr;				\
		 return temp;					\
	      }							\
	   } while ((curr = curr->next) != last->next);		\
	   /* wrapped back around to first, so search failed */	\
	   cerr << "attempt to remove non-existent TYPE node\n";\
	}
#endif DLINK_H

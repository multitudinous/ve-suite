/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#ifndef _SET
#define _SET
#include "blitz/array.h"

template <class Type>
class SetOperators
{
    public:
        SetOperators(){ };
        bool contains( blitz::Array<Type,1> A, Type x);
        bool contains( blitz::Array<Type,1> A, blitz::Array<Type,1> B);
        blitz::Array<Type,1> unite( blitz::Array<Type,1> A, blitz::Array<Type,1> B);
        blitz::Array<Type,1> intersect( blitz::Array<Type,1> A, blitz::Array<Type,1> B);
        blitz::Array<Type,1> subtract( blitz::Array<Type,1> A, blitz::Array<Type,1> B);
        blitz::Array<Type,1> uniteSubtract( blitz::Array<Type,1> A, blitz::Array<Type,1> B);
        blitz::Array<Type,1> uniteSubtractIntersect( blitz::Array<Type,1> A, blitz::Array<Type,1> B);
        void print( blitz::Array<Type,1> A);
        void fill( blitz::Array<Type,1> &A, Type a[]);

        //blitz::Array<Type,1> or( blitz::Array<Type,1> A, blitz::Array<Type,1> B){ return unite(A,B) ;}
        //blitz::Array<Type,1> and( blitz::Array<Type,1> A, blitz::Array<Type,1> B){ return intersect(A,B) ;}
        //blitz::Array<Type,1> xor( blitz::Array<Type,1> A, blitz::Array<Type,1> B){ return uniteSubtract(A,B) ;}
        blitz::Array<Type,1> xor2( blitz::Array<Type,1> A, blitz::Array<Type,1> B){ return uniteSubtractIntersect(A,B) ;}
        blitz::Array<Type,1> diff( blitz::Array<Type,1> A, blitz::Array<Type,1> B){ return subtract(A,B) ;}
        blitz::Array<Type,1> symDiff( blitz::Array<Type,1> A, blitz::Array<Type,1> B){ return uniteSubtract(A,B) ;}       
    private:

};

/* A contains item x */
template<class Type> void insert( blitz::Array<Type,1> &A, Type x)
{
	int n = A.size();
	A.resizeAndPreserve(n + 1);	
	A(n) = x;
}


template<class Type> bool SetOperators<Type>::contains( blitz::Array<Type,1> A, Type x)
{
    for (int i = 0; i < A.size(); i++)
        {
        if (x == A(i)) return true;
        }
    return false;
}

template<class Type> bool SetOperators<Type>::contains( blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
	for (int i = 0; i < B.size(); i++)
		{
		if ( ! contains(A, B(i)) ) return false;
		}	 	
	return true;
}

/* A or B */
template<class Type> blitz::Array<Type,1> SetOperators<Type>::unite( blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
    blitz::Array<Type,1> C(A);
    for (int i = 0; i < B.size(); i++)
        if ( !this->contains( C, B(i) ) )
            insert( C, B(i) );

    return C;
}

/* A and B */
template<class Type> blitz::Array<Type,1> SetOperators<Type>::intersect( blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
    blitz::Array<Type,1> C;
    for (int i = 0; i < A.size(); i++)
        {
        if ( contains(B, A(i)) ) insert( C, A(i));
        }            
    return C;
}

/* A - B */
template<class Type> blitz::Array<Type,1> SetOperators<Type>::subtract( blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
    blitz::Array<Type,1> C;
    for (int i = 0; i < A.size(); i++)
        {
        if ( !contains(B, A(i)) ) insert( C, A(i));
        } 
    return C;
}

/* A xor B =  (A - B) or (B - A)  =  (A or B) - (A and B) */
template<class Type> blitz::Array<Type,1> SetOperators<Type>::uniteSubtract( blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
    blitz::Array<Type,1> C = subtract(A, B);
    blitz::Array<Type,1> D = subtract(B, A);
    return unite(C, D);
}

template<class Type> blitz::Array<Type,1> SetOperators<Type>::uniteSubtractIntersect( blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
    blitz::Array<Type,1> C = unite(A, B);
    blitz::Array<Type,1> D = intersect(A, B);
    return subtract(C, D);
}

/*
template<class Type> void SetOperators<Type>::print( blitz::Array<Type,1> A)
{
    std::cout << "\n[";
    for (int i = 0; i < A.size(); i++)
        std::cout << " " << A(i);
    std::cout << " ]";

}
*/

template<class Type> void SetOperators<Type>::fill( blitz::Array<Type,1> &A, Type a[])
{    
    for (int i = 0; i < A.size(); i++)
        A(i) = A(i);
}

class MapOperators
{
    public:
        MapOperators(){ };
        void invertMapRestrict( blitz::Array<int,2> map, int map_first, 
            blitz::Array<int,2> &inverse_map, blitz::Array<int,1> &inverse_count, 
	    int inv_first, int inv_last );
        void invertMap( blitz::Array<int,2> map, int map_first, 
            blitz::Array<int,2> &inverse_map, blitz::Array<int,1> &inverse_count, 
	    int &inv_first, int &inv_last );
    private:

};


#endif

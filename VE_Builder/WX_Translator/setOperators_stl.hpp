/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#ifndef _SET_STL
#define _SET_STL
#include "blitz/array.h"
#include <vector>

template <class Type>
class SetOperators
{
    public:
        SetOperators(){ };
        bool contains( std::vector<Type> A, Type x);
        bool contains( std::vector<Type> A, std::vector<Type> B);
        std::vector<Type> unite( std::vector<Type> A, std::vector<Type> B);
        std::vector<Type> intersect( std::vector<Type> A, std::vector<Type> B);
        std::vector<Type> subtract( std::vector<Type> A, std::vector<Type> B);
        std::vector<Type> uniteSubtract( std::vector<Type> A, std::vector<Type> B);
        std::vector<Type> uniteSubtractIntersect( std::vector<Type> A, std::vector<Type> B);
        void print( std::vector<Type> A);
        void fill( std::vector<Type> &A, Type a[]);

        //std::vector<Type> or( std::vector<Type> A, std::vector<Type> B){ return unite(A,B) ;}
        //std::vector<Type> and( std::vector<Type> A, std::vector<Type> B){ return intersect(A,B) ;}
        //std::vector<Type> xor( std::vector<Type> A, std::vector<Type> B){ return uniteSubtract(A,B) ;}
        std::vector<Type> xor2( std::vector<Type> A, std::vector<Type> B){ return uniteSubtractIntersect(A,B) ;}
        std::vector<Type> diff( std::vector<Type> A, std::vector<Type> B){ return subtract(A,B) ;}
        std::vector<Type> symDiff( std::vector<Type> A, std::vector<Type> B){ return uniteSubtract(A,B) ;}
      
 

    private:

};

/* A contains item x */
template<class Type> bool SetOperators<Type>::contains( std::vector<Type> A, Type x)
{
    for (int i = 0; i < A.size(); i++)
        {
        if (x == A[i]) return true;
        }
    return false;
}

template<class Type> bool SetOperators<Type>::contains( std::vector<Type> A, std::vector<Type> B)
{
	for (int i = 0; i < B.size(); i++)
		{
		if ( ! contains(A, B[i]) ) return false;
		}	 	
	return true;
}

/* A or B */
template<class Type> std::vector<Type> SetOperators<Type>::unite( std::vector<Type> A, std::vector<Type> B)
{
    std::vector<Type> C(A);
    for (int i = 0; i < B.size(); i++)
        if ( !this->contains( C, B[i] ) )
            C.insert( C.end(), B[i] );

    return C;
}

/* A and B */
template<class Type> std::vector<Type> SetOperators<Type>::intersect( std::vector<Type> A, std::vector<Type> B)
{
    std::vector<Type> C;
    for (int i = 0; i < A.size(); i++)
        {
        if ( contains(B, A[i]) ) C.insert( C.end(), A[i]);
        }            
    return C;
}

/* A - B */
template<class Type> std::vector<Type> SetOperators<Type>::subtract( std::vector<Type> A, std::vector<Type> B)
{
    std::vector<Type> C;
    for (int i = 0; i < A.size(); i++)
        {
        if ( !contains(B, A[i]) ) C.insert( C.end(), A[i]);
        } 
    return C;
}

/* A xor B =  (A - B) or (B - A)  =  (A or B) - (A and B) */
template<class Type> std::vector<Type> SetOperators<Type>::uniteSubtract( std::vector<Type> A, std::vector<Type> B)
{
    std::vector<Type> C = subtract(A, B);
    std::vector<Type> D = subtract(B, A);
    return unite(C, D);
}

template<class Type> std::vector<Type> SetOperators<Type>::uniteSubtractIntersect( std::vector<Type> A, std::vector<Type> B)
{
    std::vector<Type> C = unite(A, B);
    std::vector<Type> D = intersect(A, B);
    return subtract(C, D);
}

template<class Type> void SetOperators<Type>::print( std::vector<Type> A)
{
    cout << "\n[";
    for (int i = 0; i < A.size(); i++)
        cout << " " << A[i];
    cout << " ]";

}

template<class Type> void SetOperators<Type>::fill( std::vector<Type> &A, Type a[])
{    
    for (int i = 0; i < A.size(); i++)
        A[i] = a[i];
}


class MapOperators
{
    public:
        MapOperators(){ };
        void invertMapRestrict( blitz::Array<int,2> map, int map_first, 
            blitz::Array<int,2> &inverse_map, int inv_first, int inv_last );
        void invertMap( blitz::Array<int,2> map, int map_first, 
            blitz::Array<int,2> &inverse_map, int &inv_first, int &inv_last);
    private:

};


#endif

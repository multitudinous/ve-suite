/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#include "setOperators.hpp"
//#include <vector>
#include <string>
#include "blitz/array.h"

template<class Type>
void testOperators(blitz::Array<Type,1> A, blitz::Array<Type,1> B)
{
    SetOperators<Type> set;
    std::cout << "\nA " << A << std::endl;
    std::cout << "\nB " << B << std::endl;
    for (int i = 0; i < A.size(); i++)
        std::cout << "\n\tA[" << i << "] = " << A(i) << " in B = " << set.contains( B, A(i) );
    std::cout << "\nA or B " << set.unite(A,B)<< std::endl;
    std::cout << "\nA and B " << set.intersect(A,B)<< std::endl;
    std::cout << "\nA - B " << set.subtract(A,B)  << std::endl;
    std::cout << "\nB - A " << set.subtract(B,A)  << std::endl;
    std::cout << "\nA xor B " << set.uniteSubtract(A, B) << std::endl;
    std::cout << "\nA xor B " << set.uniteSubtractIntersect(A, B) << std::endl;
    std::cout << std::endl;
}   




int sub_main()
{    
    SetOperators<int> set;

    blitz::Array<int,1> A(4),B(4),C(4);
	A = 1,2,3,4;
	B = 3,4,5,6;
	std::cout << "first" << std::endl;
    testOperators(A,B);
    std::cout << "\nsecond" << std::endl;
    C = 4,3,2,1; 
    testOperators(A,C);

    SetOperators<std::string> string_set;
    blitz::Array<std::string,1> stringA(4),stringB(4);
    stringA = "a","b","c","d";
    stringB = "e","f","g","a";
    testOperators(stringA, stringB);

    return 0;
}



/*
blitz::Array<int,1> = calcUniqueList( blitz::Array<int,1> list, )
{
    new_list[0] = list[0];
    for (int i = 1; i < list.size(); i++)
        {
        bool isUnique = true;
        for (int j = 0; j < new_list.size(); j++)
            {
            if ( list[i] == new_list[j] )
                {
                isUnique = false;
                break;
                }
            }
        if (isUnique)
            new_list.insert( new_list.end(), list[i] );
        }
}
*/

void MapOperators::invertMapRestrict( 
        blitz::Array<int,2> map, int map_first, blitz::Array<int,2> &inverse_map, blitz::Array<int,1> &inverse_count, int inv_first, int inv_last )
{
    int map_size = map.extent(blitz::firstDim);
    int map_members = map.extent(blitz::secondDim);   

    int inv_size = inv_last - inv_first + 1;
   
    blitz::Array<int,1> member_size;
    member_size.resize(inv_size);

    member_size = 0;
    for (int n = 0; n < map_size ; n++){        
        for (int m = 0; m < map_members; m++){ 
            int k = map(n,m);  
	    int j = k - inv_first;
            if (k >= inv_first && k <= inv_last) member_size( j ) = member_size(j) + 1;
            }
        }
 
    int inv_members = blitz::max(member_size);
    std::cout << "max number of members for inverse is = " << inv_members << std::endl;
    inverse_map.resize( inv_size, inv_members );
    inverse_map = -1; // + std::min(map_first,0); /* initialize to an id below the range */
    inverse_count.resize(inv_size);
    inverse_count = 0;
    for (int n = 0; n < map_size; n++) {
        for (int m = 0; m < map_members; m++) {
            int k = map(n,m);  
            if (k >= inv_first && k <= inv_last) {                                   
		int j = k - inv_first;
                inverse_map( j , inverse_count(j) ) = n + map_first;
                inverse_count(j) = inverse_count(j) + 1;
                }
            }
        }  

    /*
    cout << "inverse map" << std::endl;
    for (int k = 0; k < inv_size; k++ ) {
        for (int i = 0; i < inverse_count(k); i ++ )
            std::cout << " " << inverse_map(k,i);
	std::cout << "\n";
        }
    */
        
}

/* #include "math.h" */
inline int min(int x, int y)
{
    if (x <= y) return x;
    return y;
}

inline int max(int x, int y)
{
    if (x >= y) return x;
    return y;
}

void MapOperators::invertMap( blitz::Array<int,2> map, int map_first, blitz::Array<int,2> &inverse_map, 
		blitz::Array<int,1> &inverse_count, int &inv_first, int &inv_last)
{

    /* valid id's are bounded by the entry values on inv_first and inv_last */
    inv_last = min( max(map), inv_last );  
    inv_first = max( min(map), inv_first );
    int inv_size = inv_last - inv_first + 1;
    invertMapRestrict( map, map_first, inverse_map, inverse_count, inv_first, inv_last );
   
}

/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/

/*

    Reorder faces in a cells and assign nodes


    
    cell_thread ->faceID(icell,iface)
                ->nodeID(inode)

*/

#include "fluentObjects.h"
#include "var_defs.h"
#include "setOperators_stl.hpp"
#include "blitz/array.h"
#include "blitz/range.h"
#include <algorithm>
#include <vector>
#include "reorder.h"


namespace FluentReader {


class VectorOperators
{
	public:	
			
		double det( blitz::Array<double,1> u, blitz::Array<double,1> v)
			{ return u(0)*v(1) - u(1)*v(0); }
		double det( double u0, double u1, double v0, double v1)
			{
			return u0*v1 - u1*v0;	
			}
			
		blitz::Array<double,1>cross( blitz::Array<double,1> u, blitz::Array<double,1> v)
			{
			blitz::Array<double,1> w;
			w(2) =  det( u(0),u(1),v(0),v(1) );
			w(0) =  det( u(1),u(2),v(1),v(2) );
			w(1) = -det( u(0),u(2),v(0),v(2) );
			return w;	
			} 
								
		
		double dot( blitz::Array<double,1> u, blitz::Array<double,1> v)
			{
			return u(0)*v(0) + u(1)*v(1) + u(2)*v(2);							
			}	
			
		double mag( blitz::Array<double,1> u)
			{
			return sqrt( dot(u,u) );
			}
		
	private:
		
		
};

bool isFaceValid( NodeList node_list, PosVectorList x)
{

    double tol = 1e-8;
	VectorOperators vec_op;
 	PosVectorList v(4);
    const int nFaces = 4;
	
    if (node_list.size() < 4)
        return true;

   for (int i = 0; i < nFaces; i++)
       v[i] = x[ node_list[i] ] - x[ node_list[0] ];
    
    PosVector A1 = vec_op.cross( v[1], v[2] );
    PosVector A2 = vec_op.cross( v[2], v[3] );	
	double dot12 = blitz::sum( A1*A2 );
    if ( dot12 < 0.0 )
		{
        std::cout << "face is invalid, failed angle test" << std::endl;
        return false;
		}
	PosVector cross12 = vec_op.cross(A1,A2);  
    if ( vec_op.mag(cross12) > tol )
		{
        std::cout << "face is invalid, failed plane test" << std::endl;
        return false;
		}
    return true;
}

void swap(int &A, int &B)
{
    int C = A;
    A = B;
    B = C;   
}
/*
	Reorders faces so that the nodes form a loop
*/
int reorderNodes( FaceThread *face, PosVectorList node_pos, int face_id)
{
	int nNodes = face->nNodes(face_id);	
	NodeList node_list(nNodes);	
		
	face->getNodeList(face_id, node_list);
	
	/* the first node in the list the node with the lowest ID# */

    std::sort( node_list.begin(), node_list.end() );
    /*
	for (int i = 0; i < nNodes-1; i++) {        
        for (int j = i + 1; j < nNodes; j++)
            if (  node_list[j] > node_list[i] ) swap(node_list[i] ,node_list[j]);
        }
    */
		
	/* tri/linear meshes need no further modeification, 
        (did this is the first place)	*/	
	if (nNodes < 4)
		return 0;
				
	/* test three different orderings - the vector represents the sorted ordering */
	
	/* first test current ordering (0 1 2 3), 
	
		for loop testing this is the same as (0 3 2 1) since the ordering is cyclic */
	
	if ( isFaceValid( node_list, node_pos) ) {
		face->setNodeList( face_id, node_list );
		return 0;
		}
	
	/* test (0 2 1 3) : (0 3 1 2) */
	swap( node_list[1], node_list[2] );
	if ( isFaceValid( node_list, node_pos) ) {
		face->setNodeList( face_id, node_list );
		return 0;
		}
		
	swap( node_list[1], node_list[2] );
	/* test(0 1 3 2) : (0 2 3 1) */		
	if ( isFaceValid( node_list, node_pos) ) {
		face->setNodeList( face_id, node_list );
		return 0;
		}							
	/* none of the tested faces are valid */	
	return 1;
}	 
/*

	Tetrahedrons do not need to be reordered


*/
/*

*/
void sort( std::vector<int> A )
{
    std::sort( A.begin(), A.end());
}

/* returns true is B "is included" in A */

bool isIncluded( std::vector<int> A, std::vector<int> B )
{
    int n = 0;
    for (int i = 0; i < B.size(); i++) {
        for (int j = 0; j < A.size(); j++){
            if ( A[j] == B[i] ) {
                n++;
                break;
                }
            }
        }   
    //std::cout << "isIncluded = " << n << std::endl;
    return n == B.size();
}


void getEdgeList( int i_edge, NodeList face_node, NodeList &edge_node) {
    edge_node.resize(2,-1); // fill with invalid digit
	//std::cout << "getEdgeList" << std::endl;
	//for (int j = 0; j < face_node.size(); j++)
	//    std::cout << face_node[j] << std::endl;
        edge_node[0] = face_node[i_edge];
        if (i_edge < face_node.size() - 1) 
            edge_node[1] = face_node[i_edge + 1];
        else
            edge_node[1] = face_node[0];

    //std::cout << "\tgetEdgeList" << edge_node[0] << " " << edge_node[1] << std::endl;
    } 


void reorderFace_3D( CellThread *cell, FaceThread *face, int cell_id )
{
    int nFaces = cell->nFaces(cell_id);
    FaceList face_list; // ( nFaces );

/* three dimensional algorithm */   
    cell->getFaceList(cell_id, face_list);


    NodeList fourNodes;
    NodeList threeNodes;
    for (int i = 0; i < nFaces; i++) {
        int id = face_list[i];
        if ( face->nNodes(i) == 3 )
            threeNodes.insert( threeNodes.end(), id );
        else
            fourNodes.insert( fourNodes.end(), id );
        }
    /*

*/
    sort( fourNodes );
    sort( threeNodes );

    face_list.resize(0);
    /* for tets and wedges control face (0) is 3 node face */
   
    if (cell->elementType(cell_id) == cell->TET )           
        face_list.insert( face_list.end(),threeNodes.begin(), threeNodes.end()  );
    if (cell->elementType(cell_id) == cell->HEX )           
        face_list.insert( face_list.end(), fourNodes.begin(), fourNodes.end() );
    if (cell->elementType(cell_id) == cell->PYRAMID )           
        face_list.insert( face_list.end(),fourNodes.begin(), fourNodes.end()  );
        face_list.insert( face_list.end(),threeNodes.begin(), threeNodes.end()  );
    if (cell->elementType(cell_id) == cell->PYRAMID )   
        face_list.insert( face_list.end(),threeNodes.begin(), threeNodes.end()  );	        
        face_list.insert( face_list.end(),fourNodes.begin(), fourNodes.end()  );
     			
/* sort faces based on which verticies are contained in root-face  */
/*
    face 1 - contains nodes 0 and 1 of root face
    face 2 - contains nodes 1 and 2 of root face
    face 3 - contains nodes 3 and 0 of root face
*/

/* loop over the edges in the first face of the cell */


    NodeList edge_node;
    NodeList node_list, node_list0;       
    int face0 = face_list[0];
    /* node list must have correct orientation - 
        the cross product of the edges should point toward inside of the cell
        if cell if the right cell of the face, then orientation is OK, if the cell
        is the left cell then orientation should be swithced.
    */
    face->getNodeList( face_list[0], cell_id, node_list0);     
    //std::cout << "node_size = " << node_list0.size() << std::endl;

    for (int iedge = 0; iedge < face->nNodes( face0 ); iedge++) {        
        //face->getEdgeList(face0, iedge, edge_node);  removed since could only valid for right cells
        getEdgeList( iedge, node_list0, edge_node);
        //std::cout << "nodes = " << edge_node[0] << " " << edge_node[1] << std::endl;
        for (int jface = iedge + 1; jface < cell->nFaces(cell_id); jface++ ) {            			
            face->getNodeList( face_list[jface], node_list );
            //std::cout << "\tface = ";
            //for (int j = 0; j < node_list.size(); j++)
            //    std::cout << "  " << node_list[j];
            //std::cout << std::endl;
            if ( isIncluded( node_list, edge_node ) ) {                
                int temp = face_list[iedge + 1];
                face_list[iedge+1] = face_list[jface];
                face_list[jface] = temp;
                break;
                }            
            }
        }
		
	cell->setFaceList( cell_id, face_list );
		
}		

#include <algorithm>
void set_unite_subtract( NodeList side1, NodeList side2, NodeList side3, NodeList &result  )
{
    NodeList::iterator begin,end,r;
    NodeList temp; //( side1.size() + side2.size() );
    /*
    std::cout << "intersection" << std::endl;
    end = std::set_intersection( side1.begin(), side1.end(), side2.begin(), side2.end(), temp.begin() ); 
    temp.erase( end, temp.end() + 1 ); 
    for (int i = 0; i < temp.size(); i++)
        std::cout << "\t" << i << temp[i] << std::endl;   
    result.resize( side3.size() + temp.size() );
    end = std::set_difference( temp.begin(), temp.end(), side3.begin(), side3.end(), result.begin() );
    for (int i = 0; i < result.size(); i++)
        std::cout << "\t" << i << result[i] << std::endl;   
    result.erase( end, result.end() );
    for (int i = 0; i < result.size(); i++)
        std::cout << "\t" << i << " " << result[i] << std::endl;   
    */
    
    for (int i = 0; i < side1.size(); i++) {        
        for (int j = 0; j < side2.size(); j++) {
            if ( side2[j] == side1[i] ) 
                temp.insert( temp.end(), side2[j] );  
            }
        }
    
    result.resize(0);
    for (int i = 0; i < temp.size(); i++){
        bool include = true;   
        for (int j = 0; j < side3.size(); j++) {
            if ( temp[i] == side3[j] ) {
                include = false;
                break;
                }
            }
        if (include) result.insert( result.end(), temp[i] );
        }
        

}
    //new_node = set.subtract( set.unite( side1, side2 ) , side0 );

void reorderNode_3D( CellThread *cell, FaceThread *face, int cell_id )
{
 	FaceList face_list ( cell->maxFaces() );
	NodeList node_list ( cell->maxNodes() );

	cell->getFaceList( cell_id, face_list );

/* now determine vertex ordering */

/*
    nodes 0 through 3 (2) are node from the "base" face of the object
*/
        //face->getNodeList( face_list[0], cell_id, node_list0); 
    NodeList side0; 	
	face->getNodeList( face_list[0], cell_id, side0 );
   
/*
	"base" face is Quad:
    	node 4 is the node shared by face 4 and face 1 which is not a part of base (face 0)
    	node 5                       face 1 and face 2
    	node 6                       face 2 and face 3
    	node 7                       face 3 and face 4
  
    
    "base" face is Tri(Tet):
    	node 3 is the same ans node 2
    	node 4 is the top of the tet, 5,6,7 are copies	
    	  	
    pyramids and wedges will have redundant node numbers 	
*/
    NodeList side1( c_maxNodesPerFace );
    NodeList side2( c_maxNodesPerFace );
    SetOperators<int> set;
    NodeList new_node( 2*c_maxNodesPerFace );
    
    int n0 = face->nNodes( face_list[0] );
	int n = cell->nNodes( cell_id) - n0;
 
    for (int i = 0; i < n ; i++) {  
      
        if (i == 0)
            face->getNodeList( face_list[ n0 ], side1) ;
        else 
            face->getNodeList( face_list[ i ], side1 );

        face->getNodeList( face_list[i + 1], side2 );

		/* the new node is determined by set operations */
		/*         n = (s1 + s2) - s0  */
        //new_node = set.subtract( set.unite( side1, side2 ) , side0 );
    
        //for (int i = 0; i < n0; i++){
        //    std::cout << "\t side0 " << side0[i] << "\t" << side1[i] << "\t" << side2[i] << std::endl;}
  
        set_unite_subtract( side1, side2, side0, new_node  );

        if (new_node.size() != 1) 
        	std::cout << "Error Cell::reorderNodes_3D" << " node list = " << new_node.size() << std::endl;


        node_list[i] = side0[i];
        node_list[i + face->nNodes( face_list[0] ) ] = new_node[0];
        //std::cout << "\t node added " << new_node[0] << std::endl;
        }

    //std::cout << " Cell::reorderNodes_3D" << " node list = " << new_node.size() << std::endl;
    cell->setNodeList( cell_id, node_list);
}
/*  two dimensional algorithm */

void reorderFace_2D( CellThread *cell, FaceThread *face, int cell_id )
{

    SetOperators<int> set;
    //NodeList v_list( cell->maxNodes() );
	//NodeList new_node( cell->nNodes(i),0 );
	int nNodes = cell->nNodes( cell_id );
	int nFaces = cell->nFaces( cell_id );
    NodeList node_list( nNodes,0 );
    FaceList face_list( nFaces,0 );
	//std::cout << cell_id << "\n";

	cell->getFaceList(cell_id, face_list ); 
	face->getNodeList( face_list[0], node_list );
    //for (int i = 0; i < nNodes-1; i++) std::cout << face_list[i] << " ";
    //for (int i = 0; i < nNodes-1; i++) std::cout << node_list[i] << " ";
    //std::cout << "\n";
    
    int common_node = node_list[1];
    for (int inode = 1; inode < nNodes - 1; inode++) {        
        for (int iface = inode; iface < nFaces; iface++) {
            NodeList face_node(2);
            face->getNodeList( face_list[iface], face_node );
            if ( common_node == face_node[0] || common_node == face_node[1] ) {                
                swap( face_list[iface], face_list[inode] );
                if (face_node[0] == common_node)
                    common_node = face_node[1];
                else
		            common_node = face_node[0];

                node_list.insert( node_list.end(), common_node );
                }
            }
        }
    
/*
        {         
        NodeList ilist,jlist;
        face->getNodeList( face_list[iface], ilist );        
        for (int jface = iface+1; jface < nFaces-1; jface++)
            {
            face->getNodeList( face_list[jface], jlist );
            if ( set.contains( jlist, ilist ) )
                {                
                swap( face_list[iface+1], face_list[jface] );
                if ( node_list.size() != 1 )
                	std::cout << "Error Cell::reorder_2D" << " node list = " << node_list.size() << std::endl;
                
                for (i
                node_list[iface + 2] = node_list[0];
  
                }

            }
        }
*/
    cell->setFaceList( cell_id, face_list );
    cell->setNodeList( cell_id, node_list );
    //for (int i = 0; i < nNodes-1; i++) std::cout << face_list[i] << " " << cell->face(cell_id,i) << " ";
    //for (int i = 0; i < nNodes-1; i++) std::cout << node_list[i] << " " << cell->node(cell_id,i) << " ";
    //std::cout << "\n";
   
}
    

}  /* end namespace FluentReader */





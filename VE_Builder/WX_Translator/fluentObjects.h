/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#ifndef FLUENT_OBJECTS
#define FLUENT_OBJECTS

#include <vector>
#include <string>
#include <algorithm>
#include "blitz/array.h"
#include "fluentIO.h"
#include "var_defs.h"

namespace FluentReader {


/*
typedef double** doubleArray2;
typedef int** intArray2;
typedef int*** intArray3;163
*/


class Comment
{

    public:
        Comment(FluentIO *infile) { m_comment = "Comment"; }

    private:
        std::string m_comment;


};

class Header
{


};

/* all objects should inherit from a common base */
class FluentObject
{
    public:
	FluentObject(FluentIO *infile);
        const int ID(){ return m_zone; }
         
    private:
        int m_zone;
};


class NodeThread
{

    public:
        NodeThread(FluentIO *infile);
        double* coords() { return m_coords.dataFirst(); }
        double* coords(int i){ return &m_coords(i, 0 ); }
        void getCoords(int i, double x[])
            { for (int j = 0; j < m_nDims; j++) x[j] = m_coords(i,j) ; }            
        doubleArray_2 b_coords() { return m_coords; }
        int nNodes(){ return m_nNodes; }
        int nDims() { return m_nDims; }
        void textDump( FILE *outfile);
        bool hasCoords() { return m_coords.size() > 0 ; }
        const int zoneID(){ return m_zone; }
        
    private:
        int m_zone;     /* zone-id */
        int m_first;    /* first node id */
        int m_last;     /* last node id  */
        int m_type;     /* type of node (on/off */
        int m_nDims;    /* number of dimension */
		
        int m_nNodes;   /* number of nodes */
        doubleArray_2 m_coords; /* coordinates  (m_nNodes,m_nDims) */
		blitz::Range m_rangeDims;
};

class FaceThread
{

    public:

        enum elementTypes {MIXED,INVALID,LINEAR,TRI,QUAD};

        FaceThread(FluentIO *infile);
        void textDump( FILE *outfile);
        // ~FaceThread(){ delete m_cell; delete m_node; }
        
        
        const int nNodes(int i){ 
            if (m_elementType == MIXED) 
                return m_nodesPerFace( i );
            return m_maxNodes; }

        const int elementType(int i){ return nNodes(i); }

        const int nFaces(){ return m_nFaces; }
        const int firstID(){ return m_first; }
        const int lastID(){ return m_last; }
        const int zoneID(){ return m_zone; }
        const int maxNodes(){ return m_maxNodes; }				
		
        int nDims(){ if (this->nNodes(0) ==  2) 
                        return 2;
                    else
                        return 3; }
        int elementType(){ return m_elementType; }
        
        int calcMaxNodesPerFace(intArray_2 array, int i_size);
 
        int calcMaxNodesPerFace(int **array, int i_size);
        int calcGlobalElementType(std::vector<int> nElementType);
        int calcMaxNodesPerFace(int element_type);
        void fillGlobal( std::vector<FaceThread> face_thread);
        void fillLocal( std::vector<FaceThread> face_thread){ 
            std::cout << "FaceThread::extractFromGlobal no implemented" << std::endl; }

        bool hasNodes() { return m_node.extent(blitz::firstDim) == m_nFaces; }
        bool hasCells() { return m_cell.extent(blitz::firstDim) == m_nFaces; }
        bool hasFace( int face_id ){ 
            return (face_id >= m_first && face_id <= m_last); }

        int findThread( std::vector<FaceThread> face_thread, int face_id)
            {
            for (int i = 0; i < face_thread.size(); i++)
                {
                if ( face_thread[i].hasFace( face_id) && 
                        face_thread[i].hasNodes() ) return i;
                }
            return -1;
            };

		intArray_2 cells(){ return m_cell; }
        intArray_2 nodes(){ return m_node; }
        intArray_1 nodesPerFace() { return m_nodesPerFace; }
		
		intArray_1 cells(int id){ return m_cell(id, m_rangeCells); }
		intArray_1 nodes(int id){ return m_node(id, m_rangeNodes); }
						
		int cell(int id, int icell){ return m_cell(id, icell); }
		int node(int id, int inode){ return m_node(id, inode); }
		
        void setCells( intArray_2 cell );
		void setNodes( intArray_2 node );
		
		void setCells( int id, intArray_1 cell);
		void setNodes( int id, intArray_1 node);
		
		void setCell( int id, int cell);
		void setNode( int id, int node);
				
        void getNodeList( int id, NodeList &node_list ){
			node_list.resize( this->nNodes(id) );
            for (int i = 0; i < this->nNodes(id); i++)
                node_list[i] = m_node(id,i);
            }


        /* under some circumstances the (building cell-to-face connectivity)
        the node list must have correct orientation - 
        the cross product of the edges should point toward inside of the cell
        if cell if the right cell of the face, then orientation is OK, if the cell
        is the left cell then orientation should be swithced.
        */
        
	    void getNodeList( int id, int cell_id, NodeList &node_list){
            
            if (cell_id == m_cell(id,c_leftCell) ) {
                int n = this->nNodes(id);
                NodeList reverse_list;
                this->getNodeList(id, reverse_list);            
                node_list.resize(n);
                for (int i = 0; i < n; i++) 
                    node_list[i] = reverse_list[n-1-i];
                }
            else {
                this->getNodeList(id, node_list);               
                if (cell_id != m_cell(id,c_rightCell) ) {
                     std::cout << "Error: NodeThread: " << m_zone << "cell = " << 
                    cell_id << " is not adjacent to face " << id << std::endl;
                    }
                }			
            }
			
		void setNodeList( int id, NodeList node_list ){
			int n = std::min( (int) node_list.size(), this->nNodes(id) );
			for (int i = 0; i < n; i++)
				m_node(id,i) = node_list[i];
			}	
					
		void getCellList( int id, CellList &cell_list ){
			cell_list.resize( c_cellsPerFace );
            for (int i = 0; i < c_cellsPerFace; i++)
                cell_list[i] = m_cell(id,i);
            }
				
		void setCellList( int id, CellList cell_list ){
			int n = std::min( (int) cell_list.size(), c_cellsPerFace );
			for (int i = 0; i < n; i++)
				m_cell(id,i) = cell_list[i];
			}			
		/*	
        void getCellList( int iface, std::vector<int> cell_list ){
                cell_list[0] = m_cell(iface,0);
                cell_list[1] = m_cell(iface,1);
                }
		*/
        int cellID(int iface, int icell){ return m_cell(iface,icell); }

        void setCell( intArray_2 cellConnect ) { m_cell = cellConnect; }
		
		int nodeID(int iface, int inode){ return m_cell(iface,inode); }
		
        void getEdgeList( int iface, int iedge, std::vector<int> node_list){
            node_list.resize( c_nodesPerEdge );
            node_list[0] = m_node(iface, iedge );
            if ( iedge + 1 < this->nNodes( iface )  )            
                node_list[1] = m_node(iface, iedge + 1);
            else
                node_list[1] = m_node(iface, 0);
            }

		/*
		NodeList nodeList( int iface ){
			return m_node( iface, m_rangeNodes );
			}
		*/
		/* replace with member functions		
		bool hasNodes(){ return m_cell.size() > 0; }
		bool hasCells(){ return m_node.size() > 0; }
		*/
		int reorder( NodePositions node_pos );
        
        bool hasConnect(){ return (m_cell.size() > 0 && m_node.size() > 0 ); }

        void copyToGlobal( FaceThread *global_face );

        void copyFromGlobal( FaceThread *global_face );
	
    private:
        int m_zone; 
        int m_first;
        int m_last;
        int m_bcType;
        int m_elementType;
		
        int m_nDims;		
        int m_nFaces;
        int m_maxNodes; 		/* max nodes per face */
 
        //intArray_2* m_cell;
        //intArray_2* m_node;
        //intArray_1* m_nodesPerFace;

        intArray_2 m_cell;  /* cell id's   m_nFaces,c_cellsPerFace */
        intArray_2 m_node;  /* node id's   m_nFaces,m_maxNodes */
        intArray_1 m_nodesPerFace; /* only used for heterogeneous zones   m_nFaces */		
		
		blitz::Range m_rangeNodes;
		blitz::Range m_rangeCells;
		
        bool m_hasNodes;  
        bool m_hasCells;
};


class PeriodicFace
{


};


class CellThread
{
    public:

        enum elementTypes {MIXED,TRI,TET,QUAD,HEX,PYRAMID,WEDGE};

        CellThread(FluentIO *infile);
        void textDump( FILE *outfile);
        int nDims(){ return m_nDims; }

        intArray_2 getFaces(){ return m_face; }
        intArray_2 getNodes(){ return m_node; }
        intArray_1 getElementType(){ return m_cellElementType; }        
        //intArray_1 getFacesPerCell(){ return m_facesPerCell; }
        //intArray_1 getNodesPerCell(){ return m_nodesPerCell; }               
   
        const int nFaces(int i){ 
            if (m_elementType == MIXED) 
                return c_facesPerCell[ m_cellElementType(i) ]; 
            return m_maxFaces; }
        const int nNodes(int i){ 
            if (m_elementType == MIXED) 
                return c_nodesPerCell[ m_cellElementType(i) ];
            return m_maxNodes; }
        const int elementType(int i){
            if (m_elementType == MIXED) return m_cellElementType(i); 
            return m_elementType; } 

           
        const int elementType(){ return m_elementType; }
        const int nCells(){ return (m_last - m_first + 1); }        
        const int firstID(){ return m_first; }
        const int lastID(){ return m_last; }
        const int zoneID(){ return m_zone; }
        const int maxNodes(){ return m_maxNodes; }
        const int maxFaces(){ return m_maxFaces; } 

        void setMaxNodes( int maxNodes){ m_maxNodes = maxNodes; }
        void setMaxFaces( int maxFaces){ m_maxFaces = maxFaces; }
		
		bool hasNodes() { return m_node.extent(blitz::firstDim) == m_nCells; }
        bool hasFaces() { return m_face.extent(blitz::firstDim) == m_nCells; }
    
		       
        intArray_2 faces(){ return m_face; }
        intArray_2 nodes(){ return m_node; }		
		intArray_1 faces(int id){ return m_face(id, m_rangeFaces); }
		intArray_1 nodes(int id){ return m_node(id, m_rangeNodes); }
									 
    	int face(int icell, int iface){ return m_face(icell,iface); }
        int node(int icell, int inode){ return m_node(icell,inode); }

		void setFaces( intArray_2 face );
		void setFaces( intArray_2 faceConnect, int firstID );
        void setNodes( intArray_2 node );
		void setFaces( int icell, intArray_1 face );
		void setNodes( int icell, intArray_1 node );
		void setNode( int icell, int inode, int id ){ m_face(icell,inode) = id; }
		void setFace( int icell, int iface, int id ){ m_face(icell,iface) = id; }
				     	
      	void getNodeList( int id, NodeList &node_list ){
			node_list.resize( this->nNodes(id) );
            for (int i = 0; i < this->nNodes(id); i++)
                node_list[i] = m_node(id,i);
            }
				
		void setNodeList( int id, NodeList node_list ){
			int n = std::min( (int) node_list.size(), this->nNodes(id) );
			for (int i = 0; i < n; i++)
				m_node(id,i) = node_list[i];
			}	
			
		void getFaceList( int id, FaceList &face_list ){
			face_list.resize( this->nFaces(id) );
            for (int i = 0; i < this->nFaces(id); i++)
                face_list[i] = m_face(id,i);
            }
				
		void setFaceList( int id, FaceList face_list ){
			int n = std::min( (int) face_list.size(), this->nFaces(id) );
			for (int i = 0; i < n; i++)
				m_face(id,i) = face_list[i];
			}


		/*
        void setNodeID( std::vector<int> newNodeID, int icell )     
            {
            for (int i = 0; i < this->nNodes(icell); i++)
                m_node(icell, i) = newNodeID[i];
            }
            

        void getNodeList( int icell, std::vector<int> &node_list ){
                for (int i = 0; i < this->nNodes(icell); i++)
                    node_list[i] = m_node(icell,i);
                }

        void getFaceList( int icell, std::vector<int> &face_list ){
                for (int i = 0; i < this->nFaces(icell); i++)
                    face_list[i] = m_face(icell,i);
                }

    	void getFaceList( int icell, FaceList &face_list ){
			face_list = m_face(icell, m_rangeFaces );
			}  
			
		void setFaceList( int icell, FaceList face_list){
			m_face(icell, m_rangeFaces) = face_list;
			}      
			
		void getNodeList( int icell, NodeList &node_list ){
			node_list = m_node(icell, m_rangeNodes );
			} 
			 
		void setNodeList( int icell, NodeList node_list ){
			m_node(icell, m_rangeNodes ) = node_list;
			} 
		*/

    	int calcGlobalElementType( std::vector<int> nElementType);
        void fillGlobal( std::vector<CellThread> cellThread);
        void fillLocal( std::vector<CellThread> cellThread){ 
            std::cout << "FaceThread::extractFromGlobal no implemented" << std::endl; }

        void setFaceFromGlobal( intArray_2 cellConnect );
        //void reorder_3D( intArray_2 face_NodeID, intArray_1 face_nNodes, int icell);
        void reorder_3D( std::vector<int> face_nodeList[] ,std::vector<int> face_nNodes , int icell);

        void reorderFaceID( std::vector<int> newFaceOrder, int icell )
            {
            intArray_1 old_face = m_face( icell, blitz::Range::all() );

            for (int i = 0; i < this->nFaces(icell); i++)
            m_face(icell, i) = old_face( icell, newFaceOrder[i] );
            }
		
        void allocateArrays() { 
                m_node.resize( m_nCells, m_maxNodes );
                m_face.resize( m_nCells, m_maxFaces ); }
	
	private:
	
        int m_zone;
        int m_first;
        int m_last;
        int m_bcType;   // fluid = 0, solid = 1
        int m_elementType;
        int m_nDims;
        int m_nCells;
        int m_maxNodes;
        int m_maxFaces;

        /* could be replaced with nested heterogeneous arrays  */        				
		intArray_1 m_cellElementType; /* only used with heterogeneous arrays */
		
		/* these are only needed when converting to alternate connectivity ordering */
      	intArray_2 m_face;   
        intArray_2 m_node;
		/*  these are not strictly required, calculate on the fly */    		  
        intArray_1 m_nodesPerCell;
        intArray_1 m_facesPerCell;        
		
		blitz::Range m_rangeFaces;
		blitz::Range m_rangeNodes;
};

class VariableTag
{
    public:
        VariableTag(FluentIO *infile);

    private:
        int m_zone;
        

};

class NodeFlag
{
    public:
        NodeFlag(FluentIO *infile);

    private:
        int m_zone;
        int m_size;
        intArray_1 m_flags;
};

/*
    works with both Face and Cell trees
*/
class ElementTree
{

    public:
        ElementTree(FluentIO *infile, bool isFaceTree );
        int nParents() { return m_nParents; }
        int nKids(int i){ return m_kidsPerParent(i); }
        int parentID(int i){ return m_first + i; }
        void getKidList( int id, std::vector<int> &kid_list ){
			kid_list.resize( this->nKids(id) );
            for (int i = 0; i < this->nKids(id); i++)
                kid_list[i] = m_kid(id,i);
            }
    private:

        int m_first;
        int m_last;
        int m_parentZone;
        int m_kidZone;
        //int m_type;   
        bool m_isFaceTree;
        int m_nParents;
        int m_maxKids;
        intArray_2 m_kid;
        intArray_1 m_kidsPerParent;
};


class FaceParents
{
    public:
        FaceParents(FluentIO *infile);

    private:
        int m_first;
        int m_last;
        int m_nKids;
        intArray_2 m_parent;
};

class Partition
{
    public:
        Partition(FluentIO *infile);

    private:
        int m_zone;
        int m_first;
        int m_last;
        int m_nPartitions;
        int m_nCells;
        intArray_1 m_partition;

};

class GridSize
{
    public:
        GridSize(FluentIO *infile);

    private:
        int m_nElements;  /* should this be cells (typo in fluent manual) ? */
        int m_nFaces;
        int m_nNodes;

};

class SegData
{
    public:
        SegData(FluentIO *infile);
        const int nElements(){ return m_nElements; }
        const int nVars(){ return m_nVars; }
        const int zoneID(){ return m_zone; }
        const int varID(){ return m_varId; } 
        const int firstID(){ return m_first; }
        const int lastID(){ return m_last; }
        double data(int ivar, int i){ return m_data(ivar,i); }
        doubleArray_1 data(int ivar){ return m_data( ivar, 
            blitz::Range::all() ); }      
        doubleArray_1 dataVector( int i){ return m_data( blitz::Range::all(), i) ; }
        doubleArray_2 data(){ return m_data; }
	    void textDump(FILE *file);

    private:
        int m_varId;
        int m_zone;
        int m_nVars;    
        int m_nTimeLevels;
        int m_nPhases;
        int m_first;
        int m_last;
        int m_nElements;

        doubleArray_2 m_data;
};

class SegResiduals
{
    public:
        SegResiduals(FluentIO *infile);

    private:
        int m_nVars;
        int m_nResids;
        int m_residId;

        doubleArray_2 m_data;
};

/* conversion */
void faceToCell(FaceThread *face, CellThread *cell, std::vector<ElementTree> face_tree);
void cellToFace(CellThread *cell, FaceThread *face);

}

#endif

/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#include "fluentObjects.h"
#include <string>
#include <vector>
#include "setOperators.hpp"
#include "reorder.h"
// #include "setOperators.hpp"
#include "blitz/array.h"
#include "var_defs.h"

namespace FluentReader {

/* move these to "var_defs.cpp" */
const int c_cellsPerFace = 2;
const int c_leftCell = 1;
const int c_rightCell = 0;
const int c_nFaceElementTypes = 5;
const int c_nodesPerFace[c_nFaceElementTypes] = {-1,-1,2,3,4};
const std::string c_faceElementNames[] = {"mixed","invalid","linear","tri","quad"};
const int c_nCellElementTypes = 7;
const int c_nodesPerCell[c_nCellElementTypes] = {-1,3,4,4,8,5,6};
const int c_facesPerCell[c_nCellElementTypes] = {-1,3,4,4,6,5,5};
const std::string c_cellElementNames[c_nCellElementTypes] = 
    {"mixed","tri","tet","quad","hex","pyramid","wedge"};
const int c_parentsPerKid = 2;
const int c_maxNodesPerFace = 4;
const int c_nodesPerEdge = 2;

/*
   String elemenetNames[] = 
        {"Mixed","Tri","Tet","Quad","Hex","Pyramid","Wedge"};
    int[] nodesPerCell[] = {0, 3, 4, 4, 8, 5, 6};
    int[] facesPerCell[] = {0, 3, 4, 4, 6, 5, 5};
*/


NodeThread::NodeThread(FluentIO *infile)
{    
    infile->findChar(OPEN_PAR);
    m_zone = infile->getInt();
    m_first = infile->getHex() - 1;
    m_last = infile->getHex() - 1;
    m_type = infile->getInt();
    m_nDims = infile->getInt();
    m_nNodes = (m_last - m_first + 1);
 
    printf("Reading node thread\n");
    printf("\tmax Nodes = %i\n", m_nNodes );
    printf("\tmax First, Last = %i, %i\n", m_first, m_last );
    fprintf(infile->infoFile(),"\t%s\t%s\t%s\t%s\t%s\n",
                 "zone-id","first","last","type","nd");
    fprintf(infile->infoFile(),"\t%i\t%i\t%i\t%i\t%i\n",
                 m_zone,m_first,m_last,m_type,m_nDims);             
    if (infile->nDims() != m_nDims ) 
        printf(" -- problem --, incompatible dimensions in node zone %i",m_zone);

    if (m_zone == 0)
        {

        }
    else
        {
        int size = m_nDims*m_nNodes;
        //double *array = infile->readRealArray(size);
        m_coords.resize( m_nNodes, m_nDims);
        m_coords = 0.0;
        infile->readRealArray(size, m_coords.dataFirst() );
        /*    
        for (int i = 0; i < m_nNodes; i++) {
            for (int j = 0; j < m_nDims; j++) m_coords(i,j) = array[i*m_nDims + j];            
            }
        } 
        */ 
        //std::cout << m_coords;
        }         
        
}


int FaceThread::calcMaxNodesPerFace(int **array, int i_size)
{
    int maxNodes = 0;
    for (int i = 0; i < m_nFaces; i++)
        {
        if ( maxNodes < array[i][i_size]) maxNodes = array[i][i_size];
        }
    return maxNodes;
}

int FaceThread::calcMaxNodesPerFace(intArray_2 array, int i_size)
{
    return blitz::max( array( blitz::Range::all() , 0) );
}


FaceThread::FaceThread(FluentIO *infile)
{    
    infile->findChar(OPEN_PAR);
    m_zone = infile->getHex();
    m_first = infile->getHex() - 1;
    m_last = infile->getHex() - 1;
    m_bcType = infile->getHex();   

    m_nFaces = (m_last - m_first + 1);
    fprintf(infile->infoFile(),"\t%s\t%s\t%s\t%s\t%s\n",
                 "zone-id","first","last","bc_type","zone_element_type");
   
	/* the zone with an id = 0 is an informational zone which gives the global size of the array */    
    if (m_zone == 0)
        {
        fprintf(infile->infoFile(),"\t%i\t%i\t%i\t%i\n",
                              m_zone,m_first,m_last,m_bcType);  
                              
        m_cell.resize(0,0);
        m_node.resize(0,0);
        m_nodesPerFace.resize(0);
        m_maxNodes = 0;        
        
        }
    else
        {
        m_elementType = infile->getInt();

        fprintf(infile->infoFile(),"\t%i\t%i\t%i\t%i\t%i\n",
                              m_zone,m_first,m_last,m_bcType,m_elementType);    
        // m_cell = new intArray_2(m_nFaces, c_cellsPerFace ); 
        m_cell.resize( m_nFaces, c_cellsPerFace );
        m_cell = 0;
        if (m_elementType == 0)
            {
            printf("\tHeterogeneous Face\n");
            //fprintf(infile->infoFile(),"\t%s\t%s\t%s\t%s\n",
            //          "element_type","vertex(1..n)","right_cell","left_cell");                  
			/* read in a imhomogeneous array */         
            /*  max number of face per node */   
            intArray_2 array = infile->readIntArray2(m_nFaces, 
                1 + c_maxNodesPerFace + c_cellsPerFace, c_cellsPerFace);
            m_maxNodes = calcMaxNodesPerFace( array, 0);            
            printf("\tmax Nodes = %i\n",m_maxNodes);       
            m_node.resize(m_nFaces, m_maxNodes );
            m_node = 0;
            m_nodesPerFace.resize( m_nFaces); 
            
            for (int i = 0; i < m_nFaces; i++)
                {
                printf("\tmoving face = %i \n",i);
                m_nodesPerFace(i) = array(i,0); // array[i][0];
                for (int j = 0; j < m_nodesPerFace(i); j++)
                    {
                    m_node(i, j) = array(i, j + 1); // array[i][j + 1];
                    printf("\tmoving node = %i\n",j);
                    }                
                m_cell(i, c_leftCell)  = array(i, c_leftCell + m_nodesPerFace(i) + 1); // array[i][c_leftCell  + m_nodesPerFace(i) + 1];
                m_cell(i, c_rightCell) = array(i, c_rightCell + m_nodesPerFace(i) + 1); //array[i][c_rightCell + m_nodesPerFace(i) + 1];
                }
            
            printf("\tmax Nodes = %i\n", blitz::max( m_nodesPerFace) ); 
            //std::cout << "face_nodePerFace = " << m_nodesPerFace << std::endl;           
            }
        else 
            {
           //fprintf(infile->infoFile(),
            //    "\t%i\n\tvertex(1..n) \tright_cell\tleft_cell\n",m_elementType);   
            
             // m_nodesPerFace = new Blitz::intArray_2(1,1);
            m_nodesPerFace.resize( 1 );
            m_nodesPerFace(0) = c_nodesPerFace[ m_elementType ];
            m_maxNodes = c_nodesPerFace[ m_elementType ];
            m_node.resize( m_nFaces, m_maxNodes );
            m_node = 0;
            int size = (m_maxNodes + c_cellsPerFace )*m_nFaces;   
			/* array is homogeneous so read as a single vector */
            int *array = infile->readIntArray(size);
            int pos = 0;
			/* split array into components */
            for (int i = 0; i < m_nFaces; i ++ )                    
                {                
                for (int j = 0; j < m_maxNodes ; j++ )
                    {
                    m_node(i, j) = array[pos + j];
                    }
                m_cell(i, c_leftCell)  = array[pos + c_leftCell  + m_maxNodes];
                m_cell(i, c_rightCell) = array[pos + c_rightCell + m_maxNodes];
                pos = pos + (m_maxNodes + c_cellsPerFace);
                }
            }
            std::cout << "faces = " << m_nFaces << " id = " << m_zone << std::endl;
            //std::cout << "face_cell = " << m_cell << std::endl;
            //std::cout << "face_node = " << m_node << std::endl;
            printf("\tmax Nodes = %i\n", blitz::max( m_nodesPerFace) );
            printf("\tmax Cell id = %i\n", blitz::max( m_cell) );
            printf("\tmax Face id = %i\n\n", blitz::max( m_node) );
            
        }
        
    printf("Face Thread Completed\n");
    m_node = m_node - 1;
    m_cell = m_cell - 1;
}


int FaceThread::calcGlobalElementType(std::vector<int> nElementType)
{
    //if (m_nDims == 2) 
    //    return (int) LINEAR;
   
    if (nElementType[MIXED] > 0)
        {
        return (int) MIXED;
        }
    else
        {       
        if ( nElementType[TRI] > 0 )
            {
            if ( nElementType[QUAD] > 0 )
                {
                return (int) MIXED;
                }
            else
                {
                return (int) TRI;
                }
            }
        else
            {
            return (int) QUAD;        
            }        
        }    
}

int FaceThread::calcMaxNodesPerFace(int element_type)
{

    if (m_nDims == 2)
        {
        return c_nodesPerFace[ LINEAR ];
        }
        
    if (element_type == MIXED)        
        return c_nodesPerFace[ QUAD ];
    else        
        return c_nodesPerFace[ element_type ];  

}

void FaceThread::fillGlobal( std::vector<FaceThread> faceThread)
{
    /* calculate number of cells */
    int nFaces = 0;
    int maxNodes = 0;
  
    if ( this->zoneID() != 0 )
        {
        std::cout << "Error: FaceThread::fillGlobal, zoneID = " << this->zoneID() << 
            "is not a global cell thread" << std::endl;
        return;
        }

    /*
        1) count total numbers of faces
    */        
    for (int i = 0; i < faceThread.size(); i++)        
        if ( faceThread[i].zoneID() > 0 ) nFaces = nFaces + faceThread[i].nFaces();
    if (nFaces != this->nFaces() ) {
        std::cout << "Error: FaceThread::fillGlobal, list has more faces than global\n";
        m_nFaces = nFaces;
        }

    /* 
        2) determin element types  
    */
    std::vector<int> nElementType( c_nFaceElementTypes ,0);
    for (int i = 0; i < faceThread.size(); i++)        
        if ( faceThread[i].zoneID() > 0 ) nElementType[ faceThread[i].elementType()  ]++;            
            
    /*  
        Use element type to determine home many nodes 
            - only needed for 3D
            - assumes no invalid elements
    */

    m_elementType = calcGlobalElementType(nElementType);

    /* determine number of nodes from element type */
    //m_maxNodes = calcMaxNodesPerFace(m_elementType);
    m_maxNodes = 0;
    for (int i = 0; i < faceThread.size(); i++)
        if ( faceThread[i].zoneID() > 0 ) m_maxNodes = std::max( m_maxNodes, faceThread[i].maxNodes() );
   
    /*
        fill arrays
    */
    m_node.resize( m_nFaces, m_maxNodes );
    m_cell.resize( m_nFaces, c_cellsPerFace );
    m_nodesPerFace.resize(m_nFaces);
  
    for (int i = 0; i < faceThread.size(); i++)
        {        
        if ( faceThread[i].zoneID() > 0 )
            {            
            blitz::Range faceRange(faceThread[i].firstID(), faceThread[i].lastID() );
            blitz::Range nodeRange(0, faceThread[i].maxNodes() );
            if (faceThread[i].elementType() == MIXED)
                m_nodesPerFace( faceRange ) = faceThread[i].nodesPerFace();
            else
                m_nodesPerFace( faceRange ) = faceThread[i].maxNodes();
                
            m_node( faceRange, blitz::Range::all() ) = faceThread[i].nodes();
            m_cell( faceRange, blitz::Range::all() ) = faceThread[i].cells();           
            }           
        }


}

 

CellThread::CellThread(FluentIO *infile)
{    
    infile->findChar(OPEN_PAR);
    m_zone = infile->getHex();
    m_first = infile->getHex() - 1;
    m_last = infile->getHex() - 1;
    m_bcType = infile->getHex();   
    m_nCells = (m_last - m_first + 1);

    fprintf(infile->infoFile(),"\t%s\t%s\t%s\t%s\t%s\n",
                 "zone-id","first","last","bc_type","zone_element_type");
    fprintf(infile->infoFile(),"\t%i\t%i\t%i\t%i\t",
                              m_zone,m_first,m_last,m_bcType);    
    m_maxNodes = 0;
    m_maxFaces = 0;
    //std::cout << "Cell Dump " << m_zone << std::endl;

    if (m_zone == 0) {
        m_maxNodes = 0;
        m_maxFaces = 0;
        fprintf(infile->infoFile(),"\n");
        }
    else {
        m_elementType = infile->getInt();
        fprintf(infile->infoFile(),"%i\n",m_elementType);
        //std::cout << "MIXED type = " << MIXED << std::endl;
        //std::cout << m_elementType << std::endl;
        if (m_elementType == MIXED)  {
            int *data = infile->readIntArray(m_nCells);
            m_cellElementType.resize( m_nCells  );           
            m_cellElementType = 0;
            for (int i = 0; i < m_nCells; i++)  {
                m_cellElementType(i) = data[i];
                m_maxNodes = std::max( m_maxNodes,c_nodesPerCell[ m_cellElementType(i) ] ); 
                m_maxFaces = std::max( m_maxFaces,c_facesPerCell[ m_cellElementType(i) ] );
                std::cout << i << " " << data[i] << " " << m_cellElementType(i) << std::endl;         
                }
            }
        else
            {
            std::cout << m_elementType << std::endl;   
            m_cellElementType.resize(1);
            m_cellElementType = m_elementType;
            m_maxNodes = c_nodesPerCell[ m_elementType ];
            m_maxFaces = c_facesPerCell[ m_elementType ];
            }
        }

    //m_node = m_node - 1;
    //m_face = m_face - 1;

    /*
    std::cout << "MIXED type = " << MIXED;
    std::cout << m_cellElementType << endl;
    std::cout << m_elementType << endl;
    */
}

int CellThread::calcGlobalElementType(std::vector<int> nElementType)
{

    //for (int i = 0; i < nElementType.size(); i++) {
    //    std::cout << "elment type coungin = " << i << " " << nElementType[i] << std::endl;
    //    }

    if (nElementType[MIXED] > 0)
        return (int) MIXED;

    int numberOfElementTypes = 0; /* number of element types in the thread */

    for (int i = 0; i < nElementType.size(); i++) {
        if (nElementType[i] > 0) numberOfElementTypes++;
        //std::cout << "elment type coungin = " << nElementType[i] << " " << numberOfElementTypes << std::endl;
        }


    if (numberOfElementTypes > 1)
        return (int) MIXED;
 
    if (numberOfElementTypes < 1)
        {
        std::cout << "Error: calcGlobalElementType, numberOfElementTypes = " << numberOfElementTypes << std::endl;
        return (int) MIXED;
        }
    
    for (int i = 0; i < nElementType.size(); i++)        
        if (nElementType[i] > 0) return i;       
}

void CellThread::fillGlobal( std::vector<CellThread> cellThread)
{
    /* calculate number of cells */
    int nCells = 0;
    int maxFaces = 0;
    int maxNodes = 0;
    std::vector<int> nElementType( c_nCellElementTypes ,0);

    if ( this->zoneID() != 0 )
        {
        std::cout << "Error: CellThread::fillGlobal, zoneID = " << this->zoneID() << 
            "is not a global cell thread" << std::endl;
        return;
        }

    std::cout << " looking at element types " << std::endl;       
    for (int i = 0; i < cellThread.size(); i++) {        
        if ( cellThread[i].zoneID() > 0 )
            {
            nCells = nCells + cellThread[i].nCells();
            maxFaces = std::max(maxFaces, cellThread[i].maxFaces() );
            maxNodes = std::max(maxNodes, cellThread[i].maxNodes() );                    
            nElementType[ cellThread[i].elementType() ]++;
            int k = cellThread[i].elementType();
            //std::cout << i <<  " " << k << " " << nElementType[k] << std::endl;
            }
        }

    if (nCells != this->nCells() )
        {
        std::cout << "Error: CellThread::fillGlobal, list has more cells than global" << std::endl;
        m_nCells = nCells;  
        }

    m_elementType = calcGlobalElementType(nElementType);
    m_maxNodes = maxNodes;
    m_maxFaces = maxFaces;

    /*
        fill arrays
    */
    m_node.resize( m_nCells, m_maxNodes );
    m_face.resize( m_nCells, m_maxFaces );
    m_node = -1;
    m_face = -1;

    for (int i = 0; i < cellThread.size(); i++)
        {        
        if ( cellThread[i].zoneID() > 0 )
            {            
            blitz::Range cellRange(cellThread[i].firstID(), cellThread[i].lastID() );
            if ( cellThread[i].hasNodes() )
                m_node( cellRange, blitz::Range::all() ) = cellThread[i].getNodes();
            if ( cellThread[i].hasFaces() )
                m_face( cellRange, blitz::Range::all() ) = cellThread[i].getFaces();           
            }           
        }

    std::cout << "Error: CellThread::fillGlobal, m_elementType == MIXED" << (m_elementType == MIXED)
            << " " << m_elementType << " " << m_nCells << std::endl;

     if (m_elementType == MIXED) {
         m_cellElementType.resize( m_nCells ); 
        
        for (int i = 0; i < cellThread.size(); i++) {  
            //std::cout << i << " of "  << cellThread.size() << " id = " << cellThread[i].zoneID() << std::endl;        
            if ( cellThread[i].zoneID() > 0 ){                  
                blitz::Range cellRange(cellThread[i].firstID(), cellThread[i].lastID() );                
                //std::cout << i << "  " << cellRange << std::endl;
                //std::cout << i << "  " << cellThread[i].getElementType() << std::endl;
                /*
                for (int j = 0; j < cellThread[i].nCells(); j ++ )
                    m_cellElementType(j + cellThread[i].firstID() ) =  cellThread[i].elementType(j);   
     
                */
                if ( cellThread[i].elementType() == MIXED )
                    m_cellElementType(cellRange) = cellThread[i].getElementType();  //does not seem to work
                else
                    m_cellElementType(cellRange) = cellThread[i].elementType();
                
                }           
            }
        }
    else{
        m_cellElementType.resize( 1 );
        m_cellElementType = m_elementType;
        }

    /*
    std::cout << "elem_types = " << m_cellElementType << endl;
    */
}

void CellThread::setFaces( intArray_2 faceConnect, int firstID )
{
    intArray_2 inverse;
    MapOperators map_oper; 
    //map_oper.invertMapRestrict( faceConnect, firstID, inverse, m_first, m_last );    
    m_face = inverse;
}

void CellThread::setFaces( intArray_2 cellConnect )
{
    std::cout << "CellThread::setFaces" << std::cout << std::endl;
    std::cout << cellConnect.shape() << std::endl;
    std::cout << m_face.shape() << std::endl;
    std::cout << m_maxFaces << std::endl;
    m_face ( blitz::Range( m_first, m_last), blitz::Range::all()) = cellConnect;
}

void CellThread::setFaceFromGlobal( intArray_2 cellConnect )
{
    m_face = cellConnect( blitz::Range( m_first, m_last), blitz::Range::all() );
}

void swap( std::vector<int> A, int i, int j)
{
    if (i < 0 || j < 0 || i >= A.size() || j >= A.size() )
        std::cout << "called swap with invalid argument" << std::endl;

    int temp = A[i];
    A[i] = A[j];
    A[j] = temp;
}


/*
    requires a list of node list for each face
*/

void reorder_3D( std::vector<FaceThread> face_thread, CellThread &cell)
{
    const int max_faces = c_facesPerCell[CellThread::HEX];
    const int max_nodes = c_nodesPerFace[FaceThread::QUAD];
    std::vector<int> face_nodeList[ max_faces ];
    std::vector<int> face_nNodes( max_faces );

    for (int iface = 0; iface < max_faces; iface++ )
        {
        face_nodeList[iface].resize( max_nodes );
        }

    for (int icell = 0; icell < cell.nCells(); icell++)
        {
        
        for (int iface = 0; iface < cell.nFaces(icell); iface++)
            {
            int face_id = cell.face(icell, iface);
            int ithread = face_thread[0].findThread( face_thread, face_id );
            if (ithread >= 0)
                {
                int jface = face_id - face_thread[ithread].firstID();        
                face_thread[ithread].getNodeList( jface, face_nodeList[iface] );
                face_nNodes[iface] = face_thread[ithread].nNodes(jface);
                }
            }    

        }
}

VariableTag::VariableTag(FluentIO *infile)
{
    char c;    
    infile->findChar(OPEN_PAR);
    m_zone = infile->getInt(); 
    fprintf(infile->infoFile(),"\t%s\t%s\t%s\n","zone_id","zone_type","name");
    fprintf(infile->infoFile(),"\t%i\t'",m_zone);
    
    while ((c = infile->nextChar()) != CLOSE_PAR)
        {              
        infile->writeIndent(c);
        if (c == ' ') fprintf(infile->infoFile(),"',\t'");
        fprintf(infile->infoFile(),"%c",c);                            
        }
    fprintf(infile->infoFile(),"'\n");
}



NodeFlag::NodeFlag(FluentIO *infile)
{
    infile->findChar(OPEN_PAR);
    m_zone = infile->getInt(); 
    m_size = infile->getHex();
    fprintf(infile->infoFile(),"\t%s \n\t%i\t%i\n",
                            "zone-id\tsize",m_zone,m_size);
    fprintf(infile->infoFile(),"\tflags\n");
    intArray_1 array( infile->readIntArray(m_size), blitz::shape(m_size) );
    m_flags.reference( array );
}


/*
    works with both face trees and cell trees
*/
const int c_maxChildPerParent = 8;
ElementTree::ElementTree(FluentIO *infile, bool isFaceTree )
{
    infile->findChar(OPEN_PAR);
    m_first = infile->getHex() - 1;
    m_last = infile->getHex() - 1;
    m_parentZone = infile->getHex();
    m_kidZone = infile->getHex();
    //m_type = infile->getHex();   
    m_isFaceTree = isFaceTree;
    m_nParents = (m_last - m_first + 1);

    fprintf(infile->infoFile(),"\t%s \n\t%i\t%i\t%i\t%i\n",
                           "\tfirst\tlast\tparent_id\tchild_id",
                           m_first,m_last,m_parentZone,m_kidZone);
    fprintf(infile->infoFile(),"\tnkids\tkid_id(1..nkids)\n");


    //std::cout << "Tree = " << m_nParents << std::endl;
    //std::cout << m_first << " " << m_last << " " << m_parentZone << " " << m_kidZone << std::endl;
    //int **array = infile->readIntArray(m_nParents, 0);
    intArray_2 array = infile->readIntArray2(m_nParents, 
                1 + c_maxChildPerParent, 0);
    m_maxKids = 0;
    m_maxKids = blitz::max( array( blitz::Range::all(), 0));   
    
    printf("MaxKids = %i\n",m_maxKids);
    m_kid.resize( m_nParents, m_maxKids ); 
    m_kid = 0;       
    m_kidsPerParent.resize( m_nParents ); 
 
    m_kidsPerParent = blitz::max( array( blitz::Range::all(), 0) );
        
    for (int i = 0; i < m_nParents; i++)
        {
        //printf("\tParent = %i\n",i);
        //m_kidsPerParent(i) = array[i][0];
        for (int j = 0; j < m_kidsPerParent(i); j++ )
            {
            //m_kid(i,j) = array[i][j+1];
            m_kid(i,j) = array(i,j+1);
            //printf("\tKid = %i\n",j);
            }
        }

    m_kid = m_kid - 1;
}

FaceParents::FaceParents(FluentIO *infile)
{
    infile->findChar(OPEN_PAR);
    m_first = infile->getHex() - 1;
    m_last = infile->getHex() - 1;
    m_nKids = m_last - m_first + 1;
 
    fprintf(infile->infoFile(),"\t%s \n\t%i\t%i\n",
                           "\tfirst_child_id\tlast_child_id",
                           m_first,m_last);
    fprintf(infile->infoFile(),"\tright_parent_id\tleft_parent_id\n");
    
    int *array = infile->readIntArray( m_nKids * c_parentsPerKid );
    m_parent.resize( m_nKids, c_parentsPerKid );
    m_parent = 0;
    for (int i = 0; i < m_nKids; i++) {        
        for (int j = 0; j < c_parentsPerKid; j++ )
            m_parent(i,j) = array[i*c_parentsPerKid + j];
        }

    m_parent = m_parent - 1;
}    

Partition::Partition(FluentIO *infile)
{
    infile->findChar(OPEN_PAR);
    m_zone = infile->getHex();
    m_first = infile->getHex() - 1;
    m_last = infile->getHex() - 1;
    m_nPartitions = infile->getHex();
    m_nCells = (m_last - m_first + 1);
    intArray_1 array( infile->readIntArray(m_nCells), blitz::shape( m_nCells) );
    m_partition.reference(array);

    fprintf(infile->infoFile(),"\t%s \n\t%i\t%i\t%i\t%i\n",
                           "\tzone_id\tcell_id\tcell_id\tn_partitions",
                           m_zone,m_first,m_last,m_nPartitions);
    fprintf(infile->infoFile(),"\tcell_partition\n");          
}

GridSize::GridSize(FluentIO *infile)
{
    infile->findChar(OPEN_PAR);
    m_nElements    = infile->getInt();
    m_nFaces       = infile->getInt();
    m_nNodes       = infile->getInt(); 
    fprintf(infile->infoFile(),"\t%s\t%s\t%s\n \t%i\t%i\t%i\t\n",
                           "n_elem","n_faces","n_nodes",
                               m_nElements,m_nFaces,m_nNodes);  
}

SegData::SegData(FluentIO *infile)
{
    infile->findChar(OPEN_PAR);
    m_varId = infile->getInt();
    m_zone = infile->getInt();
    m_nVars = infile->getInt();             /* size: number variable per element */
    m_nTimeLevels = infile->getInt();
    m_nPhases = infile->getInt();
    m_first = infile->getInt() - 1;
    m_last = infile->getInt() - 1;
    m_nElements = (m_last - m_first + 1);  /* number of faces or cells */

    printf(" [zone_id = %4i] , [var =  %3i] ",m_zone,m_varId);            
    int size = m_nVars*m_nElements;

    fprintf(infile->infoFile(),"\t%s\t%s\t%s\t%s\t%s\t%s\t%s\n",
                "var_id","zone_id","n_data","n_time","n_phase","id_1","id_n");               
    fprintf(infile->infoFile(),"\t%i\t%i\t%i\t%i\t%i\t%i\t%i\n",
                m_varId,m_zone,m_nVars,m_nTimeLevels,m_nPhases,m_first,m_last);
    fprintf(infile->infoFile(),"\t%s","data(1..n_data)\n");                         
    
    doubleArray_2 array( infile->readRealArray(size), blitz::shape(m_nElements, m_nVars) );
    m_data.reference( array );    
}


SegResiduals::SegResiduals(FluentIO *infile)
{

    infile->findChar(OPEN_PAR);
    m_nResids = infile->getInt();       /* n */
    m_residId = infile->getInt();       /* residual subsection id */
    m_nVars = infile->getInt();         /* size */

    int size = m_nVars*m_nResids;

    doubleArray_2 array( infile->readRealArray(size), blitz::shape(m_nResids, m_nVars) );
    m_data.reference( array ); 

    
    fprintf(infile->infoFile(),"\t%s\t%s\t%s\n",
                "length","residual_id","variables");               
            fprintf(infile->infoFile(),"\t%i\t%i\t%i\n",
                m_nResids,m_residId,m_nVars);
 

}
           

/* 
    The following routines are be used to convert between two
    different connectivity methods.
    The threads are not required to be in the same or any any case.
    However, case should be taken for ID number are     
*/
/*
    converts "face based" face connectivity (face->cell) to
    "cell based" connectivity (cell->face) for any two threads

    note: the routines removes any connectivity information 
        which does not pertain to CellThread which is recieving the map.

    thus faceToCell(face, cell) and cellToFace(cell, face) are only
    inverses when face only contains cell id's corresponding to the
    id's in the cellThread.
        
*/

/* should these be members of face or cell */
#include "stdio.h"
#include <algorithm>
/* this is used by the below algorithm, with a loop over the element trees, instead of starting with
    a loop over all the cells. In this algorithm, hopefully we loop only over those cells for which
    there are hanging node elements.  */

void fixHangingNodes(ElementTree face_tree, FaceThread face, intArray_2 inverse, intArray_1 inverse_count)
{
    /* loop over parent faces */
    for (int j = 0; j < face_tree.nParents(); j++) {
        FaceList kid_list;
        face_tree.getKidList(j, kid_list);
        /* loop over kids faces - should actually find on first pass */
        bool foundCell = false;
        for (int kid = 0; kid < kid_list.size() && !foundCell ; kid++) {
            CellList cell_list;
            face.getCellList(kid, cell_list);
            /* loop over cells on each face */
            for (int i = 0; i < cell_list.size() && !foundCell ; i++) {
                if (cell_list[i] < 0); break;
                FaceList face_list;                          
                for (int k = 0; k < inverse_count( cell_list[i] ); k++) {
                    face_list.insert( face_list.end(), inverse(cell_list[i] ,j) );         
                /* if all kid faces are a subset of the faces on a cell then replace with the parent */
                    if ( std::includes(  face_list.begin(),face_list.end(),
                                      kid_list.begin(), kid_list.end() )) {
                        std::remove( face_list.begin(), face_list.end(), kid_list[kid] );
                        face_list.insert( face_list.end(), face_tree.parentID(j) );
                        foundCell = true;
                        break;
                        }
                    }
                }
            }
        }               

    /* exiting loops based on foundCell variable will save generation of lists, it should not
        effect the algorithm for a consistent data set. */
}



void faceToCell(FaceThread *face, CellThread *cell, vector<ElementTree> face_tree)
{
    blitz::Array<int,2> inverse;
    blitz::Array<int,1> inverse_count;
    MapOperators map_oper;

    std::cout << "faceToCell" << std::endl;       
    std::cout << "zone ID = " << cell->zoneID() << std::endl;
    std::cout << "face first ID = " << face->firstID() << std::endl;
    std::cout << "cell first ID = " << cell->firstID() << std::endl;
    std::cout << "cell last ID = " << cell->lastID() << std::endl;

    map_oper.invertMapRestrict( face->cells(), face->firstID(), 
        inverse, inverse_count, cell->firstID(), cell->lastID() );    
   
    //cell->setMaxFaces( inverse.extent( blitz::secondDim ) );
    //cell->setMaxNodes(1);
    //cell->allocateArrays();
    //cell->setFaces( inverse ); 
    std::cout << "Map\n";
    std::cout << blitz::min(face->cells()) << " " << blitz::max(face->cells()) << "\n";
    std::cout << "Inverse Map\n";
    std::cout << blitz::min(inverse) << " " << blitz::max(inverse) << "\n";
 
    /* account for hanging node grid */
    for (int i = 0; i < cell->nCells(); i++) {
        int need_to_find = inverse_count(i) - cell->nFaces(i);
        vector<int> face_list( inverse_count(i) );                          
        for (int j = 0; j < inverse_count(i); j++)
                face_list[j] = inverse(i,j);         
        if ( need_to_find > 0 ) { 
            vector<int> kid_list;
            for (int k = 0; k < face_tree.size(); k ++ ){
                ElementTree sub_tree = face_tree[k];
                for (int j = 0; j < sub_tree.nParents(); j++) {
                    sub_tree.getKidList(j, kid_list);
                    if ( std::includes( 
                        face_list.begin(),face_list.end(), 
                        kid_list.begin(), kid_list.end() )) {
                        for (int kid = 0; kid < kid_list.size(); kid++) 
                            std::remove( face_list.begin(), face_list.end(), kid_list[kid] );
                        face_list.insert( face_list.end(), sub_tree.parentID(j) );

                       }
                    }
                }                      
            }       
        cell->setFaceList( i, face_list );
        }

    //FILE *file = fopen("faceToCell.out","w");
    //face->textDump(file);
    //cell->textDump(file);    

    for (int i = 0; i < cell->nCells(); i++) {
        //std::cout << "cell_id = " << i << std::endl;
        if ( face->nDims() == 3) {
            reorderFace_3D( cell, face, i);
            reorderNode_3D( cell, face, i);
            }
        else {
            reorderFace_2D( cell, face, i);
            }
        }           
    //cell->textDump(file);
    std::cout << "nodes " << blitz::min(cell->nodes()) << " " << blitz::max(cell->nodes()) << "\n";
    std::cout << "faces " << blitz::min(cell->faces()) << " " << blitz::max(cell->faces()) << "\n";
    //fclose(file);
}
/*
    inverse of the above routine (not quite)
*/
void cellToFace(CellThread *cell, FaceThread *face)
{
    blitz::Array<int,2> inverse;
    MapOperators map_oper;
    // map_oper.invertMapRestrict( cell->getFaces(), cell->firstID(), 
    //    inverse, face->firstID(), face->lastID() );    
    face->setCell( inverse );
}


} /* end namespace FluentReader */

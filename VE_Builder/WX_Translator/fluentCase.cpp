/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#include <iostream>
#include "fluentIO.h"
#include "fluentObjects.h"
#include "fluentCase.h"
#include "blitz/array.h"
#include "setOperators.hpp"

namespace FluentReader {


int Case::getKeyword()
{
    int keyword_int = m_file->getKeyword();
    const int maxAsciiIndex = 1000;
    const int minDoubleIndex = 3000;
    const int minFloatIndex = 2000;
    const int maxDoubleIndex = 3999;

    if (keyword_int < maxAsciiIndex)
        {
        /* subsection keywords < 1000 refer to ascii data sections in a "Data" file but have other
            meanings in a "Case" file regardless of where the file contains ascii or binary data. */
        if ( !m_file->isAscii() )            
            std::cout << "this is a data section the file might not read correctly if the data is not ascii " << std::endl;
            //<< m_file->isBinary() << std::endl;                        
    
        return keyword_int;
        }

    if (keyword_int > maxDoubleIndex)
        {
        std::cout << 
        "section is not ascii, binary-float or binary double " << std::endl;                         
        }

    if (keyword_int >= minDoubleIndex)               
        {
        if ( !m_file->isBinary() )
            std::cout << "section might not read correctly fileType is not binary "  << std::endl;
    
        if ( !m_file->isDouble() )           
            std::cout << "section might not read correctly dataType is not double "  << std::endl;                        
        return keyword_int - minDoubleIndex;
        }


    if (keyword_int >= minFloatIndex)
        {
        if ( !m_file->isBinary() )
            std::cout << "section might not read correctly fileType is not binary "  << std::endl;
        if ( !m_file->isFloat() )           
            std::cout << "section might not read correctly dataType is not float "  << std::endl;                        
 
        return keyword_int - minFloatIndex;
        }    

    std::cout << 
        "section is not ascii, binary-float or binary double " << std::endl;                       
    return keyword_int - maxAsciiIndex;

}

int Case::readSection()
{   
    int keyword_int = this->getKeyword();
                    
/* each subsection has a different method of reading the meta-deta */    
/*  read parameter list */     

/* each subsection has a different method of reading the meta-deta */    
/*  read parameter list */                              

    printf("readSection: keyword = %i\n",keyword_int);
    switch(keyword_int)
        {
/*      index = 0 */
        case COMMENT:
            {  
            m_file->writeKeyword("COMMENT",keyword_int);            
            m_comment.insert( m_comment.end(), Comment(m_file) );
            }
            break;     
/*      index = 2 */
        case DIMENSION:
            { 
            m_file->writeKeyword("DIMENSION",keyword_int);                 
            m_nDims = m_file->getInt(); 
            m_file->setDims(m_nDims);
            m_file->prevChar();
            // m_file->setTreeLevel(0); 
            }
            break;
/*      index = 4 */
         case MACHINE_CONFIG:
            {
            m_file->writeKeyword("MACHINE CONFIGUATION",keyword_int); 
            }
            break;
/*      index = 10 */
         case NODE:
            {
            m_file->writeKeyword("NODE COORDINATES",keyword_int);
            NodeThread a(m_file);        
            m_nodeThread.insert( m_nodeThread.end(), a );         
            if ( a.zoneID() == 0) {
                m_globalNode = &a;
                m_nNodes = a.nNodes();
                }
            }
            break;            
/*      index = 13 */
         case FACE:
            {
            m_file->writeKeyword("FACE THREAD",keyword_int); 
            FaceThread a(m_file);    
            m_faceThread.insert( m_faceThread.end(), a );
            if ( a.zoneID() == 0 ) {
                m_globalFace = &a;
                m_nFaces = a.nFaces();
                }
            }
            break;            
/*       index = 12  */
         case CELL:
            {
            m_file->writeKeyword("CELL THREAD",keyword_int); 
            CellThread a(m_file);
            m_cellThread.insert( m_cellThread.end(), a );            
            if ( a.zoneID() == 0 ) {
                m_globalCell = &a;
                m_nCells = a.nCells();
                }
            }
            break;            
/*        index = 37  rampant variables */
         case RP_VAR:
            {
            fprintf(m_file->infoFile(),"'RAPANT VARIABLES', %i\n",keyword_int);      
            }
            break;
         case CX_VAR:
            {
            fprintf(m_file->infoFile(),"'CORTEX VARIABLES', %i\n",keyword_int);      
            }
            break;
/*        index = 39 */
         case RP_TV:
            {
            fprintf(m_file->infoFile(),"'VARIABLE TAGS', %i\n",keyword_int);
            m_variableTag.insert( m_variableTag.end(), VariableTag(m_file) );
            }
            break;
/*       index = 41 */
         case NODE_FLAG:
            {
            fprintf(m_file->infoFile(),"'NODE FLAGS', %i\n",keyword_int); 
            m_nodeFlag.insert( m_nodeFlag.end(), NodeFlag(m_file) );
            }                                 
            break;
/*       index = 59, p. 3-40 */
         case FACE_TREE:   
            {
            fprintf(m_file->infoFile(),"'FACE TREE', %i\n",keyword_int);
            m_faceTree.insert( m_faceTree.end(), ElementTree(m_file, true) ); 
            }
            break;
/*       index = 58, p. 3-41 */
         case CELL_TREE:   
            {
            fprintf(m_file->infoFile(),"'CELL TREE', %i\n",keyword_int);
            m_cellTree.insert( m_cellTree.end(), ElementTree(m_file, false) );
            }
            break;
/*       index = 61, p. 3-42 */
         case FACE_PARENTS:   
            {
            fprintf(m_file->infoFile(),"'FACE PARENTS', %i\n",keyword_int);
            m_faceParents.insert( m_faceParents.end(), FaceParents(m_file) );        
            }
            break;
/*       index = 40, p. 3-53 */
         case PARTITION:   
            {
            int partition_count;
            m_partition.insert( m_partition.end(), Partition(m_file) );
            fprintf(m_file->infoFile(),"'PARTITION',%i\n",keyword_int);               
            }
            break;
/*       index = 33, p. 3-53 */
         case GRID_SIZE:   
            {
            m_gridSize.insert( m_gridSize.end(), GridSize(m_file) );         
            fprintf(m_file->infoFile(),"'GRID SIZE',%i\n",keyword_int);                
            }
            break;
/*       index = 300, p. 3-53 */
         case RF_SEG_DATA:   
            {
            printf("---SegData---\n");
            fprintf(m_file->infoFile(),"'DATA', %i\t(cells/faces)\n",keyword_int);  
            m_segData.insert( m_segData.end(), SegData(m_file) );
            }
            break;
/*       index = 301 p. 3-55 */
        case RF_SEG_RESIDUALS:   
            {
            m_segResiduals.insert( m_segResiduals.end(), SegResiduals(m_file) );
            fprintf(m_file->infoFile(),"'RESIDUALS', %i\t(global)\n",keyword_int);
            }                  
            break;
        default:
            {
            fprintf(m_file->infoFile(),"'OTHER', %i\n",keyword_int);  
            break;
            }
        }

    return keyword_int;
}  

/* determine which threads are global thread ids */
/*
template<class Type>
int Case::findGenericGlobal( vector<Type> zone, int id )
{
    for (int i = 0; i < zone.size(); i++)
        if (zone[i]->zoneID() == id) return i;
    return -1;
}
*/
/*
    FLUENT file parser - main loop assumes files have been op

*/
void Case::read()
{
    m_file->setMaxTreeLevel(0);  
    m_file->setTreeLevel(0);
   
    int c;
    /* search for subsection ID's, 
        a subsection ID can be identitified by the following

            1) open parenthesis "("
            2) indent == 1

        indent is file global which indicates the tree level
    */            

    while(  (c = m_file->nextChar() ) != EOF )
        {
        m_file->writeIndent(c);
/*      if (isdigit(c)) fprintf(case_info->output_file,"    c is digit \n"); */
/*      if (('a' <= c) && (c <= 'z') ) fprintf(case_info->output_file,"    c is lowercase \n");
        if (('A' <= c) && (c <= 'Z') ) fprintf(case_info->output_file,"    c is uppercase \n"); */


        switch(c)
            {
            case NEWLINE:
                {
                m_file->writeLine();
                }
                break;
            case OPEN_PAR:
                {
                m_file->toChild();                         
                /*
                indent = indent + 1;
                if (max_index < indent) 
                    {
                    max_indent = indent;
                    }
                */      
		/* this should be a call to member of FLUENT IO */
                if (m_file->checkFile()) fprintf(m_file->checkFile(),"treeLevel = %i\n",m_file->treeLevel() );          
                if (m_file->treeLevel() == 1)
                    {
                    m_file->setMaxTreeLevel(1);
                    m_file->writeOpen();
                    this->readSection();
                    }
                }
                break;
            case CLOSE_PAR:
                {
                if (m_file->treeLevel() == 1) m_file->writeClose(); 
                m_file->toParent();
                }
                break;
            }                
     
        }   
}

void Case::calcCellConnectivity()
{
    int max_faces = c_facesPerCell[ CellThread::HEX ];
    int n_cells = this->nCells();

    /* Fill the cells face data with all the faces which are connected */
    intArray_2 cell2face_id( n_cells, max_faces ); 
    intArray_1 cell2face_filledFaces( n_cells );
     
    cell2face_id = -1;        
    cell2face_filledFaces = 0;
    int n = m_faceThread.size();
    for (int i = 0; i < n; ++i )
        {
        //int cell_ID = face_thread->getCells();
        FaceThread *face_thread = &m_faceThread[i];
        for (int iface = 0; iface < face_thread->nFaces();  iface++ ) 
            {
            int face_id = face_thread->firstID() + iface;            
            for (int icell = 0; icell < n_cells; icell++)
                {
                int cell_id = face_thread->cellID(iface, icell);
                if (cell_id > 0)
                    {
                    if ( cell2face_filledFaces(cell_id) + 1 < max_faces )
                        {
                        cell2face_filledFaces(cell_id)++;
                        int iface = cell2face_filledFaces(cell_id) + 1;                        
                        cell2face_id(cell_id,iface) = face_id;                    
                        }
                    else
                        {
                        printf("Error in Case::calcCellConnecvtivity()\n");
                        printf("\tface_thread = %i, face_id = %i\n",face_thread,face_id);
                        printf("\tface = %i of cell_id = %i\n",iface,cell_id);
                        }
                    /* endif cell2face_filledFaces */
                    }
                /* endif cell_id > 0 */
                } 
            /* end for icell */
            } 
        /* end for face_id */
        } 
    /* end for face_thread */



    /* Put connectivity in the proper locations inside of the face threads */

    for (int i = 0; i < m_cellThread.size();  i++ )
        {
        


        }
    /* end do cell_thread */


    /* Order the faces in the correct order as the nodes are placed */


}



/*

conversion between to connectivity-methods is as follows 
                      
   face->cell(local)  <=> face->cell(global) <=> cell->face(global) <=> cell->face(local)

*/

void printLocation( std::string loc, bool local, bool face)
{
    std::cout << "Switching to " << loc << std::endl;
    std::cout << "\tisConnectLocal = " << local << std::endl;
    std::cout << "\tisConnectFace = " << face << std::endl;
}

void Case::switchLocalToGlobal()
{
    printLocation( "LocalToGlobal:start", m_isConnectLocal, m_isConnectFace);
    if (!m_isConnectLocal) return;
    findGlobalThreads();

    /* some information must be transferred from both threads */
    m_globalFace->fillGlobal( m_faceThread ); 
    m_globalCell->fillGlobal( m_cellThread );

    m_isConnectLocal = false;
    printLocation( "LocalToGlobal:end", m_isConnectLocal, m_isConnectFace);
 
}
void Case::switchGlobalToLocal()
{
    printLocation( "GlobalToLocal", m_isConnectLocal, m_isConnectFace);

    if (m_isConnectLocal) return;
        findGlobalThreads();

     if (m_isConnectFace)
        m_globalFace->fillLocal( m_faceThread );
     else
       m_globalCell->fillLocal( m_cellThread );
     m_isConnectLocal = true;
}
void Case::switchFaceToCell()
{
    printLocation( "FaceToCell", m_isConnectLocal, m_isConnectFace);

    if (m_isConnectLocal)
        std::cout << "Error: Case::switchFaceToCell - called with local connectivity" << std::endl;        

    if (m_isConnectFace)
        faceToCell( m_globalFace, m_globalCell, m_faceTree); 
    m_isConnectFace = false;    
    printLocation( "FaceToCell", m_isConnectLocal, m_isConnectFace);
}
void Case::switchCellToFace()
{
    printLocation( "CellToFace", m_isConnectLocal, m_isConnectFace);

    if (m_isConnectLocal)
        std::cout << "Error: Case::switchCellToFace - called with local connectivity" << std::endl;
       
    if (!m_isConnectFace)
        cellToFace( m_globalCell, m_globalFace); 
    m_isConnectFace = true;     
}

void Case::toLocalFaceConnect()
{
    if (!m_isConnectFace) 
        toGlobalFaceConnect();
    if (!m_isConnectLocal)
        switchGlobalToLocal();
}

void Case::toGlobalFaceConnect()
{
    if (m_isConnectLocal)
        switchLocalToGlobal();   
    if (!m_isConnectFace)
        switchFaceToCell();
                                   
}

void Case::toGlobalCellConnect()
{
    if (m_isConnectLocal)
        switchLocalToGlobal();
    if (m_isConnectFace)
        switchFaceToCell();
}

void Case::toLocalCellConnect()
{
    if (!m_isConnectFace)
        toGlobalCellConnect();
    if (!m_isConnectLocal)
        switchGlobalToLocal();
}


void Case::findGlobalThreads()
{
    int n = m_cellThread.size();
    for (int i = 0; i < n; i++) {
        int id = m_cellThread[i].zoneID();
        if (id == 0)
            m_globalCell = &m_cellThread[i];
        }
    for (int i = 0; i < n; i++) {
        int id = m_faceThread[i].zoneID();
        if (id == 0)
            m_globalFace = &m_faceThread[i];
        }
    for (int i = 0; i < n; i++) {
        int id = m_nodeThread[i].zoneID();
        if (id == 0)
            m_globalNode = &m_nodeThread[i];
        }
}



    
} /* end namespace FluentReader */

/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#include <iostream>
#include <string>
#include "fluentCase.h"
#include "fluentObjects.h"

namespace FluentReader {

void NodeThread::textDump( FILE *file )
{
    fprintf(file,"id = %i\n",m_zone);
    fprintf(file,"nodes = %i\n",m_nNodes);
    fprintf(file,"dims = %i\n",m_nDims);

    if ( this->hasCoords() )
    {
    if (this->nDims() == 3)
        {
        for (int i = 0; i < this->nNodes(); i++)
            fprintf(file, "n%5i %12.5e  %12.5e  %12.5e\n",i,m_coords(i,0),m_coords(i,1));
        }
    else
        {
        for (int i = 0; i < this->nNodes(); i++)
            fprintf(file, "n%5i %12.5e  %12.5e\n",i,m_coords(i,0),m_coords(i,1),m_coords(i,2));
        }
    }
    fprintf(file,"\n");
}

void FaceThread::textDump( FILE *file  )
{
    fprintf(file,"id = %i\n",m_zone);
    fprintf(file,"faces = %i\nmax_nodes =%i\n",this->nFaces(),this->maxNodes());
    
    for (int i = 0; i < this->nFaces(); i++) {
        fprintf(file, "f%5i %i",i,this->nNodes(i));
        if (m_cell.size() > 0)
            fprintf(file, "  c%5i c%5i",m_cell(i,0),m_cell(i,1) );                
        for (int j = 0; j < m_node.extent(blitz::secondDim); j++)
            fprintf(file, " n%5i",m_node(i,j));
        fprintf(file, "\n");
        }    
    fprintf(file,"\n");
        
    //std::cout << "Face Dump " << m_zone << std::endl;
    //std::cout << "cell = " << m_cell;
    //std::cout << "node = " << m_node << std::endl;
}

void CellThread::textDump( FILE *file )
{
    fprintf(file,"id = %i\n",m_zone);
    fprintf(file,"cells = %i\n",m_nCells);
      
    if ( m_zone == 0 && !this->hasFaces() && !this->hasNodes() ) return;       
  
    for (int i = 0; i < m_nCells; i++) {
        fprintf(file,"c%5i %2i %2i %2i",i, this->elementType(i),this->nFaces(i), this->nNodes(i));
        //fprintf(file,"%7i %2i %2i",i, this->nFaces(i), this->nNodes(i));                      
        if ( this->hasFaces() ) 
            for (int j = 0; j < m_face.extent(blitz::secondDim); j++)
                fprintf(file," f%5i",m_face(i,j));
        if ( this->hasNodes() )
            for (int j = 0; j < m_node.extent(blitz::secondDim); j++)
                fprintf(file," n%5i",m_node(i,j));            
        fprintf(file, "\n");
        }        
}

void SegData::textDump( FILE *file )
{
    fprintf(file,"id = %i\n",m_zone);
    fprintf(file,"elements = %i\nvarLength = %i\nvar_ID=%i\n",m_nElements,m_nVars,m_varId);
  
    for (int i = 0; i < this->nElements(); i++) {
        fprintf(file,"%7i",i);
        for (int j = 0; j < this->nVars(); j++)
            fprintf(file, " %12.5e",m_data(i,j));
        fprintf(file, "\n");
        }
}

void Case::textDump( std::string filename )
{
    /* write nodes */
    FILE *file;
    
    file = fopen( filename.data(), "w" );
    
    std::cout << "textDump" << filename << std::endl;
    /* skip 0 thread since it is only informational */
    for (int i = 0; i < this->nNodeThreads(); i++)
        {
        std::cout << "\t node = " << i << std::endl;
        NodeThread *node = this->nodeThread(i);
        node->textDump(file);
        }    
    /* write faces */
    std::cout << "\t faces = " << this->nFaces() << std::endl; 
    std::cout << "\t faces = " << this->nFaces() << std::endl; 

    for (int i = 0; i < this->nFaceThreads(); i++)
        {
        FaceThread *face = this->faceThread(i);     
        std::cout << "\t face[" << i << "] = " << face->zoneID() << " " << face->hasConnect() << " " << face->nFaces() << std::endl ;        
        face->textDump(file);       
        }

    for (int i = 0; i < this->nCellThreads(); i++)
        {
        m_cellThread[i].textDump(file);
        }

    for (int i = 0; i < m_segData.size(); i++)
        {
        std::cout << "\t seg = " << i << std::endl;
        m_segData[i].textDump(file);
        }
        
    fclose(file);         
}

}

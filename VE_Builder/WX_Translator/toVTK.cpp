/*
    Author: E. David Huckaby
    Org:    NETL
            huckaby@netl.doe.gov
    Date:   3/25/2004
*/
#include "fluentObjects.h"
#include "fluentCase.h"
#include "vtk/vtkPoints.h"
#include "vtk/vtkCell.h"
#include "vtk/vtkTriangle.h"
#include "vtk/vtkLine.h"
#include "vtk/vtkQuad.h"
#include "vtk/vtkUnstructuredGrid.h"
#include "vtk/vtkXMLUnstructuredGridWriter.h"
#include "vtk/vtkUnstructuredGridWriter.h"
#include "vtk/vtkFloatArray.h"
#include "vtk/vtkIntArray.h"
#include "vtk/vtkPointData.h"
#include "vtk/vtkCellData.h"
#include "vtk/vtkTetra.h"
#include "vtk/vtkWedge.h"
#include "vtk/vtkPyramid.h"
#include "vtk/vtkHexahedron.h"
#include "vtk/vtkDataArray.h" 
#include "vtk/vtkDataSetToDataSetFilter.h"
#include "vtk/vtkCellDataToPointData.h"
#include <string>
namespace FluentReader {

/*
void FluentCase::vtkFaces()
{

    std::vtkUnStructuredGrid *grid std::vtkUnStructuredGrid::New();
    grid->SetDimensions(dims);
    
    vtkPoints *points = vtkPoints::New();
    points->Allocate( this->m_nNodes );
    
    xyz = cell->coords();
    for (int inode = 0; i < cell->        
        points->InsertPoint(i, cell->coord()

}
*/

void nodeToVTK( NodeThread *node, vtkPoints *points, int firstNode )
{
    double x[3];
    if (node->nDims() == 3)
        {
        for (int i = 0; i < node->nNodes(); i++)  {     
            node->getCoords(i, x); 
            points->InsertPoint( i + firstNode , x );
            }
        }
    else
        {
        for (int i = 0; i < node->nNodes(); i++)  {
            node->getCoords(i, x);
            points->InsertPoint( i + firstNode , x);
            }
        
        }
}


void faceToVTK( FaceThread *face, vtkUnstructuredGrid *grid )
{
    vtkTriangle *tri = vtkTriangle::New();
    vtkLine *line = vtkLine::New();
    vtkQuad *quad = vtkQuad::New();    
    vtkCell *cell;
    
    for (int i = 0; i < face->nFaces(); i++) {
        if ( face->nNodes(i) == 2 )
            cell = line;
        if ( face->nNodes(i) == 3 )
            cell = tri;
        if ( face->nNodes(i) == 4 )
            cell = quad;            
        std::cout << "\t\tfaceid = " << i << " " << face->nNodes(i);
        for (int j = 0; j < face->nNodes(i); j++)
            {
            // std::cout << " " << j;
            int id = face->node(i,j)-1;
            cell->GetPointIds()->SetId( j, id  );    
            }
        std::cout << std::endl;    
        grid->InsertNextCell( cell->GetCellType(), cell->GetPointIds() );
        }
}

void cellToVTK( CellThread *in_cell, vtkUnstructuredGrid *grid )
{
    vtkTriangle *tri = vtkTriangle::New();
    vtkQuad *quad = vtkQuad::New(); 
    vtkTetra *tet = vtkTetra::New();
    vtkWedge *wedge = vtkWedge::New();
    vtkPyramid *pyra = vtkPyramid::New();  
    vtkHexahedron *hex = vtkHexahedron::New();  
    vtkCell *cell;
    
    for (int i = 0; i < in_cell->nCells(); i++) {
        int etype = in_cell->elementType(i);
        if ( etype == in_cell->TRI ) cell = tri;
        if ( etype == CellThread::QUAD ) cell = quad;
        if ( etype == CellThread::TET ) cell = tet;
        if ( etype == CellThread::HEX ) cell = hex;
        if ( etype == CellThread::PYRAMID) cell = pyra;
        if ( etype == CellThread::WEDGE) cell = wedge;
        for (int j = 0; j < in_cell->nNodes(i); j++) {            
            // std::cout << " " << j;
            int id = in_cell->node(i,j);
            cell->GetPointIds()->SetId( j, id  ); 
            }
        // std::cout << std::endl;    
        grid->InsertNextCell( cell->GetCellType(), cell->GetPointIds() );
        }
}


void caseToVTK_faces( Case *fcase, std::string filename )
{
    /* write nodes */
    vtkPoints *points = vtkPoints::New();
    points->SetNumberOfPoints( fcase->nNodes() );
    std::cout << "caseToVTK " << filename << std::endl;
    std::cout << "nodes = " << fcase->nNodes() << std::endl;
    std::cout << "nodes = " << points->GetNumberOfPoints() << std::endl;
    /* skip 0 thread since it is only informational */
    for (int i = 0; i < fcase->nNodeThreads(); i++)
        {
        std::cout << "\t node = " << i << std::endl;
        NodeThread *node = fcase->nodeThread(i);
        if (node->hasCoords() ) nodeToVTK( node, points, 0);
        }    
    /* write faces */
    vtkUnstructuredGrid *grid = vtkUnstructuredGrid::New();
    grid->Allocate( fcase->nFaces(), 1000 );
    std::cout << "\t faces = " << fcase->nFaces(); 

    grid->SetPoints( points );
    for (int i = 0; i < fcase->nFaceThreads(); i++)
        {
        FaceThread *face = fcase->faceThread(i);     
        std::cout << "\t face = " << i << " " << face->hasConnect() << " " << face->nFaces() << std::endl ;        
        if ( face->hasConnect() ) faceToVTK( face, grid );        
        }
        
        
    /* write cells */
    cout << "# of cells = " << grid->GetNumberOfCells() << std::endl;
    //vtkXMLUnstructuredGridWriter *writer =  vtkXMLUnstructuredGridWriter::New();
    vtkUnstructuredGridWriter *writer =  vtkUnstructuredGridWriter::New();
  
    writer->SetFileName( filename.c_str() );
    writer->SetInput( grid );
    //writer->SetDataModeToAscii();
    writer->Write();

    writer->Delete();
    grid->Delete();
    points->Delete();
}

void caseToVTK( Case *fcase, std::string filename )
{
    /* write nodes */
    vtkPoints *points = vtkPoints::New();
    points->SetNumberOfPoints( fcase->nNodes() );
    std::cout << "caseAndDataToVTK " << filename << std::endl;
    std::cout << "nodes = " << fcase->nNodes() << std::endl;
    std::cout << "nodes = " << points->GetNumberOfPoints() << std::endl;
    /* skip 0 thread since it is only informational */
    for (int i = 0; i < fcase->nNodeThreads(); i++) {
        std::cout << "\t node = " << i << std::endl;
        NodeThread *node = fcase->nodeThread(i);
        if (node->hasCoords() ) nodeToVTK( node, points, 0);
        }    


    /* write faces */
    vtkUnstructuredGrid *grid = vtkUnstructuredGrid::New();

    if (1 == 0) {    
        grid->Allocate( fcase->nFaces(), 1000 );
        std::cout << "\t faces = " << fcase->nFaces(); 
        grid->SetPoints( points );

        for (int i = 0; i < fcase->nFaceThreads(); i++)
            {
            FaceThread *face = fcase->faceThread(i);     
            std::cout << "\t face = " << i << " " << face->hasConnect() 
                << " " << face->nFaces() << std::endl ;        
            if ( face->hasConnect() ) faceToVTK( face, grid );        
            }
    }

   
    CellThread *cell = fcase->cell();     
    grid->Allocate( fcase->nCells(), 1000 );
    std::cout << "\t faces = " << fcase->nCells(); 
    grid->SetPoints( points );    
    cellToVTK( cell, grid );

    /*
    vtkFloatArray *vectors = vtkFloatArray::New();
    vectors->SetNumberOfComponents(3);
    vectors->SetNumberOfTuples( grid->GetNumberOfPoints() );

    for (int i = 0; i < grid->GetNumberOfPoints(); i++)
        vectors->InsertTuple3( i,1.0,1.0,(double) i );
    grid->GetPointData()->SetVectors(vectors);         
    */

    /* write cells */
    cout << "# of cells = " << grid->GetNumberOfCells() << std::endl;
    //vtkXMLUnstructuredGridWriter *writer =  vtkXMLUnstructuredGridWriter::New();
    vtkUnstructuredGridWriter *writer =  vtkUnstructuredGridWriter::New();
  
    writer->SetFileName( filename.c_str() );
    writer->SetInput( grid );
    //writer->SetDataModeToAscii();
    writer->SetFileTypeToASCII();
    writer->Write();

    writer->Delete();
    grid->Delete();
    points->Delete();
}


void caseToVTK( Case *fcase, Case *fdata, int var_id, std::string filename )
{
    /* write nodes */
    vtkPoints *points = vtkPoints::New();
    points->SetNumberOfPoints( fcase->nNodes() );
    std::cout << "caseAndDataToVTK " << filename << std::endl;
    std::cout << "F nodes = " << fcase->nNodes() << std::endl;
    std::cout << "VTK nodes = " << points->GetNumberOfPoints() << std::endl;
    /* skip 0 thread since it is only informational */
    for (int i = 0; i < fcase->nNodeThreads(); i++)
        {
        std::cout << "\t node = " << i << std::endl;
        NodeThread *node = fcase->nodeThread(i);
        if (node->hasCoords() ) nodeToVTK( node, points, 0);
        }   
 
    vtkUnstructuredGrid *grid = vtkUnstructuredGrid::New();

    /*
        write face->node connectivity
    */
    if (1 == 0) {    
    grid->Allocate( fcase->nFaces(), 10 );
    std::cout << "\t faces = " << fcase->nFaces() << std::endl; 
    grid->SetPoints( points );

    for (int i = 0; i < fcase->nFaceThreads(); i++)
        {
        FaceThread *face = fcase->faceThread(i);     
        std::cout << "\t face = " << i << " " << face->hasConnect() 
            << " " << face->nFaces() << std::endl ;        
        if ( face->hasConnect() ) faceToVTK( face, grid );        
        }
    }   
   
    /* 
        write cell->node connectivity
    */
    CellThread *cell = fcase->cell();     
    grid->Allocate( fcase->nCells(), 10 );
    std::cout << "\t cell = " << fcase->nCells() << std::endl; 
    grid->SetPoints( points );    
    cellToVTK( cell, grid );

    /* 
        fill vtk scalar with fluent data

        data is still organized by threads
    */
    vtkFloatArray *scalar = vtkFloatArray::New();
    scalar->SetNumberOfComponents(1);
    scalar->SetNumberOfTuples( fcase->nCells() );
    scalar->SetName("Y_CH4");
    
    /* this will be made in to a funcion dataToVTK */
    for (int i = 0; i < fcase->nCellThreads(); i++) {
        CellThread *cell2 = fcase->cellThread(i);    
        if (cell2->zoneID() != 0) {            
            for (int j = 0; j < fdata->nDataThreads(); j++) {
                SegData *data = fdata->dataThread(j);
                if ( var_id == data->varID() && cell2->zoneID() == data->zoneID() ) {
                    std::cout << "\twriting zone = " << data->zoneID() << " j = " << j << std::endl;
                    for (int k = 0; k < cell->nCells(); k++) 
                        scalar->InsertValue( k + cell2->firstID(), data->data(0,k) );
                    } /*end if */
                } /* end for j */
            } /* end if */
        } /* end for i */

    grid->GetCellData()->SetScalars(scalar);


    /*
        fill vtk scalar with tree information, 
        gives a variable to turn off grid cells

        again will be made into its own function
    */
    vtkIntArray *scalar2 = vtkIntArray::New();
    scalar2->SetNumberOfComponents(1);
    scalar2->SetNumberOfTuples( fcase->nCells() );
    scalar2->SetName( "parentFlag" );

    for (int i = 0; i < fcase->nCells(); i++) 
        scalar2->InsertValue( i, 0 );
  
    for (int i = 0; i < fcase->nCellTree(); i++) {
        ElementTree *cell_tree = fcase->cellTree(i);
        for (int j = 0; j < cell_tree->nParents(); j++ )
            scalar2->InsertValue( cell_tree->parentID(j), 1 );
        } /* end for i */

    grid->GetCellData()->AddArray(scalar2);

    cout << "# of cells = " << grid->GetNumberOfCells() << std::endl;


    /* output to a vtk file */
    vtkUnstructuredGridWriter *writer =  vtkUnstructuredGridWriter::New();       
    writer->SetFileTypeToBinary();

    //vtkXMLUnstructuredGridWriter *writer =  vtkXMLUnstructuredGridWriter::New(); 
    //writer->SetDataModeToAscii(); 

    writer->SetFileName( filename.c_str() );  
    writer->SetInput( grid );    
    writer->Write();
    writer->Delete();
    grid->Delete();
    points->Delete();

}

void caseToVTK( Case *fcase, Case *fdata, std::string filename )
{
    caseToVTK( fcase, fdata, 200, filename );
}


} /* end namespace fluent reader */

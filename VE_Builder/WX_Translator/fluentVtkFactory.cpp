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
#include "vtk/vtkDoubleArray.h"

#include "fluentVtkFactory.h"
#include "fluentObjects.h"
#include "fluentCase.h"

#include <string>

namespace FluentReader {


VtkFactory::VtkFactory()
{
    m_grid = vtkUnstructuredGrid::New();
    m_verbose = false;
    m_data = 0;
    m_case = 0;
    m_faceGrid = 0;
}    

VtkFactory::VtkFactory(Case *fcase, Case *fdata)
{
    //std::cout << "caseAndDataToVTK " << filename << std::endl;
    //std::cout << "F nodes = " << fcase->nNodes() << std::endl;
    //std::cout << "VTK nodes = " << points->GetNumberOfPoints() << std::endl;
  
    m_grid = vtkUnstructuredGrid::New();
    m_faceGrid = 0;
    setCase(fcase);
    setData(fdata);
    m_verbose = false;
}

void VtkFactory::nodeToVTK( NodeThread *node, vtkPoints *points, int firstNode )
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

void VtkFactory::faceToVTK( FaceThread *face )
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
        m_faceGrid->InsertNextCell( cell->GetCellType(), cell->GetPointIds() );
        }
}

void VtkFactory::cellToVTK( CellThread *in_cell )
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
        m_grid->InsertNextCell( cell->GetCellType(), cell->GetPointIds() );
        }
}


void VtkFactory::dataToVTK( CellThread *cell_thread, SegData *seg_data, 
    vtkDoubleArray *scalar, int var_id, int ivar)
{
    int nVars = scalar->GetNumberOfComponents();

    if ( var_id == seg_data->varID() && 
        cell_thread->zoneID() == seg_data->zoneID() ) {
            if (m_verbose) 
                std::cout << "\twriting zone = " << 
                     seg_data->zoneID() << " var_id = " << var_id << std::endl;
            if (nVars == 1) {
                for (int k = 0; k < cell_thread->nCells(); k++) 
                    scalar->InsertValue( k + cell_thread->firstID(), seg_data->data(0,k) );             
                }
            else {
                for (int k = 0; k < cell_thread->nCells(); k++) 
                    scalar->InsertComponent( k + cell_thread->firstID(), ivar, seg_data->data(0,k) );
                }
            
        }
}

void VtkFactory::addPoints()
{    
    if (!m_grid || !m_case ) return;
    vtkPoints *points = vtkPoints::New();
    points->SetNumberOfPoints( m_case->nNodes() );    
    for (int i = 0; i < m_case->nNodeThreads(); i++) {
        if (m_verbose) std::cout << "\t node = " << i << std::endl;
        NodeThread *node = m_case->nodeThread(i);
        if (node->hasCoords() ) nodeToVTK( node, points, 0);
        }   
    m_grid->SetPoints( points ); 
}

void VtkFactory::addCellConnect()
{
    if (!m_grid || !m_case ) return;    
    CellThread *cell = m_case->cell();     
    int extSize = 10;
    m_grid->Allocate( m_case->nCells(), extSize );
    if (m_verbose) 
        std::cout << "\t cell = " << m_case->nCells() << std::endl;        
    cellToVTK( cell );
}

void VtkFactory::addFaceConnect()
{
    if (!m_faceGrid || !m_case ) return;

    for (int i = 0; i < m_case->nFaceThreads(); i++)
        {
        FaceThread *face = m_case->faceThread(i);     
        std::cout << "\t face = " << i << " " << face->hasConnect() 
            << " " << face->nFaces() << std::endl ;        
        if ( face->hasConnect() ) faceToVTK( face );        
        }
}


void VtkFactory::addScalar(int var_id, std::string name)
{
    if (!m_case || !m_data) return;
  
    vtkDoubleArray *scalar = vtkDoubleArray::New();
    scalar->SetNumberOfComponents(1);
    scalar->SetNumberOfTuples( m_case->nCells() );
    scalar->SetName( name.c_str() );
    
    for (int i = 0; i < m_case->nCellThreads(); i++) {
        CellThread *cell_thread = m_case->cellThread(i);    
        if (cell_thread->zoneID() != 0) {            
            for (int j = 0; j < m_data->nDataThreads(); j++) {
                SegData *seg_data = m_data->dataThread(j);
                int ivar = 0;
                dataToVTK( cell_thread, seg_data, scalar, var_id);               
                } /* end for j */
            } /* end if */
        } /* end for i */

    m_grid->GetCellData()->AddArray(scalar);
}

void VtkFactory::addVector(vector<int> var_ids, std::string name )
{
    vtkDoubleArray *vector = vtkDoubleArray::New();
    int nVars = var_ids.size();
    int nDims = 3;
    if (nVars > nDims) nVars == nDims;

    /* first add the component as scalars */
       
    vector->SetNumberOfComponents( nDims );
    vector->SetNumberOfTuples( m_case->nCells() );
    vector->SetName( name.c_str() );

    for (int i = 0; i < m_case->nCells(); i++ )
        vector->SetTuple3( i, 0.0,0.0,0.0 );
  
    for (int i = 0; i < m_case->nCellThreads(); i++) {
        CellThread *cell_thread = m_case->cellThread(i);    
        if (cell_thread->zoneID() != 0) {     
            if (m_verbose) std::cout << "\tdata thread " << i << std::endl;       
            for (int j = 0; j < m_data->nDataThreads(); j++) {
                SegData *seg_data = m_data->dataThread(j);           
                for (int ivar = 0; ivar < nVars; ivar ++) {
                    int var_id = var_ids[ivar];
                     dataToVTK( cell_thread, seg_data, vector, var_id, ivar);
                    } /*end for ivar */
                } /* end for j */
            } /* end if */
        } 


    m_grid->GetCellData()->AddArray(vector);
    //int ierr = m_grid->GetCellData()->SetVectors(vector);
     
    //m_grid->GetCellData()->SetActiveAttribute( name.c_str(), vtkCellData::VECTORS );
    m_grid->GetCellData()->SetActiveVectors(name.c_str() );
}

void VtkFactory::addVectorAsScalar(vector<int> var_ids, std::string name )
{
    vtkDoubleArray *vector = vtkDoubleArray::New();
    int nVars = var_ids.size();

    /* first add the component as scalars */
       
    vector->SetNumberOfComponents( nVars );
    vector->SetNumberOfTuples( m_case->nCells() );
    vector->SetName( name.c_str() );
  
    for (int i = 0; i < m_case->nCellThreads(); i++) {
        CellThread *cell_thread = m_case->cellThread(i);    
        if (cell_thread->zoneID() != 0) {     
            if (m_verbose) std::cout << "\tdata thread " << i << std::endl;       
            for (int j = 0; j < m_data->nDataThreads(); j++) {
                SegData *seg_data = m_data->dataThread(j);           
                for (int ivar = 0; ivar < nVars; ivar ++) {
                    int var_id = var_ids[ivar];
                     dataToVTK( cell_thread, seg_data, vector, ivar);
                    } /*end for ivar */
                } /* end for j */
            } /* end if */
        } 


    m_grid->GetCellData()->AddArray(vector);
    //int ierr = m_grid->GetCellData()->SetVectors(vector);
     
    m_grid->GetCellData()->SetActiveAttribute( name.c_str(), vtkCellData::VECTORS );
    m_grid->GetCellData()->SetActiveVectors(name.c_str() );
}
/*
    if the grid as been refined using adaptation, the data values in the parents cells
    do not correspond to those in the childs cells, this flag tells weather the is a parent
    cell
*/

void VtkFactory::addParentFlag()
{
    vtkIntArray *scalar = vtkIntArray::New();
    scalar->SetNumberOfComponents(1);
    scalar->SetNumberOfTuples( m_case->nCells() );
    scalar->SetName( "parentFlag" );

    for (int i = 0; i < m_case->nCells(); i++) 
        scalar->InsertValue( i, 0 );

    if (m_verbose) cout << "writing parent flags " << std::endl;

    for (int i = 0; i < m_case->nCellTree(); i++) {
        ElementTree *cell_tree = m_case->cellTree(i);
        cout << "\t cell three = " << i << " containing " << cell_tree->nParents() << std::endl;
        for (int j = 0; j < cell_tree->nParents(); j++ )
            scalar->InsertValue( cell_tree->parentID(j), 1 );
        } /* end for i */

   
    m_grid->GetCellData()->AddArray(scalar);
}

void VtkFactory::toFile( std::string filename, bool isXML = false, bool isBinary = true )
{

    /* xml writer does not seem to read into paraview because the number of points 
    is not written to the file 
     */

    if (isXML) {
        vtkXMLUnstructuredGridWriter *writer =  vtkXMLUnstructuredGridWriter::New(); 
        if (!isBinary) 
            writer->SetDataModeToAscii();  
        else
            writer->SetDataModeToBinary();
        writer->SetFileName( filename.c_str() );  
        writer->SetInput( m_grid ); 
        int ierr = writer->Write();    
        if (ierr)
            cout << "file " << filename << " successfully written" << std::endl;
        else
            cout << "file " << filename << " incorrectly written" << std::endl;   
        }
    else {        
        vtkUnstructuredGridWriter *writer =  vtkUnstructuredGridWriter::New();  
        if (!isBinary) 
            writer->SetFileTypeToASCII();  
        else
            writer->SetFileTypeToBinary();
        writer->SetFileName( filename.c_str() );  
        writer->SetInput( m_grid );    
        writer->Write();
        }       

}
     
} /* end namespace FluentReader */

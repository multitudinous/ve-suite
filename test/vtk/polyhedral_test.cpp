// fluentmulti.cpp : Defines the entry point for the console application.
//

//#include "stdafx.h"
#include <vtkCubeSource.h>
#include <vtkElevationFilter.h>
#include <vtkSmartPointer.h>
#include <vtkCellArray.h>
#include <vtkPolyhedron.h>
#include <vtkIdTypeArray.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkPointData.h>
#include <vtkConvexPointSet.h>

#include "vtkActor.h"
#include "vtkCellDataToPointData.h"
#include "vtkContourFilter.h"
#include "vtkDebugLeaks.h"
#include "vtkMultiBlockDataSet.h"
#include "vtkGeometryFilter.h"
#include "vtkLookupTable.h"
#include "vtkOutlineCornerFilter.h"
#include "vtkPolyData.h"
#include "vtkPolyDataMapper.h"
#include "vtkProperty.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkShrinkPolyData.h"
#include "vtkUnstructuredGrid.h"
#include "vtkStructuredGridOutlineFilter.h"
#include "vtkTestUtilities.h"
#include "vtkXMLUnstructuredGridReader.h"
#include "vtkOutlineFilter.h"
#include "vtkCutter.h"
#include "vtkPlane.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkCompositeDataPipeline.h"
#include "vtkFLUENTReader.h"
#include "vtkCutter.h"
#include <vtksys/ios/sstream>

#include <iostream>

void testConvex()
{
    vtkConvexPointSet* aConvex = vtkConvexPointSet::New();
    aConvex->GetPointIds()->InsertNextId( 0 );
    aConvex->GetPointIds()->InsertNextId( 1 );
    aConvex->GetPointIds()->InsertNextId( 2 );
    aConvex->GetPointIds()->InsertNextId( 3 );
    aConvex->GetPointIds()->InsertNextId( 4 );
    aConvex->GetPointIds()->InsertNextId( 5 );
    aConvex->GetPointIds()->InsertNextId( 6 );
    aConvex->GetPointIds()->InsertNextId( 7 );  
}


void testGrid()
{
    // create the a cube
    vtkSmartPointer<vtkCubeSource> cube = 
    vtkSmartPointer<vtkCubeSource>::New();
    cube->SetXLength(10); 
    cube->SetYLength(10); 
    cube->SetZLength(20); 
    cube->SetCenter(0, 0, 0);  
    cube->Update();

    // add scaler
    vtkSmartPointer<vtkElevationFilter> ele = 
    vtkSmartPointer<vtkElevationFilter>::New();
    ele->SetInput(cube->GetOutput());
    ele->SetLowPoint(0,0,-10);
    ele->SetHighPoint(0,0,10);
    ele->Update();
    vtkPolyData* poly = vtkPolyData::SafeDownCast(ele->GetOutput());

    vtkIdList* tempIdList = vtkIdList::New();
    tempIdList->InsertNextId( 6 );
    // create a test polyhedron
    vtkIdType pointIds[8] = {0, 1, 2, 3, 4, 5, 6, 7};

    vtkSmartPointer<vtkCellArray> faces = vtkSmartPointer<vtkCellArray>::New();
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 0 );
    tempIdList->InsertNextId( 2 );
    tempIdList->InsertNextId( 6 );
    tempIdList->InsertNextId( 4 );
    //vtkIdType face0[4] = {0, 2, 6, 4};
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 1 );
    tempIdList->InsertNextId( 3 );
    tempIdList->InsertNextId( 7 );
    tempIdList->InsertNextId( 5 );
    //vtkIdType face1[4] = {1, 3, 7, 5};
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 0 );
    tempIdList->InsertNextId( 1 );
    tempIdList->InsertNextId( 3 );
    tempIdList->InsertNextId( 2 );
    //vtkIdType face2[4] = {0, 1, 3, 2};
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 5 );
    tempIdList->InsertNextId( 7 );
    tempIdList->InsertNextId( 6 );
    //vtkIdType face3[4] = {4, 5, 7, 6};
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 0 );
    tempIdList->InsertNextId( 1 );
    tempIdList->InsertNextId( 5 );
    tempIdList->InsertNextId( 4 );
    //vtkIdType face4[4] = {0, 1, 5, 4};
    tempIdList->InsertNextId( 4 );
    tempIdList->InsertNextId( 2 );
    tempIdList->InsertNextId( 3 );
    tempIdList->InsertNextId( 7 );
    tempIdList->InsertNextId( 6 );
    //vtkIdType face5[4] = {2, 3, 7, 6};
    /*faces->InsertNextCell(4, face0);
    faces->InsertNextCell(4, face1);
    faces->InsertNextCell(4, face2);
    faces->InsertNextCell(4, face3);
    faces->InsertNextCell(4, face4);
    faces->InsertNextCell(4, face5);*/

    vtkConvexPointSet* aConvex = vtkConvexPointSet::New();
    aConvex->GetPointIds()->InsertNextId( 0 );
    aConvex->GetPointIds()->InsertNextId( 2 );
    aConvex->GetPointIds()->InsertNextId( 6 );
    aConvex->GetPointIds()->InsertNextId( 4 );
    aConvex->GetPointIds()->InsertNextId( 1 );
    aConvex->GetPointIds()->InsertNextId( 3 );
    aConvex->GetPointIds()->InsertNextId( 7 );
    aConvex->GetPointIds()->InsertNextId( 5 );  
    
    vtkSmartPointer<vtkUnstructuredGrid> ugrid0 = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
    ugrid0->SetPoints(poly->GetPoints());
    ugrid0->GetPointData()->DeepCopy(poly->GetPointData());

    //ugrid0->InsertNextCell(VTK_POLYHEDRON, 8, pointIds, 6, faces->GetPointer());
    //ugrid0->InsertNextCell(VTK_POLYHEDRON, tempIdList);
    ugrid0->InsertNextCell(aConvex->GetCellType(), aConvex->GetPointIds());

    /*vtkPolyhedron *polyhedron = static_cast<vtkPolyhedron*>(ugrid0->GetCell(0));

    vtkCellArray * cell = ugrid0->GetCells();
    vtkIdTypeArray * pids = cell->GetData();
    std::cout << "num of cells: " << cell->GetNumberOfCells() << std::endl;
    std::cout << "num of tuples: " << pids->GetNumberOfTuples() << std::endl;
    for (int i = 0; i < pids->GetNumberOfTuples(); i++)
    {
    std::cout << pids->GetValue(i) << " ";
    }
    std::cout << std::endl;
    cell->Print(std::cout);

    // Print out basic information
    std::cout << "Testing polyhedron is a cube of with bounds "
        << "[-5, 5, -5, 5, -10, 10]. It has "
        << polyhedron->GetNumberOfEdges() << " edges and " 
        << polyhedron->GetNumberOfFaces() << " faces." << std::endl;*/

    //
    // test writer
    vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer =
        vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    writer->SetInput(ugrid0);
    writer->SetFileName("test.vtu");
    //writer->SetDataModeToAscii();
    writer->Update();
}


int main(int argc, char* argv[])
{
    testGrid();
    
    if( argc == 1 )
    {
        return 1;
    }
  vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
  vtkAlgorithm::SetDefaultExecutivePrototype(prototype);
  prototype->Delete();

  // Standard rendering classes
  vtkRenderer *ren = vtkRenderer::New();
  vtkRenderWindow *renWin = vtkRenderWindow::New();
  renWin->AddRenderer(ren);
  vtkRenderWindowInteractor *iren = vtkRenderWindowInteractor::New();
  iren->SetRenderWindow(renWin);

  // We will read three files and collect them together in one
  // multi-block dataset. I broke the combustor dataset into
  // three pieces and wrote them out separately.
  vtkXMLUnstructuredGridReader* reader = vtkXMLUnstructuredGridReader::New();
  reader->SetFileName( argv[ 1 ] );


  // Multi-block can be processed with regular VTK filters in two ways:
  // 1. Pass through a multi-block aware consumer. Since a multi-block 
  //    aware mapper is not yet available, vtkMultiGroupDataGeometryFilter
  //    can be used
  // 2. Assign the composite executive (vtkMultiGroupDataPipeline) to
  //    all "simple" (that work only on simple, non-composite datasets) filters  

  // geometry filter
  // This filter is multi-block aware and will request blocks from the
  // input. These blocks will be processed by simple processes as if they
  // are the whole dataset
  vtkGeometryFilter* geom1 = 
    vtkGeometryFilter::New();
  geom1->SetInput(reader->GetOutput());

  // cell 2 point and contour
  vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
  c2p->SetInput(reader->GetOutput());

  //Cut through the data set
  double center[3]= {0,0,0};
    
  center[0] = 0.0;
  center[1] = 0.1;
  center[2] = 0.0;
  
  vtkPlane* plane = vtkPlane::New();
  plane->SetOrigin( center );
  plane->SetNormal(1, 0, 0);
    
  vtkCutter* planeCut = vtkCutter::New();
  planeCut->SetInput(reader->GetOutput());
  planeCut->SetCutFunction(plane);

  // geometry filter
  vtkGeometryFilter* geom2 = 
    vtkGeometryFilter::New();
  geom2->SetInputConnection(0, planeCut->GetOutputPort(0));

  vtkLookupTable* lut = vtkLookupTable::New();
  lut->SetNumberOfTableValues( 256 );
  lut->SetHueRange( 0.6667, 0 );
  lut->SetSaturationRange( 1, 1 );
  lut->SetValueRange( 1, 1 );
  lut->SetVectorComponent( 0 );
  lut->SetVectorMode( 1 );

  vtkPolyDataMapper* mapper = vtkPolyDataMapper::New();
  mapper->SetInputConnection( geom2->GetOutputPort() );
  mapper->SetScalarModeToUseCellFieldData();
  mapper->ColorByArrayComponent( "Volume Fraction of Phase 1", 0 );
  mapper->SetScalarRange( 0.1, 0.9 );
  mapper->SetLookupTable( lut );

  vtkActor* contActor = vtkActor::New();
  contActor->SetMapper(mapper);
  //Use colors from scalars!!
  //contActor->GetProperty()->SetColor(1, 0, 0);
  ren->AddActor(contActor);

  ren->SetBackground(1,1,1);
  renWin->SetSize(300,300);
  renWin->Render();
  iren->Start();

  // Cleanup
  reader->Delete();
  geom1->Delete();
  geom2->Delete();
  lut->Delete();
  mapper->Delete();
  c2p->Delete();
  plane->Delete();
  planeCut->Delete();
  contActor->Delete();
  ren->Delete();
  renWin->Delete();
  iren->Delete();
  
  return 0;
}

/*=========================================================================

  Program:   Visualization Toolkit
  Module:    $RCSfile: MultiBlock.cxx,v $

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
// This example demonstrates how multi-block datasets can be processed
// using the new vtkMultiBlockDataSet class.
// 
// The command line arguments are:
// -D <path> => path to the data (VTKData); the data should be in <path>/Data/

#include "vtkActor.h"
#include "vtkCellDataToPointData.h"
#include "vtkContourFilter.h"
#include "vtkDebugLeaks.h"
#include "vtkMultiBlockDataSet.h"
//#include "vtkMultiGroupDataGeometryFilter.h"
#include "vtkMultiGroupDataGeometryFilter.h"
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
#include <vtksys/ios/sstream>

int main(int argc, char* argv[])
{
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
  int i;
  vtkXMLUnstructuredGridReader* reader = vtkXMLUnstructuredGridReader::New();

  // vtkMultiBlockDataSet respresents multi-block datasets. See
  // the class documentation for more information.
  vtkMultiBlockDataSet* mb = vtkMultiBlockDataSet::New();

  for (i=0; i<3; i++)
  {
    // Here we load the three separate files (each containing
    // a structured grid dataset)
    vtksys_ios::ostringstream fname;
    fname << "test_point/test_point_" << i << ".vtu" << std::ends;    char* cfname = 
    vtkTestUtilities::ExpandDataFileName(argc, argv, fname.str().c_str());
    reader->SetFileName(cfname);
    // We have to update since we are working without a VTK pipeline.
    // This will read the file and the output of the reader will be
    // a valid structured grid data.
    reader->Update();
    delete[] cfname;

    // We create a copy to avoid adding the same data three
    // times (the output object of the reader does not change
    // when the filename changes)
    vtkUnstructuredGrid* sg = vtkUnstructuredGrid::New();
    sg->ShallowCopy(reader->GetOutput());

    // Add the structured grid to the multi-block dataset
    //mb->SetBlock(i, sg);
    mb->SetDataSet(0,i, sg);
    sg->Delete();
    }
  reader->Delete();

  // Multi-block can be processed with regular VTK filters in two ways:
  // 1. Pass through a multi-block aware consumer. Since a multi-block 
  //    aware mapper is not yet available, vtkMultiGroupDataGeometryFilter
  //    can be used
  // 2. Assign the composite executive (vtkMultiGroupDataPipeline) to
  //    all "simple" (that work only on simple, non-composite datasets) filters  

  // outline
  //vtkStructuredGridOutlineFilter* of = vtkStructuredGridOutlineFilter::New();
  vtkOutlineFilter* of = vtkOutlineFilter::New();
  of->SetInput(mb);

  // geometry filter
  // This filter is multi-block aware and will request blocks from the
  // input. These blocks will be processed by simple processes as if they
  // are the whole dataset
  vtkMultiGroupDataGeometryFilter* geom1 = 
    vtkMultiGroupDataGeometryFilter::New();
  geom1->SetInputConnection(0, of->GetOutputPort(0));

  // Rendering objects
  vtkPolyDataMapper* geoMapper = vtkPolyDataMapper::New();
  geoMapper->SetInputConnection(0, geom1->GetOutputPort(0));

  vtkActor* geoActor = vtkActor::New();
  geoActor->SetMapper(geoMapper);
  geoActor->GetProperty()->SetColor(0, 0, 0);
  ren->AddActor(geoActor);

  // cell 2 point and contour
  vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
  c2p->SetInput(mb);

  //Cut through the data set
  double center[3]= {0,0,0};
    
  center[0] = 0.0;
  center[1] = 5.0;
  center[2] = 0.0;
  
  vtkPlane* plane = vtkPlane::New();
  plane->SetOrigin( center );
  plane->SetNormal(1, 0, 0);
    
  vtkCutter* planeCut = vtkCutter::New();
  planeCut->SetInput(mb);
  planeCut->SetCutFunction(plane);
    
  //vtkContourFilter* contour = vtkContourFilter::New();
  //contour->SetInputConnection(0, c2p->GetOutputPort(0));
  //contour->SetValue(0, 0.45);

  // geometry filter
  vtkMultiGroupDataGeometryFilter* geom2 = 
    vtkMultiGroupDataGeometryFilter::New();
  geom2->SetInputConnection(0, planeCut->GetOutputPort(0));

  vtkXMLPolyDataWriter* tempWriter = vtkXMLPolyDataWriter::New();
  tempWriter->SetFileName( "temp.vtp" );
  tempWriter->SetInput( planeCut->GetOuput() );
  tempWriter->SetDataModeToAscii();
  tempWriter->Write();

  // Rendering objects
  vtkPolyDataMapper* contMapper = vtkPolyDataMapper::New();
  contMapper->SetInputConnection(0, geom2->GetOutputPort(0));

  vtkActor* contActor = vtkActor::New();
  contActor->SetMapper(contMapper);
  //Use colors from scalars!!
  //contActor->GetProperty()->SetColor(1, 0, 0);
  ren->AddActor(contActor);

  ren->SetBackground(1,1,1);
  renWin->SetSize(300,300);
  iren->Start();

  // Cleanup
  of->Delete();
  geom1->Delete();
  geoMapper->Delete();
  geoActor->Delete();
  c2p->Delete();
  //contour->Delete();
  plane->Delete();
  planeCut->Delete();
  geom2->Delete();
  contMapper->Delete();
  contActor->Delete();
  ren->Delete();
  renWin->Delete();
  iren->Delete();
  mb->Delete();

  return 0;
}

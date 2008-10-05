// fluentmulti.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#include "vtkActor.h"
#include "vtkCellDataToPointData.h"
#include "vtkContourFilter.h"
#include "vtkDebugLeaks.h"
#include "vtkMultiBlockDataSet.h"
//#include "vtkCompositeDataGeometryFilter.h"
#include "vtkMultiGroupDataGeometryFilter.h"
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

int _tmain(int argc, _TCHAR* argv[])
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
  vtkFLUENTReader* reader = vtkFLUENTReader::New();
  reader->SetFileName("C:/Work/fluentmulti/release/gasifier.cas");


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
  vtkMultiGroupDataGeometryFilter* geom1 = 
    vtkMultiGroupDataGeometryFilter::New();
  geom1->SetInput(reader->GetOutput());

  // cell 2 point and contour
  vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
  c2p->SetInput(reader->GetOutput());

  //Cut through the data set
  double center[3]= {0,0,0};
    
  center[0] = 0.0;
  center[1] = 5.0;
  center[2] = 0.0;
  
  vtkPlane* plane = vtkPlane::New();
  plane->SetOrigin( center );
  plane->SetNormal(1, 0, 0);
    
  vtkCutter* planeCut = vtkCutter::New();
  planeCut->SetInput(reader->GetOutput());
  planeCut->SetCutFunction(plane);

  // geometry filter
  vtkMultiGroupDataGeometryFilter* geom2 = 
    vtkMultiGroupDataGeometryFilter::New();
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
  mapper->ColorByArrayComponent( "TEMPERATURE", 0 );
  mapper->SetScalarRange(-2243.4, 3087.88);
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


-----------------------------25191297996859
Content-Disposition: form-data; name="Item.Attachment.unused"

0

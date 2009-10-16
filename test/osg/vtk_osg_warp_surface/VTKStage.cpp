#include "VTKStage.h"

#include <vtkXMLUnstructuredGridReader.h>
#include <vtkCellData.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkStructuredGrid.h>


VTKStage::VTKStage(void)
{
    vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    vtkAlgorithm::SetDefaultExecutivePrototype(prototype);
    prototype->Delete();

	polydataReader = NULL;
	triangleFilter = NULL;
	triangleStripper = NULL;
}

VTKStage::VTKStage(string name)
{ 
	xmlPolydateFname =name; 
	polydataReader = NULL;
	triangleFilter = NULL;
	triangleStripper = NULL;
	
}

VTKStage::~VTKStage(void)
{
	if (polydataReader)
		polydataReader->Delete();

	if (triangleFilter)
		triangleFilter->Delete();

	if (triangleStripper)
		triangleStripper->Delete();	
}

vtkPolyData* VTKStage::GetOutput()
{
	return triangleStripper->GetOutput();
}

void VTKStage::Update()
{
	if (polydataReader)
		polydataReader->Delete();
	polydataReader = vtkXMLPolyDataReader::New();
	polydataReader->SetFileName(xmlPolydateFname.c_str());//("C:\\Dougm\\testvecglyphs_large.vtp");
    polydataReader->Update();
	
	if (triangleFilter)
		triangleFilter->Delete();
	triangleFilter = vtkTriangleFilter::New();
	triangleFilter->SetInput( polydataReader->GetOutput() );
	triangleFilter->Update();

	if (triangleStripper)
		triangleStripper->Delete();
	triangleStripper = vtkStripper::New();
	triangleStripper->SetInput(triangleFilter->GetOutput());
	triangleStripper->Update();

	return;

}

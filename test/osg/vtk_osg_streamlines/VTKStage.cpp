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
	
}

VTKStage::VTKStage(string name)
{ 
	xmlPolydateFname =name; 
	polydataReader = NULL;
	
}

VTKStage::~VTKStage(void)
{
	if (polydataReader)
		polydataReader->Delete();	
	
}

vtkPolyData* VTKStage::GetOutput()
{
	return polydataReader->GetOutput();
}

void VTKStage::Update()
{
	/*vtkCellDataToPointData* c2p = vtkCellDataToPointData::New();
	c2p->SetInput(  GetActiveDataSet()->GetDataSet() );*/ //hook up the CellDataToPointData with reader stages indead

	if (polydataReader)
		polydataReader->Delete();
	polydataReader = vtkXMLPolyDataReader::New();
	polydataReader->SetFileName(xmlPolydateFname.c_str());//("C:\\Dougm\\testvecglyphs_large.vtp");
    polydataReader->Update();
	
	return;

}

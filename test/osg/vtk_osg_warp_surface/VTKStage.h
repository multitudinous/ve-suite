#pragma once

//CYANG 07/10/09, a simple wrapping class for VTK stage processing. 
//And it includes all necessary VTK headers

#include <vtkActor.h>
#include <vtkCellDataToPointData.h>
#include <vtkContourFilter.h>
#include <vtkDebugLeaks.h>

#include <vtkMultiBlockDataSet.h>
#include <vtkCompositeDataGeometryFilter.h>
#include <vtkLookupTable.h>
#include <vtkThresholdPoints.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkRenderWindowInteractor.h>
#include <vtkGeometryFilter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkConeSource.h>
#include <vtkGlyph3D.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkDataSet.h>
#include <vtkDataSetReader.h>
#include <vtkXMLDataReader.h>

#include <vtkCutter.h>
#include <vtkPlane.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkCompositeDataPipeline.h>
#include <vtkFLUENTReader.h>
#include <vtkCutter.h>
#include <vtksys/ios/sstream>

#include <vtkTriangleFilter.h>
#include <vtkStripper.h>
#include <vtkPolyDataNormals.h>
#include <vtkPointData.h>
#include <vtkCellArray.h>
#include <vtkMaskPoints.h>
#include <vtkStreamTracer.h>
#include <vtkRungeKutta4.h>
#include <vtkCleanPolyData.h>
#include <vtkTriangleFilter.h>
#include <vtkStripper.h>

#include <iostream>
#include <string>

using namespace std;
class VTKStage
{
public:
	VTKStage(void);
	VTKStage(string name) ;
	~VTKStage(void);

	//result accessor, This is supposed to the osg stage the triangle strips
	vtkPolyData* GetOutput();
	
	//Set Interface
	void SetXMLPolyDataFileName(string name);

	//real read code
	void Update();

	
private:
	string xmlPolydateFname; //the file name

	//all variable allocated by VTK New and not release immediately
	vtkXMLPolyDataReader* polydataReader;
	vtkTriangleFilter *triangleFilter;
	vtkStripper *triangleStripper;

	
};

//#include "readWriteVtkThings.h"
//#include "viewCells.h"
#include "cfdGrid2Surface.h"
//#include "setScalarAndVector.h"

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkContourFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkGeometryFilter.h>
#include <vtkFloatArray.h>

#include <vtkTriangleFilter.h>
#include <vtkSTLWriter.h>

class makeSurf
{
	private:
		void writeVtkGeomToStl( vtkDataSet * dataset, char filename [] );
	
			
	public:
		void writeSurface(  );
		void writePolyDataSurface(  );
			
			
};

void makeSurf::writeVtkGeomToStl( vtkDataSet * dataset, char filename [] )
{
	vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   	vtkGeometryFilter *gFilter = NULL;

   	// convert dataset to vtkPolyData 
   	if ( dataset->IsA("vtkPolyData") )
      		tFilter->SetInput( (vtkPolyData*)dataset );
   	else 
   	{
      		//cout << "Using vtkGeometryFilter to convert to polydata" << endl;
      		gFilter = vtkGeometryFilter::New();
      		gFilter->SetInput( dataset );
      		tFilter->SetInput( gFilter->GetOutput() );
   	}

   	//cout << "Writing \"" << filename << "\"... ";
   	//cout.flush();
   	vtkSTLWriter *writer = vtkSTLWriter::New();
      	writer->SetInput( tFilter->GetOutput() );
      	writer->SetFileName( filename );
      	writer->SetFileTypeToBinary();
      	writer->Write();
      	writer->Delete();
   	//cout << "... done\n" << endl;

   	tFilter->Delete();

   	if ( gFilter ) 
      		gFilter->Delete();
}


void makeSurf::writeSurface(  )
{
	
}

void makeSurf::writePolyDataSurface(  )
{
	
}


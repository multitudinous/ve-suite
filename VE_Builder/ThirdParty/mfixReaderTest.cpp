#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <sstream>

#include "vtkUnstructuredGrid.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkMFIXReader.h"

using namespace std;

//**************************************************************************
//*****                     Main                                        ****
//**************************************************************************

int main(int argc, char **argv) 
{  

	vtkMFIXReader *reader = vtkMFIXReader::New();
	reader->SetFileName(argv[1]);
	reader->Update();
	int TimeStepRange[2]; 
	reader->GetTimeStepRange(TimeStepRange);
	cout << "Number of Timesteps = " << TimeStepRange[0] << ", "<< TimeStepRange[1] << endl;
	reader->SetTimeStep(atoi(argv[2]));

	vtkUnstructuredGrid *grid;
	grid = reader->GetOutput();

	vtkXMLUnstructuredGridWriter *Writer = vtkXMLUnstructuredGridWriter::New();
	Writer->SetInput(grid);
	Writer->SetFileName(argv[3]);
	Writer->Write();

	return 1;
}


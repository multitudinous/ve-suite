#include <iostream.h>
#include <fstream.h>
#include <iomanip.h>
#include <stdlib.h>
#include <sstream>

#include "vtkUnstructuredGrid.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkFLUENTReader.h"

using namespace std;

//**************************************************************************
//*****                     Main                                        ****
//**************************************************************************

int main(int argc, char **argv) 
{  

	vtkFLUENTReader *reader = vtkFLUENTReader::New();
	reader->SetFileName(argv[1]);
	reader->Update();

	vtkUnstructuredGrid *grid;
	grid = reader->GetOutput();

	vtkXMLUnstructuredGridWriter *Writer = vtkXMLUnstructuredGridWriter::New();
	Writer->SetInput(grid);
	Writer->SetFileName(argv[2]);
	Writer->Write();

	return 1;
}


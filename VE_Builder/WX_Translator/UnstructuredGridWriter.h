// UnstructuredGridWriter.cpp
// The VTK unstructured grid writer.
// Inputs: (char *inFile, char *outDir, int tStep, int maxTs, int type, resHead *resH, spHead *spH, mfixData *mfD)
// Outputs: vtkUnstructuredGrid file to disk
//          if type = 1, output = ASCII legacy format (.vtk)
//          if type = 2, output = binary legacy format (.vtk)
//          if type = 3, output = XML serial format (.vtu)
//          if type = 4, output = XML parallel format (.pvtu)
// Author: Jim Canon (jcanon@csee.wvu.edu)
// Last revision: 04-08-04
// Version: 1.3

#ifndef UNSTRUCTUREDGRIDWRITER_H
#define UNSTRUCTUREDGRIDWRITER_H

#include <iostream>
#include <stdlib.h>
#include <vtkUnstructuredGrid.h>
#include <vtkRectilinearGrid.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkXMLPUnstructuredGridWriter.h>
#include <vtkStructuredGrid.h>
#include <vtkIdList.h>
#include <vtkGenericCell.h>
#include <vtkFloatArray.h>
#include <vtkPoints.h>
#include <vtkCellData.h>
#include <vtkCellArray.h>
#include <vtkHexahedron.h>
#include <vtkDoubleArray.h>

#include "mfixDataHeaders.h"
#include "UnstructuredGridWriter.h"
#include "converter.h"

int UnstructuredGridWriter(char *inFile, char *outDir, int tStep, int MaxTs, int type, resHead *resH, spHead *spH, mfixData *mfD);

#endif

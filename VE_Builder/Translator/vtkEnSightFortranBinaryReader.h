/*=========================================================================

  Program:   Visualization Toolkit
  Module:    $RCSfile: vtkEnSightFortranBinaryReader.h,v $
  Language:  C++
  Date:      $Date: 2003/12/13 21:26:08 $
  Version:   $Revision: 1.4 $

  Copyright (c) 1993-2002 Ken Martin, Will Schroeder, Bill Lorensen 
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
// .NAME vtkEnSightFortranBinaryReader - class to read binary EnSight6 files
// .SECTION Description
// vtkEnSightFortranBinaryReader is a class to read binary EnSight6 files into vtk.
// Because the different parts of the EnSight data can be of various data
// types, this reader produces multiple outputs, one per part in the input
// file.
// All variable information is being stored in field data.  The descriptions
// listed in the case file are used as the array names in the field data.
// For complex vector variables, the description is appended with _r (for the
// array of real values) and _i (for the array if imaginary values).  Complex
// scalar variables are stored as a single array with 2 components, real and
// imaginary, listed in that order.
// .SECTION Caveats
// You must manually call Update on this reader and then connect the rest
// of the pipeline because (due to the nature of the file format) it is
// not possible to know ahead of time how many outputs you will have or
// what types they will be.
// This reader can only handle static EnSight datasets (both static geometry
// and variables).

#ifndef __vtkEnSightFortranBinaryReader_h
#define __vtkEnSightFortranBinaryReader_h

#include <vtkEnSightReader.h>

class vtkIdTypeArray;
class vtkPoints;

class VTK_IO_EXPORT vtkEnSightFortranBinaryReader : public vtkEnSightReader
{
public:
  static vtkEnSightFortranBinaryReader *New();
  vtkTypeRevisionMacro(vtkEnSightFortranBinaryReader, vtkEnSightReader);
  virtual void PrintSelf(ostream& os, vtkIndent indent);
  
protected:
  vtkEnSightFortranBinaryReader();
  ~vtkEnSightFortranBinaryReader();
  
  // Description:
  // Read the geometry file.  If an error occurred, 0 is returned; otherwise 1.
  virtual int ReadGeometryFile(char* fileName, int timeStep);

  // Description:
  // Read the measured geometry file.  If an error occurred, 0 is returned;
  // otherwise 1.
  virtual int ReadMeasuredGeometryFile(char* fileName, int timeStep);

  // Description:
  // Read scalars per node for this dataset.  If an error occurred, 0 is
  // returned; otherwise 1.  If there will be more than one component in
  // the scalars array, we assume that 0 is the first component added to the array.
  virtual int ReadScalarsPerNode(char* fileName, char* description,
                                 int timeStep, int measured = 0,
                                 int numberOfComponents = 1,
                                 int component = 0);
  
  // Description:
  // Read vectors per node for this dataset.  If an error occurred, 0 is
  // returned; otherwise 1.
  virtual int ReadVectorsPerNode(char* fileName, char* description,
                                 int timeStep, int measured = 0);

  // Description:
  // Read tensors per node for this dataset.  If an error occurred, 0 is
  // returned; otherwise 1.
  virtual int ReadTensorsPerNode(char* fileName, char* description,
                                 int timeStep);

  // Description:
  // Read scalars per element for this dataset.  If an error occurred, 0 is
  // returned; otherwise 1.  If there will be more than one component in the
  // scalars array, we assume that 0 is the first component added to the array.
  virtual int ReadScalarsPerElement(char* fileName, char* description,
                                    int timeStep, int numberOfComponents = 1,
                                    int component = 0);

  // Description:
  // Read vectors per element for this dataset.  If an error occurred, 0 is
  // returned; otherwise 1.
  virtual int ReadVectorsPerElement(char* fileName, char* description,
                                    int timeStep);

  // Description:
  // Read tensors per element for this dataset.  If an error occurred, 0 is
  // returned; otherwise 1.
  virtual int ReadTensorsPerElement(char* fileName, char* description,
                                    int timeStep);

  // Description:
  // Read an unstructured part (partId) from the geometry file and create a
  // vtkUnstructuredGrid output.  Return 0 if EOF reached.
  virtual int CreateUnstructuredGridOutput(int partId, char line[256]);
  
  // Description:
  // Read a structured part from the geometry file and create a
  // vtkStructuredGridOutput.  Return 0 if EOF reached.
  virtual int CreateStructuredGridOutput(int partId, char line[256]);
  
  // Description:
  // Internal function to read in a line up to 80 characters.
  // Returns zero if there was an error.
  int ReadLine(char result[80]);

  // Description:
  // Internal function to read in a single integer.
  // Returns zero if there was an error.
  int ReadInt(int *result);

  // Description:
  // Internal function to read in an integer array.
  // Returns zero if there was an error.
  int ReadIntArray(int *result, int numInts);
  int ReadIntArrayNoPadding(int *result, int numInts);   //sjk

  // Description:
  // Internal function to read in a float array.
  // Returns zero if there was an error.
  int ReadFloatArray(float *result, int numFloats);
  int ReadFloatArrayNoPadding(float *result, int numFloats);   //sjk

  // Description:
  // Read to the next time step in the geometry file.
  void SkipTimeStep();
  int SkipStructuredGrid(char line[256]);
  int SkipUnstructuredGrid(char line[256]);
  
  // global list of points for the unstructured parts of the model
  int NumberOfUnstructuredPoints;
  vtkPoints* UnstructuredPoints;
  vtkIdTypeArray* UnstructuredNodeIds; // matching of node ids to point ids
  
  int ElementIdsListed;
  
  FILE *IFile;

  int VerifyFileName(char* fileName, char fileType[]);   //sjk

  int endianFlip;   //sjk

private:
  vtkEnSightFortranBinaryReader(const vtkEnSightFortranBinaryReader&);  // Not implemented.
  void operator=(const vtkEnSightFortranBinaryReader&);  // Not implemented.
};

#endif

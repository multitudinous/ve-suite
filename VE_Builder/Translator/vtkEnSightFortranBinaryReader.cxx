/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
/*=========================================================================

  Program:   Visualization Toolkit
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 1993-2003 Steve Kirstukas ISU
All rights reserved.

=========================================================================*/
#include "VE_Builder/Translator/vtkEnSightFortranBinaryReader.h"

#include <vtkByteSwap.h>
#include <vtkCellData.h>
#include <vtkFloatArray.h>
#include <vtkIdList.h>
#include <vtkIdTypeArray.h>
#include <vtkObjectFactory.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkRectilinearGrid.h>
#include <vtkStructuredGrid.h>
#include <vtkStructuredPoints.h>
#include <vtkUnstructuredGrid.h>

#include <ctype.h>

#include "VE_Xplorer/fileIO.h"  //sjk

#ifdef _MSC_VER
#pragma warning (push, 3)
#endif

#include <string>

#ifdef _MSC_VER
#pragma warning(pop)
#endif
using namespace VE_Util;
vtkCxxRevisionMacro(vtkEnSightFortranBinaryReader, "$Revision$");
vtkStandardNewMacro(vtkEnSightFortranBinaryReader);

//----------------------------------------------------------------------------
vtkEnSightFortranBinaryReader::vtkEnSightFortranBinaryReader()
{
  this->NumberOfUnstructuredPoints = 0;
  this->UnstructuredPoints = vtkPoints::New();
  this->UnstructuredNodeIds = NULL;

  this->IFile = NULL;

  this->endianFlip = 0;   //sjk
}

//----------------------------------------------------------------------------
vtkEnSightFortranBinaryReader::~vtkEnSightFortranBinaryReader()
{
  if (this->UnstructuredNodeIds)
    {
    this->UnstructuredNodeIds->Delete();
    this->UnstructuredNodeIds = NULL;
    }
  this->UnstructuredPoints->Delete();
  this->UnstructuredPoints = NULL;
  
  if (this->IFile)
    {
    fclose(this->IFile);
    this->IFile = NULL;
    }
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadGeometryFile(char* fileName, int timeStep)
{
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "geometry") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL

   // read first line 
   int lineRead;
   lineRead = this->ReadLine(line);
   vtkDebugMacro( "\"" << line << "\"");

   if (strncmp(line, "Fortran Binary", 14) != 0)
   {
      vtkErrorMacro("This is not an EnSight Fortran Binary file.");
      return 0;
   }

   vtkDebugMacro( "this->UseFileSets = " << this->UseFileSets);
   int i;

  if (this->UseFileSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->SkipTimeStep();
      }

    while (strncmp(line, "BEGIN TIME STEP", 15) != 0 && lineRead)
      {
      lineRead = this->ReadLine(line);
      }
    }

   // read (three) lines to get to the node id line
   // "Geometry file from GMTEC output                                                 "
   // "This now contains additional wall parts                                         "
   // "node id assign                                                                  "
   do
   {
      this->ReadLine(line);
      vtkDebugMacro( "\"" << line << "\"");
   }
   while (strncmp(line, "node id",7));

   char subLine[81];
   subLine[80] = '\0';  // set last character to NULL
   sscanf(line, " %*s %*s %s", subLine);
   vtkDebugMacro( "subLine is \"" << subLine << "\"");
   int pointIdsListed;

   if (strcmp(subLine, "given") == 0)
   {
      this->UnstructuredNodeIds = vtkIdTypeArray::New();
      pointIdsListed = 1;
   }
   else if (strcmp(subLine, "ignore") == 0)
   {
      pointIdsListed = 1;
   }
   else
   {
      pointIdsListed = 0;
   }
   vtkDebugMacro( "pointIdsListed = " << pointIdsListed << "\"");
  
   // read (one) line to get to the element id line
   do
   {
      this->ReadLine(line);
      vtkDebugMacro( "\"" << line << "\"");
   }
   while (strncmp(line, "element id",10));
   sscanf(line, " %*s %*s %s", subLine);
   vtkDebugMacro( "subLine is \"" << subLine << "\"");
   if (strcmp(subLine, "given") == 0 || strcmp(subLine, "ignore") == 0)
   {
      this->ElementIdsListed = 1;
   }
   else
   {
      this->ElementIdsListed = 0;
   }
   vtkDebugMacro( "this->ElementIdsListed = " << this->ElementIdsListed << "\"");
  
   // read (one) line to get to the coordinates line
   do
   {
      this->ReadLine(line);
      vtkDebugMacro( "\"" << line << "\"");
   }
   while (strncmp(line, "coordinates",11));
   this->ReadInt(&this->NumberOfUnstructuredPoints);
   vtkDebugMacro( "NumberOfUnstructuredPoints = " << this->NumberOfUnstructuredPoints);
   cout << "\tNumberOfCoordinates = " << this->NumberOfUnstructuredPoints << endl;

   this->UnstructuredPoints->Allocate(this->NumberOfUnstructuredPoints);

   if (pointIdsListed)
   {
      int *pointIds = new int[this->NumberOfUnstructuredPoints];
      this->ReadIntArray(pointIds, this->NumberOfUnstructuredPoints);

      if (this->UnstructuredNodeIds)
      {
         int maxId = 0;
         for (i = 0; i < this->NumberOfUnstructuredPoints; i++)
         {
            if (pointIds[i] > maxId)
            {
               maxId = pointIds[i];
            }
         }

         this->UnstructuredNodeIds->Allocate(maxId);
         this->UnstructuredNodeIds->FillComponent(0, -1);

         for (i = 0; i < this->NumberOfUnstructuredPoints; i++)
         {
            this->UnstructuredNodeIds->InsertValue(pointIds[i]-1, i);
         }
      }
      delete [] pointIds;
   }

   cout << "Reading vertex data... ";
   cout.flush();
   float point[3];
   fseek(this->IFile,4L,SEEK_CUR);
   for (i=0; i<this->NumberOfUnstructuredPoints; i++)
   {
      ReadFloatArrayNoPadding( point, 3 );// read a point
      this->UnstructuredPoints->InsertNextPoint( point[0], point[1], point[2] );
   }
   fseek(this->IFile,4L,SEEK_CUR);
   cout << " ...done reading " << this->NumberOfUnstructuredPoints << " lines of vertex data." << endl;
   vtkDebugMacro("Last point:" << "\t" << point[0] << "\t" << point[1] << "\t" << point[2]);

   lineRead = this->ReadLine(line); // "part"
   vtkDebugMacro("lineRead = " << lineRead << ":\t\"" << line << "\"");

   int partId;
   while (lineRead && strncmp(line, "part", 4) == 0)
   {
      sscanf(line, " part %d", &partId);
      partId--; // EnSight starts #ing at 1.

      // part description line
      // "GMTEC Fluid Cells
      lineRead = this->ReadLine(line);
      vtkDebugMacro( "\"" << line << "\"");
      VE_Util::fileIO::StripTrailingSpaces( line );
      cout << "vtkPartId " << partId << ": " << line << endl;

      if (strncmp(line, "block", 5) == 0)
      {
         lineRead = this->CreateStructuredGridOutput(partId, line);
      }
      else  //hexa8,...
      {
         lineRead = this->CreateUnstructuredGridOutput(partId, line);
      }
   }

   fileIO::readToFileEnd( this->IFile );

   fclose(this->IFile);
   this->IFile = NULL;
   return 1;
}

//----------------------------------------------------------------------------
void vtkEnSightFortranBinaryReader::SkipTimeStep()
{
  char line[81], subLine[81];
  line[80] = '\0';      // set last character to NULL
  subLine[80] = '\0';   // set last character to NULL
  int lineRead;
  float *coordinateArray;
  int pointIdsListed;
  int *pointIds;

  this->ReadLine(line);
  while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
    {
    this->ReadLine(line);
    }
  
  // Skip the 2 description lines.
  this->ReadLine(line);
  this->ReadLine(line);
  
  // Read the node id and element id lines.
  this->ReadLine(line); // node id *
  sscanf(line, " %*s %*s %s", subLine);
  if (strcmp(subLine, "given") == 0 ||
      strcmp(subLine, "ignore") == 0)
    {
    pointIdsListed = 1;
    }
  else
    {
    pointIdsListed = 0;
    }
  
  this->ReadLine(line); // element id *
  sscanf(line, " %*s %*s %s", subLine);
  if (strcmp(subLine, "given") == 0 || strcmp(subLine, "ignore") == 0)
    {
    this->ElementIdsListed = 1;
    }
  else
    {
    this->ElementIdsListed = 0;
    }
  
  this->ReadLine(line); // "coordinates"
  this->ReadInt(&this->NumberOfUnstructuredPoints); // number of points
  
  if (pointIdsListed)
    {
    pointIds = new int[this->NumberOfUnstructuredPoints];
    this->ReadIntArray(pointIds, this->NumberOfUnstructuredPoints);
    delete [] pointIds;
    }
  
  coordinateArray = new float[this->NumberOfUnstructuredPoints * 3];
  this->ReadFloatArray(coordinateArray, this->NumberOfUnstructuredPoints * 3);
  delete [] coordinateArray;
  
  lineRead = this->ReadLine(line); // "part"
  
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    this->ReadLine(line); // part description line
    lineRead = this->ReadLine(line);
    
    if (strncmp(line, "block", 5) == 0)
      {
      lineRead = this->SkipStructuredGrid(line);
      }
    else
      {
      lineRead = this->SkipUnstructuredGrid(line);
      }
    }
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::SkipStructuredGrid(char line[])
{
  char subLine[81];
  subLine[80] = '\0';   // set last character to NULL
  int lineRead = 1;
  int iblanked = 0;
  int dimensions[3];
  int numPts;
  float *coordsRead;
  int *iblanks;
  
  if (sscanf(line, " %*s %s", subLine) == 1)
    {
    if (strcmp(subLine, "iblanked") == 0)
      {
      iblanked = 1;
      }
    }

  this->ReadIntArray(dimensions, 3);
  numPts = dimensions[0] * dimensions[1] * dimensions[2];
  
  coordsRead = new float[numPts*3];
  this->ReadFloatArray(coordsRead, numPts*3);
  delete [] coordsRead;
  
  if (iblanked)
    {
    iblanks = new int[numPts];
    this->ReadIntArray(iblanks, numPts);
    delete [] iblanks;
    }
  
  // reading next line to check for EOF
  lineRead = this->ReadLine(line);
  return lineRead;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::SkipUnstructuredGrid(char line[])
{
  int lineRead = 1;
  int *nodeIdList;
  int numElements;
  int cellType;
  
  while(lineRead && strncmp(line, "part", 4) != 0)
    {
    if (strncmp(line, "point", 5) == 0)
      {
      vtkDebugMacro("point");
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      nodeIdList = new int[numElements];
      this->ReadIntArray(nodeIdList, numElements);
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "bar2", 4) == 0)
      {
      vtkDebugMacro("bar2");
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      nodeIdList = new int[numElements * 2];
      this->ReadIntArray(nodeIdList, numElements*2);

      delete [] nodeIdList;
      }
    else if (strncmp(line, "bar3", 4) == 0)
      {
      vtkDebugMacro("bar3");
      vtkWarningMacro("Only vertex nodes of this element will be read.");
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      nodeIdList = new int[numElements * 3];
      this->ReadIntArray(nodeIdList, numElements*3);

      delete [] nodeIdList;
      }
    else if (strncmp(line, "tria3", 5) == 0 ||
             strncmp(line, "tria6", 5) == 0)
      {
      if (strncmp(line, "tria3", 5) == 0)
        {
        vtkDebugMacro("tria3");
        cellType = vtkEnSightReader::TRIA3;
        }
      else
        {
        vtkDebugMacro("tria6");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::TRIA6;
        }
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::TRIA3)
        {
        nodeIdList = new int[numElements * 3];
        this->ReadIntArray(nodeIdList, numElements*3);
        }
      else
        {
        nodeIdList = new int[numElements * 6];
        this->ReadIntArray(nodeIdList, numElements*6);
        }
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "quad4", 5) == 0 ||
             strncmp(line, "quad8", 5) == 0)
      {
      if (strncmp(line, "quad8", 5) == 0)
        {
        vtkDebugMacro("quad8");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::QUAD8;
        }
      else
        {
        vtkDebugMacro("quad4");
        cellType = vtkEnSightReader::QUAD4;
        }
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::QUAD4)
        {
        nodeIdList = new int[numElements * 4];
        this->ReadIntArray(nodeIdList, numElements*4);
        }
      else
        {
        nodeIdList = new int[numElements * 8];
        this->ReadIntArray(nodeIdList, numElements*8);
        }
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "tetra4", 6) == 0 ||
             strncmp(line, "tetra10", 7) == 0)
      {
      if (strncmp(line, "tetra10", 7) == 0)
        {
        vtkDebugMacro("tetra10");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::TETRA10;
        }
      else
        {
        vtkDebugMacro("tetra4");
        cellType = vtkEnSightReader::TETRA4;
        }
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::TETRA4)
        {
        nodeIdList = new int[numElements * 4];
        this->ReadIntArray(nodeIdList, numElements*4);
        }
      else
        {
        nodeIdList = new int[numElements * 10];
        this->ReadIntArray(nodeIdList, numElements*10);
        }
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "pyramid5", 8) == 0 ||
             strncmp(line, "pyramid13", 9) == 0)
      {
      if (strncmp(line, "pyramid13", 9) == 0)
        {
        vtkDebugMacro("pyramid13");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::PYRAMID13;
        }
      else
        {
        vtkDebugMacro("pyramid5");
        cellType = vtkEnSightReader::PYRAMID5;
        }

      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::PYRAMID5)
        {
        nodeIdList = new int[numElements * 5];
        this->ReadIntArray(nodeIdList, numElements*5);
        }
      else
        {
        nodeIdList = new int[numElements * 13];
        this->ReadIntArray(nodeIdList, numElements*13);
        }
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "hexa8", 5) == 0 ||
             strncmp(line, "hexa20", 6) == 0)
      {
      if (strncmp(line, "hexa20", 6) == 0)
        {
        vtkDebugMacro("hexa20");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::HEXA20;
        }
      else
        {
        vtkDebugMacro("hexa8");
        cellType = vtkEnSightReader::HEXA8;
        }
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::HEXA8)
        {
        nodeIdList = new int[numElements * 8];
        this->ReadIntArray(nodeIdList, numElements*8);
        }
      else
        {
        nodeIdList = new int[numElements * 20];
        this->ReadIntArray(nodeIdList, numElements*20);
        }
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "penta6", 6) == 0 ||
             strncmp(line, "penta15", 7) == 0)
      {
      if (strncmp(line, "penta15", 7) == 0)
        {
        vtkDebugMacro("penta15");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::PENTA15;
        }
      else
        {
        vtkDebugMacro("penta6");
        cellType = vtkEnSightReader::PENTA6;
        }
      
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::PENTA6)
        {
        nodeIdList = new int[numElements * 6];
        this->ReadIntArray(nodeIdList, numElements*6);
        }
      else
        {
        nodeIdList = new int[numElements * 15];
        this->ReadIntArray(nodeIdList, numElements*15);
        }
      
      delete [] nodeIdList;
      }
    else if (strncmp(line, "END TIME STEP", 13) == 0)
      {
      break;
      }
    lineRead = this->ReadLine(line);
    }
  
  return lineRead;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadMeasuredGeometryFile(char* fileName,
                                                      int timeStep)
{
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "measured geometry") ) return 0;

   char line[81];
   line[80] = '\0';      // set last character to NULL

   // read first line 
   this->ReadLine(line);
   vtkDebugMacro( "\"" << line << "\"");

   if (strncmp(line, "Fortran Binary", 14) != 0)
   {
      vtkErrorMacro("This is not an EnSight Fortran Binary file.");
      return 0;
   }

  int i;
  int *pointIds;
  float *xCoords, *yCoords, *zCoords;
  vtkPoints *points = vtkPoints::New();
  vtkPolyData *pd = vtkPolyData::New();
  
  this->NumberOfNewOutputs++;

  if (this->GetOutput(this->NumberOfGeometryParts) &&
      ! this->GetOutput(this->NumberOfGeometryParts)->IsA("vtkPolyData"))
    {
    vtkErrorMacro("Cannot change type of output");
    this->OutputsAreValid = 0;
    return 0;
    }
  
   vtkDebugMacro( "this->UseFileSets = " << this->UseFileSets);

  if (this->UseFileSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      
      // Skip the description line.
      this->ReadLine(line);
      vtkDebugMacro( "\"" << line << "\"");
      
      this->ReadLine(line); // "particle coordinates"
      vtkDebugMacro( "\"" << line << "\"");
      
      this->ReadInt(&this->NumberOfMeasuredPoints);
      vtkDebugMacro( "NumberOfMeasuredPoints = " << this->NumberOfUnstructuredPoints);
      cout << "\tNumberOfMeasuredPoints = " << this->NumberOfMeasuredPoints << endl;
      
      pointIds = new int[this->NumberOfMeasuredPoints];
      xCoords = new float [this->NumberOfMeasuredPoints];
      yCoords = new float [this->NumberOfMeasuredPoints];
      zCoords = new float [this->NumberOfMeasuredPoints];
      
      this->ReadIntArray(pointIds, this->NumberOfMeasuredPoints);
      this->ReadFloatArray(xCoords, this->NumberOfMeasuredPoints);
      this->ReadFloatArray(yCoords, this->NumberOfMeasuredPoints);
      this->ReadFloatArray(zCoords, this->NumberOfMeasuredPoints);
      
      delete [] pointIds;
      delete [] xCoords;
      delete [] yCoords;
      delete [] zCoords;
      
      this->ReadLine(line); // END TIME STEP
      }
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
      {
      this->ReadLine(line);
      }
    }
  
   // Skip the description line.
   this->ReadLine(line);
   vtkDebugMacro( "\"" << line << "\"");

   this->ReadLine(line); // "particle coordinates"
   vtkDebugMacro( "\"" << line << "\"");
  
   this->ReadInt(&this->NumberOfMeasuredPoints);
   vtkDebugMacro( "NumberOfMeasuredPoints = " << this->NumberOfMeasuredPoints);
   cout << "\tNumberOfMeasuredPoints = " << this->NumberOfMeasuredPoints << endl;

   this->MeasuredNodeIds->Allocate(this->NumberOfMeasuredPoints);

   pointIds = new int[this->NumberOfMeasuredPoints];
   points->Allocate(this->NumberOfMeasuredPoints);
   pd->Allocate(this->NumberOfMeasuredPoints);

   cout << "Reading measured pointIds" << endl;
   this->ReadIntArray(pointIds, this->NumberOfMeasuredPoints);

   cout << "Reading measured point data... ";
   cout.flush();
   float point[3];
   fseek(this->IFile,4L,SEEK_CUR);
   for (i = 0; i < this->NumberOfMeasuredPoints; i++)
   {
      this->MeasuredNodeIds->InsertNextId(pointIds[i]-1);               // sjk inserted offset of one
      ReadFloatArrayNoPadding( point, 3 );                              // read a point
      points->InsertNextPoint( point[0], point[1], point[2] );          // add point to array
      pd->InsertNextCell(VTK_VERTEX, 1, ((vtkIdType*)&pointIds[i])-1);  // sjk inserted offset of one
   }
   fseek(this->IFile,4L,SEEK_CUR);
   cout << " ...done reading " << this->NumberOfMeasuredPoints << " lines of point data." << endl;
   vtkDebugMacro("Last point:" << "\t" << point[0] << "\t" << point[1] << "\t" << point[2]);

   pd->SetPoints(points);
   vtkDebugMacro("NumberOfGeometryParts = " << this->NumberOfGeometryParts);
   this->SetNthOutput(this->NumberOfGeometryParts, pd);

   points->Delete();
   pd->Delete();
   delete [] pointIds;

   fileIO::readToFileEnd( this->IFile );

   fclose(this->IFile);
   this->IFile = NULL;
   return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadScalarsPerNode(char* fileName,
                                                char* description,
                                                int timeStep, int measured,
                                                int numberOfComponents,
                                                int component)
{
  int lineRead;
  
  int partId, numPts, numParts, i;
  vtkFloatArray *scalars;
  float* scalarsRead;
  fpos_t pos;
  vtkDataSet *output;
  
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "scalar per node") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL

  if (this->UseFileSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      this->ReadLine(line); // skip the description line
      
      fgetpos(this->IFile, &pos);
      this->ReadLine(line); // 1st data line or part #
      if (strncmp(line, "part", 4) != 0)
        {
        fsetpos(this->IFile, &pos);
        if (!measured)
          {
          numPts = this->UnstructuredPoints->GetNumberOfPoints();
          }
        else
          {
          numPts = this->GetOutput(this->NumberOfGeometryParts)->
            GetNumberOfPoints();
          }

        scalarsRead = new float[numPts];
        this->ReadFloatArray(scalarsRead, numPts);
        
        delete [] scalarsRead;
        }
      
      // scalars for structured parts
      while (this->ReadLine(line) && strncmp(line, "part", 4) == 0)
        {
        sscanf(line, " part %d", &partId);
        partId--;
        this->ReadLine(line); // block
        numPts = this->GetOutput(partId)->GetNumberOfPoints();
        scalarsRead = new float[numPts];
        this->ReadFloatArray(scalarsRead, numPts);

        delete [] scalarsRead;
        }
      }
    lineRead = this->ReadLine(line);
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0 && lineRead)
      {
      lineRead = this->ReadLine(line);
      }
    }
  
  this->ReadLine(line); // skip the description line

  fgetpos(this->IFile, &pos);
  lineRead = this->ReadLine(line); // 1st data line or part #
  if (strncmp(line, "part", 4) != 0)
    {
    fsetpos(this->IFile, &pos);
    if (!measured)
      {
      numPts = this->UnstructuredPoints->GetNumberOfPoints();
      }
    else
      {
      numPts = this->GetOutput(this->NumberOfGeometryParts)->
        GetNumberOfPoints();
      }
    if (component == 0)
      {
      scalars = vtkFloatArray::New();
      scalars->SetNumberOfTuples(numPts);
      scalars->SetNumberOfComponents(numberOfComponents);
      scalars->Allocate(numPts * numberOfComponents);
      }
    else
      {
      partId = this->UnstructuredPartIds->GetId(0);
      scalars = (vtkFloatArray*)(this->GetOutput(partId)->GetPointData()->
                                 GetArray(description));
      }
    scalarsRead = new float[numPts];
    this->ReadFloatArray(scalarsRead, numPts);
    for (i = 0; i < numPts; i++)
      {
      scalars->InsertComponent(i, component, scalarsRead[i]);
      }
    
    if (!measured)
      {
      numParts = this->UnstructuredPartIds->GetNumberOfIds();
      for (i = 0; i < numParts; i++)
        {
        partId = this->UnstructuredPartIds->GetId(i);
        output = this->GetOutput(partId);
        if (component == 0)
          {
          scalars->SetName(description);
          output->GetPointData()->AddArray(scalars);
          if (!output->GetPointData()->GetScalars())
            {
            output->GetPointData()->SetScalars(scalars);
            }
          scalars->Delete();
          }
        else
          {
          output->GetPointData()->AddArray(scalars);
          }
        }
      }
    else
      {
      scalars->SetName(description);
      output = this->GetOutput(this->NumberOfGeometryParts);
      output->GetPointData()->AddArray(scalars);
      if (!output->GetPointData()->GetScalars())
        {
        output->GetPointData()->SetScalars(scalars);
        }
      scalars->Delete();
      }
    delete [] scalarsRead;
    }

  // scalars for structured parts
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    sscanf(line, " part %d", &partId);
    partId--;
    output = this->GetOutput(partId);
    this->ReadLine(line); // block
    numPts = output->GetNumberOfPoints();
    scalarsRead = new float[numPts];
    if (component == 0)
      {
      scalars = vtkFloatArray::New();
      scalars->SetNumberOfTuples(numPts);
      scalars->SetNumberOfComponents(numberOfComponents);
      scalars->Allocate(numPts * numberOfComponents);
      }
    else
      {
      scalars = (vtkFloatArray*)(output->GetPointData()->
                                 GetArray(description));
      }
    this->ReadFloatArray(scalarsRead, numPts);
    for (i = 0; i < numPts; i++)
      {
      scalars->InsertComponent(i, component, scalarsRead[i]);        
      }
    if (component == 0)
      {
      scalars->SetName(description);
      output->GetPointData()->AddArray(scalars);
      if (!output->GetPointData()->GetScalars())
        {
        output->GetPointData()->SetScalars(scalars);
        }
      scalars->Delete();
      }
    else
      {
      output->GetPointData()->AddArray(scalars);
      }
    delete [] scalarsRead;
    lineRead = this->ReadLine(line);
    }
  
  fclose(this->IFile);
  this->IFile = NULL;
  return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadVectorsPerNode(char* fileName,
                                                char* description,
                                                int timeStep, int measured)
{
  int lineRead;
  int partId, numPts, i;
  vtkFloatArray *vectors;
  float vector[3];
  float *vectorsRead;
  fpos_t pos;
  vtkDataSet *output;
  
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "vector per node") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL

  if (this->UseFileSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      this->ReadLine(line); // skip the description line
      
      fgetpos(this->IFile, &pos);
      this->ReadLine(line); // 1st data line or part #
      if (strncmp(line, "part", 4) != 0)
        {
        fsetpos(this->IFile, &pos);
        if (!measured)
          {
          numPts = this->UnstructuredPoints->GetNumberOfPoints();
          }
        else
          {
          numPts = this->GetOutput(this->NumberOfGeometryParts)->
            GetNumberOfPoints();
          }
        
        vectorsRead = new float[numPts*3];
        this->ReadFloatArray(vectorsRead, numPts*3);

        delete [] vectorsRead;
        }
      
      // vectors for structured parts
      while (this->ReadLine(line) && strncmp(line, "part", 4) == 0)
        {
        sscanf(line, " part %d", &partId);
        partId--;
        this->ReadLine(line); // block
        numPts = this->GetOutput(partId)->GetNumberOfPoints();
        vectorsRead = new float[numPts*3];
        
        this->ReadFloatArray(vectorsRead, numPts*3);

        delete [] vectorsRead;
        }
      }
    lineRead = this->ReadLine(line);
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0 && lineRead)
      {
      lineRead = this->ReadLine(line);
      }
    }
  
  this->ReadLine(line); // skip the description line

  fgetpos(this->IFile, &pos);
  lineRead = this->ReadLine(line); // 1st data line or part #
  if (strncmp(line, "part", 4) != 0)
    {
    fsetpos(this->IFile, &pos);
    if (!measured)
      {
      numPts = this->UnstructuredPoints->GetNumberOfPoints();
      }
    else
      {
      numPts = this->GetOutput(this->NumberOfGeometryParts)->
        GetNumberOfPoints();
      }
    
    vectors = vtkFloatArray::New();
    vectors->SetNumberOfTuples(numPts);
    vectors->SetNumberOfComponents(3);
    vectors->Allocate(numPts*3);
    vectorsRead = new float[numPts*3];
    this->ReadFloatArray(vectorsRead, numPts*3);
    for (i = 0; i < numPts; i++)
      {
      vector[0] = vectorsRead[3*i];
      vector[1] = vectorsRead[3*i+1];
      vector[2] = vectorsRead[3*i+2];
      vectors->InsertTuple(i, vector);
      }
    
    if (!measured)
      {
      for (i = 0; i < this->UnstructuredPartIds->GetNumberOfIds(); i++)
        {
        partId = this->UnstructuredPartIds->GetId(i);
        output = this->GetOutput(partId);
        vectors->SetName(description);
        output->GetPointData()->AddArray(vectors);
        if (!output->GetPointData()->GetVectors())
          {
          output->GetPointData()->SetVectors(vectors);
          }
        }
      }
    else
      {
      vectors->SetName(description);
      output = this->GetOutput(this->NumberOfGeometryParts);
      output->GetPointData()->AddArray(vectors);
      if (!output->GetPointData()->GetVectors())
        {
        output->GetPointData()->SetVectors(vectors);
        }
      }
    
    vectors->Delete();
    delete [] vectorsRead;
    }

  // vectors for structured parts
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    sscanf(line, " part %d", &partId);
    partId--;
    output = this->GetOutput(partId);
    this->ReadLine(line); // block
    numPts = output->GetNumberOfPoints();
    vectors = vtkFloatArray::New();
    vectors->SetNumberOfTuples(numPts);
    vectors->SetNumberOfComponents(3);
    vectors->Allocate(numPts*3);
    vectorsRead = new float[numPts*3];
    
    this->ReadFloatArray(vectorsRead, numPts*3);
    for (i = 0; i < numPts; i++)
      {
      vector[0] = vectorsRead[3*i];
      vector[1] = vectorsRead[3*i+1];
      vector[2] = vectorsRead[3*i+2];
      vectors->InsertTuple(i, vector);
      }
      
    vectors->SetName(description);
    output->GetPointData()->AddArray(vectors);
    if (!output->GetPointData()->GetVectors())
      {
      output->GetPointData()->SetVectors(vectors);
      }
    vectors->Delete();
    delete [] vectorsRead;
    lineRead = this->ReadLine(line);
    }
  
  fclose(this->IFile);
  this->IFile = NULL;
  return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadTensorsPerNode(char* fileName,
                                                char* description,
                                                int timeStep)
{
  int lineRead;
  int partId, numPts, i;
  vtkFloatArray *tensors;
  float tensor[6];
  float* tensorsRead;
  fpos_t pos;
  vtkDataSet *output;
  
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "tensor symm per node") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL

  if (this->UseTimeSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      this->ReadLine(line); // skip the description line
      
      fgetpos(this->IFile, &pos);
      this->ReadLine(line); // 1st data line or part #
      if (strncmp(line, "part", 4) != 0)
        {
        fsetpos(this->IFile, &pos);
        numPts = this->UnstructuredPoints->GetNumberOfPoints();
        tensorsRead = new float[numPts*6];
        this->ReadFloatArray(tensorsRead, numPts*6);

        delete [] tensorsRead;
        }
      
      // vectors for structured parts
      while (this->ReadLine(line) &&
             strncmp(line, "part", 4) == 0)
        {
        sscanf(line, " part %d", &partId);
        partId--;
        this->ReadLine(line); // block
        numPts = this->GetOutput(partId)->GetNumberOfPoints();
        tensorsRead = new float[numPts*6];
        this->ReadFloatArray(tensorsRead, numPts*6);

        delete [] tensorsRead;
        }      
      }
    this->ReadLine(line);
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
      {
      this->ReadLine(line);
      }
    }
  
  this->ReadLine(line); // skip the description line

  fgetpos(this->IFile, &pos);
  lineRead = this->ReadLine(line); // 1st data line or part #
  if (strncmp(line, "part", 4) != 0)
    {
    fsetpos(this->IFile, &pos);
    numPts = this->UnstructuredPoints->GetNumberOfPoints();
    tensors = vtkFloatArray::New();
    tensors->SetNumberOfTuples(numPts);
    tensors->SetNumberOfComponents(6);
    tensors->Allocate(numPts*6);
    tensorsRead = new float[numPts*6];
    this->ReadFloatArray(tensorsRead, numPts*6);
    for (i = 0; i < numPts; i++)
      {
      tensor[0] = tensorsRead[6*i];
      tensor[1] = tensorsRead[6*i+1];
      tensor[2] = tensorsRead[6*i+2];
      tensor[3] = tensorsRead[6*i+3];
      tensor[4] = tensorsRead[6*i+4];
      tensor[5] = tensorsRead[6*i+5];
      tensors->InsertTuple(i, tensor);
      }

    for (i = 0; i < this->UnstructuredPartIds->GetNumberOfIds(); i++)
      {
      partId = this->UnstructuredPartIds->GetId(i);
      tensors->SetName(description);
      this->GetOutput(partId)->GetPointData()->AddArray(tensors);
      }
    tensors->Delete();
    delete [] tensorsRead;
    }

  // vectors for structured parts
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    sscanf(line, " part %d", &partId);
    partId--;
    output = this->GetOutput(partId);
    this->ReadLine(line); // block
    numPts = output->GetNumberOfPoints();
    tensors = vtkFloatArray::New();
    tensors->SetNumberOfTuples(numPts);
    tensors->SetNumberOfComponents(6);
    tensors->Allocate(numPts*6);
    tensorsRead = new float[numPts*6];
    this->ReadFloatArray(tensorsRead, numPts*6);
    
    for (i = 0; i < numPts; i++)
      {
      tensor[0] = tensorsRead[6*i];
      tensor[1] = tensorsRead[6*i+1];
      tensor[2] = tensorsRead[6*i+2];
      tensor[3] = tensorsRead[6*i+3];
      tensor[4] = tensorsRead[6*i+4];
      tensor[5] = tensorsRead[6*i+5];      
      tensors->InsertTuple(i, tensor);
      }
    
    tensors->SetName(description);
    output->GetPointData()->AddArray(tensors);
    tensors->Delete();
    delete [] tensorsRead;
    
    lineRead = this->ReadLine(line);
    }
  
  fclose(this->IFile);
  this->IFile = NULL;
  return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadScalarsPerElement(char* fileName,
                                                   char* description,
                                                   int timeStep,
                                                   int numberOfComponents,
                                                   int component)
{
  int partId, numCells, numCellsPerElement, i, idx;
  vtkFloatArray *scalars;
  int elementType;
  float* scalarsRead;
  vtkDataSet *output;
  
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "scalar per element") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL
   int lineRead;

  if (this->UseFileSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      this->ReadLine(line); // skip the description line
      lineRead = this->ReadLine(line);
      
      while (lineRead && strncmp(line, "part", 4) == 0)
        {
        sscanf(line, " part %d", &partId);
        partId--; // EnSight starts #ing with 1.
        numCells = this->GetOutput(partId)->GetNumberOfCells();
        lineRead = this->ReadLine(line); // element type or "block"
        
        // need to find out from CellIds how many cells we have of this element
        // type (and what their ids are) -- IF THIS IS NOT A BLOCK SECTION
        if (strcmp(line, "block") != 0)
          {
          while (lineRead && strncmp(line, "part", 4) != 0 &&
                 strncmp(line, "END TIME STEP", 13) != 0)
            {
            elementType = this->GetElementType(line);
            if (elementType < 0)
              {
              vtkErrorMacro("invalid element type");
              fclose(this->IFile);
              this->IFile = NULL;
              return 0;
              }
            idx = this->UnstructuredPartIds->IsId(partId);
            numCellsPerElement = this->CellIds[idx][elementType]->
              GetNumberOfIds();
            scalarsRead = new float[numCellsPerElement];
            this->ReadFloatArray(scalarsRead, numCellsPerElement);

            delete [] scalarsRead;
            lineRead = this->ReadLine(line);
            } // end while
          }
        else
          {
          scalarsRead = new float[numCells];
          this->ReadFloatArray(scalarsRead, numCells);

          delete [] scalarsRead;
          lineRead = this->ReadLine(line);
          }
        }
      }
    this->ReadLine(line);
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
      {
      this->ReadLine(line);
      }
    }
  
  this->ReadLine(line); // skip the description line
  lineRead = this->ReadLine(line);
  
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    sscanf(line, " part %d", &partId);
    partId--; // EnSight starts #ing with 1.
    output = this->GetOutput(partId);
    numCells = output->GetNumberOfCells();
    lineRead = this->ReadLine(line); // element type or "block"
    if (component == 0)
      {
      scalars = vtkFloatArray::New();
      scalars->SetNumberOfTuples(numCells);
      scalars->SetNumberOfComponents(numberOfComponents);
      scalars->Allocate(numCells * numberOfComponents);
      }
    else
      {
      scalars = (vtkFloatArray*)(output->GetCellData()->GetArray(description));
      }
    
    // need to find out from CellIds how many cells we have of this element
    // type (and what their ids are) -- IF THIS IS NOT A BLOCK SECTION
    if (strcmp(line, "block") != 0)
      {
      while (lineRead && strncmp(line, "part", 4) != 0 &&
        strncmp(line, "END TIME STEP", 13) != 0)
        {
        elementType = this->GetElementType(line);
        if (elementType < 0)
          {
          vtkErrorMacro("invalid element type");
          fclose(this->IFile);
          this->IFile = NULL;
          return 0;
          }
        idx = this->UnstructuredPartIds->IsId(partId);
        numCellsPerElement = this->CellIds[idx][elementType]->GetNumberOfIds();
        scalarsRead = new float[numCellsPerElement];
        this->ReadFloatArray(scalarsRead, numCellsPerElement);
        for (i = 0; i < numCellsPerElement; i++)
          {
          scalars->InsertComponent(this->CellIds[idx][elementType]->GetId(i),
                                   component, scalarsRead[i]);
          }
        delete [] scalarsRead;
        lineRead = this->ReadLine(line);
        } // end while
      }
    else
      {
      scalarsRead = new float[numCells];
      this->ReadFloatArray(scalarsRead, numCells);
      for (i = 0; i < numCells; i++)
        {
        scalars->InsertComponent(i, component, scalarsRead[i]);
        }
      delete [] scalarsRead;
      lineRead = this->ReadLine(line);
      }
    
    if (component == 0)
      {
      scalars->SetName(description);
      output->GetCellData()->AddArray(scalars);
      if (!output->GetCellData()->GetScalars())
        {
        output->GetCellData()->SetScalars(scalars);
        }
      scalars->Delete();
      }
    else
      {
      output->GetCellData()->AddArray(scalars);
      }
    }
  
  fclose(this->IFile);
  this->IFile = NULL;
  return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadVectorsPerElement(char* fileName,
                                                   char* description,
                                                   int timeStep)
{
  int partId, numCells, numCellsPerElement, i, idx;
  vtkFloatArray *vectors;
  int elementType;
  float vector[3];
  float *vectorsRead;
  vtkDataSet *output;
  
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "vector per element") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL
   int lineRead;

  if (this->UseFileSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      this->ReadLine(line); // skip the description line
      lineRead = this->ReadLine(line);
      
      while (lineRead && strncmp(line, "part", 4) == 0)
        {
        sscanf(line, " part %d", &partId);
        partId--; // EnSight starts #ing with 1.
        numCells = this->GetOutput(partId)->GetNumberOfCells();
        lineRead = this->ReadLine(line); // element type or "block"
        
        // need to find out from CellIds how many cells we have of this element
        // type (and what their ids are) -- IF THIS IS NOT A BLOCK SECTION
        if (strcmp(line, "block") != 0)
          {
          while (lineRead && strncmp(line, "part", 4) != 0 &&
                  strncmp(line, "END TIME STEP", 13) != 0)
            {
            elementType = this->GetElementType(line);
            if (elementType < 0)
              {
              vtkErrorMacro("invalid element type");
              delete this->IS;
              this->IS = NULL;
              return 0;
              }
            idx = this->UnstructuredPartIds->IsId(partId);
            numCellsPerElement =
              this->CellIds[idx][elementType]->GetNumberOfIds();
            vectorsRead = new float[numCellsPerElement*3];
            this->ReadFloatArray(vectorsRead, numCellsPerElement*3);

            delete [] vectorsRead;
            lineRead = this->ReadLine(line);
            } // end while
          }
        else
          {
          vectorsRead = new float[numCells*3];
          this->ReadFloatArray(vectorsRead, numCells*3);

          delete [] vectorsRead;
          lineRead = this->ReadLine(line);
          }
        }
      }
    this->ReadLine(line);
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
      {
      this->ReadLine(line);
      }
    }
  
  this->ReadLine(line); // skip the description line
  lineRead = this->ReadLine(line);
  
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    vectors = vtkFloatArray::New();
    sscanf(line, " part %d", &partId);
    partId--; // EnSight starts #ing with 1.
    output = this->GetOutput(partId);
    numCells = output->GetNumberOfCells();
    lineRead = this->ReadLine(line); // element type or "block"
    vectors->SetNumberOfTuples(numCells);
    vectors->SetNumberOfComponents(3);
    vectors->Allocate(numCells*3);
    
    // need to find out from CellIds how many cells we have of this element
    // type (and what their ids are) -- IF THIS IS NOT A BLOCK SECTION
    if (strcmp(line, "block") != 0)
      {
      while (lineRead && strncmp(line, "part", 4) != 0 &&
        strncmp(line, "END TIME STEP", 13) != 0)
        {
        elementType = this->GetElementType(line);
        if (elementType < 0)
          {
          vtkErrorMacro("invalid element type");
          delete this->IS;
          this->IS = NULL;
          return 0;
          }
        idx = this->UnstructuredPartIds->IsId(partId);
        numCellsPerElement = this->CellIds[idx][elementType]->GetNumberOfIds();
        vectorsRead = new float[numCellsPerElement*3];
        this->ReadFloatArray(vectorsRead, numCellsPerElement*3);
        
        for (i = 0; i < numCellsPerElement; i++)
          {
          vector[0] = vectorsRead[3*i];
          vector[1] = vectorsRead[3*i+1];
          vector[2] = vectorsRead[3*i+2];
          vectors->InsertTuple(this->CellIds[idx][elementType]->GetId(i),
                               vector);
          }
        delete [] vectorsRead;
        lineRead = this->ReadLine(line);
        } // end while
      }
    else
      {
      vectorsRead = new float[numCells*3];
      this->ReadFloatArray(vectorsRead, numCells*3);
      for (i = 0; i < numCells; i++)
        {
        vector[0] = vectorsRead[3*i];
        vector[1] = vectorsRead[3*i+1];
        vector[2] = vectorsRead[3*i+2];
        vectors->InsertTuple(i, vector);
        }
      delete [] vectorsRead;
      lineRead = this->ReadLine(line);
      }
    vectors->SetName(description);
    output->GetCellData()->AddArray(vectors);
    if (!output->GetCellData()->GetVectors())
      {
      output->GetCellData()->SetVectors(vectors);
      }
    vectors->Delete();
    }
  
  fclose(this->IFile);
  this->IFile = NULL;
  return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::ReadTensorsPerElement(char* fileName,
                                                   char* description,
                                                   int timeStep)
{
  int partId, numCells, numCellsPerElement, i, idx;
  vtkFloatArray *tensors;
  int elementType;
  float tensor[6];
  float *tensorsRead;
  vtkDataSet *output;
  
   // Using the path and filename try to open the file specified in the case file...
   if ( ! this->VerifyFileName(fileName, "tensor per element") ) return 0;

   char line[81];
   line[80] = '\0';  // set last character to NULL
   int lineRead;

  if (this->UseTimeSets)
    {
    for (i = 0; i < timeStep - 1; i++)
      {
      this->ReadLine(line);
      while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
        {
        this->ReadLine(line);
        }
      this->ReadLine(line); // skip the description line
      lineRead = this->ReadLine(line);
      
      while (lineRead && strncmp(line, "part", 4) == 0)
        {
        sscanf(line, " part %d", &partId);
        partId--; // EnSight starts #ing with 1.
        numCells = this->GetOutput(partId)->GetNumberOfCells();
        lineRead = this->ReadLine(line); // element type or "block"
        
        // need to find out from CellIds how many cells we have of this element
        // type (and what their ids are) -- IF THIS IS NOT A BLOCK SECTION
        if (strcmp(line, "block") != 0)
          {
          while (lineRead && strncmp(line, "part", 4) != 0 &&
                 strncmp(line, "END TIME STEP", 13) != 0)
            {
            elementType = this->GetElementType(line);
            if (elementType < 0)
              {
              vtkErrorMacro("invalid element type");
              fclose(this->IFile);
              this->IFile = NULL;
              return 0;
              }
            idx = this->UnstructuredPartIds->IsId(partId);
            numCellsPerElement = this->CellIds[idx][elementType]->
              GetNumberOfIds();
            tensorsRead = new float[numCellsPerElement*6];
            this->ReadFloatArray(tensorsRead, numCellsPerElement*6);

            delete [] tensorsRead;
            lineRead = this->ReadLine(line);
            } // end while
          }
        else
          {
          tensorsRead = new float[numCells*6];
          this->ReadFloatArray(tensorsRead, numCells*6);

          delete [] tensorsRead;
          lineRead = this->ReadLine(line);
          }
        }
      }
    this->ReadLine(line);
    while (strncmp(line, "BEGIN TIME STEP", 15) != 0)
      {
      this->ReadLine(line);
      }
    }
  
  this->ReadLine(line); // skip the description line
  lineRead = this->ReadLine(line);
  
  while (lineRead && strncmp(line, "part", 4) == 0)
    {
    tensors = vtkFloatArray::New();
    sscanf(line, " part %d", &partId);
    partId--; // EnSight starts #ing with 1.
    output = this->GetOutput(partId);
    numCells = output->GetNumberOfCells();
    lineRead = this->ReadLine(line); // element type or "block"
    tensors->SetNumberOfTuples(numCells);
    tensors->SetNumberOfComponents(6);
    tensors->Allocate(numCells*6);
    
    // need to find out from CellIds how many cells we have of this element
    // type (and what their ids are) -- IF THIS IS NOT A BLOCK SECTION
    if (strcmp(line, "block") != 0)
      {
      while (lineRead && strncmp(line, "part", 4) != 0 &&
             strncmp(line, "END TIME STEP", 13) != 0)
        {
        elementType = this->GetElementType(line);
        if (elementType < 0)
          {
          vtkErrorMacro("invalid element type");
          fclose(this->IFile);
          this->IFile = NULL;
          return 0;
          }
        idx = this->UnstructuredPartIds->IsId(partId);
        numCellsPerElement = this->CellIds[idx][elementType]->GetNumberOfIds();
        tensorsRead = new float[numCellsPerElement*6];
        this->ReadFloatArray(tensorsRead, numCellsPerElement*6);
        
        for (i = 0; i < numCellsPerElement; i++)
          {
          tensor[0] = tensorsRead[6*i];
          tensor[1] = tensorsRead[6*i+1];
          tensor[2] = tensorsRead[6*i+2];
          tensor[3] = tensorsRead[6*i+3];
          tensor[4] = tensorsRead[6*i+4];
          tensor[5] = tensorsRead[6*i+5];
          
          tensors->InsertTuple(this->CellIds[idx][elementType]->GetId(i),
                               tensor);
          }
        delete [] tensorsRead;
        lineRead = this->ReadLine(line);
        } // end while
      }
    else
      {
      tensorsRead = new float[numCells*6];
      this->ReadFloatArray(tensorsRead, numCells*6);
      
      for (i = 0; i < numCells; i++)
        {
        tensor[0] = tensorsRead[6*i];
        tensor[1] = tensorsRead[6*i+1];
        tensor[2] = tensorsRead[6*i+2];
        tensor[3] = tensorsRead[6*i+3];
        tensor[4] = tensorsRead[6*i+4];
        tensor[5] = tensorsRead[6*i+5];
        tensors->InsertTuple(i, tensor);
        }
      delete [] tensorsRead;
      lineRead = this->ReadLine(line);
      }
    tensors->SetName(description);
    output->GetCellData()->AddArray(tensors);
    tensors->Delete();
    }
  
  fclose(this->IFile);
  this->IFile = NULL;
  return 1;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::CreateUnstructuredGridOutput(int partId,
                                                          char line[])
{
  int lineRead = 1;
  int i, j;
  int *nodeIdList;
  vtkIdType *nodeIds;
  int numElements;
  int idx, cellType;
  vtkIdType cellId;

  this->NumberOfNewOutputs++;
  
  if (this->GetOutput(partId) == NULL)
    {
    vtkDebugMacro("creating new unstructured output");
    vtkUnstructuredGrid* ugrid = vtkUnstructuredGrid::New();
    this->SetNthOutput(partId, ugrid);
    ugrid->Delete();
    
    this->UnstructuredPartIds->InsertNextId(partId);

    idx = this->UnstructuredPartIds->IsId(partId);
    if (this->CellIds == NULL)
      {
      this->CellIds = new vtkIdList **[16];
      }
    
    this->CellIds[idx] = new vtkIdList *[16];
    for (i = 0; i < 16; i++)
      {
      this->CellIds[idx][i] = vtkIdList::New();
      }
    }
  else if ( ! this->GetOutput(partId)->IsA("vtkUnstructuredGrid"))
    {
    vtkErrorMacro("Cannot change type of output");
    this->OutputsAreValid = 0;
    return 0;
    }
  else
    {
    idx = this->UnstructuredPartIds->IsId(partId);
    for (i = 0; i < 16; i++)
      {
      this->CellIds[idx][i]->Reset();
      }
    }
  
  ((vtkUnstructuredGrid *)this->GetOutput(partId))->Allocate(1000);
  
  while(lineRead && strncmp(line, "part", 4) != 0)
    {
    if (strncmp(line, "point", 5) == 0)
      {
      vtkDebugMacro("point");
      
      nodeIds = new vtkIdType[1];
      this->ReadInt(&numElements);
      vtkDebugMacro("numElements = " << numElements);

      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      nodeIdList = new int[numElements];
      this->ReadIntArray(nodeIdList, numElements);
      
      for (i = 0; i < numElements; i++)
        {
        nodeIds[0] = nodeIdList[i] - 1;
        if (this->UnstructuredNodeIds)
          {
          nodeIds[0] = this->UnstructuredNodeIds->GetValue(nodeIds[0]);
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_VERTEX, 1, nodeIds);
        this->CellIds[idx][vtkEnSightReader::POINT]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "bar2", 4) == 0)
      {
      vtkDebugMacro("bar2");
      
      nodeIds = new vtkIdType[2];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      nodeIdList = new int[numElements * 2];
      this->ReadIntArray(nodeIdList, numElements*2);
      
      for (i = 0; i < numElements; i++)
        {
        for (j = 0; j < 2; j++)
          {
          nodeIds[j] = nodeIdList[2*i+j] - 1;
          }

        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 2; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_LINE, 2, nodeIds);
        this->CellIds[idx][vtkEnSightReader::BAR2]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "bar3", 4) == 0)
      {
      vtkDebugMacro("bar3");
      vtkWarningMacro("Only vertex nodes of this element will be read.");
      
      nodeIds = new vtkIdType[2];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      nodeIdList = new int[numElements * 3];
      this->ReadIntArray(nodeIdList, numElements*3);
      
      for (i = 0; i < numElements; i++)
        {
        for (j = 0; j < 2; j++)
          {
          nodeIds[j] = nodeIdList[3*i+2*j] - 1;
          }

        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 2; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_LINE, 2, nodeIds);
        this->CellIds[idx][vtkEnSightReader::BAR3]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "tria3", 5) == 0 ||
             strncmp(line, "tria6", 5) == 0)
      {
      if (strncmp(line, "tria3", 5) == 0)
        {
        vtkDebugMacro("tria3");
        cellType = vtkEnSightReader::TRIA3;
        }
      else
        {
        vtkDebugMacro("tria6");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::TRIA6;
        }
      
      nodeIds = new vtkIdType[3];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::TRIA3)
        {
        nodeIdList = new int[numElements * 3];
        this->ReadIntArray(nodeIdList, numElements*3);
        }
      else
        {
        nodeIdList = new int[numElements * 6];
        this->ReadIntArray(nodeIdList, numElements*6);
        }
      
      for (i = 0; i < numElements; i++)
        {
        if (cellType == vtkEnSightReader::TRIA3)
          {
          for (j = 0; j < 3; j++)
            {
            nodeIds[j] = nodeIdList[3*i+j] - 1;
            }
          }
        else
          {
          for (j = 0; j < 3; j++)
            {
            nodeIds[j] = nodeIdList[6*i+j] - 1;
            }
          }
        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 3; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_TRIANGLE, 3, nodeIds);
        this->CellIds[idx][cellType]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "quad4", 5) == 0 ||
             strncmp(line, "quad8", 5) == 0)
      {
      if (strncmp(line, "quad8", 5) == 0)
        {
        vtkDebugMacro("quad8");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::QUAD8;
        }
      else
        {
        vtkDebugMacro("quad4");
        cellType = vtkEnSightReader::QUAD4;
        }
      
      nodeIds = new vtkIdType[4];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::QUAD4)
        {
        nodeIdList = new int[numElements * 4];
        this->ReadIntArray(nodeIdList, numElements*4);
        }
      else
        {
        nodeIdList = new int[numElements * 8];
        this->ReadIntArray(nodeIdList, numElements*8);
        }
      
      for (i = 0; i < numElements; i++)
        {
        if (cellType == vtkEnSightReader::QUAD4)
          {
          for (j = 0; j < 4; j++)
            {
            nodeIds[j] = nodeIdList[4*i+j] - 1;
            }
          }
        else
          {
          for (j = 0; j < 4; j++)
            {
            nodeIds[j] = nodeIdList[8*i+j] - 1;
            }
          }
        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 4; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_QUAD, 4, nodeIds);
        this->CellIds[idx][cellType]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "tetra4", 6) == 0 ||
             strncmp(line, "tetra10", 7) == 0)
      {
      if (strncmp(line, "tetra10", 7) == 0)
        {
        vtkDebugMacro("tetra10");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::TETRA10;
        }
      else
        {
        vtkDebugMacro("tetra4");
        cellType = vtkEnSightReader::TETRA4;
        }
      
      nodeIds = new vtkIdType[4];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::TETRA4)
        {
        nodeIdList = new int[numElements * 4];
        this->ReadIntArray(nodeIdList, numElements*4);
        }
      else
        {
        nodeIdList = new int[numElements * 10];
        this->ReadIntArray(nodeIdList, numElements*10);
        }
      
      for (i = 0; i < numElements; i++)
        {
        if (cellType == vtkEnSightReader::TETRA4)
          {
          for (j = 0; j < 4; j++)
            {
            nodeIds[j] = nodeIdList[4*i+j] - 1;
            }
          }
        else
          {
          for (j = 0; j < 4; j++)
            {
            nodeIds[j] = nodeIdList[10*i+j] - 1;
            }
          }
        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 3; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_TETRA, 4, nodeIds);
        this->CellIds[idx][cellType]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "pyramid5", 8) == 0 ||
             strncmp(line, "pyramid13", 9) == 0)
      {
      if (strncmp(line, "pyramid13", 9) == 0)
        {
        vtkDebugMacro("pyramid13");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::PYRAMID13;
        }
      else
        {
        vtkDebugMacro("pyramid5");
        cellType = vtkEnSightReader::PYRAMID5;
        }

      nodeIds = new vtkIdType[5];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::PYRAMID5)
        {
        nodeIdList = new int[numElements * 5];
        this->ReadIntArray(nodeIdList, numElements*5);
        }
      else
        {
        nodeIdList = new int[numElements * 13];
        this->ReadIntArray(nodeIdList, numElements*13);
        }
      
      for (i = 0; i < numElements; i++)
        {
        if (cellType == vtkEnSightReader::PYRAMID5)
          {
          for (j = 0; j < 5; j++)
            {
            nodeIds[j] = nodeIdList[5*i+j] - 1;
            }
          }
        else
          {
          for (j = 0; j < 5; j++)
            {
            nodeIds[j] = nodeIdList[13*i+j] - 1;
            }
          }
        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 5; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_PYRAMID, 5, nodeIds);
        this->CellIds[idx][cellType]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "hexa8", 5) == 0 ||
             strncmp(line, "hexa20", 6) == 0)
      {
      if (strncmp(line, "hexa20", 6) == 0)
        {
        vtkDebugMacro("hexa20");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::HEXA20;
        }
      else
        {
        vtkDebugMacro("hexa8");
        cellType = vtkEnSightReader::HEXA8;
        }
      
      nodeIds = new vtkIdType[8];
      this->ReadInt(&numElements);
      vtkDebugMacro("numElements = " << numElements);

      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::HEXA8)
        {
        nodeIdList = new int[numElements * 8];
        this->ReadIntArray(nodeIdList, numElements*8);
        }
      else
        {
        nodeIdList = new int[numElements * 20];
        this->ReadIntArray(nodeIdList, numElements*20);
        }
      
      for (i = 0; i < numElements; i++)
        {
        if (cellType == vtkEnSightReader::HEXA8)
          {
          for (j = 0; j < 8; j++)
            {
            nodeIds[j] = nodeIdList[8*i+j] - 1;
            }
          }
        else
          {
          for (j = 0; j < 8; j++)
            {
            nodeIds[j] = nodeIdList[20*i+j] - 1;
            }
          }
        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 8; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_HEXAHEDRON, 8, nodeIds);
        this->CellIds[idx][cellType]->InsertNextId(cellId);
        }
      if (cellType == vtkEnSightReader::HEXA8) vtkDebugMacro("Last HEXA8 cell:" 
         << "\t" << nodeIds[0] << "\t" << nodeIds[1] << "\t" << nodeIds[2] << "\t" << nodeIds[3] 
         << "\t" << nodeIds[4] << "\t" << nodeIds[5] << "\t" << nodeIds[6] << "\t" << nodeIds[7] );
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    else if (strncmp(line, "penta6", 6) == 0 ||
             strncmp(line, "penta15", 7) == 0)
      {
      if (strncmp(line, "penta15", 7) == 0)
        {
        vtkDebugMacro("penta15");
        vtkWarningMacro("Only vertex nodes of this element will be read.");
        cellType = vtkEnSightReader::PENTA15;
        }
      else
        {
        vtkDebugMacro("penta6");
        cellType = vtkEnSightReader::PENTA6;
        }
      
      nodeIds = new vtkIdType[6];
      this->ReadInt(&numElements);
      if (this->ElementIdsListed)
        {
        // There is probably a better way to advance the file pointer.
        int *tempArray = new int[numElements];
        this->ReadIntArray(tempArray, numElements);
        delete [] tempArray;
        }
      
      if (cellType == vtkEnSightReader::PENTA6)
        {
        nodeIdList = new int[numElements * 6];
        this->ReadIntArray(nodeIdList, numElements*6);
        }
      else
        {
        nodeIdList = new int[numElements * 15];
        this->ReadIntArray(nodeIdList, numElements*15);
        }
      
      for (i = 0; i < numElements; i++)
        {
        if (cellType == vtkEnSightReader::PENTA6)
          {
          for (j = 0; j < 6; j++)
            {
            nodeIds[j] = nodeIdList[6*i+j] - 1;
            }
          }
        else
          {
          for (j = 0; j < 6; j++)
            {
            nodeIds[j] = nodeIdList[15*i+j] - 1;
            }
          }
        if (this->UnstructuredNodeIds)
          {
          for (j = 0; j < 6; j++)
            {
            nodeIds[j] = this->UnstructuredNodeIds->GetValue(nodeIds[j]);
            }
          }
        cellId = ((vtkUnstructuredGrid*)this->GetOutput(partId))->
          InsertNextCell(VTK_WEDGE, 6, nodeIds);
        this->CellIds[idx][cellType]->InsertNextId(cellId);
        }
      delete [] nodeIds;
      delete [] nodeIdList;
      }
    lineRead = this->ReadLine(line);
    vtkDebugMacro("at bottom of loop, lineRead = " << lineRead << ", line is \"" << line << "\"");
    }

  ((vtkUnstructuredGrid*)this->GetOutput(partId))->
    SetPoints(this->UnstructuredPoints);
  vtkDebugMacro("leaving CreateUnstructuredGridOutput with lineRead = " << lineRead);
  return lineRead;
}

//----------------------------------------------------------------------------
int vtkEnSightFortranBinaryReader::CreateStructuredGridOutput(int partId,
                                                        char line[])
{
  char subLine[81];
  line[80] = '\0';  // set last character to NULL
  int lineRead = 1;
  int iblanked = 0;
  int dimensions[3];
  int i;
  vtkPoints *points = vtkPoints::New();
  int numPts;
  float *coordsRead;
  int *iblanks;
  
  this->NumberOfNewOutputs++;
  
  if (this->GetOutput(partId) == NULL)
    {
    vtkDebugMacro("creating new structured grid output");
    vtkStructuredGrid* sgrid = vtkStructuredGrid::New();
    this->SetNthOutput(partId, sgrid);
    sgrid->Delete();
    }
  else if ( ! this->GetOutput(partId)->IsA("vtkStructuredGrid"))
    {
    vtkErrorMacro("Cannot change type of output");
    this->OutputsAreValid = 0;
    return 0;
    }
  
  if (sscanf(line, " %*s %s", subLine) == 1)
    {
    if (strcmp(subLine, "iblanked") == 0)
      {
      iblanked = 1;
      }
    }

  this->ReadIntArray(dimensions, 3);
  ((vtkStructuredGrid*)this->GetOutput(partId))->SetDimensions(dimensions);
  ((vtkStructuredGrid*)this->GetOutput(partId))->
    SetWholeExtent(0, dimensions[0]-1, 0, dimensions[1]-1, 0, dimensions[2]-1);
  numPts = dimensions[0] * dimensions[1] * dimensions[2];
  points->Allocate(numPts);
  
  coordsRead = new float[numPts*3];
  this->ReadFloatArray(coordsRead, numPts*3);
  
  for (i = 0; i < numPts; i++)
    {
    points->InsertNextPoint(coordsRead[i], coordsRead[numPts+i],
                            coordsRead[2*numPts+i]);
    }
  
  delete [] coordsRead;
  
  ((vtkStructuredGrid*)this->GetOutput(partId))->SetPoints(points);  
  if (iblanked)
    {
    ((vtkStructuredGrid*)this->GetOutput(partId))->BlankingOn();
    iblanks = new int[numPts];
    this->ReadIntArray(iblanks, numPts);
    for (i = 0; i < numPts; i++)
      {
      if (!iblanks[i])
        {
        ((vtkStructuredGrid*)this->GetOutput(partId))->BlankPoint(i);
        }
      }
    delete [] iblanks;
    }
  
  points->Delete();
  // reading next line to check for EOF
  lineRead = this->ReadLine(line);
  return lineRead;
}

/*
// Internal function to read in a line up to 80 characters.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadLine(char result[80])
{
  fread(result, sizeof(char), 80, this->IFile);

  if (feof(this->IFile) || ferror(this->IFile))
    {
    return 0;
    }
  
  return 1;
}

// Internal function to read a single integer.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadInt(int *result)
{
  fread(result, sizeof(int), 1, this->IFile);
  if (feof(this->IFile) || ferror(this->IFile))
    {
    return 0;
    }
  if (this->ByteOrder == FILE_LITTLE_ENDIAN)
    {
    vtkByteSwap::Swap4LE(result);
    }
  else
    {
    vtkByteSwap::Swap4BE(result);
    }
  
  return 1;
}

// Internal function to read an integer array.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadIntArray(int *result,
                                          int numInts)
{
  fread(result, sizeof(int), numInts, this->IFile);
  if (feof(this->IFile) || ferror(this->IFile))
    {
    return 0;
    }
  if (this->ByteOrder == FILE_LITTLE_ENDIAN)
    {
    vtkByteSwap::Swap4LERange(result, numInts);
    }
  else
    {
    vtkByteSwap::Swap4BERange(result, numInts);
    }
  
  return 1;
}

// Internal function to read a float array.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadFloatArray(float *result,
                                            int numFloats)
{
  fread(result, sizeof(float), numFloats, this->IFile);
  if (feof(this->IFile) || ferror(this->IFile))
    {
    return 0;
    }

  if (this->ByteOrder == FILE_LITTLE_ENDIAN)
    {
    vtkByteSwap::Swap4LERange(result, numFloats);
    }
  else
    {
    vtkByteSwap::Swap4BERange(result, numFloats);
    }
  
  return 1;
}
*/

// Internal function to read in an 80-character line.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadLine(char result[])
{
   //sjk redid to handle padded binary
   fseek(this->IFile,4L,SEEK_CUR);
   if (fread(result, sizeof(char), 80, this->IFile) != 80) return 0;
   fseek(this->IFile,4L,SEEK_CUR);

   if (feof(this->IFile) || ferror(this->IFile)) return 0;  // doesn't seem to work: sjk
   return 1;
}

// Internal function to read a single integer.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadInt(int *result)
{
   //sjk redid to handle padded binary
   fseek(this->IFile,4L,SEEK_CUR);
   if (fileIO::readNByteBlockFromFile( result, sizeof(int), 1, this->IFile, this->endianFlip))
      return 0;
   fseek(this->IFile,4L,SEEK_CUR);
   return 1;
}

// Internal function to read an integer array.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadIntArray(int *result, int numInts)
{
   //sjk redid to handle padded binary
   fseek(this->IFile,4L,SEEK_CUR);
   if (fileIO::readNByteBlockFromFile( result, sizeof(int), numInts, this->IFile, this->endianFlip))
      return 0;
   fseek(this->IFile,4L,SEEK_CUR);
   return 1;
}

// Internal function to read a int array.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadIntArrayNoPadding(int *result, int numFloats )
{
   //sjk redid to handle binary
   if (fileIO::readNByteBlockFromFile( result, sizeof(int), numFloats, this->IFile, this->endianFlip))
      return 0;
   return 1;
}

// Internal function to read a float array.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadFloatArray(float *result, int numFloats)
{
   //sjk redid to handle padded binary
   fseek(this->IFile,4L,SEEK_CUR);
   if (fileIO::readNByteBlockFromFile( result, sizeof(float), numFloats, this->IFile, this->endianFlip))
      return 0;
   fseek(this->IFile,4L,SEEK_CUR);
   return 1;
}

// Internal function to read a float array.
// Returns zero if there was an error.
int vtkEnSightFortranBinaryReader::ReadFloatArrayNoPadding(float *result, int numFloats)
{
   //sjk redid to handle binary
   if (fileIO::readNByteBlockFromFile( result, sizeof(float), numFloats, this->IFile, this->endianFlip))
      return 0;
   return 1;
}

int vtkEnSightFortranBinaryReader::VerifyFileName(char* fileName, char fileType[])
{
   // Create full name from path and filename ...
   if (!fileName)
   {
      vtkErrorMacro("NULL " << fileType << " file name");
      return 0;
   }

   char * inputFile = new char[ strlen(this->FilePath) + strlen(fileName) + 1 ]; //remember space for null character
   if (this->FilePath)
   {
      strcpy(inputFile, this->FilePath);
      strcat(inputFile, fileName);
   }
   else
   {
      strcpy(inputFile, fileName);
   }
   vtkDebugMacro("full path to " << fileType << " file: \"" << inputFile << "\"");

   this->IFile = fopen(inputFile, "rb");
   if (this->IFile == NULL)
   {
      vtkErrorMacro("Unable to open file: " << inputFile);
      delete [] inputFile;
      return 0;
   }
   delete [] inputFile;
   return 1;
}

//----------------------------------------------------------------------------
void vtkEnSightFortranBinaryReader::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}

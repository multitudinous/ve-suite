/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h>  // this code requires VTK4
#include <vtkPointData.h>
#include <vtkCellType.h>

#include "converter.h"      // for "letUsersAddParamsToField"

// for info on avs format...
// http://www.ncsa.uiuc.edu/Divisions/Communities/CSM/courses/csm-02/avs-ucd/format.html
vtkUnstructuredGrid * avsReader( std::string fluentAVSFileName, int debug )
{
   vtkUnstructuredGrid * uGrid = NULL;

   if ( debug )
      std::cout << "AVS input file is " << fluentAVSFileName << std::endl;

   std::fstream fvert;
   fvert.open( fluentAVSFileName.c_str(), std::ios::in );

   if ( !fvert.is_open() )
   {
      std::cout <<"\n\nERROR - Cannot open the designated file\n"<< std::endl;
      return uGrid;
   }

   int i, j, k;               // counters

   char sjunk[ 256 ];         // variable to store junk words

   // TODO: The following line assumes just one comment, but in avs format
   // there can be many comments preceded by the '#' sign
   fvert >> sjunk;            // skip over # sign

   int numOrigVertices = 0;
   fvert >> numOrigVertices;  // read in number of vertices

   int num_cells = 0;         // the exact number of cells in data
   fvert >> num_cells;        // read in number of cells

   int numDataColumns = 0;
   fvert >> numDataColumns;   // read in number of node-based data columns

   int junk = 0;              // variable to store junk ints
   fvert >> junk;             // length of vector data associated with cells
   fvert >> junk;             // length of vector data associated with model
   
   if ( debug )
   {
      std::cout << "numOrigVertices = " << numOrigVertices << std::endl;
      std::cout << "num_cells = " << num_cells << std::endl;
      std::cout << "numDataColumns=" << numDataColumns << std::endl;
   }

   std::cout << "\nReading vertex data...";
   std::cout.flush();

   // Read the first vertex line...
   // Assign the first vertexId to vShift, the offset of the vertex numbering
   int vShift = 0;
   fvert >> vShift;
   int maxOrigVertexId = vShift;

   // Read the first vertex coordinates. Store the first z coordinate
   float pts[ 3 ];
   for ( j=0; j < 3; j++ )
      fvert >> pts[ j ];
   float firstZCoord = pts[ 2 ];

   // Read the remaining vertex numbers and update vShift as needed
   // Also, watch if the vertices lie in the same z-plane
   int is2D = 1;   //assume we are looking at 2D data (z coord is zero)
   int vertexIdTemp;
   for ( i=1; i < numOrigVertices; i++ )
   {
      fvert >> vertexIdTemp;
      if ( vShift > vertexIdTemp )
         vShift = vertexIdTemp;

      if ( maxOrigVertexId < vertexIdTemp )
         maxOrigVertexId = vertexIdTemp;

      for ( j=0; j < 3; j++ )
         fvert >> pts[ j ];

      if ( firstZCoord != pts[ 2 ] )
         is2D = 0;
   }

   int maxVertexId = maxOrigVertexId - vShift;

   if ( debug )
   {
      std::cout << "\n\tAfter reading in " << numOrigVertices
           << " vertices..." << std::endl;
      std::cout << "\tmaxOrigVertexId = " << maxOrigVertexId << std::endl;
      std::cout << "\tvShift = " << vShift << std::endl;
      std::cout << "\tmaxVertexId = " << maxVertexId << std::endl;
      std::cout << "\tis2D = " << is2D << std::endl;
   }

   // ifstream doesn't have a rewind function,
   // but this takes it back to the beginning...
   fvert.clear();                // forget if we hit the end of file
   fvert.seekg( 0, std::ios::beg );   // move to the start of the file
   
   fvert >> sjunk;               // skip over # sign
   fvert >> numOrigVertices;     // read in number of vertices
   fvert >> num_cells;           // read in number of cells
   fvert >> numDataColumns;      // read in number of data columns
   fvert >> junk;                // skip over next two unknown values
   fvert >> junk;

   // allocate array to store vertex identification numbers
   int * vertexId;
   if ( is2D )
   {
      // for 2D case, make room for double the number of vertices
      vertexId = new int [ numOrigVertices * 2 ];
   }
   else
   {
      vertexId = new int [ numOrigVertices ];
   }
   
   vtkPoints *v = vtkPoints::New();
   for ( i=0; i < numOrigVertices; i++ )
   {
      // read vertexId and the corresponding coordinates...
      fvert >> vertexId[ i ];
      vertexId[ i ] -= vShift;
      for ( j=0; j < 3; j++ )
         fvert >> pts[ j ];
      
      v->InsertPoint( vertexId[ i ], pts[ 0 ], pts[ 1 ], pts[ 2 ] );
      if ( debug > 1 )
      {
         std::cout << "\tInserted point # " << vertexId[ i ] << ": " << "\t" 
              << pts[ 0 ] << "\t" << pts[ 1 ] << "\t" << pts[ 2 ] << std::endl;
      }

      if ( is2D )
      {
         int newVertexId = vertexId[ i ] + maxVertexId + 1;
         v->InsertPoint( newVertexId, pts[ 0 ], pts[ 1 ], (pts[ 2 ]+1.0) );
         if ( debug > 1 )
         {
            std::cout << "\tInserted point # " << newVertexId << ": " << "\t" 
                 << pts[ 0 ] << "\t" << pts[ 1 ] << "\t" << (pts[ 2 ]+1.0)
                 << std::endl;
         }
      }
   }
   std::cout << " ...done reading " << numOrigVertices
        << " lines of vertex data." << std::endl;

   uGrid = vtkUnstructuredGrid::New();
   uGrid->SetPoints( v );
   v->Delete();

   std::cout << "\nReading cell data...";
   std::cout.flush();

   // Will use function strtok to pull off text and integers from a text string
   char cellDataLine[ 256 ];
   char excludes[] =" ,\t"; // won't read in any spaces, commas, or tabs)
   char * pointerToCharArray;

   fvert.getline( cellDataLine, 256 );         // go to next line

   // read cellID, cell type, and the corresponding vertices...
   int cellID;          // variable to locate cell vertex points
   int cPt[ 8 ];        // variable used to store cells when read
   for ( k=0; k < num_cells; k++ )
   { 
      fvert.getline( cellDataLine, 256 );
      if ( debug > 1 )
         std::cout << cellDataLine << std::endl;

      // extract first word (cell identifier number)
      pointerToCharArray = strtok( cellDataLine, excludes );
      if (pointerToCharArray == NULL)
      {
         std::cout << "\n\nERROR: Could not find cell identifier number\n" << std::endl;
         continue;
      }
      cellID = atoi( pointerToCharArray );
      
      // extract next word (material number)
      pointerToCharArray = strtok( NULL, excludes );
      if (pointerToCharArray == NULL)
      {
         std::cout << "\n\nERROR: Could not find cell group number\n" << std::endl;
         continue;
      }
      junk = atoi( pointerToCharArray );

      // extract next word (cell type)
      pointerToCharArray = strtok( NULL, excludes );
      if (pointerToCharArray == NULL)
      {
         std::cout << "\n\nERROR: Could not find cell type\n" << std::endl;
         continue;
      }
      strcpy( sjunk, pointerToCharArray );

      if ( debug > 1 )
      {
         std::cout << "\tcell identifier number is " << cellID
              << ", cell group number is " << junk
              << ", cell type is \"" << sjunk <<  "\"" << std::endl; 
      }

      if ( !strcmp( sjunk, "tet" ) )
      {
         for ( j=0; j < 4; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               std::cout << "\n\nERROR: missing \'tet\' vertex data\n" << std::endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
         }
         uGrid->InsertNextCell(VTK_TETRA, 4, cPt);
         if ( debug > 1 )
         {
            std::cout << "\tInserted tetrahedron cell with vertices: " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << std::endl; 
         }
      }
      else if ( !strcmp( sjunk, "pyr" ) ) 
      {
         for ( j=0; j < 5; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               std::cout << "\n\nERROR: missing \'pyr\' vertex data\n" << std::endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
         }
         uGrid->InsertNextCell( VTK_PYRAMID, 5, cPt );
         if ( debug > 1 )
         {
            std::cout << "\tInserted a pyramid cell with vertices:   " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << std::endl; 
         }
      }
      else if ( !strcmp( sjunk, "hex" ) ) 
      {
         for ( int j=0; j < 8; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               std::cout << "\n\nERROR: missing \'hex\' vertex data\n" << std::endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
         }
         uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cPt );
         if ( debug > 1 )
         {
            std::cout << "\tInserted hexahedron cell with vertices:  " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << "\t" << cPt[ 5 ]
                 << "\t" << cPt[ 6 ] << "\t" << cPt[ 7 ] << std::endl;
         }
      }
      else if ( !strcmp( sjunk, "quad" ) )
      {
         // this is a 2D element - can be converted into a 3D hex
         for ( j=0; j < 4; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               std::cout << "\n\nERROR: missing \'quad\' vertex data\n" << std::endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
            cPt[j+4] = cPt[ j ] + maxVertexId + 1;
         }
         uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cPt );
         if ( debug > 1 )
         {
            std::cout << "\tInserted hexahedron cell with vertices:  " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << "\t" << cPt[ 5 ]
                 << "\t" << cPt[ 6 ] << "\t" << cPt[ 7 ] << std::endl;
         }
      }
      else if ( !strcmp( sjunk, "tri" ) )
      {
         // this is a 2D element - can be converted into a 3D wedge
         for ( j=0; j < 3; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               std::cout << "\n\nERROR: missing \'tri\' vertex data\n" << std::endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
            cPt[j+3] = cPt[ j ] + maxVertexId + 1;
         }
         uGrid->InsertNextCell( VTK_WEDGE, 6, cPt );
         if ( debug > 1 )
         {
            std::cout << "\tInserted wedge cell with vertices:  " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << "\t" << cPt[ 5 ]
                 << std::endl;
         }
      }
      else
      {
         std::cout << "\n\nERROR: Unsupported cell type \"" << sjunk
              << "\"\n" << std::endl;
         continue;
      }
   }
   std::cout << " ...done reading " << num_cells << " lines of cell data." << std::endl;

   std::cout << "\nReading solution data...";
   std::cout.flush();

   int numSolnValues;
   fvert >> numSolnValues;      // the number of columns of data per vertex
   if ( debug )
      std::cout << "\n\tnumSolnValues = " << numSolnValues << std::endl;

   if ( numSolnValues != numDataColumns )
   {
      std::cout << "\n\nWARNING: numSolnValues (" << numSolnValues 
           << ") != numDataColumns (" << numDataColumns << ")\n" << std::endl;
   }

   int junk2;
   for ( i=0; i < numSolnValues; i++ )
      fvert >> junk2;                                 // skip over the ones

   fvert.getline( cellDataLine, 256 );                // go to next line
   char * pDataColumnTitle;      
   char ** dataColumnTitle = new char * [numSolnValues];
   for ( i=0; i < numSolnValues; i++ )
      dataColumnTitle[ i ] = new char[100];

   // count the number of parameters (scalars and vectors)
   int numParameters = 0;
   int numVectorComponents = 2;
   for ( i=0; i < numSolnValues; i++ )   // two words for each column of data
   {
      fvert.getline( cellDataLine, 256 );
      if ( debug > 1 )
      {
         std::cout << cellDataLine << std::endl;
      }

      // extract data column identifier, i.e., "vel-magnitude" or "x-velocity"
      pDataColumnTitle = strtok( cellDataLine, excludes );
      if ( pDataColumnTitle == NULL )
      {
         std::cout << "\nERROR: unexpected column identifier read in" << std::endl;
      }
      strcpy( dataColumnTitle[ i ], pDataColumnTitle );
      if ( debug > 1 )
      {
         std::cout << "\tdata column identifier: " << dataColumnTitle[ i ] << std::endl;
      }

      // looking for a word that starts with "x-" 
      if ( !strcspn( dataColumnTitle[ i ], "x-" ) )
         numParameters++;
      else if ( !strcspn( dataColumnTitle[ i ], "y-" ) ) 
         continue;
      else if ( !strcspn( dataColumnTitle[ i ], "z-" ) )
      {
         numVectorComponents = 3;
         continue;
      }
      else
         numParameters++;

     // extract next word (units identifier)
     pointerToCharArray = strtok( NULL, excludes );
     if ( pointerToCharArray == NULL )
     {
         std::cout << "ERROR: unexpected units identifier read in" << std::endl;
     }
     strcpy( sjunk, pointerToCharArray );
     if ( debug > 1 )
     {
        std::cout << "\tunits identifier: " << sjunk << std::endl;
     }
   }

   if ( debug )
   {
      std::cout << "\n\tnumParameters = " << numParameters << std::endl;
      std::cout << "\tnum components per vector = " << numVectorComponents
           << "\n" << std::endl;
   }

   if ( ! numParameters )
   {
      std::cerr << "ERROR: file does not contain any parameters, so exiting" << std::endl;
      uGrid->Delete();
      uGrid = NULL;
      return uGrid;
   }
   
   // set up arrays to store scalar and vector data over entire mesh...
   vtkFloatArray ** parameterData = NULL;
   parameterData = new vtkFloatArray * [ numParameters ];
   for ( i=0; i < numParameters; i++ )
   {
      parameterData[ i ] = vtkFloatArray::New();
/*
      if ( parameterData[ i ] == NULL )
      {
         std::cerr << "ERROR: can't get memory for parameterData, exiting" << std::endl;
         uGrid->Delete();
         uGrid = NULL;
         return uGrid;
      }
*/
   }

   if ( debug )
   {
      std::cout << std::endl;
   }

   // specify the name and number of components for each of the parameter arrays
   int jj = 0;
   for ( i=0; i < numSolnValues; i++ )
   {
      if      ( !strcspn( dataColumnTitle[ i ], "x-" ) )
      {
         //strip off the "x-"
         parameterData[ jj ]->SetName( &dataColumnTitle[ i ][ 2 ] );
         parameterData[ jj ]->SetNumberOfComponents( 3 );
         if ( is2D )
            parameterData[ jj ]->SetNumberOfTuples( (maxVertexId+1)*2 );
         else
            parameterData[ jj ]->SetNumberOfTuples( maxVertexId+1 );
      }
      else if ( !strcspn( dataColumnTitle[ i ], "y-" ) )
         continue;
      else if ( !strcspn( dataColumnTitle[ i ], "z-" ) )
         continue;
      else
      {
         parameterData[ jj ]->SetName( dataColumnTitle[ i ] );
         parameterData[ jj ]->SetNumberOfComponents( 1 );
         if ( is2D )
            parameterData[ jj ]->SetNumberOfTuples( (maxVertexId+1)*2 );
         else
            parameterData[ jj ]->SetNumberOfTuples( maxVertexId+1 );
      }

      if ( debug )
      {
         std::cout << "parameter array " << jj << " is named \""
              << parameterData[ jj ]->GetName() << "\"" << std::endl;
      }
      jj++;
   }

   // initialize all components to zero
   for ( i=0; i < numParameters; i++ )
   {
      for ( k=0; k < parameterData[ i ]->GetNumberOfComponents(); k++ )
      {
         parameterData[ i ]->FillComponent( k, 0.0 );
      }
   }

   // create an array to store scalar and vector data at a single vertex
   float * solnValue = new float [ numSolnValues ];

   // In avs files there is a 1-to-1 correspondence between solution points
   // and vertices.  We find that the vertex numbering is usually one-based,
   // the solution numbering may be zero- or one-based.  The thing to do is
   // ignore the vertex numbering in the solution section that follows.
   for ( j=0; j < numOrigVertices; j++ )
   {
      fvert >> junk;

      // Read (and optionally print to screen) scalar and vector data
      // at a single vertex
      for ( i=0; i < numSolnValues; i++ )
         fvert >> solnValue[ i ];

      if ( debug > 1 )
      {
         std::cout << "\nvertexId[" << j << "] = " << vertexId[j] << ", solnValue:";
         for ( i=0; i < numSolnValues; i++ )
            std::cout << "\t" << solnValue[ i ];
         std::cout << std::endl;
      }

      // load the data into the appropriate scalar or vector data array...
      int jj = 0;
      for ( i=0; i < numSolnValues; i++ )
      {
         // see if the dataColumnTitle starts with "x-"
         if ( !strcspn( dataColumnTitle[ i ], "x-") )
         {
            for ( k=0; k<numVectorComponents; k++ )
            {
               parameterData[ jj ]
                  ->SetComponent( vertexId[ j ], k, solnValue[i+k] );
            }

            // give a boost, for loop will increment one more to properly
            // go to next quantity
            i += (numVectorComponents-1);
         }
         else  //then we have scalar data...
         {
            parameterData[ jj ]->SetTuple1( vertexId[ j ], solnValue[ i ] );
         }

         if ( debug > 1 ) 
         {
            std::cout << "\tparameter " << jj << ":";
            std::cout.flush();
            for ( k=0; k < parameterData[ jj ]->GetNumberOfComponents(); k++ )
            {
               std::cout << "\t" 
                    << parameterData[ jj ]->GetComponent(vertexId[ j ],k);
            }
            std::cout << std::endl;
         }
         jj++;
      }
   }

   std::cout << " ...done reading " << numOrigVertices << " lines of solution data."
        << std::endl << std::endl;
   fvert.close();

   // expand 2D data into 3D data...
   if ( is2D ) 
   {
      if ( debug > 1 )
      {
         std::cout << "expanding 2D data into 3D data..." << std::endl;
      }

      for ( int i=0; i < numParameters; i++ )
      {
         // for each vertex, reproduce tuples to expand 2D to 3D data...
         for ( int j=0; j < maxVertexId+1; j++ )
         {
            if ( debug > 1 )
            {
               std::cout << "setting vertex = " << vertexId[ j ] + maxVertexId + 1
                    << "\t" << parameterData[ i ]->GetComponent(vertexId[j],0)
                    << std::endl;
            }
            parameterData[ i ]->SetTuple( vertexId[ j ] + maxVertexId + 1,
                              parameterData[ i ]->GetTuple( vertexId[ j ] ) ); 
         }

         if ( debug > 2 )
         {
            // Breaks Deere's Build
            //parameterData[ i ]->Print( std::cout );
         }
      }
   }

   // Optionally print to screen the parameter data arrays
   if ( debug > 1 )
   {
      std::cout << "parameterData" << std::endl;
      for ( j=0; j < numOrigVertices; j++ )
      {
         for ( i=0; i < numParameters; i++ )
            for ( k=0; k < parameterData[ i ]->GetNumberOfComponents(); k++ )
               std::cout << "\t" << parameterData[ i ]->GetComponent(vertexId[j],k);
         std::cout << std::endl;
      }
   }

   // Set selected scalar and vector quantities to be written to pointdata array
   letUsersAddParamsToField( numParameters, parameterData, 
                             uGrid->GetPointData() );

   for ( i=0; i < numParameters; i++ )
      parameterData[ i ]->Delete();
   delete [] parameterData;
   parameterData = NULL;

   for ( i=0; i<numSolnValues; i++ )
      delete dataColumnTitle[ i ];
   delete [] dataColumnTitle;
   dataColumnTitle = NULL;

   delete [] solnValue;
   solnValue = NULL;

   delete [] vertexId;
   vertexId = NULL;

   return uGrid;
}


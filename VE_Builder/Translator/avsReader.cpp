/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: avsReader.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdio>

#include "vtkUnstructuredGrid.h"
#include "vtkPoints.h"
#include "vtkFloatArray.h"  // this code requires VTK4
#include "vtkPointData.h"
#include "vtkCellType.h"

#include "converter.h"      // for "letUsersAddParamsToField"

// for info on avs format...
// http://www.ncsa.uiuc.edu/Divisions/Communities/CSM/courses/csm-02/avs-ucd/format.html
vtkUnstructuredGrid * avsReader( char * fluentAVSFileName, int debug )
{
   vtkUnstructuredGrid * uGrid = NULL;

   if ( debug )
      cout << "AVS input file is " << fluentAVSFileName << endl;

   ifstream fvert;
   fvert.open( fluentAVSFileName, ios::in );

   if ( fvert == NULL )
   {
      cout <<"\n\nERROR - Cannot open the designated file\n"<< endl;
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
      cout << "numOrigVertices = " << numOrigVertices << endl;
      cout << "num_cells = " << num_cells << endl;
      cout << "numDataColumns=" << numDataColumns << endl;
   }

   cout << "\nReading vertex data...";
   cout.flush();

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
      cout << "\n\tAfter reading in " << numOrigVertices
           << " vertices..." << endl;
      cout << "\tmaxOrigVertexId = " << maxOrigVertexId << endl;
      cout << "\tvShift = " << vShift << endl;
      cout << "\tmaxVertexId = " << maxVertexId << endl;
      cout << "\tis2D = " << is2D << endl;
   }

   // ifstream doesn't have a rewind function,
   // but this takes it back to the beginning...
   fvert.clear();                // forget if we hit the end of file
   fvert.seekg( 0, ios::beg );   // move to the start of the file
   
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
         cout << "\tInserted point # " << vertexId[ i ] << ": " << "\t" 
              << pts[ 0 ] << "\t" << pts[ 1 ] << "\t" << pts[ 2 ] << endl;
      }

      if ( is2D )
      {
         int newVertexId = vertexId[ i ] + maxVertexId + 1;
         v->InsertPoint( newVertexId, pts[ 0 ], pts[ 1 ], (pts[ 2 ]+1.0) );
         if ( debug > 1 )
         {
            cout << "\tInserted point # " << newVertexId << ": " << "\t" 
                 << pts[ 0 ] << "\t" << pts[ 1 ] << "\t" << (pts[ 2 ]+1.0)
                 << endl;
         }
      }
   }
   cout << " ...done reading " << numOrigVertices
        << " lines of vertex data." << endl;

   uGrid = vtkUnstructuredGrid::New();
   uGrid->SetPoints( v );
   v->Delete();

   cout << "\nReading cell data...";
   cout.flush();

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
         cout << cellDataLine << endl;

      // extract first word (cell identifier number)
      pointerToCharArray = strtok( cellDataLine, excludes );
      if (pointerToCharArray == NULL)
      {
         cout << "\n\nERROR: Could not find cell identifier number\n" << endl;
         continue;
      }
      cellID = atoi( pointerToCharArray );
      
      // extract next word (material number)
      pointerToCharArray = strtok( NULL, excludes );
      if (pointerToCharArray == NULL)
      {
         cout << "\n\nERROR: Could not find cell group number\n" << endl;
         continue;
      }
      junk = atoi( pointerToCharArray );

      // extract next word (cell type)
      pointerToCharArray = strtok( NULL, excludes );
      if (pointerToCharArray == NULL)
      {
         cout << "\n\nERROR: Could not find cell type\n" << endl;
         continue;
      }
      strcpy( sjunk, pointerToCharArray );

      if ( debug > 1 )
      {
         cout << "\tcell identifier number is " << cellID
              << ", cell group number is " << junk
              << ", cell type is \"" << sjunk <<  "\"" << endl; 
      }

      if ( !strcmp( sjunk, "tet" ) )
      {
         for ( j=0; j < 4; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               cout << "\n\nERROR: missing \'tet\' vertex data\n" << endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
         }
         uGrid->InsertNextCell(VTK_TETRA, 4, cPt);
         if ( debug > 1 )
         {
            cout << "\tInserted tetrahedron cell with vertices: " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << endl; 
         }
      }
      else if ( !strcmp( sjunk, "pyr" ) ) 
      {
         for ( j=0; j < 5; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               cout << "\n\nERROR: missing \'pyr\' vertex data\n" << endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
         }
         uGrid->InsertNextCell( VTK_PYRAMID, 5, cPt );
         if ( debug > 1 )
         {
            cout << "\tInserted a pyramid cell with vertices:   " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << endl; 
         }
      }
      else if ( !strcmp( sjunk, "hex" ) ) 
      {
         for ( int j=0; j < 8; j++ ) 
         {
            pointerToCharArray = strtok( NULL, excludes );     // get next word
            if (pointerToCharArray == NULL)
            {
               cout << "\n\nERROR: missing \'hex\' vertex data\n" << endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
         }
         uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cPt );
         if ( debug > 1 )
         {
            cout << "\tInserted hexahedron cell with vertices:  " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << "\t" << cPt[ 5 ]
                 << "\t" << cPt[ 6 ] << "\t" << cPt[ 7 ] << endl;
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
               cout << "\n\nERROR: missing \'quad\' vertex data\n" << endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
            cPt[j+4] = cPt[ j ] + maxVertexId + 1;
         }
         uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cPt );
         if ( debug > 1 )
         {
            cout << "\tInserted hexahedron cell with vertices:  " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << "\t" << cPt[ 5 ]
                 << "\t" << cPt[ 6 ] << "\t" << cPt[ 7 ] << endl;
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
               cout << "\n\nERROR: missing \'tri\' vertex data\n" << endl;
               exit( 1 );
            }
            cPt[ j ] = atoi( pointerToCharArray );             // vertex number
            cPt[ j ] -= vShift;
            cPt[j+3] = cPt[ j ] + maxVertexId + 1;
         }
         uGrid->InsertNextCell( VTK_WEDGE, 6, cPt );
         if ( debug > 1 )
         {
            cout << "\tInserted wedge cell with vertices:  " 
                 << "\t" << cPt[ 0 ] << "\t" << cPt[ 1 ] << "\t" << cPt[ 2 ]
                 << "\t" << cPt[ 3 ] << "\t" << cPt[ 4 ] << "\t" << cPt[ 5 ]
                 << endl;
         }
      }
      else
      {
         cout << "\n\nERROR: Unsupported cell type \"" << sjunk
              << "\"\n" << endl;
         continue;
      }
   }
   cout << " ...done reading " << num_cells << " lines of cell data." << endl;

   cout << "\nReading solution data...";
   cout.flush();

   int numSolnValues;
   fvert >> numSolnValues;      // the number of columns of data per vertex
   if ( debug )
      cout << "\n\tnumSolnValues = " << numSolnValues << endl;

   if ( numSolnValues != numDataColumns )
   {
      cout << "\n\nWARNING: numSolnValues (" << numSolnValues 
           << ") != numDataColumns (" << numDataColumns << ")\n" << endl;
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
         cout << cellDataLine << endl;
      }

      // extract data column identifier, i.e., "vel-magnitude" or "x-velocity"
      pDataColumnTitle = strtok( cellDataLine, excludes );
      if ( pDataColumnTitle == NULL )
      {
         cout << "\nERROR: unexpected column identifier read in" << endl;
      }
      strcpy( dataColumnTitle[ i ], pDataColumnTitle );
      if ( debug > 1 )
      {
         cout << "\tdata column identifier: " << dataColumnTitle[ i ] << endl;
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
         cout << "ERROR: unexpected units identifier read in" << endl;
     }
     strcpy( sjunk, pointerToCharArray );
     if ( debug > 1 )
     {
        cout << "\tunits identifier: " << sjunk << endl;
     }
   }

   if ( debug )
   {
      cout << "\n\tnumParameters = " << numParameters << endl;
      cout << "\tnum components per vector = " << numVectorComponents
           << "\n" << endl;
   }

   if ( ! numParameters )
   {
      cerr << "ERROR: file does not contain any parameters, so exiting" << endl;
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
         cerr << "ERROR: can't get memory for parameterData, exiting" << endl;
         uGrid->Delete();
         uGrid = NULL;
         return uGrid;
      }
*/
   }

   if ( debug )
   {
      cout << endl;
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
         cout << "parameter array " << jj << " is named \""
              << parameterData[ jj ]->GetName() << "\"" << endl;
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
         cout << "\nvertexId[" << j << "] = " << vertexId[j] << ", solnValue:";
         for ( i=0; i < numSolnValues; i++ )
            cout << "\t" << solnValue[ i ];
         cout << endl;
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
            cout << "\tparameter " << jj << ":";
            cout.flush();
            for ( k=0; k < parameterData[ jj ]->GetNumberOfComponents(); k++ )
            {
               cout << "\t" 
                    << parameterData[ jj ]->GetComponent(vertexId[ j ],k);
            }
            cout << endl;
         }
         jj++;
      }
   }

   cout << " ...done reading " << numOrigVertices << " lines of solution data."
        << endl << endl;
   fvert.close();

   // expand 2D data into 3D data...
   if ( is2D ) 
   {
      if ( debug > 1 )
      {
         cout << "expanding 2D data into 3D data..." << endl;
      }

      for ( int i=0; i < numParameters; i++ )
      {
         // for each vertex, reproduce tuples to expand 2D to 3D data...
         for ( int j=0; j < maxVertexId+1; j++ )
         {
            if ( debug > 1 )
            {
               cout << "setting vertex = " << vertexId[ j ] + maxVertexId + 1
                    << "\t" << parameterData[ i ]->GetComponent(vertexId[j],0)
                    << endl;
            }
            parameterData[ i ]->SetTuple( vertexId[ j ] + maxVertexId + 1,
                              parameterData[ i ]->GetTuple( vertexId[ j ] ) ); 
         }

         if ( debug > 2 )
         {
            parameterData[ i ]->Print( cout );
         }
      }
   }

   // Optionally print to screen the parameter data arrays
   if ( debug > 1 )
   {
      cout << "parameterData" << endl;
      for ( j=0; j < numOrigVertices; j++ )
      {
         for ( i=0; i < numParameters; i++ )
            for ( k=0; k < parameterData[ i ]->GetNumberOfComponents(); k++ )
               cout << "\t" << parameterData[ i ]->GetComponent(vertexId[j],k);
         cout << endl;
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


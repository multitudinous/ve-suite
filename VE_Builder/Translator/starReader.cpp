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
 * File:          $RCSfile: starReader.cpp,v $
 * Date modified: $Date: 2004/03/23 16:32:49 $
 * Version:       $Revision: 1.15 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "starReader.h"

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <fstream>

#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h> // this code requires VTK4
#include <vtkPointData.h>
#include <vtkCellType.h>
#include "converter.h"     // for "letUsersAddParamsToField"
#include "fileIO.h"        // for "getTagAndValue"

starReader::starReader( char * paramFile )
{
   strcpy( this->paramFileName, paramFile );

   this->debug = 0;

   for ( int i = 0; i < 3; i++ )
   {
      this->rotations[ i ] = 0.0f;
      this->translations[ i ] = 0.0f;
   }
   this->scaleValue = 1.0f;
   this->numScalars = 0;
   this->numVectors = 0;
   this->writeOption = 0;  // default is to let vtk write the file
   strcpy(this->vtkFileName,"starFlowdata.vtk");
}

starReader::~starReader( void )
{
   if ( this->numScalars > 0 )
   {
      for ( int i = 0; i < this->numScalars; i++ )
         delete [] this->scalarName[ i ];

      this->scalarName.clear();
   }

   if ( this->numVectors > 0 )
   {
      for ( int i = 0; i < this->numVectors; i++ )
         delete [] this->vectorName[ i ];

      this->vectorName.clear();
   }
}

void starReader::SetDebugLevel( int _debug )
{
   this->debug = _debug;
}

vtkUnstructuredGrid * starReader::GetUnsGrid( void )
{
   vtkUnstructuredGrid * uGrid = NULL;

   FILE * fvertices = fopen( this->starVertFileName, "r" );

   if ( fvertices == NULL )
   {
      cerr <<"\nError - Cannot open the designated vertex file: "
           << this->starVertFileName << endl;
      return uGrid;
   }

   cout << "\nReading vertex data from " << this->starVertFileName << endl;

   // The first thing we do is count the vertices and compute the minimum
   // and maximum vertex IDs

   // Read the first vertex line and assign the first vertexId to vShift,
   // the offset of the vertex numbering
   int vertexId;
   float vx, vy, vz;
   fscanf(fvertices, "%d %f %f %f\n", &vertexId, &vx, &vy, &vz);
   int vShift = vertexId;
   int maxOrigVertexId = vertexId;
   int numStarVertices = 1;

   // Read the remainder of the vertex lines and continue to count the vertices
   // and keep track of the minimum and maximum vertex IDs
   while( ! feof(fvertices) )
   {
      fscanf(fvertices, "%d %f %f %f\n", &vertexId, &vx, &vy, &vz);
      if ( vShift > vertexId ) vShift = vertexId;
      if ( maxOrigVertexId < vertexId ) maxOrigVertexId = vertexId;
      numStarVertices++;
   }

   cout << "\tTotal no. of points in vertex file = "
        << numStarVertices << endl;

   if ( this->debug )
      cout << "\tvShift = " << vShift << endl;

   // Because we know the number of vertices, use vtk's SetNumberOfPoints
   // and SetPoint functions for efficiency
   vtkPoints *vertices = vtkPoints::New();
   vertices->SetNumberOfPoints( maxOrigVertexId-vShift+1 );

   // Rewind the file and reread all of the vertex lines and store the vertex
   // info in the vertices array using shifted vertex numbering
   rewind( fvertices );
   while( ! feof(fvertices) )
   {
      fscanf(fvertices, "%d %f %f %f\n", &vertexId, &vx, &vy, &vz);
      vertices->SetPoint( vertexId-vShift, vx, vy, vz );
      if ( this->debug > 1 )
         cout << "Inserted point # " << vertexId-vShift << ": "
              << "\t" << vx << "\t" << vy << "\t" << vz << endl;
   }
   fclose( fvertices );

   uGrid = vtkUnstructuredGrid::New();
   uGrid->SetPoints( vertices );
   vertices->Delete();

   // now for the cell data...
   cout << "\nReading cell data from " << this->starCellFileName << endl;

   FILE * fcells = fopen( this->starCellFileName, "r" );
   if ( fcells == NULL )
   {
      cerr <<"\nError - Cannot open the designated cell file: "
           << this->starCellFileName << endl;
      uGrid->Delete();
      uGrid = NULL;
      return uGrid;
   }

   int cId, cPt[8], cGroup, cType;
   int sId, sPt[8], sGroup, sType;  // used for samm cells - second record line
   int tPt[8];                      // used for samm cells - third record line
   int resolutionType, REG;         // used for samm cells
   //int PERM;                      // used for samm cells
   int temp[8];                     // used for samm cells
   int numVtkCells = 0;
   int numStarCells = 0;
   int i;

   // First compute the number of cells needed for the data set...
   while( !feof(fcells) )
   {
      fscanf(fcells,"%d %d %d %d %d %d %d %d %d %d %d\n", 
      &cId, 
      &cPt[0], &cPt[1], &cPt[2], &cPt[3], &cPt[4], &cPt[5], &cPt[6], &cPt[7], 
      &cGroup, &cType); 

      // reject all types except for fluid cells or samm cells
      if ( cType != 1 && cType != -1) continue;

      if ( cType == -1 )                // star-cd samm cells
      {
         numStarCells++;
         // read second line of this record...
         fscanf(fcells,"%d %d %d %d %d %d %d %d %d %d %d\n", 
         &sId, 
         &sPt[0], &sPt[1], &sPt[2], &sPt[3], &sPt[4], &sPt[5], &sPt[6], &sPt[7],
         &sGroup, &sType);
         if ( sId != cId ) cerr << "ERROR: On reading line 2 of samm cell "
                                << cId << ", sId=" << sId << endl;
         if ( sType != 0 ) cerr << "ERROR: For cell " << sId 
                                << ", record line 2, sType=" << sType 
                                << ".  Expected sType != 0" << endl;
                           
         // read third line of this record...
         fscanf(fcells,"%d %d %d %d %d %d %d %d %d %d %d\n", 
         &sId, 
         &tPt[0], &tPt[1], &tPt[2], &tPt[3], &tPt[4], &tPt[5], &tPt[6], &tPt[7], 
         &sGroup, &sType);
         if ( sId != cId ) cerr << "ERROR: On reading line 3 of samm cell " 
                                << cId << ", sId=" << sId << endl;
         if ( sType != 0 ) cerr << "ERROR: For cell " << sId 
                                << ", record line 3, sType=" << sType 
                                << ".  Expected sType != 0" << endl;
         resolutionType = tPt[5];
         REG = tPt[6];

         if ( resolutionType == 1 )                   // hex with one corner cut away
         {
            if      ( REG == 0 ) numVtkCells += 1;    // the corner (a tetrahedron)
            else if ( REG == 1 ) numVtkCells += 3;    // remainder to be a hexahedron and 2 tets
            else    
               cerr << "ERROR: samm cell resolutionType " << resolutionType
                    << ", REG=" << REG << " is not handled" << endl;
         }

         else if ( resolutionType == 2 )              // hex with two corners (wedge) cut away
         {
            if      ( REG == 0 ) numVtkCells += 1;    // the wedge
            else if ( REG == 1 ) numVtkCells += 2;    // hex with wedge cut away
            else    
               cerr << "ERROR: samm cell resolutionType " << resolutionType
                    << ", REG=" << REG << " is not handled" << endl;
         }

         else if ( resolutionType == 3 )              // samm hexahedron
         {
            if      ( REG == 0 ) numVtkCells += 1;
            else    
               cerr << "ERROR: samm cell resolutionType " << resolutionType
                    << ", REG=" << REG << " is not handled" << endl;
         }

         else if ( resolutionType == 7 )              // hex with plane cutting three corners 
         {
            if      ( REG == 0 ) numVtkCells += 4;    // 
            else if ( REG == 1 ) numVtkCells += 3;    // 
            else    
               cerr << "ERROR: samm cell resolutionType " << resolutionType
                    << ", REG=" << REG << " is not handled" << endl;
         }

         else if ( resolutionType == 8 ) numVtkCells += 5;    // hex with plane cutting four corners 

         else if ( resolutionType == 85) numVtkCells += 2;    // hex with two opposing edges removed

         else if ( resolutionType == 275 && REG == 0 ) numVtkCells += 1;    // samm pyramid

         else
            cerr << "ERROR: samm cell resolutionType=" << resolutionType
                 << " with REG=" << REG << " is not yet handled" << endl;
      }
      else
      {
         numStarCells++;
         numVtkCells++;
      }
   }
   cout << "\tTotal no. of cells in star-cd model  = " << numStarCells << endl;
   cout << "\tTotal no. of vtk cells to be created = " << numVtkCells << endl;

   int cMemSize = numVtkCells;
   uGrid->Allocate(numVtkCells, cMemSize);

   // Read the cell vertex connectivity and write vtk cells...  
   rewind( fcells );
   while( ! feof(fcells) )
   {
      fscanf(fcells,"%d %d %d %d %d %d %d %d %d %d %d\n", 
      &cId, 
      &cPt[0], &cPt[1], &cPt[2], &cPt[3], &cPt[4], &cPt[5], &cPt[6], &cPt[7], 
      &cGroup, &cType); 

      // reject all types except for cells or samm cells
      if ( cType != 1 && cType != -1) continue;

      for ( i=0; i<8; i++ ) cPt[i] -= vShift;

      if ( this->debug > 1 ) 
         cout << "After vShift adjustment, just read cell " << cId 
              << " with vertices: " 
              << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2] 
              << "\t" << cPt[3] << "\t" << cPt[4] << "\t" << cPt[5] 
              << "\t" << cPt[6] << "\t" << cPt[7] << endl;

      // Skip the cell if all vertices are zero...
      if ( cPt[0] == 0 && cPt[1] == 0 && cPt[2] == 0 && cPt[3] == 0 && 
           cPt[4] == 0 && cPt[5] == 0 && cPt[6] == 0 && cPt[7] == 0   )
      {
         if ( this->debug ) 
            cout << "***Skipping cell " << cId << " with vertices: " 
                 << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2] 
                 << "\t" << cPt[3] << "\t" << cPt[4] << "\t" << cPt[5]
                 << "\t" << cPt[6] << "\t" << cPt[7] << endl;
         continue;    // to next cell record
      }

      if ( cType == -1 )                // star-cd samm cells
      {
         // read second line of this record...
         fscanf(fcells,"%d %d %d %d %d %d %d %d %d %d %d\n", 
         &sId, 
         &sPt[0], &sPt[1], &sPt[2], &sPt[3], &sPt[4], &sPt[5], &sPt[6], &sPt[7],
         &sGroup, &sType);

         for ( i=0; i<8; i++ ) sPt[i] -= vShift;

         if ( this->debug ) 
            cout << "               After vShift adjustment, line 2 vertices: " 
                 << "\t" << sPt[0] << "\t" << sPt[1] << "\t" << sPt[2]
                 << "\t" << sPt[3] << "\t" << sPt[4] << "\t" << sPt[5]
                 << "\t" << sPt[6] << "\t" << sPt[7] << endl;
                   
         // read third line of this record...
         fscanf(fcells,"%d %d %d %d %d %d %d %d %d %d %d\n", 
                &sId, &tPt[0], &tPt[1], &tPt[2], &tPt[3],
                &tPt[4], &tPt[5], &tPt[6], &tPt[7], 
                &sGroup, &sType);

         if ( this->debug ) 
            cout << "                                    On line 3, vertices: " 
                 << "\t" << tPt[0] << "\t" << tPt[1] << "\t" << tPt[2]
                 << "\t" << tPt[3] << "\t" << tPt[4] << "\t" << tPt[5]
                 << "\t" << tPt[6] << "\t" << tPt[7] << endl;

         resolutionType = tPt[5];
         REG = tPt[6];
         //PERM = tPt[7];                 // not used at this point

         if ( resolutionType == 1 )       // hex with one corner cut away
         {
            if ( REG == 0 )               // the corner (a tetrahedron)
            {
               temp[0] = cPt[0];
               temp[1] = sPt[3];
               temp[2] = sPt[4];
               temp[3] = sPt[1];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug ) 
                  cout << "For samm cell 1, inserted tetrahedron cell w/ vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl;
            }        
            else if ( REG == 1 )    // write the [hex with corner cut away] as a hexahedron and two tetrahedrons
            {
               temp[0] = sPt[1];
               temp[1] = cPt[1];
               temp[2] = cPt[2];
               temp[3] = cPt[3];
               temp[4] = cPt[4];
               temp[5] = cPt[5];
               temp[6] = cPt[6];
               temp[7] = cPt[7];
               uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, temp );
               if ( this->debug ) 
                  cout << "For samm cell 1, inserted hexahedron cell w/ vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << "\t" << temp[6] << "\t" << temp[7] << endl;
               temp[0] = sPt[1];
               temp[1] = sPt[3];
               temp[2] = sPt[4];
               temp[3] = cPt[3];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug ) 
                  cout << "For samm cell 1, inserted tetrahedron cell w/ vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
               temp[0] = sPt[1];
               temp[1] = cPt[3];
               temp[2] = cPt[4];
               temp[3] = sPt[4];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug )
                  cout << "For samm cell 1, inserted tetrahedron cell w/ vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
            }
            else 
               cerr << "ERROR: For samm cell cId=" << cId << ", with resolutionType="
                    << resolutionType << ", invalid REG=" << REG << endl;
         }

         else if ( resolutionType == 2 )  // hex with wedge cut away
         {
            if ( REG == 0 )               // the wedge
            {
               temp[0] = cPt[0];
               temp[1] = sPt[3];
               temp[2] = sPt[4];
               temp[3] = cPt[1];
               temp[4] = sPt[2];
               temp[5] = sPt[5];
               uGrid->InsertNextCell( VTK_WEDGE, 6, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 2, inserted wedge cell with vertices: " 
                  << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                  << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                  << endl;
            }        
            else if ( REG == 1 )    // write the [hex with wedge cut away] as a hexahedron and a wedge
            {
               temp[0] = cPt[3];
               temp[1] = cPt[7];
               temp[2] = sPt[3];
               temp[3] = cPt[2];
               temp[4] = cPt[6];
               temp[5] = sPt[2];
               uGrid->InsertNextCell( VTK_WEDGE, 6, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 2, inserted wedge cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << endl;
               temp[0] = sPt[3];
               temp[1] = cPt[7];
               temp[2] = cPt[4];
               temp[3] = sPt[4];
               temp[4] = sPt[2];
               temp[5] = cPt[6];
               temp[6] = cPt[5];
               temp[7] = sPt[5];
               uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 2, inserted hex cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << "\t" << temp[6] << "\t" << temp[7] << endl; 
            }
            else cerr << "ERROR: For samm cell cId=" << cId << ", with resolutionType=" << resolutionType
                 << ", invalid REG=" << REG << endl;
            }

            else if ( resolutionType == 3 )   // samm hexahedron
            {
            if ( REG == 0 )            // convert samm hexahedron into vtk hexahedron
            {
               temp[0] = cPt[0];
               temp[1] = cPt[1];
               temp[2] = cPt[2];
               temp[3] = cPt[3];
               temp[4] = sPt[4];
               temp[5] = sPt[5];
               temp[6] = sPt[6];
               temp[7] = sPt[7];
               uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 3, inserted hex cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << "\t" << temp[6] << "\t" << temp[7] << endl; 
            }
            else                    // samm hexahedron will only be "inside"
            {
               cerr << "ERROR: For samm cell cId=" << cId << ", with resolutionType="
                    << resolutionType << ", invalid REG=" << REG << endl;
            }
         }

         else if ( resolutionType == 7 )   // hex with plane cutting three corners 
         {
            if ( REG == 0 )         // divide the piece with the five original corners into two wedges, a pyr, and a tet
            {
               temp[0] = cPt[0];
               temp[1] = cPt[1];
               temp[2] = cPt[2];
               temp[3] = sPt[0];
               temp[4] = sPt[1];
               temp[5] = sPt[2];
               uGrid->InsertNextCell( VTK_WEDGE, 6, temp );
               if ( this->debug ) 
                  cout << "For samm cell 7, inserted 1st wedge cell with vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << endl; 
               temp[0] = cPt[0];
               temp[1] = cPt[2];
               temp[2] = cPt[3];
               temp[3] = sPt[4];
               temp[4] = sPt[2];
               temp[5] = sPt[3];
               uGrid->InsertNextCell( VTK_WEDGE, 6, temp );
               if ( this->debug )
                  cout << "For samm cell 7, inserted 2nd wedge cell with vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << endl; 
               temp[0] = cPt[0];
               temp[1] = cPt[2];
               temp[2] = sPt[2];
               temp[3] = sPt[0];
               temp[4] = sPt[4];
               uGrid->InsertNextCell( VTK_PYRAMID, 5, temp );
               if ( this->debug ) 
                  cout << "For samm cell 7, inserted pyramid cell with vertices:    " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << endl; 
               temp[0] = cPt[0];
               temp[1] = sPt[0];
               temp[2] = sPt[4];
               temp[3] = cPt[4];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug ) 
                  cout << "For samm cell 7, inserted tetrahedron cell w/ vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
            }
            else if ( REG == 1 )    // divide the piece with the three original corners into two pyramids and a tet
            {
               temp[0] = sPt[1];
               temp[1] = sPt[2];
               temp[2] = cPt[6];
               temp[3] = cPt[5];
               temp[4] = sPt[0];
               uGrid->InsertNextCell( VTK_PYRAMID, 5, temp );
               if ( this->debug ) 
                  cout << "For samm cell 7, inserted 1st pyramid cell w/ vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << endl; 
               temp[0] = sPt[2];
               temp[1] = sPt[3];
               temp[2] = cPt[7];
               temp[3] = cPt[6];
               temp[4] = sPt[4];
               uGrid->InsertNextCell( VTK_PYRAMID, 5, temp );
               if ( this->debug )
                  cout << "For samm cell 7, inserted 2nd pyramid cell w/ vertices:  " 
                  << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                  << "\t" << temp[3] << "\t" << temp[4] << endl; 
               temp[0] = sPt[0];
               temp[1] = cPt[6];
               temp[2] = sPt[2];
               temp[3] = sPt[4];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug )
                  cout << "For samm cell 7, inserted tetrahedron cell w/ vertices:  " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
            }
         }

         else if ( resolutionType == 8 )// hex with plane cutting four corners 
         {
            if ( REG == 0 )         // divide the piece containing the original 0th vertex into 2 pyrs, and 3 tets
            {
               temp[0] = sPt[0];
               temp[1] = sPt[1];
               temp[2] = sPt[4];
               temp[3] = sPt[5];
               temp[4] = cPt[0];
               uGrid->InsertNextCell( VTK_PYRAMID, 5, temp );
               if ( this->debug ) 
                  cout << "For samm cell 8, inserted 1st pyramid cell with vertices:" 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << endl; 
               temp[0] = sPt[1];
               temp[1] = sPt[2];
               temp[2] = sPt[3];
               temp[3] = sPt[4];
               temp[4] = cPt[0];
               uGrid->InsertNextCell( VTK_PYRAMID, 5, temp );
               if ( this->debug )
                  cout << "For samm cell 8, inserted 2nd pyramid cell with vertices:" 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << endl; 
               temp[0] = cPt[1];
               temp[1] = sPt[0];
               temp[2] = sPt[5];
               temp[3] = cPt[0];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug )
                  cout << "For samm cell 8, inserted 1st tetra cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
               temp[0] = cPt[3];
               temp[1] = sPt[2];
               temp[2] = sPt[1];
               temp[3] = cPt[0];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug ) 
                  cout << "For samm cell 8, inserted 2nd tetra cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
               temp[0] = cPt[4];
               temp[1] = sPt[4];
               temp[2] = sPt[3];
               temp[3] = cPt[0];
               uGrid->InsertNextCell( VTK_TETRA, 4, temp );
               if ( this->debug )
                  cout << "For samm cell 8, inserted 3rd tetra cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << endl; 
            }
            else                     // Wayne Oaks of adapco says resolution type 8 cells will only be "inside"
            {
               cerr << "ERROR: For samm cell cId=" << cId << ", with resolutionType="
                    << resolutionType << ", invalid REG=" << REG << endl;
            }
         }

         else if ( resolutionType == 85 )   // hex with two opposing edges removed can be a six-sided cylinder or 2 wedges
         {
            if ( REG == 0 )             // specify two wedges
            {
               temp[0] = cPt[0];
               temp[1] = sPt[3];
               temp[2] = sPt[0];
               temp[3] = cPt[1];
               temp[4] = sPt[2];
               temp[5] = sPt[1];
               uGrid->InsertNextCell( VTK_WEDGE, 6, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 85, inserted first wedge cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << endl; 
               temp[0] = cPt[7];
               temp[1] = sPt[4];
               temp[2] = sPt[7];
               temp[3] = cPt[6];
               temp[4] = sPt[5];
               temp[5] = sPt[6];
               uGrid->InsertNextCell( VTK_WEDGE, 6, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 85, inserted second wedge cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << endl; 
            }
            else if ( REG == 1 )        // write the six-sided cylinder as two hexahedrons
            {
               temp[0] = sPt[0];
               temp[1] = sPt[3];
               temp[2] = cPt[3];
               temp[3] = sPt[7];
               temp[4] = sPt[1];
               temp[5] = sPt[2];
               temp[6] = cPt[2];
               temp[7] = sPt[6];
               uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 85, inserted first hex cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << "\t" << temp[6] << "\t" << temp[7] << endl;
               temp[0] = sPt[0];
               temp[1] = sPt[7];
               temp[2] = sPt[4];
               temp[3] = cPt[4];
               temp[4] = sPt[1];
               temp[5] = sPt[6];
               temp[6] = sPt[5];
               temp[7] = cPt[5];
               uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, temp );
               if ( this->debug ) 
                  cout << "For samm cell resolutionType 85, inserted second hex cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << "\t" << temp[5]
                       << "\t" << temp[6] << "\t" << temp[7] << endl; 
            }
            else 
               cerr << "ERROR: For samm cell cId=" << cId << ", with resolutionType=" 
                    << resolutionType << ", invalid REG=" << REG << endl;
         }
         else if ( resolutionType == 275 )  // samm pyramid
         {
            if ( REG == 0 )         // convert samm pyramid into vtk pyramid
            {
               temp[0] = cPt[0];
               temp[1] = cPt[1];
               temp[2] = cPt[2];
               temp[3] = cPt[3];
               temp[4] = sPt[0];
               uGrid->InsertNextCell( VTK_PYRAMID, 5, temp );
               
               if ( this->debug ) 
                  cout << "For samm cell 275, inserted a pyramid cell with vertices: " 
                       << "\t" << temp[0] << "\t" << temp[1] << "\t" << temp[2]
                       << "\t" << temp[3] << "\t" << temp[4] << endl; 
            }
            else                    // samm pyramid will only be "inside"
            {
               cerr << "ERROR: For samm cell cId=" << cId << ", with resolutionType=" 
                    << resolutionType << ", invalid REG=" << REG << endl;
            }
         }
      }

      // StarCD tetrahedron cell vertex connectivity:      12334444 
      // use a tet if third and fourth are same and the last four are equal and non-zero
      else if (cPt[2]==cPt[3] && (cPt[4]==cPt[5] && cPt[5]==cPt[6] && cPt[6]==cPt[7] && cPt[4]!=0))
      {
         cPt[3] = cPt[4]; 
         uGrid->InsertNextCell(VTK_TETRA, 4, cPt);
         if ( this->debug > 1 )
            cout << "Inserted tetrahedron cell with vertices: " << "\t\t\t" 
                 << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2]
                 << "\t" << cPt[3] << endl; 
      }
      // use a tet if third and fourth are unique and the last four are zero
      else if ( cPt[2]!=cPt[3] && (cPt[4]==cPt[5] && cPt[5]==cPt[6] && cPt[6]==cPt[7] && cPt[7]==0))
      {
         uGrid->InsertNextCell(VTK_TETRA, 4, cPt);
         if ( this->debug > 1 )
            cout << "Inserted tetrahed. cell with vertices: " << "\t\t\t" 
                 << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2]
                 << "\t" << cPt[3] << endl; 
      }

      // StarCD pyramid cell vertex connectivity:  12345555
      else if ( cPt[4]==cPt[5] && cPt[5]==cPt[6] && cPt[6]==cPt[7] && cPt[7]!=0 )
      {
         uGrid->InsertNextCell(VTK_PYRAMID, 5, cPt);
         if ( this->debug > 1 )
            cout << "Inserted pyramid cell with vertices: " << "\t\t\t" 
                 << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2]
                 << "\t" << cPt[3] << "\t" << cPt[4] << endl; 
      }

      // StarCD prism (wedge) cell vertex connectivity:  12334566  
      else if ( cPt[2]==cPt[3] && cPt[2]!=0 && cPt[6]==cPt[7]  && cPt[6]!=0 )
      {
         cPt[3] = cPt[4]; 
         cPt[4] = cPt[5];
         cPt[5] = cPt[6];
         uGrid->InsertNextCell(VTK_WEDGE, 6, cPt);
         if ( this->debug > 1 )
            cout << "Inserted wedge cell with vertices: " 
                 << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2]
                 << "\t" << cPt[3] << "\t" << cPt[4] << "\t" << cPt[5]
                 << endl; 
      }

      // StarCD hexahedron cell vertex connectivity:    12345678
      else
      {
         uGrid->InsertNextCell(VTK_HEXAHEDRON, 8, cPt);
         if ( this->debug > 1 )
            cout << "Inserted hex cell with vertices: " << "\t\t\t" 
                 << "\t" << cPt[0] << "\t" << cPt[1] << "\t" << cPt[2]
                 << "\t" << cPt[3] << "\t" << cPt[4] << "\t" << cPt[5]
                 << "\t" << cPt[6] << "\t" << cPt[7] << endl; 
      }
   }
   fclose( fcells );

   // now for the solution data
   cout << "\nReading solution data from " << this->starUsrFileName << endl;

   if ( this->debug )
   {
      cout << "numScalars = " << this->numScalars << endl;
      cout << "numVectors = " << this->numVectors << endl;
   }

   int numColumns = 3 * this->numVectors + this->numScalars;
   if ( this->debug )
      cout << "numColumns = " << numColumns << endl;   

   float * data = new float [ numColumns ];
   
   ifstream fsolns( this->starUsrFileName, ios::in );
   if ( fsolns == NULL )
   {
       cerr <<"\nError - Cannot open the designated solution file: " 
            << this->starUsrFileName << endl;
       uGrid->Delete();
       uGrid = NULL;
       return uGrid;
   }

   // there can be at most one vector in starCD   
   vtkFloatArray * vec = NULL;
   if ( this->numVectors )
   {
      if ( this->debug )
         cout << "\tcreating vector for " << this->vectorName[ 0 ] << endl;   

      vec = vtkFloatArray::New();
      vec->SetNumberOfComponents(3);
      vec->SetName( this->vectorName[ 0 ] );
      vec->SetNumberOfTuples( maxOrigVertexId-vShift+1 );
   }

   // set up arrays to store scalar data...
   vtkFloatArray ** scalarData = NULL;
   if ( vec == NULL )
   {
      scalarData = new vtkFloatArray * [ this->numScalars ];
   }
   else
   {
      // This is to calculate vmag from vector data
      scalarData = new vtkFloatArray * [ this->numScalars + 1 ];
   }
   
   for ( i=0; i < this->numScalars; i++ )
   {
      scalarData[ i ] = vtkFloatArray::New();
      scalarData[ i ]->SetNumberOfComponents( 1 );
      scalarData[ i ]->SetName( this->scalarName[ i ] );
      scalarData[ i ]->SetNumberOfTuples( maxOrigVertexId-vShift+1 );
   }
   
   if ( this->numVectors > 0 )
   {
      // This is to calculate vmag from vector data
      // NOTE arrays are zero based therfore numScalars is the last index for vmag
      scalarData[ this->numScalars ] = vtkFloatArray::New();
      scalarData[ this->numScalars ]->SetNumberOfComponents( 1 );
      scalarData[ this->numScalars ]->SetName( "Velocity_Magnitude" );
      scalarData[ this->numScalars ]->SetNumberOfTuples( maxOrigVertexId-vShift+1 );
   }

   int numSolns = 0;

   // read the first term in the first line of data...
   fsolns >> sId;
   while( ! fsolns.eof() )
   {
      // read all of the data columns in that line...
      for ( i = 0; i < numColumns; i++)
         fsolns >> data[ i ];
      fsolns.getline( textline, 256 );   //skip past remainder of line

      if ( this->debug > 1) 
      {
         cout << "raw data: " << sId << "\t";
         for ( i = 0; i < numColumns; i++)
            cout <<  data[ i ] << "\t";        
         cout << endl;
      }
   
      // if solution pertains to a vertex higher than the range already defined,
      // then skip...
      if ( sId > maxOrigVertexId )
      {
         if ( this->debug ) 
            cout << "skipping solution " << sId << endl;
         
         // try to read the first term in the next line of data
         // (failure will get us out of loop)...
         fsolns >> sId;
         continue;
      }

      sId -= vShift;

      if ( sId < 0 )
      {
         if ( this->debug ) 
            cout << "Invalid sId = " << sId << endl;

         // try to read the first term in the next line of data
         // (failure will get us out of loop)...
         fsolns >> sId;
         continue;
      }

      // assumes that the vector is in first three columns
      int scalarStartIndex = 0;
      if ( this->numVectors )
      {
         if ( this->debug > 1 )
         {
            cout << "VECTOR: " << sId;
            for ( i=0; i<3; i++ ) 
               cout << "\t" << data[ i ];
            cout << endl;
         }

         vec->SetTuple( sId, data );
         scalarStartIndex = 3;
      }

      // add in all of the scalar data...
      for ( i = 0; i < this->numScalars; i++ )
      {
         scalarData[ i ]->SetTuple1( sId, data[ scalarStartIndex + i ] );
      }

      // if there is a vector, create an extra scalar term consisting of the
      // vector magnitude
      if ( vec != NULL )
      {
         // NOTE: scalarData array is zero-based therefore numScalars is
         // the last index for vmag
         float inputVmag = sqrt( (data[ 0 ] * data[ 0 ]) + 
                                 (data[ 1 ] * data[ 1 ]) + 
                                 (data[ 2 ] * data[ 2 ]) );
         scalarData[ this->numScalars ]->SetTuple1( sId, inputVmag );
      }
      numSolns++;

      // try to read the first term in the next line of data
      // (failure will get us out of loop)...
      fsolns >> sId;
   }

   delete [] data;      
 
   cout << "\tTotal no. of solutions = " << numSolns << endl;

   if ( this->debug && numSolns != numStarVertices) 
      cout << "Warning: numSolns != numStarVertices" << endl;

   cout << endl;

   // If vector data exists, then vector magnitude scalar will exist as well.
   // Let the user select the scalar and vector quantities to be written
   // to the pointdata array
   vtkPointData * uGridPointData = uGrid->GetPointData();
   if ( vec != NULL )
   {
      uGridPointData->SetVectors( vec );
      //uGridPointData->AddArray( vec );//vectors stored as point data arrays don't get rotated
      vec->Delete();

      letUsersAddParamsToField( this->numScalars + 1, scalarData, uGridPointData, 0 );
      for (i=0; i < this->numScalars + 1; i++) 
         scalarData[ i ]->Delete();
   }
   else
   {
      letUsersAddParamsToField( this->numScalars, scalarData, uGridPointData, 0 );
      for (i=0; i < this->numScalars; i++) 
         scalarData[ i ]->Delete();
   }
   
   delete [] scalarData;      

   return uGrid;
}

void starReader::ReadParameterFile( void )
{
   this->numScalars = 0;

   std::fstream StarParamFile;
   StarParamFile.open( this->paramFileName, std::ios::in );

   char tagName[ 50 ];
   char tagValue[ 50 ];
  
   while( 1 )
   {
      StarParamFile.getline(textline,256);
      if ( StarParamFile.eof() )
      {
         break;
      }
      fileIO::getTagAndValue(textline, tagName, tagValue);
      
      if ( this->debug )
      {
         cout << "textline = " << textline << endl;
         std::cout << "tagValue = " << tagValue << std::endl;
      }
   
      if (strcmp("STARVRT", tagName)==0)
      {
         strcpy(this->starVertFileName,tagValue);
      }
      else if (strcmp("STARCEL", tagName)==0)
      {
         strcpy(this->starCellFileName,tagValue);
      }
      else if (strcmp("STARUSR", tagName)==0)
      {
         strcpy(this->starUsrFileName,tagValue);
      }
      else if (strcmp("VECTORNAME", tagName)==0) 
      {
         char * newSpace = new char[ strlen(tagValue)+1 ];
         strcpy(newSpace,tagValue);
         this->vectorName.push_back( newSpace );
         if ( this->debug )
            cout << "vectorName[" << this->numVectors << "] = " 
                 << this->vectorName[ this->numVectors ] << endl;
         this->numVectors++;
         if ( this->numVectors > 1 )
         {
            cerr <<"\nError - Star-CD format should have no more than 1 vector"
                 << endl;
            exit ( 1 );
         }
      }
      else if (strcmp("SCALARNAME", tagName)==0)
      {
         char * newSpace = new char[ strlen(tagValue)+1 ];
         strcpy(newSpace,tagValue);
         this->scalarName.push_back( newSpace );
         if ( this->debug )
            cout << "scalarName[" << this->numScalars << "] = " 
                 << this->scalarName[ this->numScalars ] << endl;
         this->numScalars++;
      }
      else if ( strcmp("SCALEFACTOR", tagName)==0 )
      {
         this->scaleValue = atof( tagValue );            
         if ( this->debug )
            std::cout << "scale factor = " << this->scaleValue << std::endl;
      }
      else if ( strcmp("OUTPUTFILENAME", tagName)==0 )
      {
         strcpy(this->vtkFileName,tagValue);
      }
      else if ( strcmp("ROTATEX", tagName)==0 )
      {
         this->rotations[ 0 ] = atof( tagValue );
      }
      else if ( strcmp("ROTATEY", tagName)==0 )
      {
         this->rotations[ 1 ] = atof( tagValue );
      }
      else if ( strcmp("ROTATEZ", tagName)==0 )
      {
         this->rotations[ 2 ] = atof( tagValue );
      }
      else if ( strcmp( "TRANSLATEX", tagName)==0 )
      {
         this->translations[ 0 ] = atof( tagValue );
      }
      else if ( strcmp( "TRANSLATEY", tagName)==0 )
      {
         this->translations[ 1 ] = atof( tagValue );
      }
      else if ( strcmp( "TRANSLATEZ", tagName)==0 )
      {
         this->translations[ 2 ] = atof( tagValue );
      }
      else if ( strcmp( "WRITEOPTION", tagName)==0 )
      {
         this->writeOption = atoi( tagValue );
      }
      else
      {
         std::cerr << "Parameter " << tagName << " not found!" <<std::endl;
      }
   }
   StarParamFile.close();
}

float * starReader::GetRotations( void )
{
   return this->rotations;
}

float * starReader::GetTranslations( void )
{
   return this->translations;
}

int starReader::GetWriteOption( void )
{
   return this->writeOption;
}

float starReader::GetScaleFactor( void )
{
   return this->scaleValue;
}

char *starReader::GetVTKFileName( void )
{
   return this->vtkFileName;
}


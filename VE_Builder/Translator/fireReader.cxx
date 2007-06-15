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
//#include <istream>
#include <sstream>
#include <string>
#include <vector>
// Needed on irix
#ifndef WIN32
#include <unistd.h>
#endif
using std::string;

#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h>  // this code requires VTK4
#include <vtkPointData.h>
#include <vtkCellType.h>
#include <vtkCellDataToPointData.h>
#include <vtkCellData.h>

#include "converter.h"      // for "getOneVector" and "letUsersAddParamsToField"

enum DataTypes
{
   DATA_TYPE_ERROR,
   GLOBAL_DATA,
   VECTOR_DATA,
   SCALAR_DATA,
   ITERATION
};

int TestString( std::string &, std::string & ,int &);
int ExitReader( void );
vtkUnstructuredGrid * fireReader( std::string geomFile, std::string dataFile, int debug )
{
   vtkUnstructuredGrid * uGrid = NULL;
   
   std::ifstream geomIn( geomFile.c_str(), std::ios::in ); 
 
   if ( geomIn == NULL )
   {
      std::cout <<"\nError - Cannot open the designated geometry file: " << geomFile <<std::endl;
       return uGrid;
   }

  std::cout << "\nReading vertex data...\n";
  std::cout.flush();

   int numCoordinates = 0;
   int vert;
   float vertex[ 3 ];

   // String stuff
   std::string line;
   const std::string colon ( ":" );
   const std::string space ( " " );
   const std::string star ( "*" );
   const std::string numbers ( "123456789" );
   std::string names;
   std::string names2;
   std::string tempName;
   std::string::size_type begIdx[4], endIdx;

   //debug = 0;

   std::getline( geomIn, line );   //skip past remainder of line
   std::cout << line << std::endl;
   std::getline( geomIn, line );   //skip past remainder of line
   std::cout << line << std::endl;
   std::getline( geomIn, line );   //skip past remainder of line
   std::cout << line << std::endl;
   std::getline( geomIn, line );   //skip past remainder of line
   std::cout << line << std::endl;

   endIdx = line.rfind( numbers );
   begIdx[1] = line.rfind( space, endIdx - 1 );
   names2 = line.substr( begIdx[1]+1, endIdx - begIdx[1] );
   //std::cout << names2 << std::endl;
   std::istringstream numCoordString( names2 );
   
   numCoordString >> numCoordinates;
   numCoordinates = numCoordinates/3;
  std::cout << "\nTotal no. of points in vertex file = " << numCoordinates <<std::endl;

   vtkPoints *vertices = vtkPoints::New();
   vertices->SetNumberOfPoints( numCoordinates );

   for ( vert = 0; vert < numCoordinates; vert++ )
   {
      geomIn >> vertex[ 0 ] >> vertex[ 1 ] >> vertex[ 2 ];
      vertices->SetPoint( vert, vertex );
      //if ( debug > 1 ) 
      //  std::cout << "Inserted point # " << vert << ": " << "\t" 
      //                       << vertex[ 0 ] << "\t" << vertex[ 1 ] << "\t" << vertex[ 2 ] <<std::endl;
      //sleep( 10 );
   }

   uGrid = vtkUnstructuredGrid::New();
   uGrid->SetPoints( vertices );
   vertices->Delete();

   /***********************************************************************/
   /***********************************************************************/
   // now for the cell data...
  std::cout << "\nReading cell data...\n";
  std::cout.flush();

   int numCells = 0;
   int i, j;
   //int numVertIds = 0;
   int cellVertex[ 8 ];

   std::getline( geomIn, line );   //skip past remainder of line
   std::getline( geomIn, line );   //skip past remainder of line

   endIdx = line.rfind( numbers );
   begIdx[1] = line.rfind( space, endIdx - 1 );
   names2 = line.substr( begIdx[1]+1, endIdx - begIdx[1] );
   //std::cout << names2 << std::endl;
   std::istringstream numCellsString( names2 );
   
   numCellsString >> numCells;

  std::cout <<std::endl;
  std::cout << "Total no. of cells in FIRE/SWIFT model  = " << numCells <<std::endl;
   
   // Read cell types here. This determines connectivity
   int *cellType = new int[ numCells ];

   for ( i = 0; i < numCells; i++ )
   {
      geomIn >> cellType[ i ];
      //cout << cellType[ i ] <<std::endl;
      //sleep( 1 );   
   }

   std::getline( geomIn, line );   //skip past remainder of line
   std::getline( geomIn, line );   //skip past remainder of line

   // Read the cell vertex connectivity and write vtk cells...  
   uGrid->Allocate( numCells, numCells );

   for ( i = 0; i < numCells; i++ )
   {
      if ( cellType[ i ] == 4 ) // Is Tet
      {
         for ( j = 0; j < 4; j++ )
            geomIn >> cellVertex[ j ];
         
         uGrid->InsertNextCell( VTK_TETRA, 4, cellVertex );
         if ( debug ) 
           std::cout << "For cell 4, inserted tetrahedron cell w/ vertices:  " 
                  << "\t" << cellVertex[0] << " \t " << cellVertex[1] << " \t " 
                  << cellVertex[2] << " \t " << cellVertex[3] <<std::endl;
      }
      else if ( cellType[ i ] == 5 ) // Is Hex
      {
         for ( j = 0; j < 8; j++ )
            geomIn >> cellVertex[ j ];

          uGrid->InsertNextCell( VTK_HEXAHEDRON, 8, cellVertex );
         if ( debug ) 
           std::cout << "For cell 5, inserted hexahedron cell w vertices: " << 
                     cellVertex[0] << " \t " << cellVertex[1] << " \t " << cellVertex[2] << " \t " <<
                     cellVertex[3] << " \t " << cellVertex[4] << " \t " << cellVertex[5] << " \t " << 
                     cellVertex[6] << " \t " << cellVertex[7] <<std::endl;
      }
      else if ( cellType[ i ] == 6 ) // Is Pyra
      {
         for ( j = 0; j < 5; j++ )
            geomIn >> cellVertex[ j ];
         
         uGrid->InsertNextCell( VTK_PYRAMID, 5, cellVertex );
         if ( debug ) 
           std::cout << "Inserted pyramid cell with vertices: " << 
                     cellVertex[0] << " \t " << cellVertex[1] << " \t " << cellVertex[2] << " \t " << 
                     cellVertex[3] << " \t " << cellVertex[4] <<std::endl;
      }
      else if ( cellType[ i ] == 8 ) // Is Prisum     
      {
         for ( j = 0; j < 6; j++ )
            geomIn >> cellVertex[ j ];

         uGrid->InsertNextCell( VTK_WEDGE, 6, cellVertex );
         if ( debug ) 
           std::cout << "Inserted wedge cell with vertices: " << cellVertex[0] << " \t " << 
                     cellVertex[1] << " \t " << cellVertex[2] << " \t " << cellVertex[3] << " \t " << 
                     cellVertex[4] << " \t " << cellVertex[5] <<std::endl;
      }
#ifndef WIN32
      if ( debug ) 
         sleep( 1 );
#endif
   }
   
   delete [] cellType;

   geomIn.close();

   /***********************************************************************/
   /***********************************************************************/
   // now for the solution data
  std::cout << "\nReading solution data...\n";
  std::cout.flush();
  
   std::ifstream dataIn( dataFile.c_str(), std::ios::in ); 

   int numVectors;
   int numScalars;
   int stringTest;
   int exitDataLoop = 0;
   int numValsToRead = 0;

   typedef std::vector< float *> FloatList;
   typedef std::vector< string > StringList;
   FloatList scalarData;
   FloatList vectorData;
   StringList vectorNames;
   StringList scalarNames;

   if ( dataIn == NULL )
   {
      std::cout <<"\nError - Cannot open the designated geometry file: " << dataFile <<std::endl;
       return uGrid;
   }
   
   std::getline( dataIn, line);
   //std::cout << " 6 : " <<  line<< std::endl;
   std::getline( dataIn, line);
   //std::cout << " 6 : " <<  line<< std::endl;
   std::getline( dataIn, line);
   //std::cout << " 6 : " <<  line<< std::endl;

   while ( !dataIn.eof() && !exitDataLoop )
   {
      std::getline( dataIn, line);
      //std::cout << " While : " <<  line<< std::endl;
      //if ( stringTest == GLOBAL_DATA )
      //   sleep(10);
      stringTest = TestString ( line, tempName, numValsToRead );
   
      if ( stringTest == ITERATION )
      {   
         int iters;   
         int ans;
         int ans2 = 0;
         int flag = 0;
         std::getline( dataIn, line);
         std::istringstream inputStrings( line );
         inputStrings >> iters;
        std::cout << "Do you want the data from iteration number : " << 
                                                      iters << " (1 or 0) " <<std::endl;
         std::cin >> ans;
         if ( !ans )
         {
           std::cout << "Do you want to continue extracting data (1 or 0)? " <<std::endl;
            std::cin >> ans2;
         }

         if ( ans2 )
         {
           std::cout << "Don't get data from iteration " << iters <<std::endl;
            do
            {
               std::getline( dataIn, line );
               //std::cout << " No Data Loop : " << line << std::endl;
               stringTest = TestString( line, tempName, numValsToRead  );
               if ( dataIn.eof() )
               {
                  exitDataLoop = 1;
                  break;
               }
               if ( stringTest == ITERATION )
               {
                  std::getline( dataIn, line);
                  std::istringstream input( line );
                  input >> iters;
                 std::cout << "Do you want the data from iteration number : " << 
                                                      iters << " (1 or 0) " <<std::endl;
                  std::cin >> ans;
                  if ( ans )
                     flag = 1;
               }
            }
            while ( !flag );
         }
         else if ( !ans2 && !ans )
         {
            exitDataLoop = 1;
         }
      }
      else if ( stringTest == GLOBAL_DATA )
      {
         // Process global data
         int junk;
         //cout << numValsToRead <<std::endl;
         for ( j = 0; j < numValsToRead; j++ )
         {
            dataIn >> junk;
            //cout << j << " : " << junk << "\t";
         }
         //exit(1);
         std::getline( dataIn, line);
      }
      else if ( stringTest == VECTOR_DATA )
      {
         // Process vector data
         endIdx = line.rfind( colon );
         begIdx[1] = line.find_first_of( space, endIdx + 1 );
         begIdx[0] = line.find_first_of( space, begIdx[1]+1 );
         begIdx[2] = line.find_last_of( space );
         begIdx[1] = line.find_first_of( numbers, begIdx[0]+1 );
         names2 = line.substr( begIdx[1], begIdx[2] - begIdx[1] );
         
         std::istringstream inputString( names2 );
   
         inputString >> numVectors;
         //numScalars = numVectors;
        std::cout << " Number of Data Points : " << numVectors <<std::endl; 

         vectorData.push_back( new float[numVectors] );
         vectorNames.push_back( tempName );

         for ( j = 0; j < numVectors; j++ )
         {
            dataIn >> vectorData[ vectorData.size() - 1 ][j];
         }

         if ( debug )
         {
            for (j=0; j<20; j++) 
              std::cout << "vectorData[" << i << "][" << j << "] = " << vectorData[vectorData.size() - 1][j] <<std::endl;
           std::cout << "                ..." <<std::endl;
            for ( j = numVectors-20; j < numVectors; j++ ) 
              std::cout << "vectorData[" << i << "][" << j << "] = " << vectorData[vectorData.size() - 1][j] <<std::endl;
           std::cout <<std::endl;
         }
			//sleep( 10 );                       
         std::getline( dataIn, line);
      }
      else if ( stringTest == SCALAR_DATA )
      {
         // Process scalar data
         endIdx = line.rfind( colon );
         begIdx[1] = line.find_first_of( space, endIdx + 1 );
         begIdx[0] = line.find_first_of( space, begIdx[1]+1 );
         begIdx[2] = line.find_last_of( space );
         begIdx[1] = line.find_first_of( numbers, begIdx[0]+1 );
         names2 = line.substr( begIdx[1], begIdx[2] - begIdx[1] );
         
         std::istringstream numScalarsString( names2 );
   
         numScalarsString >> numScalars;
         //numScalars = numVectors;
        std::cout << " Number of Data Points : " << numScalars <<std::endl; 
         
         scalarData.push_back( new float[numScalars] );
         scalarNames.push_back( tempName );

         for ( j = 0; j < numScalars; j++ )
         {
            dataIn >> scalarData[scalarData.size() - 1][j];
         }

         if ( debug )
         {
            for (j=0; j<4; j++) 
              std::cout << "scalarData[" << i << "][" << j << "] = " << scalarData[scalarData.size() - 1][j] <<std::endl;
           std::cout << "                ..." <<std::endl;

            for (j=numScalars-4; j<numScalars; j++) 
              std::cout << "scalarData[" << i << "][" << j << "] = " << scalarData[scalarData.size() - 1][j] <<std::endl;
           std::cout <<std::endl;
         }
         std::getline( dataIn, line);
      }
      // sleep( 1 );
   };
   dataIn.close();

   /***********************************************************************/
   /***********************************************************************/

   vtkFloatArray **scalarArray;// = vtkFloatArray::New();
   scalarArray = new vtkFloatArray*[scalarData.size()];
   
   // Put data from scalar array into vtkFloatArrays
   for ( i = 0; i < (int)scalarData.size(); i++ )
   {
      scalarArray[ i ] = vtkFloatArray::New();
      scalarArray[ i ]->SetNumberOfComponents( 1 ); // Defines number of vertex points or cell centers
      scalarArray[ i ]->SetNumberOfTuples( numCells ); // Defines each indiviual scalar
      scalarArray[ i ]->SetName( scalarNames[i].c_str() ); // Defines each indiviual scalar
      //cout << scalarNames[ i ].c_str() <<std::endl;
      for ( j = 0; j < numCells; j++ )
         scalarArray[ i ]->SetTuple( j, &scalarData[ i ][ j ] );
      uGrid->GetCellData()->AddArray( scalarArray[ i ] );
   }

   // Put data from vector array into vtkFloatArrays
   vtkFloatArray **vecArray;
   float *temp = new float[ 3 ];
   vecArray = new vtkFloatArray*[vectorData.size()/3];
   int vectorFactor = vectorData.size()/3;
   //for ( i = 0; i < vectorFactor; i++ )
   for ( i = 0; i < 1; i++ )
   {
      vecArray[ i ] = vtkFloatArray::New();
      vecArray[ i ]->SetNumberOfComponents( 3 );
      vecArray[ i ]->SetNumberOfTuples( numCells );
      vecArray[ i ]->SetName( vectorNames[ i*3 ].c_str() );
      //cout << vectorNames[ i*3 ].c_str() <<std::endl;
      for ( j = 0; j < numCells; j++ )
      {
         temp[ 0 ] = vectorData[ i + (i*vectorFactor) ][ j ];
         temp[ 1 ] = vectorData[ i + (1 + (i*vectorFactor)) ][ j ];
         temp[ 2 ] = vectorData[ i + (2 + (i*vectorFactor)) ][ j ];
         //if ( j == 0 )
         //  std::cout << temp[0] << " : " << temp[1] << " : " << temp[2] <<std::endl;
         vecArray[ i ]->SetTuple( j, temp );
      }
      uGrid->GetCellData()->AddArray( vecArray[ i ] );
   }
   delete [] temp;
   
   // Convert FIRE/SWIFT cell centered data to point data
   vtkCellDataToPointData *cell2point = vtkCellDataToPointData::New();
   cell2point->SetInput( uGrid );
   cell2point->PassCellDataOff();
   cell2point->Update();
   
   vtkUnstructuredGrid *finalUGrid = vtkUnstructuredGrid::New();
   finalUGrid->DeepCopy( cell2point->GetUnstructuredGridOutput() );

   // Clean Up
   for ( i = 0; i < (int)vectorData.size()/3; i++ )
   {
      delete [] vectorData[ i ];
   }
   vecArray[ 0 ]->Delete();

   for ( i = 0; i < (int)scalarData.size(); i++ )
   {
      scalarArray[ i ]->Delete();
      delete [] scalarData[ i ];
   }

   delete [] vecArray;
   delete [] scalarArray;

   delete cellType;

   uGrid->Delete();
   uGrid = NULL;
   cell2point->Delete();
   cell2point = NULL;

   return finalUGrid;
}

int TestString( std::string &line, std::string &temp, int &numVal)
{
   const std::string colon ( ":" );
   const std::string space ( " " );
   const std::string numbers ( "123456789" );
   std::string names;
   std::string names2;
   std::string names3;
   std::string names4;
   std::string::size_type begIdx[4];//, endIdx;
   std::string iteration ( "GlobalData:Iteration" );
   if ( line.find( colon ) == string::npos )
      return DATA_TYPE_ERROR;

   begIdx[0] = line.find( space );
   begIdx[1] = line.find_first_of( space, begIdx[0] + 1 );
   names = line.substr( begIdx[0] + 1, begIdx[1] - begIdx[0] -1);
   //std::cout << names <<std::endl;
   names4 = names.substr( 0, 10);
   //std::cout << names4 << std::endl;

   if ( !names.compare( "GlobalData:Iteration" ) )
   {
     std::cout << " Iteration Data : " <<std::endl;
      return ITERATION;
   }
   else if ( !abs(names4.compare( "GlobalData" )) )
   {
      begIdx[4] = line.rfind( numbers );
      begIdx[1] = line.rfind( space, begIdx[4] - 1 );
      names2 = line.substr( begIdx[1]+1, begIdx[4] - begIdx[1] );
      std::cout << " Global Data : " << names2 << std::endl;
      std::istringstream numCellsString( names2 );
   
      numCellsString >> numVal;
      return GLOBAL_DATA;
   }
   else 
   {       
      names2 = names.substr( 17, names.size());
      //std::cout << names2 << std::endl;
      //names3 = names - names2;
      temp = names2;
      if (  !names2.compare( 0, 8,"Velocity" ) || 
            !names2.compare( 0, 9,"Vorticity" ) )
      {
         std::cout << " Vector Data : " << names2 << std::endl;
         return VECTOR_DATA;
      }
      else if ( names.find( "ElementData:Flow" ) != string::npos )
      {
         std::cout << " Scalar Data : " << names2 << std::endl;
         return SCALAR_DATA;
      }
      else
      {
         std::cerr << "Error Data Not Found Contact TSVEG " << std::endl;
         exit( 1 );
         return DATA_TYPE_ERROR;
      }
   }
}

int ExitReader( void )
{
   int ans;
  std::cout << " Do you want to exit 0 or 1 : " <<std::endl;
   std::cin >> ans;
   if ( ans )
      return ( 1 );
   else
      return ( 0 );
}

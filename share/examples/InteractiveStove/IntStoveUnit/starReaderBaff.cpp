/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * Date modified: $Date: 2005-04-03 15:10:15 -0500 (Sun, 03 Apr 2005) $
 * Version:       $Rev: 2122 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "starReaderBaff.h"

#include <iostream>
#include <fstream>

#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h> // this code requires VTK4
#include <vtkPointData.h>
#include <vtkCellType.h>
#include "VE_Builder/Translator/DataLoader/converter.h"     // for "letUsersAddParamsToField"
#include "VE_Xplorer/Utilities/fileIO.h"        // for "getTagAndValue"

using namespace VE_Util;
using std::istringstream;

starReader::starReader( std::string paramFile )
{
   this->paramFileName.assign( paramFile );//strcpy( this->paramFileName, paramFile );

   this->debug = 0;

   for ( int i = 0; i < 3; i++ )
   {
      this->rotations[ i ] = 0.0f;
      this->translations[ i ] = 0.0f;
   }
   this->scaleIndex = 0;      // scale index default is no scaling
   this->scaleFactor = 1.0f;  // custom scale default is no scaling
   this->numScalars = 0;
   this->numVectors = 0;
   this->writeOption = 0;  // default is to let vtk write the file
   this->vtkFileName.assign( "starFlowdata.vtk" );
}

starReader::~starReader( void )
{
   if ( this->numScalars > 0 )
   {
      //for ( int i = 0; i < this->numScalars; i++ )
      //   delete [] this->scalarName[ i ];

      this->scalarName.clear();
   }

   if ( this->numVectors > 0 )
   {
      //for ( int i = 0; i < this->numVectors; i++ )
      //   delete [] this->vectorName[ i ];

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

   FILE * fvertices = fopen( this->starVertFileName.c_str(), "r" );

   if ( fvertices == NULL )
   {
      std::cerr <<"\nError - Cannot open the designated vertex file: "
           << this->starVertFileName << std::endl;
      return uGrid;
   }

   std::cout << "\nReading vertex data from " << this->starVertFileName << std::endl;

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

   std::cout << "\tTotal no. of points in vertex file = "
        << numStarVertices << std::endl;

   if ( this->debug )
      std::cout << "\tvShift = " << vShift << std::endl;

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
         std::cout << "Inserted point # " << vertexId-vShift << ": "
              << "\t" << vx << "\t" << vy << "\t" << vz << std::endl;
   }
   fclose( fvertices );

   uGrid = vtkUnstructuredGrid::New();
   uGrid->SetPoints( vertices );
   vertices->Delete();

   // now for the cell data...
   std::cout << "\nReading cell data from " << this->starCellFileName << std::endl;

   FILE * fcells = fopen( this->starCellFileName.c_str(), "r" );
   if ( fcells == NULL )
   {
      std::cerr <<"\nError - Cannot open the designated cell file: "
           << this->starCellFileName << std::endl;
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

      numStarCells++;
      numVtkCells++;
   }
   std::cout << "\tTotal no. of cells in star-cd model  = " << numStarCells << std::endl;
   std::cout << "\tTotal no. of vtk cells to be created = " << numVtkCells << std::endl;

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

      for ( i=0; i<8; i++ ) cPt[i] -= vShift;

      uGrid->InsertNextCell( VTK_QUAD, 4, cPt );

   }
   fclose( fcells );

   // now for the solution data
   std::cout << "\nReading solution data from " << this->starUsrFileName << std::endl;

   if ( this->debug )
   {
      std::cout << "numScalars = " << this->numScalars << std::endl;
      std::cout << "numVectors = " << this->numVectors << std::endl;
   }

   int numColumns = 3 * this->numVectors + this->numScalars;
   if ( this->debug )
      std::cout << "numColumns = " << numColumns << std::endl;   

   float * data = new float [ numColumns ];
   
   std::ifstream fsolns( this->starUsrFileName.c_str(), std::ios::in );
   if ( fsolns == NULL )
   {
       std::cerr <<"\nError - Cannot open the designated solution file: " 
            << this->starUsrFileName << std::endl;
       uGrid->Delete();
       uGrid = NULL;
       return uGrid;
   }

   // there can be at most one vector in starCD   
   vtkFloatArray * vec = NULL;
   if ( this->numVectors )
   {
      if ( this->debug )
         std::cout << "\tcreating vector for " << this->vectorName[ 0 ] << std::endl;   

      vec = vtkFloatArray::New();
      vec->SetNumberOfComponents(3);
      vec->SetName( this->vectorName[ 0 ].c_str() );
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
      scalarData[ i ]->SetName( this->scalarName[ i ].c_str() );
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
         std::cout << "raw data: " << sId << "\t";
         for ( i = 0; i < numColumns; i++)
            std::cout <<  data[ i ] << "\t";        
         std::cout << std::endl;
      }
   
      // if solution pertains to a vertex higher than the range already defined,
      // then skip...
      if ( sId > maxOrigVertexId )
      {
         if ( this->debug ) 
            std::cout << "skipping solution " << sId << std::endl;
         
         // try to read the first term in the next line of data
         // (failure will get us out of loop)...
         fsolns >> sId;
         continue;
      }

      sId -= vShift;

      if ( sId < 0 )
      {
         if ( this->debug ) 
            std::cout << "Invalid sId = " << sId << std::endl;

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
            std::cout << "VECTOR: " << sId;
            for ( i=0; i<3; i++ ) 
               std::cout << "\t" << data[ i ];
            std::cout << std::endl;
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
 
   std::cout << "\tTotal no. of solutions = " << numSolns << std::endl;

   if ( this->debug && numSolns != numStarVertices) 
      std::cout << "Warning: numSolns != numStarVertices" << std::endl;

   std::cout << std::endl;

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
   StarParamFile.open( this->paramFileName.c_str(), std::ios::in );

   std::string tagName;//char tagName[ 50 ];//   
   std::string tagValue;//char tagValue[ 50 ];//   
   int  scaleIndexSpecified = 0;
   int  scaleFactorSpecified = 0;
  
   while( 1 )
   {
      StarParamFile.getline(textline,256);

      if ( StarParamFile.eof() )
      {
         break;
      }
      tagName.clear();
      tagValue.clear();

      fileIO::getTagAndValue( textline, tagName, tagValue);

      if ( this->debug )
      {
         std::cout << "textline = " << textline << std::endl;
         std::cout << "tagValue = " << tagValue << std::endl;
      }

      if( tagName.compare( "STARVRT" )==0 )
      {
         this->starVertFileName.assign( tagValue );
      }
      else if( tagName.compare( "STARCEL" )==0 )
      {
         this->starCellFileName.assign( tagValue );
      }
      else if( tagName.compare( "STARUSR" )==0 )
      {
         this->starUsrFileName.assign( tagValue );
      }
      else if( tagName.compare( "VECTORNAME" )==0 ) 
      {
         std::string newSpace;// = new char[ strlen(tagValue)+1 ];
         newSpace.assign( tagValue );//strcpy(newSpace,tagValue);
         this->vectorName.push_back( newSpace );
         if ( this->debug )
            std::cout << "vectorName[" << this->numVectors << "] = " 
                 << this->vectorName[ this->numVectors ] << std::endl;
         this->numVectors++;
         if ( this->numVectors > 1 )
         {
            std::cerr <<"\nError - Star-CD format should have no more than 1 vector"
                 << std::endl;
            exit ( 1 );
         }
      }
      else if( tagName.compare( "SCALARNAME" )==0 )
      {
         std::string newSpace;// = new char[ strlen(tagValue)+1 ];
         newSpace.assign( tagValue );//strcpy(newSpace,tagValue);
         this->scalarName.push_back( newSpace );
         if ( this->debug )
            std::cout << "scalarName[" << this->numScalars << "] = " 
                 << this->scalarName[ this->numScalars ] << std::endl;
         this->numScalars++;
      }
      else if( tagName.compare( "SCALEINDEX" )==0 )
      {
         scaleIndexSpecified = 1;
         // uses the integer indices defined in translateToVtk.cpp
         // 0 = No scaling, 1 = Custom scaling, 2 = meters to feet, etc.
         // The use of SCALEINDEX=0 is not necessary as it is the default
         // The use of SCALEINDEX=1 needs SCALEFACTOR to be set (or
         // scale factor defaults to 1)
  //       this->scaleIndex = atoi( tagValue );
         if ( this->debug )
            std::cout << "scale selection = " << this->scaleIndex << std::endl;
      }
      else if( tagName.compare( "SCALEFACTOR" )==0 )
      {
         scaleFactorSpecified = 1;
         // the use of this option implies that SCALEINDEX=1 (Custom scaling)
         this->scaleFactor = atof( tagValue.c_str() );            
         if ( this->debug )
            std::cout << "scale factor = " << this->scaleFactor << std::endl;
      }
      else if( tagName.compare( "OUTPUTFILENAME" )==0 )
      {
         this->vtkFileName.assign( tagValue );
      }
      else if( tagName.compare( "ROTATEX" )==0 )
      {
         this->rotations[ 0 ] = atof( tagValue.c_str() );
      }
      else if( tagName.compare( "ROTATEY" )==0 )
      {
         this->rotations[ 1 ] = atof( tagValue.c_str() );
      }
      else if( tagName.compare( "ROTATEZ" )==0 )
      {
         this->rotations[ 2 ] = atof( tagValue.c_str() );
      }
      else if( tagName.compare( "TRANSLATEX" )==0 )
      {
         this->translations[ 0 ] = atof( tagValue.c_str() );
      }
      else if( tagName.compare( "TRANSLATEY" )==0 )
      {
         this->translations[ 1 ] = atof( tagValue.c_str() );
      }
      else if( tagName.compare( "TRANSLATEZ" )==0 )
      {
         this->translations[ 2 ] = atof( tagValue.c_str() );
      }
      else if( tagName.compare( "WRITEOPTION" )==0 )
      {
         this->writeOption = atoi( tagValue.c_str() );
      }
      else
      {
         std::cerr << "Parameter " << tagName << " not found!" <<std::endl;
      }
   }
   StarParamFile.close();

   // if user failed to set the scaleIndex, but provided a scalefactor
   // reset scaleIndex to custom scale
   if ( !scaleIndexSpecified && this->scaleFactor != 1.0)
   {
      this->scaleIndex = 1;
   }
   else if ( this->scaleIndex == 1 && !scaleFactorSpecified )
   {
      std::cout << "\n!!! Custom scale factor requested but not provided!\n"
                << "Using the default values.\n"
                << std::endl;

      this->scaleIndex =0;
      this->scaleFactor =1.0;
      //exit(1);
   }

   // check for indeterminate case -- not custom, but scale factor specified
   else if ( (this->scaleIndex != 1 || this->scaleIndex !=0) && scaleFactorSpecified )
   {
      std::cout << "\n!!! Indeterminate case -- "
                << "not custom, but scale factor specified!\n"
                << "Using the default values.\n"
                << std::endl;
      //exit(1);
      this->scaleIndex =0;
      this->scaleFactor =1.0;
   }
   else
   {
      //this is do nothing
   }
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

int starReader::GetScaleIndex( void )
{
   return this->scaleIndex;
}

float starReader::GetScaleFactor( void )
{
   return this->scaleFactor;
}

std::string starReader::GetVTKFileName( void )
{
   return this->vtkFileName;
}

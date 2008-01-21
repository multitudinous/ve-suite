/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include "multiPlaneVtkOutput.h"
#include <iostream>
#include <fstream>
#include <sstream>

#include <vtkDataSet.h>
#include <vtkPlane.h>
#include <vtkCutter.h>
#include <vtkTriangleFilter.h>
#include <vtkDecimatePro.h>
#include <vtkSmoothPolyDataFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataWriter.h>
#include <vtkPointData.h>
#include <vtkAppendPolyData.h>   // used with transient data to concatenate many planes into one polydata object

multiPlaneVtkOutput::multiPlaneVtkOutput( std::string dirname )
{
   //unsigned long len = strlen(dirname);
   postDataDir.assign( dirname );//strcpy( postDataDir, dirname );
}

multiPlaneVtkOutput::~multiPlaneVtkOutput()
{
   //delete [] postDataDir;
}

void multiPlaneVtkOutput::writeMultiPlanes( vtkDataSet *unsGrid,
				 int X_cutMaxStepRange,
				 int Y_cutMaxStepRange,
				 int Z_cutMaxStepRange,
             int multiPlaneOption,
             int transientFileNumber,
             int myid, 
             int xprocs, 
             int yprocs, 
             int zprocs,
             int numProcs )
{
   // Init cutplane variables
   this->unsData = unsGrid;

/*
   std::cout << X_cutMaxStepRange << " : " << Y_cutMaxStepRange << " : " 
        << Z_cutMaxStepRange << " : " << multiPlaneOption << " : " 
        << transientFileNumber << " : " << myid << " : " << xprocs << " : " 
        << yprocs << " : " << zprocs <<" : "<< numProcs << std::endl;
*/

   double tmp[6];
   this->unsData->GetBounds( tmp );

   float xmin = (float)tmp[0];
   float xmax = (float)tmp[1];
   float ymin = (float)tmp[2];
   float ymax = (float)tmp[3];
   float zmin = (float)tmp[4];
   float zmax = (float)tmp[5];

   float dx = ( xmax - xmin ) / (float) ( X_cutMaxStepRange + 1 );
   float dy = ( ymax - ymin ) / (float) ( Y_cutMaxStepRange + 1 );
   float dz = ( zmax - zmin ) / (float) ( Z_cutMaxStepRange + 1 );

/*
std::cout << xmin << " : " << xmax << std::endl;
std::cout << ymin << " : " << ymax << std::endl;
std::cout << zmin << " : " << zmax << std::endl;
*/

   int i;

   vtkAppendPolyData * appendPolyData;
   if ( multiPlaneOption && numProcs == 1 )
   {
      appendPolyData = vtkAppendPolyData::New();
   }
   vtkPolyData ** append = NULL;
   
   if ( numProcs == 1 )
   {
      xprocs = 1;
      yprocs = 1;
      zprocs = 1;
      myid = 0;
   }

   if ( myid < xprocs )
   {
      int pppx, xrem, begin, end;
	   pppx = X_cutMaxStepRange/xprocs;
	   xrem = X_cutMaxStepRange%xprocs;
	   
      if ( myid < xrem )
	   {
  	      pppx += 1;
   	   begin = myid*pppx;
         end = begin + pppx -1;
	   }
	
      if ( myid >= xrem )
	   {
   	   begin = myid*pppx + xrem;
         end = begin + pppx -1;
	   }

      append = new vtkPolyData* [X_cutMaxStepRange];
      //append = new vtkPolyData* [ end + 1 ];
      //for (i=0; i < X_cutMaxStepRange; i++ )
      //{
    	for (i = begin; i <= end ; i++ )
    	{
         float position = xmin + dx * ( i + 1 );
         append[i] = MakePolyData( 0, position );

         if ( multiPlaneOption && numProcs == 1 )
         {
            appendPolyData->AddInput( append[i] );
         }
         else
         {
            WritePolyData( append[i], 0, i );
         }
      }
      
      for (i=0; i<X_cutMaxStepRange; i++)
    	//for (i = 0; i <= end ; i++ )
         append[i]->Delete();
   }

   if ( multiPlaneOption && numProcs == 1 )
   {
      appendPolyData->Update();
      if ( X_cutMaxStepRange ) 
         WriteMultiPolyData( appendPolyData->GetOutput(), 0, transientFileNumber );
      appendPolyData->Delete();
      appendPolyData = vtkAppendPolyData::New();
   }

   if ( numProcs == 1 )
   {
      myid = 1;
   }

   if( myid >= xprocs && myid < xprocs + yprocs )
   {
	   int pppy, yrem, begin, end;
	   pppy = Y_cutMaxStepRange/yprocs;
	   yrem = Y_cutMaxStepRange%yprocs;
	   if ( myid < xprocs + yrem)
	   {
   	   pppy += 1;
         begin = (myid - xprocs)*pppy;
         end = begin + pppy -1;
      }
      
      if ( myid >= yrem + xprocs )
	   {
   	   begin = (myid - xprocs)*pppy + yrem;
         end = begin + pppy -1;
      }

      append = new vtkPolyData* [Y_cutMaxStepRange];
      //append = new vtkPolyData* [ end + 1 ];
      //for (i=0; i < Y_cutMaxStepRange; i++ )
      //{
      for (i = begin; i <= end ; i++ )
      {
         float position = ymin + dy * ( i + 1 );
         append[i] = MakePolyData( 1, position );

         if ( multiPlaneOption && numProcs == 1 )
         {
            appendPolyData->AddInput( append[i] );
         }
         else
         {
            WritePolyData( append[i], 1, i );
         }
      }
      
      for (i=0; i<Y_cutMaxStepRange; i++)
    	//for (i = 0; i <= end ; i++ )
         append[i]->Delete();
   }

   if ( multiPlaneOption && numProcs == 1 )
   {
      appendPolyData->Update();
      if (Y_cutMaxStepRange) 
         WriteMultiPolyData( appendPolyData->GetOutput(), 1, transientFileNumber );
      appendPolyData->Delete();
      appendPolyData = vtkAppendPolyData::New();
   }

   if ( numProcs == 1 )
   {
      myid = 2;
   }

   if( myid >= xprocs + yprocs && myid < xprocs + yprocs + zprocs )
   {
      int pppz, zrem, begin, end;
	   pppz = Z_cutMaxStepRange/zprocs;
	   zrem = Z_cutMaxStepRange%zprocs;
	   if ( myid < xprocs + yprocs + zrem )
	   {
  	      pppz += 1;
         begin = (myid - xprocs - yprocs)*pppz;
         end = begin + pppz -1;
      }
	
      if ( myid >= zrem + xprocs + yprocs )
	   {
         begin = (myid - xprocs - yprocs)*pppz + zrem;
         end = begin + pppz -1;
	   }

      append = new vtkPolyData* [Z_cutMaxStepRange];
      //append = new vtkPolyData* [ end + 1 ];
      //for (i=0; i < Z_cutMaxStepRange; i++ )
      //{
      for (i = begin; i <= end ; i++ )
      {
         float position = zmin + dz * ( i + 1 );
         append[i] = MakePolyData( 2, position );

         if ( multiPlaneOption && numProcs == 1 )
         {
            appendPolyData->AddInput( append[i] );
         }
         else
         {
            WritePolyData( append[i], 2, i );
         }
      }
      
      for (i=0; i<Z_cutMaxStepRange; i++)
    	//for (i = 0; i <= end ; i++ )
         append[i]->Delete();
   }

   if ( multiPlaneOption && numProcs == 1 )
   {
      appendPolyData->Update();
      if (Z_cutMaxStepRange) 
         WriteMultiPolyData( appendPolyData->GetOutput(), 2, transientFileNumber );
      appendPolyData->Delete();
   }
   std::cout << std::endl;
}

void multiPlaneVtkOutput::readParamFileandWriteMultiPlanes( vtkDataSet *unsGrid,
                                                            std::string paramFile,
                                                            int multiPlaneOption,
                                                            int transientFileNumber )
{
   char  textLine[ 256 ];
   float Cut;
   int   numCut;

   std::ifstream inFile( paramFile.c_str(), std::ios::in );

   this->unsData = unsGrid;

   for ( int j = 0; j < 3; j++ )
   {
      inFile >> numCut;
      inFile.getline( textLine, 256 );   //skip past remainder of line

      if ( numCut == 0 ) continue;

      vtkAppendPolyData * appendPolyData;
      vtkPolyData ** append;
      if ( multiPlaneOption )
      {
         appendPolyData = vtkAppendPolyData::New();
      }
      
      append = new vtkPolyData* [numCut];

      int i;
      for ( i = 0; i < numCut; i++ )
      {
         inFile >> Cut;
         append[i] = MakePolyData( j, Cut );
         append[i]->Update();

         if ( multiPlaneOption )
         {
            appendPolyData->AddInput( append[i] );
         }
         else
         {
            WritePolyData( append[i], j, i );
         }
      }

      if ( multiPlaneOption )
      {
         appendPolyData->Update();
         WriteMultiPolyData( appendPolyData->GetOutput(), j, transientFileNumber );
         appendPolyData->Delete();
      }

      for ( i=0; i<numCut; i++ )
      {
         append[i]->Delete();
         append[i] = NULL;
      }

      inFile.getline( textLine, 256 );   //skip past remainder of line
   }
}


vtkPolyData * multiPlaneVtkOutput::MakePolyData( int xyz, float pos )
{
   //std::cout << "in MakePolyData, pos = " << pos << std::endl;
   //for (int k=0; k<80; k++) std::cout << "\b";
   std::cout << "Working on plane direction " << xyz 
        << ", position " << pos <<  "     " << std::endl;
   //std::cout.flush();

   vtkPlane *plane = vtkPlane::New();
   if      ( xyz == 0 )  //  Set up the X cutting plane
   {
      plane->SetOrigin( pos, 0.0f, 0.0f );
      plane->SetNormal( 1.0f, 0.0f, 0.0f);
   }
   else if ( xyz == 1 )  // Set up the Y cutting plane
   {
      plane->SetOrigin( 0.0f, pos, 0.0f );
      plane->SetNormal( 0.0f, 1.0f, 0.0f);
   }
   else if ( xyz == 2 )  //  Set up the Z cutting plane
   {
      plane->SetOrigin( 0.0f, 0.0f, pos );
      plane->SetNormal( 0.0f, 0.0f, 1.0f);
   }

//writeVtkThing( this->unsData, "SJK_1.vtk" );

   //"slice-through" the dataset, generating a surface that can be visualized. 
   vtkCutter *cutter = vtkCutter::New();
   cutter->SetInput( this->unsData );
   cutter->SetCutFunction( plane );

//writeVtkThing( cutter->GetOutput(), "SJK_2.vtk" );
   //create triangle polygons from input polygons and triangle strips
   vtkTriangleFilter *tFilter = vtkTriangleFilter::New();
   tFilter->SetInput( cutter->GetOutput() );

//writeVtkThing( tFilter->GetOutput(), "SJK_3.vtk" );

/*
//vtkDecimatePro was losing critical data points in 36 cubes test..
   //reduce the number of triangles in a triangle mesh
   vtkDecimatePro *deci = vtkDecimatePro::New();
   deci->SetInput( tFilter->GetOutput() );
   deci->SetTargetReduction( 0.01 );
   deci->SplittingOn();
   deci->BoundaryVertexDeletionOff();
   //deci->PreserveTopologyOn();
//writeVtkThing( deci->GetOutput(), "SJK_4.vtk" );
*/

   vtkSmoothPolyDataFilter *smoother = vtkSmoothPolyDataFilter::New();
   //smoother->SetInput( deci->GetOutput() );
   smoother->SetInput( tFilter->GetOutput() );
   smoother->SetNumberOfIterations( 0 ); // was one
   smoother->Update();

//writeVtkThing( smoother->GetOutput(), "SJK_5.vtk" );

   vtkPolyData * polyData = vtkPolyData::New();
   polyData->DeepCopy( smoother->GetOutput() );
   polyData->Update();

//writeVtkThing( polyData, "SJK_6.vtk" );

   plane->Delete();
   cutter->Delete();
   tFilter->Delete();
   //deci->Delete();
   smoother->Delete();
   return polyData;
}


void multiPlaneVtkOutput::WritePolyData( vtkPolyData * polyData, int xyz, int i )
{
   //char  contFname[50];
   //if ( i > 999 )
   std::ostringstream dirStringStream;
   {
      if      (xyz == 0 ) dirStringStream << postDataDir << "/X_Cont" << i << ".vtk";
         //sprintf( contFname, "%s/X_Cont%i.vtk", postDataDir, i );
      else if (xyz == 1 ) dirStringStream << postDataDir << "/Y_Cont" << i << ".vtk";
      //sprintf( contFname, "%s/Y_Cont%i.vtk", postDataDir, i );
      else if (xyz == 2 ) dirStringStream << postDataDir << "/Z_Cont" << i << ".vtk";
      //sprintf( contFname, "%s/Z_Cont%i.vtk", postDataDir, i );
   }
/*
// had to use above non-padded name scheme for compatibility with cfdPlanes.cxx
   else
   {
      if      (xyz == 0 ) sprintf( contFname, "%s/X_Cont%03i.vtk", postDataDir, i );
      else if (xyz == 1 ) sprintf( contFname, "%s/Y_Cont%03i.vtk", postDataDir, i );
      else if (xyz == 2 ) sprintf( contFname, "%s/Z_Cont%03i.vtk", postDataDir, i );
   }
*/

   vtkPolyDataWriter *polyDataWriter = vtkPolyDataWriter::New();
   polyDataWriter->SetFileName( dirStringStream.str().c_str() );
   polyDataWriter->SetInput( polyData );
#ifndef SJK_TEST
   polyDataWriter->SetFileTypeToBinary();
#endif
   polyDataWriter->Write();
   polyDataWriter->Delete();
}


void multiPlaneVtkOutput::WriteMultiPolyData( vtkPolyData * polyData, int xyz, int i )
{
   //char  contFname[50];
   //if ( i > 999 )
   std::ostringstream dirStringStream;
   {
      if      (xyz == 0 ) dirStringStream << postDataDir << "/X_MultiCont_" << i << ".vtk";
         //sprintf( contFname, "%s/X_Cont%i.vtk", postDataDir, i );
      else if (xyz == 1 ) dirStringStream << postDataDir << "/Y_MultiCont_" << i << ".vtk";
      //sprintf( contFname, "%s/Y_Cont%i.vtk", postDataDir, i );
      else if (xyz == 2 ) dirStringStream << postDataDir << "/Z_MultiCont_" << i << ".vtk";
      //sprintf( contFname, "%s/Z_Cont%i.vtk", postDataDir, i );
   }
/*
// had to use above non-padded name scheme for compatibility with cfdPlanes.cxx
   else
   {
      if      (xyz == 0 ) sprintf( contFname, "%s/X_MultiCont_%03i.vtk", postDataDir, i );
      else if (xyz == 1 ) sprintf( contFname, "%s/Y_MultiCont_%03i.vtk", postDataDir, i );
      else if (xyz == 2 ) sprintf( contFname, "%s/Z_MultiCont_%03i.vtk", postDataDir, i );
   }
*/

   vtkPolyDataWriter *polyDataWriter = vtkPolyDataWriter::New();
   polyDataWriter->SetFileName( dirStringStream.str().c_str() );
   polyDataWriter->SetInput( polyData );
#ifndef SJK_TEST
   polyDataWriter->SetFileTypeToBinary();
#endif
   polyDataWriter->Write();
   polyDataWriter->Delete();
}


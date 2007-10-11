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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <string>
#include <ves/xplorer/event/cfdPlanes.h>
#include <ves/xplorer/event/cfdCuttingPlane.h>
#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkPlane.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkAppendPolyData.h>
#include <vtkCutter.h>

#include <ves/xplorer/event/cfdDebug.h>

#include <sstream>

using namespace VE_Xplorer;
using namespace VE_Util;

cfdPlanes::cfdPlanes( const int xyz, const char directory[],
                      const double bounds[ 6 ] )
{
   vprDEBUG(vesDBG,2) << " cfdPlanes constructor" 
                          << std::endl << vprDEBUG_FLUSH;
   this->numPlanes = 0;
   this->append = NULL;
   this->isPlaneSelected = NULL;
   this->sliceLocation = NULL;
   this->collectivePolyData = NULL;
   this->cuttingPlane = NULL;

   this->type = xyz;

   if      ( this->type == 0 ) this->typeLabel = 'X';
   else if ( this->type == 1 ) this->typeLabel = 'Y';
   else if ( this->type == 2 ) this->typeLabel = 'Z';
   else
   {
       std::cerr << "ERROR: in cfdPlanes, xyz must be 0, 1, or 2" << std::endl;
       exit( 1 );
   }
   vprDEBUG(vesDBG,1) << "this->typeLabel = " << this->typeLabel 
                           << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vesDBG,1) << "directory: \"" << directory << "\"" 
                           << std::endl << vprDEBUG_FLUSH;

   // count the total number of cut planes
   for ( int i=0; 1; i++ )
   {
      //sprintf( planeFileName, "%s/%c_Cont%d.vtk", directory, 
      //          this->typeLabel, i );
      std::ostringstream dirStringStream;
      dirStringStream << directory << "/" << this->typeLabel << "_Cont" << i << ".vtk";
      std::string dirString = dirStringStream.str();
      //planeFileName = (char*)dirString.c_str();
       if ( ! fileIO::isFileReadable( (std::string)dirString.c_str() ) )
       {
           this->numPlanes = i;
           vprDEBUG(vesDBG,0) << "\t\tFound " << this->numPlanes 
                << " " << this->typeLabel << "-planes"
                << std::endl << vprDEBUG_FLUSH; 
           break;
       }
   }

   if ( this->numPlanes == 0 ) 
   {
      return;
   }
   
   this->append = new vtkPolyData* [ this->numPlanes ];
   this->sliceLocation = new float [ this->numPlanes ];
   for ( int i = 0; i < this->numPlanes; i++ )
   {
      //sprintf( planeFileName, "%s/%c_Cont%d.vtk", directory,
      //         this->typeLabel, i );
      std::ostringstream dirStringStream;
      dirStringStream << directory << "/" << this->typeLabel << "_Cont" << i << ".vtk";
      std::string dirString = dirStringStream.str();
      //planeFileName = (char*)dirString.c_str();

      //planeReader = vtkPolyDataReader::New();
      //planeReader->SetFileName( dirString.c_str() );//(char*)dirString.c_str() );
      //planeReader->Update();
      vtkPolyData* tempPolyData = dynamic_cast< vtkPolyData* >( readVtkThing( dirString.c_str() ) );
      // look at POINTS to see what coordinate that the plane goes through
      double vertex [ 3 ];
      tempPolyData->GetPoints()->GetPoint(0, vertex);
      this->sliceLocation[ i ] = (float)vertex[this->type];
      vprDEBUG(vesDBG,1) << "\t\tplane[" << i 
         << "] goes through coordinate " 
         << this->sliceLocation[ i ] << std::endl << vprDEBUG_FLUSH;

      this->append[ i ] = vtkPolyData::New();
      this->append[ i ]->DeepCopy( tempPolyData );
      tempPolyData->Delete();
   }

   // allocate space for the array that keeps track of which planes
   // are selected for display...
   this->isPlaneSelected = new int [ this->numPlanes ];

   // Set all planes to be selected and concatenate them all into one
   // polyData object called collectivePolyData
   //this->SetAllPlanesSelected();
   //this->ConcatenateSelectedPlanes();

   // create a cutting plane object from the bounds of the original dataset
   // to use when the user wants a close-enough plane
   this->cuttingPlane = new cfdCuttingPlane( bounds, xyz, 1 );

/*
   // create a cutting plane object from the bounds of the collectivePolyData
   // to use when the user wants a close-enough plane
   this->cuttingPlane = new cfdCuttingPlane( 
                              this->collectivePolyData->GetBounds(), xyz, 1 );
*/
}

cfdPlanes::~cfdPlanes()
{
   //vprDEBUG(vesDBG,2) << " cfdPlanes destructor" 
   //                       << std::endl << vprDEBUG_FLUSH;

   if ( this->numPlanes > 0 )
   {
      for (int i = 0; i  <this->numPlanes; i++) 
      {
         this->append[ i ]->Delete();
      }
       
      delete [] this->append; 
      this->append = NULL;
   }

   if (this->collectivePolyData != NULL )
   {
      //this->collectivePolyData->Delete();
      this->collectivePolyData = NULL;
   }

   if ( this->cuttingPlane != NULL )
   {
      delete this->cuttingPlane;
      this->cuttingPlane = NULL;
   }

   if ( this->isPlaneSelected != NULL )
   {
      delete [] this->isPlaneSelected;
      this->isPlaneSelected = NULL;
   }
   
   if ( this->sliceLocation != NULL )
   {
      delete [] this->sliceLocation;
      this->sliceLocation = NULL;
   }
}

void cfdPlanes::SetAllPlanesSelected( void )
{
   for ( int i = 0; i < this->numPlanes; i++ )
   {
      this->isPlaneSelected[ i ] = 1;
   }
}

vtkPolyData * cfdPlanes::GetPlanesData( void )
{
    return this->collectivePolyData;
}

// 0 <= sliderBarPos <= 100
vtkPolyData * cfdPlanes::GetClosestPlane( const int sliderBarPos )
{
   if ( this->numPlanes == 0 )
   {
      return NULL;
   }

   this->cuttingPlane->Advance( sliderBarPos );

   double origin[3];
   this->cuttingPlane->GetOrigin( origin );
   double coordinate = origin[ this->type ];

   vprDEBUG(vesDBG,1) 
      << "activating precomputed plane corresponding to requested coordinate: " 
      << coordinate << " : Slider Bar Position : " << sliderBarPos << std::endl << vprDEBUG_FLUSH;

   float leastSquaredDistance = 1e12;
   int index = 0;

   for ( int i = 0; i < this->numPlanes; i++)
   {
      float sqDist = ( coordinate - this->sliceLocation[ i ] ) *
                     ( coordinate - this->sliceLocation[ i ] );
      vprDEBUG(vesDBG,1)
         << "plane " << i << ": sliceLocation = " << this->sliceLocation[ i ]
         << ", sqDist = " << sqDist << std::endl << vprDEBUG_FLUSH;

      if ( leastSquaredDistance > sqDist )
      {
         leastSquaredDistance = sqDist;
         index = i;
      }
   }
   this->isPlaneSelected[ index ] = 1;

   return this->append[ index ];
}

void cfdPlanes::ConcatenateSelectedPlanes( void )
{
   if ( this->numPlanes == 0 )
   {
      return;
   }

   vtkAppendPolyData * appendPolyData = vtkAppendPolyData::New();

   for ( int i = 0; i < this->numPlanes; i++ )
   {
      vprDEBUG(vesDBG,1) 
         << "isPlaneSelected[" << i << "] = " << isPlaneSelected[ i ]
         << std::endl << vprDEBUG_FLUSH;

      if ( ! isPlaneSelected[ i ] )
      {
         continue;
      }
      appendPolyData->AddInput( append[ i ] );
   }

   appendPolyData->Update();

   if ( this->collectivePolyData != NULL )
   {
      // user classes should delete
      //this->collectivePolyData->Delete();
   }
   this->collectivePolyData = vtkPolyData::New();
   this->collectivePolyData->ShallowCopy( appendPolyData->GetOutput() );
   appendPolyData->Delete();
}

int cfdPlanes::GetNumberOfPlanes()
{
   return this->numPlanes;
}

vtkPolyData * cfdPlanes::GetPlane( const int i )
{
   return this->append[ i ];
}


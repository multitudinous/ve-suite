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
 * File:          $RCSfile: cfdDCS.h,v $
 * Date modified: $Date: 2004-05-18 13:44:18 -0700 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdSteadyStateVizHandler.h"

#include "cfdPolyData.h"      
#include "cfdIsosurface.h"    
#include "cfdPresetContour.h" 
#include "cfdContours.h"      
#include "cfdMomentum.h"      
#include "cfdPresetMomentum.h"
#include "cfdMomentums.h"     
#include "cfdVector.h"        
#include "cfdPresetVector.h"  
#include "cfdVectors.h"       
#include "cfdStreamers.h"     
#include "cfdPolyData.h"      
#include "cfdImage.h"         
#include "cfdAnimatedImage.h" 
#include "cfdAnimatedStreamlineCone.h"
#include "cfdContour.h"

#include "cfdDataSet.h"
#include "cfdEnum.h"
#include "cfdGlobalBase.h"
#include "cfdCommandArray.h"
#include "cfdObjects.h"
#include "cfdPlanes.h"
#include "cfdDCS.h"
#include "cfdTempAnimation.h"
#include "cfdNavigate.h"
#include "cfdCursor.h"

#include <vpr/Util/Debug.h>
#include <vpr/vpr.h>
#include <vpr/System.h>
#include <vpr/Thread/Thread.h>

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>

cfdSteadyStateVizHandler::cfdSteadyStateVizHandler( char* param )
{
   this->surface = NULL;
   this->isosurface = NULL;
   this->contour = NULL;
   this->momentum = NULL;
   this->vector = NULL;
   this->x_contour = NULL;
   this->y_contour = NULL;
   this->z_contour = NULL;
   this->x_momentum = NULL;
   this->y_momentum = NULL;
   this->z_momentum = NULL;
   this->x_vector = NULL;
   this->y_vector = NULL;
   this->z_vector = NULL;
   this->x_contours = NULL;
   this->y_contours = NULL;
   this->z_contours = NULL;
   this->x_momentums = NULL;
   this->y_momentums = NULL;
   this->z_momentums = NULL;
   this->x_vectors = NULL;
   this->y_vectors = NULL;
   this->z_vectors = NULL;
   this->streamlines = NULL;
   this->particles = NULL;
   this->image = NULL;
   this->animStreamer = NULL;
   this->animImg = NULL;
   
   _param = param;
}

cfdSteadyStateVizHandler::~cfdSteadyStateVizHandler( void )
{
   if ( this->isosurface ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->isosurface" << std::endl << vprDEBUG_FLUSH;
      delete this->isosurface;
   }

   if ( this->contour ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->contour" << std::endl << vprDEBUG_FLUSH;
      delete this->contour;
   }

   if ( this->momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->momentum;
   }

   if ( this->vector ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->vector" << std::endl << vprDEBUG_FLUSH;
      delete this->vector;
   }

   if ( this->x_contour ) 
   {
      vprDEBUG(vprDBG_ALL,2)   
        << "deleting this->x_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contour;
   }

   if ( this->y_contour ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->y_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contour;
   }

   if ( this->z_contour ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contour;
   }

   if ( this->x_momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->x_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentum;
   }

   if ( this->y_momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->y_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentum;
   }

   if ( this->z_momentum ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentum;
   }

   if ( this->x_vector ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->x_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vector;
   }

   if ( this->y_vector ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->y_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vector;
   }

   if ( this->z_vector ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->z_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->z_vector;
   }

   if ( this->x_contours ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->x_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contours;
   }

   if ( this->y_contours ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->y_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contours;
   }

   if ( this->z_contours ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contours;
   }

   if ( this->x_momentums ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->x_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentums;
   }

   if ( this->y_momentums ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->y_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentums;
   }

   if ( this->z_momentums ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->z_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentums;
   }

   if ( this->x_vectors ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->x_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vectors;
   }

   if ( this->y_vectors ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
         << "deleting this->y_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vectors;
   }

   if ( this->z_vectors ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "deleting this->z_vectors" << std::endl << vprDEBUG_FLUSH; 
      delete this->z_vectors;
   }
   
   if ( this->streamlines ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->streamlines" << std::endl << vprDEBUG_FLUSH;
      delete this->streamlines;
   }

   if ( this->particles ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->particles" << std::endl << vprDEBUG_FLUSH;
      delete this->particles;
   }

   if ( this->image ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->image" << std::endl << vprDEBUG_FLUSH;
      delete this->image;
   }

   if ( this->animImg ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->animImg" << std::endl << vprDEBUG_FLUSH;
      delete this->animImg;
   }

   if ( this->animStreamer ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->animStreamer" << std::endl << vprDEBUG_FLUSH;
      delete this->animStreamer;
   }
   
}
////////////////////
// Helper functions
////////////////////
void cfdSteadyStateVizHandler::SetActiveDataSet( cfdDataSet* input )
{
   _activeDataSet = input;
}

void cfdSteadyStateVizHandler::SetActiveMeshedVolume( cfdDataSet* input )
{
   _activeMeshedVolume = input;
}

void cfdSteadyStateVizHandler::SetCommandArray( cfdCommandArray* input )
{
   commandArray = input;
}

void cfdSteadyStateVizHandler::SetParameterFilename( char* param )
{
   _param = param;
}

void cfdSteadyStateVizHandler::SetWorldDCS( cfdDCS* input )
{
   _worldDCS = input;
}

void cfdSteadyStateVizHandler::SetNavigate( cfdNavigate* input )
{
   nav = input;
}

void cfdSteadyStateVizHandler::SetCursor( cfdCursor* input )
{
   cursor = input;
}
////////////////////

void cfdSteadyStateVizHandler::InitScene( void )
{
   if ( _activeMeshedVolume != NULL )
   {
      
      {
         int postData = 0;
         if ( _activeMeshedVolume->GetPrecomputedXSlices() != NULL &&
               _activeMeshedVolume->GetPrecomputedXSlices()->GetPlanesData() != NULL )
            postData += 1;

         if ( _activeMeshedVolume->GetPrecomputedYSlices() != NULL &&
               _activeMeshedVolume->GetPrecomputedYSlices()->GetPlanesData() != NULL )
            postData += 2;

         if ( _activeMeshedVolume->GetPrecomputedZSlices() != NULL &&
               _activeMeshedVolume->GetPrecomputedZSlices()->GetPlanesData() != NULL )
            postData += 4;
      
         commandArray->SetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE, postData );
      }

      //
      // Initiate the isosurface.
      //
      std::cout << "| 14. Initializing...................................... Isosurface |" << std::endl;
      this->isosurface = new cfdIsosurface( 10 );
      this->isosurface->SetObjectType( ISOSURFACE );
      this->dataList.push_back( this->isosurface );
      this->commandList.push_back( this->isosurface );
     
      //
      // Initiate the interactive contour.
      //
      std::cout << "| 15. Initializing......................................... Contour |" << std::endl;
      this->contour = new cfdContour();
      this->contour->SetObjectType( CONTOUR );
      this->dataList.push_back( this->contour ); 
      this->commandList.push_back( this->contour );
     
      // Make sure that this dataset has a vector and scalar...
      if ( _activeMeshedVolume->GetDataSet()->GetPointData()->GetVectors() &&
           _activeMeshedVolume->GetDataSet()->GetPointData()->GetScalars() )
      {
         //
         // Initiate the interactive momentum.
         //
         std::cout << "| 16. Initializing........................................ Momemtum |" << std::endl;
         this->momentum = new cfdMomentum();
         this->momentum->SetObjectType( MOMENTUM );
         this->dataList.push_back( this->momentum );
         this->commandList.push_back( this->momentum );
     
         //
         // Initiate the interactive vector.
         //
         std::cout << "| 17. Initializing.......................................... Vector |" << std::endl;
         this->vector = new cfdVector();
         this->vector->SetObjectType( VECTOR );
         this->dataList.push_back( this->vector );
         this->commandList.push_back( this->vector );
      }
     
      if ( _activeMeshedVolume->GetDataSet()->GetPointData()->GetScalars() )
      {
         //
         // Initiate the preset x contour.
         //
         std::cout << "| 19. Initializing................................ Preset x Contour |" << std::endl;
         this->x_contour = new cfdPresetContour( 0, 10 );
         this->x_contour->SetObjectType( X_CONTOUR );
         this->dataList.push_back( this->x_contour );
         this->commandList.push_back( this->x_contour );
        
         //
         // Initiate the preset y contour.
         //
         std::cout << "| 20. Initializing................................ Preset y Contour |" << std::endl;
         this->y_contour = new cfdPresetContour( 1, 10 );
         this->y_contour->SetObjectType( Y_CONTOUR );
         this->dataList.push_back( this->y_contour );
         this->commandList.push_back( this->y_contour );
        
         //
         // Initiate the preset z contour.
         //
         std::cout << "| 21. Initializing................................ Preset z Contour |" << std::endl;
         this->z_contour = new cfdPresetContour( 2, 10 );
         this->z_contour->SetObjectType( Z_CONTOUR );
         this->dataList.push_back( this->z_contour );
         this->commandList.push_back( this->z_contour );
      }
     
      // Make sure that this dataset has a vector field...
      if ( _activeMeshedVolume->GetDataSet()->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x momentum.
         //
         std::cout << "| 22. Initializing............................... Preset x Momentum |" << std::endl;
         // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
         this->x_momentum = new cfdPresetMomentum( 0, 1, 10 );
         this->x_momentum->SetObjectType( X_MOMENTUM );
         this->dataList.push_back( this->x_momentum );
         this->commandList.push_back( this->x_momentum );
        
         //
         // Initiate the preset y momentum.
         //
         std::cout << "| 23. Initializing............................... Preset y Momentum |" << std::endl;
         // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
         this->y_momentum = new cfdPresetMomentum( 1, 1, 10 );
         this->y_momentum->SetObjectType( Y_MOMENTUM );
         this->dataList.push_back( this->y_momentum );
         this->commandList.push_back( this->y_momentum );
        
         //
         // Initiate the preset z momentum.
         //
         std::cout << "| 24. Initializing............................... Preset z Momentum |" << std::endl;
         // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
         this->z_momentum = new cfdPresetMomentum( 2, 1, 10 );
         this->z_momentum->SetObjectType( Z_MOMENTUM );
         this->dataList.push_back( this->z_momentum );
         this->commandList.push_back( this->z_momentum );
        
         //
         // Initiate the preset x vector.
         //
         std::cout << "| 25. Initializing................................. Preset x Vector |" << std::endl;
         this->x_vector = new cfdPresetVector( 0, 10 );
         this->x_vector->SetObjectType( X_VECTOR );
         this->dataList.push_back( this->x_vector );
         this->commandList.push_back( this->x_vector );
        
         //
         // Initiate the preset y vector.
         //
         std::cout << "| 26. Initializing................................. Preset y Vector |" << std::endl;
         this->y_vector = new cfdPresetVector( 1, 10 );
         this->y_vector->SetObjectType( Y_VECTOR );
         this->dataList.push_back( this->y_vector );
         this->commandList.push_back( this->y_vector );

         //
         // Initiate the preset z vector.
         //
         std::cout << "| 27. Initializing................................. Preset z Vector |" << std::endl;
         this->z_vector = new cfdPresetVector( 2, 10 );
         this->z_vector->SetObjectType( Z_VECTOR );
         this->dataList.push_back( this->z_vector );
         this->commandList.push_back( this->z_vector );
      }

      //
      // Initiate the preset x contour lines.
      //
      if ( _activeMeshedVolume->GetPrecomputedXSlices() != NULL &&
           _activeMeshedVolume->GetPrecomputedXSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 28. Initializing....................Multiple X-planes of Contours |" << std::endl;
         this->x_contours = new cfdContours( 0 );
         this->x_contours->SetObjectType( X_CONTOURS );
         this->dataList.push_back( this->x_contours );
         this->commandList.push_back( this->x_contours );
      }

      //
      // Initiate the preset y contour lines.
      //
      if ( _activeMeshedVolume->GetPrecomputedYSlices() != NULL &&
           _activeMeshedVolume->GetPrecomputedYSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 29. Initializing....................Multiple Y-planes of Contours |" << std::endl;
         this->y_contours = new cfdContours( 1 );
         this->y_contours->SetObjectType( Y_CONTOURS );
         this->dataList.push_back( this->y_contours );
         this->commandList.push_back( this->y_contours );
      }

      //
      // Initiate the preset z contour lines.
      //
      if ( _activeMeshedVolume->GetPrecomputedZSlices() != NULL &&
           _activeMeshedVolume->GetPrecomputedZSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 30. Initializing....................Multiple Z-planes of Contours |" << std::endl;
         this->z_contours = new cfdContours( 2 );
         this->z_contours->SetObjectType( Z_CONTOURS );
         this->dataList.push_back( this->z_contours );
         this->commandList.push_back( this->z_contours );
      }

      // Make sure that this dataset has a vector and scalar...
      if ( _activeMeshedVolume->GetDataSet()->GetPointData()->GetVectors() &&
           _activeMeshedVolume->GetDataSet()->GetPointData()->GetScalars() )

      {
         //
         // Initiate the preset x momentums.
         //
         if ( _activeMeshedVolume->GetPrecomputedXSlices() != NULL &&
              _activeMeshedVolume->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 31. Initializing.......Multiple X-planes of Precomputed Momentums |" << std::endl;
            // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
            this->x_momentums = new cfdMomentums( 0, 1 );
            this->x_momentums->SetObjectType( X_MOMENTUMS );
            this->dataList.push_back( this->x_momentums );
            this->commandList.push_back( this->x_momentums );
         }

         //
         // Initiate the preset y momentums.
         //
         if ( _activeMeshedVolume->GetPrecomputedYSlices() != NULL &&
              _activeMeshedVolume->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 32. Initializing.......Multiple Y-planes of Precomputed Momentums |" << std::endl;
            // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
            this->y_momentums = new cfdMomentums( 1, 1 );
            this->y_momentums->SetObjectType( Y_MOMENTUMS );
            this->dataList.push_back( this->y_momentums );
            this->commandList.push_back( this->y_momentums );
         }

         //
         // Initiate the preset z momentums.
         //
         if ( _activeMeshedVolume->GetPrecomputedZSlices() != NULL &&
              _activeMeshedVolume->GetPrecomputedZSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 33. Initializing.......Multiple Z-planes of Precomputed Momentums |" << std::endl;
            // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
            this->z_momentums = new cfdMomentums( 2, 1 );
            this->z_momentums->SetObjectType( Z_MOMENTUMS );
            this->dataList.push_back( this->z_momentums );
            this->commandList.push_back( this->z_momentums );
         }
      }

      // Make sure that this dataset has a vector and scalar...
      if ( _activeMeshedVolume->GetDataSet()->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x vectors.
         //
         if ( _activeMeshedVolume->GetPrecomputedXSlices() != NULL &&
              _activeMeshedVolume->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 34. Initializing.........Multiple X-planes of Precomputed Vectors |" << std::endl;
            this->x_vectors = new cfdVectors( 0 );
            this->x_vectors->SetObjectType( X_VECTORS );
            this->dataList.push_back( this->x_vectors );
         }

         //
         // Initiate the preset y vectors.
         //
         if ( _activeMeshedVolume->GetPrecomputedYSlices() != NULL &&
              _activeMeshedVolume->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 35. Initializing.........Multiple Y-planes of Precomputed Vectors |" << std::endl;
            this->y_vectors = new cfdVectors( 1 );
            this->y_vectors->SetObjectType( Y_VECTORS );
            this->dataList.push_back( this->y_vectors );
         }
         
         //
         // Initiate the preset z vectors.
         //
         if ( _activeMeshedVolume->GetPrecomputedZSlices() != NULL &&
              _activeMeshedVolume->GetPrecomputedZSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 36. Initializing.........Multiple Z-planes of Precomputed Vectors |" << std::endl;
            this->z_vectors = new cfdVectors( 2 );
            this->z_vectors->SetObjectType( Z_VECTORS );
            this->dataList.push_back( this->z_vectors );
         }

         //
         // Initiate the streamlines.
         //
         std::cout << "| 37. Initializing..................................... Streamlines |" << std::endl;
         // Needs to be set by the gui, fix this
         this->streamlines = new cfdStreamers( 0.0f );
         this->streamlines->SetObjectType( STREAMLINES );
         this->dataList.push_back( this->streamlines );     
         this->commandList.push_back( this->streamlines );

         //
         // Initiate the animated streamers.
         //
         std::cout << "| 39. Initializing............................. Animated Streamline |" << std::endl;
         // Needs to be set by the gui, fix this
         this->animStreamer = new cfdAnimatedStreamlineCone( 0.0f );
         this->animStreamer->SetObjectType( ANIMATED_STREAMLINES );
         this->dataList.push_back( this->animStreamer );     

         //
         // Initiate the animated Images.
         //
         // Need to fix this
         //if ( this->paramReader->frames != 0 )
         {
            std::cout << "| 39.b Initializing............................. Animated Images |" << std::endl;
            this->animImg = new cfdAnimatedImage( _param );
            this->animImg->SetObjectType( ANIMATED_IMAGES );
            this->dataList.push_back( this->animImg);  
         }  
      }
   }

   //
   // Initiate the Text Prompt
   //
   //std::cout << "| 40. Initializing..................................... Text Prompt |" << std::endl;
   //this->tPrompt = new textPrompt();
   //this->text_sig = 0 ;

   //
   // Initiate the PolyData File
   //
   // Neeed to fix this
   //if ( cfdObjects::GetActiveParticleData() != NULL )
   {
      std::cout << "| 41. Initializing................................... PolyData File |" << std::endl;
      this->particles = new cfdPolyData();
      this->particles->SetObjectType( PARTICLES );
      this->dataList.push_back( this->particles );     
   } 

      std::cout << "|  5. Initializing................................. Dataset surface |" << std::endl;
      this->surface = new cfdPolyData( 1.0 );
      this->surface->SetObjectType( POLYDATA );
      this->dataList.push_back( this->surface );

   //
   // Initiate PIV data from INEL
   //
   // Need to fix this
   //if ( this->paramReader->bmpFile ) 
   {
      std::cout << "| 42. Initializing.................................... Bitmap Image |" << std::endl;
      this->image = new cfdImage( _param );
      this->image->SetObjectType( IMAGE_EX );
      this->dataList.push_back( this->image );     
   } 
   //
   // Initiate the Performer objects.
   //
   std::cout << "| 51. Initializing........................................ pfGeodes |" << std::endl;

   //bool transient_flag = true;
   for ( int i = 0; i < (int)this->dataList.size(); i++ )
   {
      //this->dataList[ i ]->SetcfdReadParam( this->paramReader );
      this->dataList[ i ]->SetUpdateFlag( false );
      this->dataList[ i ]->SetGeodeFlag( false );
      this->dataList[ i ]->SetDCS( this->_worldDCS );

      // for the objects in the virtual environment.
      // Need to rethink this section fix this
      /*
      cfdTransientFlowManager * tfmTest = dynamic_cast<cfdTransientFlowManager *>( this->dataList[ i ] );
      if ( transient_flag && tfmTest != NULL )
      {
         this->cfdTimesteps = tfmTest->GetNumberOfFrames();
         transient_flag = false;
      }
      */
      
   }
}

void cfdSteadyStateVizHandler::PreFrameUpdate( void )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) != -1 )
   {
      vprDEBUG(vprDBG_ALL,2) 
         << "preFrame: id = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ID )
         << ", iso = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << ", scalarIndex = " << commandArray->GetCommandValue( cfdCommandArray::CFD_SC )
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << ", geo_state = " << commandArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE )
         << ", pre_state = " << commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE )
         << ", teacher_state = " << commandArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE )
         << std::endl << vprDEBUG_FLUSH;
   }

   int i;
   for ( i = 0; i < (int)this->commandList.size(); i ++ )
   {
      // Fix this
      // Non of this is implemented 
      // Need to add command array to VJObs
      //bool commandApplies = this->commandList[ i ]
      //                          ->CheckCommandId( this->cfdCommandArray );
      //if ( commandApplies )
      //   break;
   }
   this->commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );

   // check any virtual objects need to be updated
   if ( this->actorsAreReady )
   {
      vprDEBUG(vprDBG_ALL,4) << "|   Updating Objects"
                                   << std::endl << vprDEBUG_FLUSH;
      for ( int i = 0; i < (int)this->dataList.size(); i++ )
      {
         if ( this->dataList[ i ]->GetGeodeFlag() )
         {
            vprDEBUG(vprDBG_ALL,2) << " have geode flag"
                                   << std::endl << vprDEBUG_FLUSH;

            if ( this->_worldDCS->SearchChild( (cfdSceneNode*)this->dataList[ i ]->GetDCS() ) < 0 )
            {
               vprDEBUG(vprDBG_ALL,1) << " adding active DCS to worldDCS"
                                   << std::endl << vprDEBUG_FLUSH;
               this->_worldDCS->AddChild( (cfdSceneNode*)this->dataList[ i ]->GetDCS() );
            }

            if ( this->dataList[ i ]->GetcfdGeode() != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << " will add geode to sg"
                                      << std::endl << vprDEBUG_FLUSH;
               // Add steady state geodes to the scene graph
               this->dataList[ i ]->AddcfdGeodeToDCS();
            }
            else if ( this->dataList[ i ]->GetSequence() != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << " will add viz object "
                                      << i << " to sequence"
                                      << std::endl << vprDEBUG_FLUSH;

               {
                  this->dataList[ i ]->GetSequence()->AddToSequence(
                                          this->dataList[ i ]->GetObjectType() );
               }
            }
            vprDEBUG(vprDBG_ALL,2) << " End Update Loop"
                                   << std::endl << vprDEBUG_FLUSH;

            // Resetting these variables is very important
            this->dataList[ i ]->SetUpdateFlag( false );
            this->dataList[ i ]->SetGeodeFlag( false );
            this->actorsAreReady = false;
         }
      }
   }
   else if ( ( ( 0 <= this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) ) &&
                ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) < 100 ) ) && 
                  ( this->computeActorsAndGeodes == false ) )
   {
      vprDEBUG(vprDBG_ALL,1) << " selected ID number = " << this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID )
                             << std::endl << vprDEBUG_FLUSH;
      for ( int i = 0; i < (int)this->dataList.size(); i ++ )
      {          
         if ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == dataList[ i ]->GetObjectType() )
         {
            // verify that if a transient sequence is desired, 
            // an appropriate DCS is active...
            if ( ( ( X_TRANSIENT_CONTOUR <= this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) ) &&
                                          ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID )<= PARTICLE_TRANSIENT ) ) &&
                 ! cfdObjects::GetActiveDataSet()->IsPartOfTransientSeries() )
            {
               std::cerr << "\nERROR: You must activate an appropriate transient "
                         << "dataset before proceeding\n" << std::endl;
               this->_activeObject = NULL;
               this->computeActorsAndGeodes = false;
               break;
            }

            vprDEBUG(vprDBG_ALL,1) << " setting viz object " << i 
                                   << " to _activeObject"
                                   << std::endl << vprDEBUG_FLUSH;

            this->_activeObject = this->dataList[ i ];

            if ( this->_activeObject->GetObjectType() == IMAGE_EX )
            {
               this->_activeDataSetDCS = _worldDCS;
            }
            else
            {
               this->_activeDataSetDCS = _activeDataSet->GetDCS();               
            }

            //if ( this->computeActorsAndGeodes == false )
            {
               // add active dataset DCS to scene graph if not already there...
               vprDEBUG(vprDBG_ALL,1) << " setting DCS to activeDCS = "
                                   << this->_activeDataSetDCS
                                   << std::endl << vprDEBUG_FLUSH;
               this->_activeObject->SetDCS( this->_activeDataSetDCS );

               this->_activeObject->SetNormal( this->nav->GetDirection() );
               this->_activeObject->SetOrigin( this->nav->GetObjLocation() );

               this->_activeObject->SetRequestedValue( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
               this->_activeObject->SetCursorType( this->cursorId );
               this->_activeObject->SetPreCalcFlag( this->commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE ) );
            
               this->computeActorsAndGeodes = true;
               this->actorsAreReady = true;
            }
            break;
         }
      }
      //extracting from inside the for loop
      this->commandArray->SetCommandValue( cfdCommandArray::CFD_ID, -1 );
   }
   else if ( this->changeGeometry ) 
   {
      // Update Scene Graph with selected or deselected geometry
      vprDEBUG(vprDBG_ALL,0) << " Change Geometry in Scene Graph."
                             << std::endl << vprDEBUG_FLUSH;
      // Maybe fix later
      //vprDEBUG(vprDBG_ALL,1) << " numGeoms = " << this->paramReader->numGeoms 
      //                        << std::endl << vprDEBUG_FLUSH;
      
      // This data will come from cfdModelHandler
      // Fix later
      /*
      int temp = 0;
      for( int q = 0; q < this->paramReader->numGeoms; q++)
      {
         //vprDEBUG(vprDBG_ALL,1)
         //   << "guiVal[ " << q << " ] = " << paramReader->guiVal[ q ] 
         //   << std::endl
         //   << vprDEBUG_FLUSH;

         if ( paramReader->guiVal[ q ] == 1 && 
               this->_worldDCS->SearchChild( (cfdSceneNode*)this->geomL[q]->getpfDCS() ) == -1 )
         {
            temp = this->_worldDCS->AddChild( (cfdSceneNode*)this->geomL[q]->getpfDCS() );
         }
         else if ( paramReader->guiVal[ q ] == 0 && 
               this->_worldDCS->SearchChild( (cfdSceneNode*)this->geomL[q]->getpfDCS() ) != -1 )
         {
            temp = this->_worldDCS->RemoveChild( (cfdSceneNode*)this->geomL[q]->getpfDCS() );
         }
         vprDEBUG(vprDBG_ALL,1) << "|   Add Child Error  " << temp 
                              << std::endl << vprDEBUG_FLUSH;
      }
      */
      this->changeGeometry = false;
   }
   else if ( this->chgMod == true)
   {
  /*    // for the active model, change opaque geometries to transparent
      for ( int q = 0; q < this->paramReader->numGeoms; q++)
      {
         if ( this->geomL[q]->transparent == 1 )
         {
            vprDEBUG(vprDBG_ALL,2) << "Changing Transparency for geom : "
                                   << q << std::endl << vprDEBUG_FLUSH;
            this->geomL[q]->setOpac( 0.2 );
         }
      }
      this->chgMod = false;*/
   }  //change the model's property
}


void cfdSteadyStateVizHandler::CreateActorThread( void )
{
   // DO NOT put scene graph manipulation code in this function
   // This thread is purely for creation of geodes

   while ( this->runIntraParallelThread )
   {
      vpr::System::msleep( 500 );  // half-second delay

      // Basically waiting for work here
      // This is a guard 
      // Sample every half second
      if ( this->computeActorsAndGeodes )
      {      
         if ( this->_activeObject != NULL )
         {
            cfdContour * contourTest = 
               dynamic_cast<cfdContour *>( this->_activeObject );
            cfdVector * vectorTest = 
               dynamic_cast<cfdVector *>( this->_activeObject );
            cfdMomentum * momentumTest = 
               dynamic_cast<cfdMomentum *>( this->_activeObject );
            cfdStreamers * streamersTest = 
               dynamic_cast<cfdStreamers *>( this->_activeObject );
            cfdAnimatedStreamlineCone * animStreamerTest = 
               dynamic_cast<cfdAnimatedStreamlineCone *>( this->_activeObject );
            cfdAnimatedImage* animImgTest = 
               dynamic_cast<cfdAnimatedImage *>( this->_activeObject );

            vprDEBUG(vprDBG_ALL,0) << " Updating cfdObject..." 
               << std::endl << vprDEBUG_FLUSH;

            this->chgMod = true;

            // May replace later , fix a later date
            //vprDEBUG(vprDBG_ALL,2) << " Memory used before update ( bytes ) : "
            //  << pfMemory::getArenaBytesUsed() << std::endl << vprDEBUG_FLUSH;

            //tt = GetTimeClock();
            if (  contourTest == NULL && 
                  vectorTest == NULL &&
                  momentumTest == NULL &&   
                  streamersTest == NULL &&
                  animStreamerTest == NULL &&
                  animImgTest == NULL )
            {
               // For everything except for the interactive and transient stuff
               vprDEBUG(vprDBG_ALL,1)
                 << "non-interactive object." << std::endl << vprDEBUG_FLUSH; 

               this->_activeObject->Update(); 
               this->_activeObject->UpdatecfdGeode();     
               this->_activeObject = NULL;
               this->computeActorsAndGeodes = false;
            }
            else if ( streamersTest != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << "interactive object." 
                                       << std::endl << vprDEBUG_FLUSH;
               // if we are not already computing streamlines
               if ( this->inter_activeObject == false )
                  this->inter_activeObject = true;
            }
            else if ( animStreamerTest != NULL )
            {
               // if we are not already computing animatedStreamlines
               this->animStreamer->SetPolyDataSource( this->streamlines->GetStreamersOutput() );
               this->animStreamer->Update();
               this->_activeObject = NULL;
               this->computeActorsAndGeodes = false;   
            }
            else if ( animImgTest != NULL )
            {
               // if we are not already computing animatedImages
               this->animImg->Update();
               this->_activeObject = NULL;
               this->computeActorsAndGeodes = false;   
            }

            // May fix later, not a crucial part
            //vprDEBUG(vprDBG_ALL,1) << " Time: " << GetTimeClock()-tt
            //                       << std::endl << vprDEBUG_FLUSH;
            //vprDEBUG(vprDBG_ALL,2) <<" Memory used after update ( bytes ) : "
            //                       << pfMemory::getArenaBytesUsed() 
            //                       << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vprDBG_ALL,0) << " Done updating cfdObject" 
               << std::endl << std::endl << vprDEBUG_FLUSH; 
            
         }
      }
   } // End of While loop
}

void cfdSteadyStateVizHandler::streamers( void * )
{
   vprDEBUG(vprDBG_ALL,1) << "In streamers" << std::endl << vprDEBUG_FLUSH;
   while ( this->runStreamersThread )
   { 
      // Wait for some  work
      while (   !this->inter_activeObject )
      {
         vpr::System::msleep( 500 );  // half-second delay
      }

      if ( this->inter_activeObject )
      {
         this->_activeObject->SetCursorType( this->cursorId );
         this->_activeObject->SetNormal( this->nav->GetDirection() );
         this->_activeObject->SetOrigin( this->nav->GetObjLocation() );

         if ( this->cursorId == CUBE )
         {
            this->_activeObject->SetBoxSize( this->cur_box );
         }

         if ( ! this->useLastSource )
         {
            vprDEBUG(vprDBG_ALL,1) <<"creating fresh streamlines"
                                   << std::endl << vprDEBUG_FLUSH;
            if ( this->lastSource )
            {
               this->lastSource->Delete();
            }

            this->lastSource = vtkPolyData::New();

            this->lastSource->DeepCopy( 
               (vtkPolyData*)this->cursor->GetSourcePoints( this->cursorId ) );

            this->_activeObject->SetSourcePoints( 
                                        (vtkPolyDataSource*)this->lastSource );
         }
         else //if ( _activeMeshedVolume->IsNewlyActivated() )// && this->useLastSource
         {
            vprDEBUG(vprDBG_ALL,1) << "using transformed last source"
                                   << std::endl << vprDEBUG_FLUSH;

            this->_activeObject->SetSourcePoints( 
                                        (vtkPolyDataSource*)this->lastSource );
         }

         this->_activeObject->Update();
         this->_activeObject->UpdatecfdGeode();
         
         this->inter_activeObject = false;
         this->_activeObject = NULL;
      }
      this->computeActorsAndGeodes = false;   
   }
}

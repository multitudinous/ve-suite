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
 * File:          $RCSfile: cfdSteadyStateVizHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
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
#include "cfdGroup.h"
/*#include "cfdSequence.h"
#include "cfdTempAnimation.h"
*/
#include "cfdNavigate.h"
#include "cfdCursor.h"
#include "cfdGraphicsObject.h"
#include "cfdModel.h"
#include "cfdTextOutput.h"
#include "cfdEnvironmentHandler.h"
#include "cfdModelHandler.h"
#include "cfdPfSceneManagement.h"

#include <vpr/Util/Debug.h>
#include <vpr/vpr.h>
#include <vpr/System.h>

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>

cfdSteadyStateVizHandler::cfdSteadyStateVizHandler( void )
{
   this->surface = 0;
   this->isosurface = 0;
   this->contour = 0;
   this->momentum = 0;
   this->vector = 0;
   this->x_contour = 0;
   this->y_contour = 0;
   this->z_contour = 0;
   this->x_momentum = 0;
   this->y_momentum = 0;
   this->z_momentum = 0;
   this->x_vector = 0;
   this->y_vector = 0;
   this->z_vector = 0;
   this->x_contours = 0;
   this->y_contours = 0;
   this->z_contours = 0;
   this->x_momentums = 0;
   this->y_momentums = 0;
   this->z_momentums = 0;
   this->x_vectors = 0;
   this->y_vectors = 0;
   this->z_vectors = 0;
   this->streamlines = 0;
   this->particles = 0;
   this->image = 0;
   this->animStreamer = 0;
   this->animImg = 0;
   this->textOutput = 0;

   this->commandArray = 0;
   this->_activeDataSetDCS = 0;
   this->_activeObject = 0;
   this->lastSource = 0;
   this->_activeTempAnimation = 0;

   this->computeActorsAndGeodes = false;
   this->actorsAreReady = false;
   this->useLastSource = false;
   //this->transientBusy = 0;
   this->transientActors = true;
   _param = 0;
}

void cfdSteadyStateVizHandler::Initialize( char* param )
{
   _param = param;
   nav = cfdEnvironmentHandler::instance()->GetNavigate();
   cursor = cfdEnvironmentHandler::instance()->GetCursor();
}

cfdSteadyStateVizHandler::~cfdSteadyStateVizHandler( void )
{
   this->runIntraParallelThread = false;
   vpr::System::msleep( 1000 );  // half-second delay
   delete this->vjTh[0];

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

   if ( this->surface ) 
   {
      vprDEBUG(vprDBG_ALL,2) 
        << "deleting this->surface" << std::endl << vprDEBUG_FLUSH;
      delete this->surface;
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

   if ( this->textOutput ) 
   {
      vprDEBUG(vprDBG_ALL,2)  
        << "deleting this->textOutput" << std::endl << vprDEBUG_FLUSH;
      delete this->textOutput;
   }
}
bool cfdSteadyStateVizHandler::TransientGeodesIsBusy()
{
   //return this->transientBusy;
   return this->computeActorsAndGeodes;
}
////////////////////
// Helper functions
////////////////////
void cfdSteadyStateVizHandler::SetCommandArray( cfdCommandArray* input )
{
   if ( input == NULL )
   {
      std::cerr << "cfdSteadyStateVizHandler::SetCommandArray input is NULL" << std::endl;
      exit( 1 );
   }
   commandArray = input;
}

cfdTempAnimation* cfdSteadyStateVizHandler::GetActiveAnimation( void )
{
   return this->_activeTempAnimation;
}
////////////////////

void cfdSteadyStateVizHandler::InitScene( void )
{
   if ( cfdModelHandler::instance()->GetActiveDataSet() != NULL )
   {
      {
         int postData = 0;
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices() != NULL &&
               cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
            postData += 1;

         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices() != NULL &&
               cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
            postData += 2;

         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices() != NULL &&
               cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
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
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() &&
           cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetScalars() )
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
     
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetScalars() )
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
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x momentum.
         //
         std::cout << "| 22. Initializing............................... Preset x Momentum |" << std::endl;
         // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
         this->x_momentum = new cfdPresetMomentum( 0, 10 );
         this->x_momentum->SetObjectType( X_MOMENTUM );
         this->dataList.push_back( this->x_momentum );
         this->commandList.push_back( this->x_momentum );
        
         //
         // Initiate the preset y momentum.
         //
         std::cout << "| 23. Initializing............................... Preset y Momentum |" << std::endl;
         // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
         this->y_momentum = new cfdPresetMomentum( 1, 10 );
         this->y_momentum->SetObjectType( Y_MOMENTUM );
         this->dataList.push_back( this->y_momentum );
         this->commandList.push_back( this->y_momentum );
        
         //
         // Initiate the preset z momentum.
         //
         std::cout << "| 24. Initializing............................... Preset z Momentum |" << std::endl;
         // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
         this->z_momentum = new cfdPresetMomentum( 2, 10 );
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
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices() != NULL &&
           cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
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
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices() != NULL &&
           cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
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
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices() != NULL &&
           cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
      {
         std::cout << "| 30. Initializing....................Multiple Z-planes of Contours |" << std::endl;
         this->z_contours = new cfdContours( 2 );
         this->z_contours->SetObjectType( Z_CONTOURS );
         this->dataList.push_back( this->z_contours );
         this->commandList.push_back( this->z_contours );
      }

      // Make sure that this dataset has a vector and scalar...
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() &&
           cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetScalars() )

      {
         //
         // Initiate the preset x momentums.
         //
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices() != NULL &&
              cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 31. Initializing.......Multiple X-planes of Precomputed Momentums |" << std::endl;
            // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
            this->x_momentums = new cfdMomentums( 0 );
            this->x_momentums->SetObjectType( X_MOMENTUMS );
            this->dataList.push_back( this->x_momentums );
            this->commandList.push_back( this->x_momentums );
         }

         //
         // Initiate the preset y momentums.
         //
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices() != NULL &&
              cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 32. Initializing.......Multiple Y-planes of Precomputed Momentums |" << std::endl;
            // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
            this->y_momentums = new cfdMomentums( 1 );
            this->y_momentums->SetObjectType( Y_MOMENTUMS );
            this->dataList.push_back( this->y_momentums );
            this->commandList.push_back( this->y_momentums );
         }

         //
         // Initiate the preset z momentums.
         //
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices() != NULL &&
              cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 33. Initializing.......Multiple Z-planes of Precomputed Momentums |" << std::endl;
            // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
            this->z_momentums = new cfdMomentums( 2 );
            this->z_momentums->SetObjectType( Z_MOMENTUMS );
            this->dataList.push_back( this->z_momentums );
            this->commandList.push_back( this->z_momentums );
         }
      }

      // Make sure that this dataset has a vector and scalar...
      if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x vectors.
         //
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices() != NULL &&
              cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 34. Initializing.........Multiple X-planes of Precomputed Vectors |" << std::endl;
            this->x_vectors = new cfdVectors( 0 );
            this->x_vectors->SetObjectType( X_VECTORS );
            this->dataList.push_back( this->x_vectors );
         }

         //
         // Initiate the preset y vectors.
         //
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices() != NULL &&
              cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetPlanesData() != NULL )
         {
            std::cout << "| 35. Initializing.........Multiple Y-planes of Precomputed Vectors |" << std::endl;
            this->y_vectors = new cfdVectors( 1 );
            this->y_vectors->SetObjectType( Y_VECTORS );
            this->dataList.push_back( this->y_vectors );
         }
         
         //
         // Initiate the preset z vectors.
         //
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices() != NULL &&
              cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetPlanesData() != NULL )
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
         this->streamlines = new cfdStreamers();
         this->streamlines->SetObjectType( STREAMLINES );
         this->dataList.push_back( this->streamlines );     
         this->commandList.push_back( this->streamlines );

         //
         // Initiate the animated streamers.
         //
         std::cout << "| 39. Initializing............................. Animated Streamline |" << std::endl;
         this->animStreamer = new cfdAnimatedStreamlineCone();
         this->animStreamer->SetObjectType( ANIMATED_STREAMLINES );
         this->dataList.push_back( this->animStreamer );     
         this->commandList.push_back( this->animStreamer );

         //
         // Initiate the animated Images.
         //
         std::cout << "| 39.b Initializing............................. Animated Images |" << std::endl;
         this->animImg = new cfdAnimatedImage( _param );
         this->animImg->SetObjectType( ANIMATED_IMAGES );
         this->dataList.push_back( this->animImg);  
      }
   }

   //
   // Initiate the PolyData File
   //
   std::cout << "| 41. Initializing................................... PolyData File |" << std::endl;
   this->particles = new cfdPolyData();
   this->particles->SetObjectType( PARTICLES );
   this->dataList.push_back( this->particles );     

   std::cout << "|  5. Initializing................................. Dataset surface |" << std::endl;
   this->surface = new cfdPolyData( 1.0 );
   this->surface->SetObjectType( POLYDATA );
   this->dataList.push_back( this->surface );

   //
   // Initiate PIV data from INEL
   //
   std::cout << "| 42. Initializing.................................... Bitmap Image |" << std::endl;
   this->image = new cfdImage( _param );
   this->image->SetObjectType( IMAGE_EX );
   this->dataList.push_back( this->image );     
   //
   // Initiate the Performer objects.
   //
   std::cout << "| 51. Initializing........................................ pfGeodes |" << std::endl;

   for ( int i = 0; i < (int)this->dataList.size(); i++ )
   {
      // Initialize all the geode creation flags and dcs flags for all the geodes
      this->dataList.at( i )->SetUpdateFlag( false );
      this->dataList.at( i )->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
   }

   // This set of thread stuff needs to be in ssvizhandler and transvizhandler
   std::cout << "|  9. Initializing......................................... Threads |" << std::endl;
   this->runIntraParallelThread = true;
   this->vjTh[0] = new vpr::Thread( new vpr::ThreadMemberFunctor< cfdSteadyStateVizHandler > ( this, &cfdSteadyStateVizHandler::CreateActorThread ) );

   //std::cout << "|  9. Initializing..................................... Text Output |" << std::endl;
   //this->textOutput = new cfdTextOutput();
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
         << ", compute actors and geode = " << computeActorsAndGeodes 
         << std::endl << vprDEBUG_FLUSH;
   }

   for ( unsigned int i = 0; i < this->commandList.size(); i++ )
   {
      // Check to see if any of the objectss need updated before we 
      // create actors
      bool commandApplies = this->commandList[ i ]->CheckCommandId( this->commandArray );
      vprDEBUG(vprDBG_ALL,4) << "|\tCommand Applies : " << commandApplies
                                   << std::endl << vprDEBUG_FLUSH;
   }

   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
            == TRANSIENT_ACTIVE )
   {
      this->transientActors = commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   }
   // check any virtual objects need to be updated
   if ( this->actorsAreReady && this->transientActors )
   {
      vprDEBUG(vprDBG_ALL,3) << "|\tUpdating Objects"
                                   << std::endl << vprDEBUG_FLUSH;
      bool alreadyRemoved = false;
      for ( unsigned int i = 0; i < this->dataList.size(); i++ )
      {
         if ( this->dataList.at( i )->GetUpdateFlag() )//|| 
               //this->dataList.at( i )->GetTransientGeodeFlag() )
         {
            vprDEBUG(vprDBG_ALL,2) << "|\tCreating Objects"
                                   << std::endl << vprDEBUG_FLUSH;
            // if object needs updated then already have a graphics object
            cfdGraphicsObject* temp = new cfdGraphicsObject();
            temp->SetTypeOfViz( cfdGraphicsObject::CLASSIC );
            //temp->SetParentNode( this->dataList[ i ]->GetActiveDataSet()->GetDCS() );
            temp->SetActiveModel( cfdModelHandler::instance()->GetActiveModel() );
            temp->SetWorldNode( cfdPfSceneManagement::instance()->GetWorldDCS() );
            temp->SetGeodes( this->dataList[ i ]->GetGeodes() );
            temp->AddGraphicsObjectToSceneGraph();
            
            // search map for other object types with the same type as this one
            std::multimap< int, cfdGraphicsObject* >::iterator pos;
            for ( pos = graphicsObjects.lower_bound( this->dataList[ i ]->GetObjectType() ); 
                  pos != graphicsObjects.upper_bound( this->dataList[ i ]->GetObjectType() ); )
            {
               // and see if they have the same parent node
               // the parent node is unique becaue each dataset has a dcs
               if ( pos->second->GetParentNode() == temp->GetParentNode() )
               {
                  pos->second->RemovecfdGeodeFromDCS();
                  delete pos->second;
                  graphicsObjects.erase( pos++ );
               }
               else
               {
                  ++pos;
               }
            }
            graphicsObjects.insert( std::make_pair( this->dataList[ i ]->GetObjectType(), temp ) );

            // Resetting these variables is very important
            this->dataList[ i ]->SetUpdateFlag( false );
            this->actorsAreReady = false;
            this->dataList[ i ]->ClearGeodes();
            vprDEBUG(vprDBG_ALL,2) << "|\tDone Creating Objects"
                                   << std::endl << vprDEBUG_FLUSH;
         }
         
         // if we have selected a viz feature and it is complete the remove the text
         if ( !computeActorsAndGeodes && !alreadyRemoved )
         {
            alreadyRemoved = true;
            //cfdPfSceneManagement::instance()->GetRootNode()->RemoveChild( textOutput->add_text( "executing..." ) );
         }
      }
   }

   if ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == USE_LAST_STREAMLINE_SEEDPOINTS ){
      this->useLastSource = this->commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   }
   else if (   ( 0 <= this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) ) &&
               ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) < 100 )  && 
               ( this->computeActorsAndGeodes == false ) )
   {
      vprDEBUG(vprDBG_ALL,1) << " selected ID number = " << this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID )
                             << std::endl << vprDEBUG_FLUSH;
      for ( int i = 0; i < (int)this->dataList.size(); i ++ )
      {          
         if ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == dataList[ i ]->GetObjectType() )
         {
            vprDEBUG(vprDBG_ALL,1) << " setting viz object " << i 
                                   << " to _activeObject"
                                   << std::endl << vprDEBUG_FLUSH;

            //cfdPfSceneManagement::instance()->GetRootNode()->AddChild( textOutput->add_text( "executing..." ) );
            this->_activeObject = this->dataList[ i ];
            
            if ( this->_activeObject->GetObjectType() == IMAGE_EX )
            {
               this->_activeDataSetDCS = cfdPfSceneManagement::instance()->GetWorldDCS();
            }
            else
            {
               this->_activeDataSetDCS = cfdModelHandler::instance()->GetActiveDataSet()->GetDCS();
            }

            //if ( this->computeActorsAndGeodes == false )
            {
               // add active dataset DCS to scene graph if not already there...
               vprDEBUG(vprDBG_ALL,1) << " setting DCS to activeDCS = "
                                   << this->_activeDataSetDCS
                                   << std::endl << vprDEBUG_FLUSH;
               this->_activeObject->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
               this->_activeObject->SetNormal( this->nav->GetDirection() );
               this->_activeObject->SetOrigin( this->nav->GetObjLocation() );

               this->_activeObject->SetRequestedValue( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
               this->_activeObject->SetCursorType( this->cursor->GetCursorID() );
               this->_activeObject->SetPreCalcFlag( this->commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE ) );
               this->computeActorsAndGeodes = true;
               this->actorsAreReady = true;
            }
            break;
         }
      }
      //extracting from inside the for loop
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL )
   { 
      vprDEBUG(vprDBG_ALL,2) << "|\tClear All Graphics Objects From Scene Graph"
                             << std::endl << vprDEBUG_FLUSH;
      std::multimap< int, cfdGraphicsObject* >::iterator pos;
      for ( pos = graphicsObjects.begin(); pos != graphicsObjects.end(); )
      {
         pos->second->RemovecfdGeodeFromDCS();
         delete pos->second;
         graphicsObjects.erase( pos++ );
      }
   }
}

void cfdSteadyStateVizHandler::CreateActorThread( void * )
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
               //this->_activeObject->SetSequence( 0 );
            }
            else if ( streamersTest != NULL )
            {
               vprDEBUG(vprDBG_ALL,1) << "interactive object." 
                                       << std::endl << vprDEBUG_FLUSH;
               // if we are not already computing streamlines
               this->streamers();  
            }
            else if ( animStreamerTest != NULL )
            {
               // if we are not already computing animatedStreamlines
               this->animStreamer->SetPolyDataSource( this->streamlines->GetStreamersOutput() );
               this->animStreamer->Update();
            }
            else if ( animImgTest != NULL )
            {
               // if we are not already computing animatedImages
               this->animImg->Update();
            }

            // May fix later, not a crucial part
            //vprDEBUG(vprDBG_ALL,1) << " Time: " << GetTimeClock()-tt
            //                       << std::endl << vprDEBUG_FLUSH;
            //vprDEBUG(vprDBG_ALL,2) <<" Memory used after update ( bytes ) : "
            //                       << pfMemory::getArenaBytesUsed() 
            //                       << std::endl << vprDEBUG_FLUSH;

            this->_activeObject = NULL;
            this->computeActorsAndGeodes = false;   
            vprDEBUG(vprDBG_ALL,0) << "|\tDone updating cfdObject" 
               << std::endl << std::endl << vprDEBUG_FLUSH; 
            
         }
      }
   } // End of While loop
}

void cfdSteadyStateVizHandler::streamers( void )
{
   vprDEBUG(vprDBG_ALL,1) << "In streamers" << std::endl << vprDEBUG_FLUSH;
   
   this->_activeObject->SetCursorType( this->cursor->GetCursorID() );
   this->_activeObject->SetNormal( this->nav->GetDirection() );
   this->_activeObject->SetOrigin( this->nav->GetObjLocation() );

   if ( this->cursor->GetCursorID() == CUBE )
   {
      this->_activeObject->SetBoxSize( this->cur_box );
   }

   if ( ! this->useLastSource )
   {
      vprDEBUG(vprDBG_ALL,1) <<"creating fresh streamlines"
                             << std::endl << vprDEBUG_FLUSH;
      if ( this->lastSource != NULL )
      {
         this->lastSource->Delete();
      }

      this->lastSource = vtkPolyData::New();
      this->lastSource->DeepCopy( 
         (vtkPolyData*)this->cursor->GetSourcePoints() );

      this->_activeObject->SetSourcePoints( 
                                  (vtkPolyDataSource*)this->lastSource );
   }
   else 
   {
      vprDEBUG(vprDBG_ALL,1) << "using transformed last source"
                             << std::endl << vprDEBUG_FLUSH;

      this->_activeObject->SetSourcePoints( 
                                  (vtkPolyDataSource*)this->lastSource );
   }

   this->_activeObject->Update();
   this->_activeObject = NULL;
}

/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: CreateVisObjectEventHandler.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/CreateVisObjectEventHandler.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_Xplorer/cfdFILE.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/fileIO.h"

#include "VE_Xplorer/cfdPolyData.h"      
#include "VE_Xplorer/cfdIsosurface.h"    
#include "VE_Xplorer/cfdPresetContour.h" 
#include "VE_Xplorer/cfdContours.h"      
#include "VE_Xplorer/cfdMomentum.h"      
#include "VE_Xplorer/cfdPresetMomentum.h"
#include "VE_Xplorer/cfdMomentums.h"     
#include "VE_Xplorer/cfdVector.h"        
#include "VE_Xplorer/cfdPresetVector.h"  
#include "VE_Xplorer/cfdVectors.h"       
#include "VE_Xplorer/cfdStreamers.h"     
#include "VE_Xplorer/cfdPolyData.h"      
#include "VE_Xplorer/cfdImage.h"         
#include "VE_Xplorer/cfdAnimatedImage.h" 
#include "VE_Xplorer/cfdAnimatedStreamlineCone.h"
#include "VE_Xplorer/cfdContour.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdEnvironmentHandler.h"
#include "VE_Xplorer/cfdSteadyStateVizHandler.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdCursor.h"
#include "VE_Xplorer/cfdTextOutput.h"

#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdClone.h"
#include "VE_SceneGraph/cfdSwitch.h"
#include "VE_SceneGraph/cfdPfSceneManagement.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Xplorer/cfdDebug.h"

#include "VE_TextureBased/cfdTextureDataSet.h"

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <iostream>

using namespace VE_EVENTS;
using namespace VE_Xplorer;
using namespace VE_TextureBased;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::CreateVisObjectEventHandler()
:VE_EVENTS::EventHandler()
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
   //_activeModel = 0;
   
   // Initialize all the vis objects from ssvishandler
   //
   // Initiate the isosurface.
   //
   std::cout << "| 14. Initializing...................................... Isosurface |" << std::endl;
   this->isosurface = new cfdIsosurface( 10 );
   this->isosurface->SetObjectType( ISOSURFACE );
   visObjectMap[ ISOSURFACE ] = this->isosurface;

   //
   // Initiate the interactive contour.
   //
   std::cout << "| 15. Initializing......................................... Contour |" << std::endl;
   this->contour = new cfdContour();
   this->contour->SetObjectType( CONTOUR );
   visObjectMap[ CONTOUR ] = this->contour;

   //
   // Initiate the interactive momentum.
   //
   std::cout << "| 16. Initializing........................................ Momemtum |" << std::endl;
   this->momentum = new cfdMomentum();
   this->momentum->SetObjectType( MOMENTUM );
   visObjectMap[ MOMENTUM ] = this->momentum;

   //
   // Initiate the interactive vector.
   //
   std::cout << "| 17. Initializing.......................................... Vector |" << std::endl;
   this->vector = new cfdVector();
   this->vector->SetObjectType( VECTOR );
   visObjectMap[ VECTOR ] = this->vector;

   //
   // Initiate the preset x contour.
   //
   std::cout << "| 19. Initializing................................ Preset x Contour |" << std::endl;
   this->x_contour = new cfdPresetContour( 0, 10 );
   this->x_contour->SetObjectType( X_CONTOUR );
   visObjectMap[ X_CONTOUR ] = this->x_contour;

   //
   // Initiate the preset y contour.
   //
   std::cout << "| 20. Initializing................................ Preset y Contour |" << std::endl;
   this->y_contour = new cfdPresetContour( 1, 10 );
   this->y_contour->SetObjectType( Y_CONTOUR );
   visObjectMap[ Y_CONTOUR ] = this->y_contour;

   //
   // Initiate the preset z contour.
   //
   std::cout << "| 21. Initializing................................ Preset z Contour |" << std::endl;
   this->z_contour = new cfdPresetContour( 2, 10 );
   this->z_contour->SetObjectType( Z_CONTOUR );
   visObjectMap[ Z_CONTOUR ] = this->z_contour;

   //
   // Initiate the preset x momentum.
   //
   std::cout << "| 22. Initializing............................... Preset x Momentum |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->x_momentum = new cfdPresetMomentum( 0, 10 );
   this->x_momentum->SetObjectType( X_MOMENTUM );
   visObjectMap[ X_MOMENTUM ] = this->x_momentum;

   //
   // Initiate the preset y momentum.
   //
   std::cout << "| 23. Initializing............................... Preset y Momentum |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->y_momentum = new cfdPresetMomentum( 1, 10 );
   this->y_momentum->SetObjectType( Y_MOMENTUM );
   visObjectMap[ Y_MOMENTUM ] = this->y_momentum;

   //
   // Initiate the preset z momentum.
   //
   std::cout << "| 24. Initializing............................... Preset z Momentum |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->z_momentum = new cfdPresetMomentum( 2, 10 );
   this->z_momentum->SetObjectType( Z_MOMENTUM );
   visObjectMap[ Z_MOMENTUM ] = this->z_momentum;

   //
   // Initiate the preset x vector.
   //
   std::cout << "| 25. Initializing................................. Preset x Vector |" << std::endl;
   this->x_vector = new cfdPresetVector( 0, 10 );
   this->x_vector->SetObjectType( X_VECTOR );
   visObjectMap[ X_VECTOR ] = this->x_vector;

   //
   // Initiate the preset y vector.
   //
   std::cout << "| 26. Initializing................................. Preset y Vector |" << std::endl;
   this->y_vector = new cfdPresetVector( 1, 10 );
   this->y_vector->SetObjectType( Y_VECTOR );
   visObjectMap[ Y_VECTOR ] = this->y_vector;

   //
   // Initiate the preset z vector.
   //
   std::cout << "| 27. Initializing................................. Preset z Vector |" << std::endl;
   this->z_vector = new cfdPresetVector( 2, 10 );
   this->z_vector->SetObjectType( Z_VECTOR );
   visObjectMap[ Z_VECTOR ] = this->z_vector;

   //
   // Initiate the preset x contour lines.
   //
   std::cout << "| 28. Initializing....................Multiple X-planes of Contours |" << std::endl;
   this->x_contours = new cfdContours( 0 );
   this->x_contours->SetObjectType( X_CONTOURS );
   visObjectMap[ X_CONTOURS ] = this->x_contours;

   //
   // Initiate the preset y contour lines.
   //
   std::cout << "| 29. Initializing....................Multiple Y-planes of Contours |" << std::endl;
   this->y_contours = new cfdContours( 1 );
   this->y_contours->SetObjectType( Y_CONTOURS );
   visObjectMap[ Y_CONTOURS ] = this->y_contours;

   //
   // Initiate the preset z contour lines.
   //
   std::cout << "| 30. Initializing....................Multiple Z-planes of Contours |" << std::endl;
   this->z_contours = new cfdContours( 2 );
   this->z_contours->SetObjectType( Z_CONTOURS );
   visObjectMap[ Z_CONTOURS ] = this->z_contours;

   //
   // Initiate the preset x momentums.
   //
   std::cout << "| 31. Initializing.......Multiple X-planes of Precomputed Momentums |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->x_momentums = new cfdMomentums( 0 );
   this->x_momentums->SetObjectType( X_MOMENTUMS );
   visObjectMap[ X_MOMENTUMS ] = this->x_momentums;

   //
   // Initiate the preset y momentums.
   //
   std::cout << "| 32. Initializing.......Multiple Y-planes of Precomputed Momentums |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->y_momentums = new cfdMomentums( 1 );
   this->y_momentums->SetObjectType( Y_MOMENTUMS );
   visObjectMap[ Y_MOMENTUMS ] = this->y_momentums;

   //
   // Initiate the preset z momentums.
   //
   std::cout << "| 33. Initializing.......Multiple Z-planes of Precomputed Momentums |" << std::endl;
   // Needs to be fixed, the isoscale should be set by the gui, 2nd parameter in constructor
   this->z_momentums = new cfdMomentums( 2 );
   this->z_momentums->SetObjectType( Z_MOMENTUMS );
   visObjectMap[ Z_MOMENTUMS ] = this->z_momentums;

   //
   // Initiate the preset x vectors.
   //
   std::cout << "| 34. Initializing.........Multiple X-planes of Precomputed Vectors |" << std::endl;
   this->x_vectors = new cfdVectors( 0 );
   this->x_vectors->SetObjectType( X_VECTORS );
   visObjectMap[ X_VECTORS ] = this->x_vectors;

   //
   // Initiate the preset y vectors.
   //
   std::cout << "| 35. Initializing.........Multiple Y-planes of Precomputed Vectors |" << std::endl;
   this->y_vectors = new cfdVectors( 1 );
   this->y_vectors->SetObjectType( Y_VECTORS );
   visObjectMap[ Y_VECTORS ] = this->y_vectors;

   //
   // Initiate the preset z vectors.
   //
   std::cout << "| 36. Initializing.........Multiple Z-planes of Precomputed Vectors |" << std::endl;
   this->z_vectors = new cfdVectors( 2 );
   this->z_vectors->SetObjectType( Z_VECTORS );
   visObjectMap[ Z_VECTORS ] = this->z_vectors;

   //
   // Initiate the streamlines.
   //
   std::cout << "| 37. Initializing..................................... Streamlines |" << std::endl;
   this->streamlines = new cfdStreamers();
   this->streamlines->SetObjectType( STREAMLINES );
   visObjectMap[ STREAMLINES ] = this->streamlines;

   //
   // Initiate the animated streamers.
   //
   std::cout << "| 39. Initializing............................. Animated Streamline |" << std::endl;
   this->animStreamer = new cfdAnimatedStreamlineCone();
   this->animStreamer->SetObjectType( ANIMATED_STREAMLINES );
   visObjectMap[ ANIMATED_STREAMLINES ] = this->animStreamer;

   //
   // Initiate the animated Images.
   //
   std::cout << "| 39.b Initializing............................. Animated Images |" << std::endl;
   //this->animImg = new cfdAnimatedImage( _param.c_str() );
   //this->animImg->SetObjectType( ANIMATED_IMAGES );
   //this->dataList.push_back( this->animImg);  

   //
   // Initiate the PolyData File
   //
   std::cout << "| 41. Initializing................................... PolyData File |" << std::endl;
   this->particles = new cfdPolyData();
   this->particles->SetObjectType( PARTICLES );
   visObjectMap[ PARTICLES ] = this->particles;

   std::cout << "|  5. Initializing................................. Dataset surface |" << std::endl;
   this->surface = new cfdPolyData( 1.0 );
   this->surface->SetObjectType( POLYDATA );
   visObjectMap[ POLYDATA ] = this->surface;

   //
   // Initiate PIV data from INEL
   //
   std::cout << "| 42. Initializing.................................... Bitmap Image |" << std::endl;
   /*this->image = new cfdImage( _param );
   this->image->SetObjectType( IMAGE_EX );
   this->dataList.push_back( this->image ); */    
   //
   // Initiate the Performer objects.
   //
   std::cout << "| 51. Initializing........................................ pfGeodes |" << std::endl;
   
   //for ( int i = 0; i < (int)this->dataList.size(); i++ )
   /*std::map< int, cfdObjects* >::iterator iter;
   for ( iter = visObjectMap.begin(); iter != viObjectMap.end(); ++iter )
   {
      // Initialize all the geode creation flags and dcs flags for all the geodes
      iter->SetUpdateFlag( false );
      iter->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
   }*/
}
////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::CreateVisObjectEventHandler(const CreateVisObjectEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   
}
////////////////////////////////////////////////////////////////////////////////
///Destructor                                      //
////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::~CreateVisObjectEventHandler()
{
   //delete all the objects from ssvishandler
   if ( this->isosurface ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->isosurface" << std::endl << vprDEBUG_FLUSH;
      delete this->isosurface;
   }
   
   if ( this->contour ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->contour" << std::endl << vprDEBUG_FLUSH;
      delete this->contour;
   }
   
   if ( this->momentum ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->momentum;
   }
   
   if ( this->vector ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->vector" << std::endl << vprDEBUG_FLUSH;
      delete this->vector;
   }
   
   if ( this->x_contour ) 
   {
      vprDEBUG(vesDBG,2)   
      << "deleting this->x_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contour;
   }
   
   if ( this->y_contour ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->y_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contour;
   }
   
   if ( this->z_contour ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->z_contour" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contour;
   }
   
   if ( this->x_momentum ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->x_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentum;
   }
   
   if ( this->y_momentum ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->y_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentum;
   }
   
   if ( this->z_momentum ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->z_momentum" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentum;
   }
   
   if ( this->x_vector ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->x_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vector;
   }
   
   if ( this->y_vector ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->y_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vector;
   }
   
   if ( this->z_vector ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->z_vector" << std::endl << vprDEBUG_FLUSH;
      delete this->z_vector;
   }
   
   if ( this->x_contours ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->x_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->x_contours;
   }
   
   if ( this->y_contours ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->y_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->y_contours;
   }
   
   if ( this->z_contours ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->z_contours" << std::endl << vprDEBUG_FLUSH;
      delete this->z_contours;
   }
   
   if ( this->x_momentums ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->x_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->x_momentums;
   }
   
   if ( this->y_momentums ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->y_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->y_momentums;
   }
   
   if ( this->z_momentums ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->z_momentums" << std::endl << vprDEBUG_FLUSH;
      delete this->z_momentums;
   }
   
   if ( this->x_vectors ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->x_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->x_vectors;
   }
   
   if ( this->y_vectors ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->y_vectors" << std::endl << vprDEBUG_FLUSH;
      delete this->y_vectors;
   }
   
   if ( this->z_vectors ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->z_vectors" << std::endl << vprDEBUG_FLUSH; 
      delete this->z_vectors;
   }
   
   if ( this->streamlines ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->streamlines" << std::endl << vprDEBUG_FLUSH;
      delete this->streamlines;
      
      // Delete the polydata array used for seed points
      // if ( this->lastSource != NULL )
      //{
      //   this->lastSource->Delete();
      //}
   }
   
   if ( this->particles ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->particles" << std::endl << vprDEBUG_FLUSH;
      delete this->particles;
   }
   
   if ( this->surface ) 
   {
      vprDEBUG(vesDBG,2) 
      << "deleting this->surface" << std::endl << vprDEBUG_FLUSH;
      delete this->surface;
   }
   
   if ( this->image ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->image" << std::endl << vprDEBUG_FLUSH;
      delete this->image;
   }
   
   if ( this->animImg ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->animImg" << std::endl << vprDEBUG_FLUSH;
      delete this->animImg;
   }
   
   if ( this->animStreamer ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->animStreamer" << std::endl << vprDEBUG_FLUSH;
      delete this->animStreamer;
   }
   
   if ( this->textOutput ) 
   {
      vprDEBUG(vesDBG,2)  
      << "deleting this->textOutput" << std::endl << vprDEBUG_FLUSH;
      delete this->textOutput;
   }
}
////////////////////////////////////////////////////////////////////////////////
///Equal operator
////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler& CreateVisObjectEventHandler::operator=(const CreateVisObjectEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CreateVisObjectEventHandler::operator=(rhs);
   }
   return *this;
}
////////////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
/*   try
   {
      if ( model )
      {
         _activeModel = dynamic_cast< VE_Xplorer::cfdModel* >( model );
      }
      else
      {
         _activeModel = VE_Xplorer::cfdModelHandler::instance()->GetActiveModel();
      }
   }
   catch(...)
   {
      _activeModel = 0;
      std::cout<<"Invalid object passed to AddVTKDataSetEventHandler::SetGlobalBaseObject!"<<std::endl;
   }*/
}
//////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::Execute( VE_XML::XMLObject* xmlObject )
{
   // Set the active dataset
   this->SetActiveDataSet( xmlObject );
   // set the active scalar and range
   this->SetActiveScalarAndRange( xmlObject );
   // set the active vector
   this->SetActiveVector( xmlObject );
   // set the xml command to the cfdObject
   std::map< int, cfdObjects* >::iterator iter;
   for ( iter = visObjectMap.begin(); iter != visObjectMap.end(); ++iter )
   {
      // Check to see if any of the objectss need updated before we 
      // create actors
      if ( cfdModelHandler::instance()->GetActiveModel() )
      {
         iter->second->SetActiveDataSet( cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet() );
         iter->second->SetVECommand( cfdModelHandler::instance()->GetActiveModel()->GetVECommand() );
         //pass in the command for a specific object
         //bool commandApplies = this->commandList[ i ]->CheckCommandId( this->commandArray );
         //vprDEBUG(vesDBG,4) << "|\tCommand Applies : " << commandApplies
         //   << std::endl << vprDEBUG_FLUSH;
      }
   }
   
   // get the active vis object
   /*if (   ( 0 <= this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) ) &&
               ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) < 100 )  && 
               ( !this->computeActorsAndGeodes ) &&
               ( !this->texturesActive ) )*/
   {
      //vprDEBUG(vesDBG,1) << " selected ID number = " << this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID )
      //<< std::endl << vprDEBUG_FLUSH;
      int activeObjectIndex = 0; 
      for ( iter = visObjectMap.begin(); iter != visObjectMap.end(); ++iter )
      {
         if ( activeObjectIndex == iter->second->GetObjectType() )
         {
            vprDEBUG(vesDBG,1) << " setting viz object " << iter->second->GetObjectType()
            << " to _activeObject"
            << std::endl << vprDEBUG_FLUSH;
            
            //cfdPfSceneManagement::instance()->GetRootNode()->AddChild( textOutput->add_text( "executing..." ) );
            this->activeObject = iter->second;
            cfdDCS* activeDataSetDCS = 0;
            /*if ( this->activeObject->GetObjectType() == IMAGE_EX )
            {
               this->_activeDataSetDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
            }
            else*/
            {
            activeDataSetDCS = cfdModelHandler::instance()->GetActiveDataSet()->GetDCS();
            }
            
            //if ( this->computeActorsAndGeodes == false )
            {
               // add active dataset DCS to scene graph if not already there...
               vprDEBUG(vesDBG,1) << " setting DCS to activeDCS = "
               << activeDataSetDCS
               << std::endl << vprDEBUG_FLUSH;
               this->activeObject->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
               this->activeObject->SetNormal( cfdEnvironmentHandler::instance()->GetNavigate()->GetDirection() );
               this->activeObject->SetOrigin( cfdEnvironmentHandler::instance()->GetNavigate()->GetObjLocation() );
               
               //this should be handled by individual cfdobjects
               //this->activeObject->SetRequestedValue( (int)this->commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
               this->activeObject->SetCursorType( cfdEnvironmentHandler::instance()->GetCursor()->GetCursorID() );
               //This should be handled in individual cfdbojects
               //this->activeObject->SetPreCalcFlag( (int)this->commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE ) );
               //call back over to ssvishandler to set the flags
               cfdSteadyStateVizHandler::instance()->SetActiveVisObject( activeObject );
               //this->computeActorsAndGeodes = true;
               cfdSteadyStateVizHandler::instance()->SetComputeActorsAndGeodes( true );
               //this->actorsAreReady = true;
               cfdSteadyStateVizHandler::instance()->SetActorsAreReady( true );
            }
            break;
         }
      }
      //extracting from inside the for loop
   }
   // set the update flag in steadystate viz handler
   // now update the vis object
   //else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID )== VIS_OPTION )
   /*{
      if ( activeModel &&  activeModel->GetActiveDataSet() )
      {
         //cfd visualization options
         int visOpt = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         
         if ( visOpt == TEXTURE_BASED_VISUALIZATION )
         {
			   _activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal(1);
            tbased = true;
         }
         else if ( visOpt == CLASSIC_VISUALIZATION )
         {
            _activeModel->GetActiveDataSet()->GetSwitchNode()->SetVal(0);
            tbased = false;
         }
      }       
   }*/
   // add it to the scene graph - this handled by steadystatevis handler
   //Call b ack to ssviz handler to do this
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::SetActiveVector( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePair* activeModelDVP = command->GetDataValuePair( "Active Vector" );
   std::string activeVector;
   activeModelDVP->GetData( activeVector );
   
   //int vectorIndex = 0;//(int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
   //vprDEBUG(vesDBG,0) << " CHANGE_VECTOR, vectorIndex = " << vectorIndex
   //   << std::endl << vprDEBUG_FLUSH;
   
   cfdModel* activeModel = cfdModelHandler::instance()->GetActiveModel();
   cfdDataSet* activeDataset = activeModel->GetActiveDataSet();
   // need to set the vector by name
   activeDataset->SetActiveVector( activeVector );
   //activeDataset->GetParent()->SetActiveVector( vectorIndex );
#ifdef _OSG
#ifdef VE_PATENTED
//This if statement may disappear since are a gaurnteed of an activemodel at this ppoint
   //if ( activeModel !=0 )
   {
      cfdTextureDataSet* activeTDSet = activeModel->GetActiveTextureDataSet();
      if ( activeTDSet )
      {
         activeTDSet->SetActiveVector( activeVector );
      }
   }
#endif
#endif
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::SetActiveDataSet( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );
   VE_XML::DataValuePair* activeModelDVP = command->GetDataValuePair( "Active Dataset" );
   std::string dataSetName;
   activeModelDVP->GetData( dataSetName );

   //Need to set the active datasetname and get the position of the dataset
   cfdModel* activeModel = cfdModelHandler::instance()->GetActiveModel();
   unsigned int i = activeModel->GetIndexOfDataSet( dataSetName );
      vprDEBUG(vesDBG,1) 
         << "CHANGE_STEADYSTATE_DATASET " << i 
         << std::endl << vprDEBUG_FLUSH;
   //update active texture dataset if it exists
#ifdef _OSG
#ifdef VE_PATENTED
   unsigned int nTextureDataSets = activeModel->GetNumberOfTextureDataSets();
   if( (nTextureDataSets) && ( i < nTextureDataSets ) )
   {
      cfdTextureDataSet* activeTDSet = activeModel->GetTextureDataSet( i );
      activeModel->SetActiveTextureDataSet( activeTDSet );
   }
#endif
#endif
   if ( ( i < activeModel->GetNumberOfCfdDataSets() ) )
   {
      vprDEBUG(vesDBG,0) << "\tcfdModelHandler::PreFrameUpdate dataset = "
      << activeModel->GetCfdDataSet( i )->GetFileName()
      << ", dcs = " << activeModel->GetCfdDataSet( i )->GetDCS()
      << std::endl << vprDEBUG_FLUSH;
      
      int cfdType = activeModel->GetCfdDataSet( i )->GetType();
      vprDEBUG(vesDBG,1) << "\tcfdModelHandler::PreFrameUpdate cfdType: " << cfdType
         << std::endl << vprDEBUG_FLUSH;
      
      // set the dataset as the appropriate dastaset type
      // (and the active dataset as well)
      cfdDataSet* activeDataset = activeModel->GetCfdDataSet( i );         
      
      std::string oldDatasetName = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetFileName();
      vprDEBUG(vesDBG,1) << "\tcfdModelHandler::PreFrameUpdate last active dataset name = " 
         << oldDatasetName
         << std::endl << vprDEBUG_FLUSH;
      
      activeModel->SetActiveDataSet( activeDataset );         
      vprDEBUG(vesDBG,1) << "\tcfdModelHandler::PreFrameUpdate Activating steady state file " 
         << activeDataset->GetFileName()
         << std::endl << vprDEBUG_FLUSH;
      
      // make sure that the user did not just hit same dataset button
      // (or change scalar since that is routed through here too)
      if ( oldDatasetName == activeDataset->GetFileName() )//if ( strcmp( oldDatasetName, activeDataset->GetFileName() ) )
      {
         vprDEBUG(vesDBG,1) << "\tcfdModelHandler::PreFrameUpdate  setting dataset as newly activated" 
         << std::endl << vprDEBUG_FLUSH;
         activeDataset->SetNewlyActivated();
         oldDatasetName.assign( activeDataset->GetFileName() );//strcpy( oldDatasetName, activeDataset->GetFileName() );
      }
      
      // Set the current active dataset for the scalar bar
      // so that it knows how to update itself
      //_scalarBar->SetActiveDataSet( activeDataset );
   }
   else
   {
      std::cerr << "ERROR: cfdModelHandler::PreFrameUpdate  requested steady state dataset " 
      //<< commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) << " must be less than " 
      << activeModel->GetNumberOfCfdDataSets()
      << std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::SetActiveScalarAndRange( VE_XML::XMLObject* xmlObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( xmlObject );

   std::string activeScalarName;
   VE_XML::DataValuePair* activeModelDVP = command->GetDataValuePair( "Active Scalar" );
   activeModelDVP->GetData( activeScalarName );   
   double scalarMin;
   activeModelDVP = command->GetDataValuePair( "Scalar Min" );
   activeModelDVP->GetData( scalarMin );
   double scalarMax;
   activeModelDVP = command->GetDataValuePair( "Scalar Max" );
   activeModelDVP->GetData( scalarMax );

   /*(int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
   vprDEBUG(vesDBG,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
      << ", scalarIndex = " << scalarIndex
      << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
      << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
      << std::endl << vprDEBUG_FLUSH;*/
   cfdDataSet* activeDataset = cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet();
   //update active scalar texture if it exists
#ifdef _OSG
#ifdef VE_PATENTED
   //if(_activeModel !=0)
   {
      cfdTextureDataSet* activeTDSet = cfdModelHandler::instance()->GetActiveModel()->GetActiveTextureDataSet();
      if( activeTDSet )
      {
         activeTDSet->SetActiveScalar( activeScalarName );
      }
   }
#endif
#endif
   
   activeDataset->SetActiveScalar( activeScalarName );
   activeDataset->GetParent()->SetActiveScalar( activeScalarName );
   
   activeDataset->ResetScalarBarRange( scalarMin, scalarMax );
   activeDataset->GetParent()->ResetScalarBarRange( scalarMin, scalarMax ); 
}
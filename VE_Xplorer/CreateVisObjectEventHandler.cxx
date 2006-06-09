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

#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdClone.h"
#include "VE_SceneGraph/cfdPfSceneManagement.h"

#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Xplorer/cfdDebug.h"

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#include <iostream>

using namespace VE_EVENTS;
////////////////////////////////////////////////////////////////////////////
//Constructor                                                             //
////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::CreateVisObjectEventHandler()
:VE_EVENTS::EventHandler()
{
   _activeModel = 0;
   
   // Initialize all the vis objects from ssvishandler
   //if ( cfdModelHandler::instance()->GetActiveDataSet() != NULL )
   {
      /*{
      int postData = 0;
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetNumberOfPlanes() > 0 )
            postData += 1;
         
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetNumberOfPlanes() > 0 )
            postData += 2;
         
         if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetNumberOfPlanes() > 0 )
            postData += 4;
         
         commandArray->SetCommandValue( cfdCommandArray::CFD_POSTDATA_STATE, postData );
      }*/
      
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
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() &&
      //     cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetScalars() )
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
      
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetScalars() )
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
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() )
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
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetNumberOfPlanes() > 0 )
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
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetNumberOfPlanes() > 0 )
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
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetNumberOfPlanes() > 0 )
      {
         std::cout << "| 30. Initializing....................Multiple Z-planes of Contours |" << std::endl;
         this->z_contours = new cfdContours( 2 );
         this->z_contours->SetObjectType( Z_CONTOURS );
         this->dataList.push_back( this->z_contours );
         this->commandList.push_back( this->z_contours );
      }
      
      // Make sure that this dataset has a vector and scalar...
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() &&
      //     cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetScalars() )
      
      {
         //
         // Initiate the preset x momentums.
         //
         //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetNumberOfPlanes() > 0 )
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
         //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetNumberOfPlanes() > 0 )
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
         //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetNumberOfPlanes() > 0 )
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
      //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetDataSet()->GetPointData()->GetVectors() )
      {
         //
         // Initiate the preset x vectors.
         //
         //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedXSlices()->GetNumberOfPlanes() > 0 )
      {
         std::cout << "| 34. Initializing.........Multiple X-planes of Precomputed Vectors |" << std::endl;
         this->x_vectors = new cfdVectors( 0 );
         this->x_vectors->SetObjectType( X_VECTORS );
         this->dataList.push_back( this->x_vectors );
         this->commandList.push_back( this->x_vectors );
      }
         
         //
         // Initiate the preset y vectors.
         //
         //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedYSlices()->GetNumberOfPlanes() > 0 )
         {
            std::cout << "| 35. Initializing.........Multiple Y-planes of Precomputed Vectors |" << std::endl;
            this->y_vectors = new cfdVectors( 1 );
            this->y_vectors->SetObjectType( Y_VECTORS );
            this->dataList.push_back( this->y_vectors );
            this->commandList.push_back( this->y_vectors );
         }
         
         //
         // Initiate the preset z vectors.
         //
         //if ( cfdModelHandler::instance()->GetActiveDataSet()->GetPrecomputedZSlices()->GetNumberOfPlanes() > 0 )
         {
            std::cout << "| 36. Initializing.........Multiple Z-planes of Precomputed Vectors |" << std::endl;
            this->z_vectors = new cfdVectors( 2 );
            this->z_vectors->SetObjectType( Z_VECTORS );
            this->dataList.push_back( this->z_vectors );
            this->commandList.push_back( this->z_vectors );
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
         //this->animImg = new cfdAnimatedImage( _param.c_str() );
         //this->animImg->SetObjectType( ANIMATED_IMAGES );
         //this->dataList.push_back( this->animImg);  
      }
   }
   
   //if ( cfdModelHandler::instance()->GetActiveDataSet() )
   {
      //
      // Initiate the PolyData File
      //
      std::cout << "| 41. Initializing................................... PolyData File |" << std::endl;
      this->particles = new cfdPolyData();
      this->particles->SetObjectType( PARTICLES );
      this->dataList.push_back( this->particles );     
      this->commandList.push_back( this->particles );
      
      std::cout << "|  5. Initializing................................. Dataset surface |" << std::endl;
      this->surface = new cfdPolyData( 1.0 );
      this->surface->SetObjectType( POLYDATA );
      this->dataList.push_back( this->surface );
      this->commandList.push_back( this->surface );
   }
   
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
   
   for ( int i = 0; i < (int)this->dataList.size(); i++ )
   {
      // Initialize all the geode creation flags and dcs flags for all the geodes
      this->dataList.at( i )->SetUpdateFlag( false );
      this->dataList.at( i )->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
   }
   
}
///////////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler::CreateVisObjectEventHandler(const CreateVisObjectEventHandler& rhs)
:VE_EVENTS::EventHandler(rhs)
{
   
}
/////////////////////////////////////////////////////
///Destructor                                      //
/////////////////////////////////////////////////////
CreateVisObjectEventHandler::~CreateVisObjectEventHandler()
{
   //delete all the objects from ssvishandler
   ;
}
///Equal operator
//////////////////////////////////////////////////////////////////////////////////////////////////
CreateVisObjectEventHandler& CreateVisObjectEventHandler::operator=(const CreateVisObjectEventHandler& rhs)
{
   if(this != &rhs)
   {
      VE_EVENTS::CreateVisObjectEventHandler::operator=(rhs);
   }
   return *this;
}
///////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model)
{
   try
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
   }
}
//////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::Execute(VE_XML::XMLObject* xmlObject)
{
   for ( unsigned int i = 0; i < this->commandList.size(); i++ )
   {
      // Check to see if any of the objectss need updated before we 
      // create actors
      if ( cfdModelHandler::instance()->GetActiveModel() )
      {
         this->dataList.at( i )->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
         this->commandList[ i ]->SetVECommand( cfdModelHandler::instance()->GetActiveModel()->GetVECommand() );
         bool commandApplies = this->commandList[ i ]->CheckCommandId( this->commandArray );
         vprDEBUG(vesDBG,4) << "|\tCommand Applies : " << commandApplies
            << std::endl << vprDEBUG_FLUSH;
      }
   }
   
   // Set the active dataset
   // set the active scalar
   // set the active scalar range
   // set the active vector
   // get the active vis object
   else if (   ( 0 <= this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) ) &&
               ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) < 100 )  && 
               ( !this->computeActorsAndGeodes ) &&
               ( !this->texturesActive ) )
   {
      vprDEBUG(vesDBG,1) << " selected ID number = " << this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID )
      << std::endl << vprDEBUG_FLUSH;
      for ( size_t i = 0; i < this->dataList.size(); i ++ )
      {          
         if ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == dataList.at( i )->GetObjectType() )
         {
            vprDEBUG(vesDBG,1) << " setting viz object " << i 
            << " to _activeObject"
            << std::endl << vprDEBUG_FLUSH;
            
            //cfdPfSceneManagement::instance()->GetRootNode()->AddChild( textOutput->add_text( "executing..." ) );
            this->_activeObject = this->dataList.at( i );
            
            if ( this->_activeObject->GetObjectType() == IMAGE_EX )
            {
               this->_activeDataSetDCS = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS();
            }
            else
            {
               this->_activeDataSetDCS = cfdModelHandler::instance()->GetActiveDataSet()->GetDCS();
            }
            
            //if ( this->computeActorsAndGeodes == false )
            {
               // add active dataset DCS to scene graph if not already there...
               vprDEBUG(vesDBG,1) << " setting DCS to activeDCS = "
               << this->_activeDataSetDCS
               << std::endl << vprDEBUG_FLUSH;
               this->_activeObject->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
               this->_activeObject->SetNormal( this->nav->GetDirection() );
               this->_activeObject->SetOrigin( this->nav->GetObjLocation() );
               
               this->_activeObject->SetRequestedValue( (int)this->commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
               this->_activeObject->SetCursorType( this->cursor->GetCursorID() );
               this->_activeObject->SetPreCalcFlag( (int)this->commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE ) );
               this->computeActorsAndGeodes = true;
               this->actorsAreReady = true;
            }
            break;
         }
      }
      //extracting from inside the for loop
   }
   // set the update flag in steadystate viz handler
   // now update the vis object
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID )== VIS_OPTION )
   {
      if ( _activeModel &&  _activeModel->GetActiveDataSet())
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
   }   
   // add it to the scene graph - this handled by steadystatevis handler
   try
   {
      VE_XML::Command* command = dynamic_cast<VE_XML::Command*>(xmlObject);
      VE_XML::DataValuePair* veModelDVP = command->GetDataValuePair("CREATE_NEW_DATASETS");
      VE_Model::Model* veModel = dynamic_cast< VE_Model::Model* >( veModelDVP->GetDataXMLObject() );
      size_t numInfoPackets = veModel->GetNumberOfInformationPackets();
      for ( size_t i = 0; i < numInfoPackets; ++i )
      {
         VE_XML::ParameterBlock* tempInfoPacket = veModel->GetInformationPacket( i );
         if ( tempInfoPacket->GetProperty( "VTK_DATA_FILE" ) )
         {
            // Assume only one model for now
            // Flexibilty to have multiply models
            if ( !_activeModel )
               return;
            _activeModel->CreateCfdDataSet();

            vprDEBUG(vesDBG,0) << "|\t************************************* "
                             << std::endl << vprDEBUG_FLUSH;

            vprDEBUG(vesDBG,0) << "|\tvtk DCS parameters:"
                             << std::endl << vprDEBUG_FLUSH;

            // Pass in -1 to GetCfdDataSet to get the last dataset added
            _activeModel->GetCfdDataSet( -1 )->GetDCS()->SetScaleArray( tempInfoPacket->GetTransform()->GetScaleArray()->GetArray() );
            _activeModel->GetCfdDataSet( -1 )->GetDCS()->SetTranslationArray( tempInfoPacket->GetTransform()->GetTranslationArray()->GetArray() );
            _activeModel->GetCfdDataSet( -1 )->GetDCS()->SetRotationArray( tempInfoPacket->GetTransform()->GetRotationArray()->GetArray() );

            // get vtk data set name...
            std::string vtk_filein = tempInfoPacket->GetProperty( "VTK_DATA_FILE" )->GetDataString();

            if ( VE_Util::fileIO::isFileReadable( vtk_filein ) ) 
            {
               vprDEBUG(vesDBG,0) << "|\tvtk file = " << vtk_filein 
                                << ", dcs = "  << _activeModel->GetCfdDataSet( -1 )->GetDCS()
                                << std::endl << vprDEBUG_FLUSH;
               _activeModel->GetCfdDataSet( -1 )->SetFileName( vtk_filein );
            }
            else
            {
               std::cerr << "ERROR: unreadable vtk file = " << vtk_filein 
                           << ".  You may need to correct your param file."
                           << std::endl;
               exit(1);
            }

            if ( tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" ) )
            {
               std::string precomputedDataSliceDir = tempInfoPacket->GetProperty( "VTK_PRECOMPUTED_DIR_PATH" )->GetDataString();
               _activeModel->GetCfdDataSet( -1 )->SetPrecomputedDataSliceDir( precomputedDataSliceDir );
            }

            if ( tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" ) )
            {
               std::string precomputedSurfaceDir = tempInfoPacket->GetProperty( "VTK_SURFACE_DIR_PATH" )->GetDataString();
               _activeModel->GetCfdDataSet( -1 )->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
            }

            LoadSurfaceFiles( _activeModel->GetCfdDataSet( -1 )->GetPrecomputedSurfaceDir() );
            //Load texture datasets
            if ( tempInfoPacket->GetProperty( "VTK_TEXTURE_DIR_PATH" ) )
            {
#ifdef _OSG
#ifdef VE_PATENTED
               vprDEBUG(vesDBG,0) << "|\tCreating texture dataset." << std::endl << vprDEBUG_FLUSH;
               _activeModel->CreateTextureDataSet();
               size_t numProperties = tempInfoPacket->GetNumberOfProperties();
               for ( size_t j = 0; j < numProperties; ++j )
               {
                  if ( tempInfoPacket->GetProperty( j )->GetDataName() == 
                        std::string( "VTK_TEXTURE_DIR_PATH" ) )
                  {
                     _activeModel->AddDataSetToTextureDataSet( 0, tempInfoPacket->GetProperty( j )->GetDataString() );
                  }
               }
#endif
#endif
            }
            //Now load up the dataset
            //_activeModel->GetCfdDataSet( -1 )->LoadData();
            for ( unsigned int i = 0; i < _activeModel->GetNumberOfCfdDataSets(); i++)
            {
               std::cout << "|\tLoading data for file " 
                           << _activeModel->GetCfdDataSet( i )->GetFileName()
                           << std::endl;
               _activeModel->GetCfdDataSet( i )->LoadData();
               _activeModel->GetCfdDataSet( i )->SetArrow( VE_Xplorer::cfdModelHandler::instance()->GetArrow() );
               if ( _activeModel->GetCfdDataSet( i )->GetParent() == _activeModel->GetCfdDataSet( i ) )
               {
                  VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->
                        AddChild( _activeModel->GetCfdDataSet( i )->GetDCS() );
                  _activeModel->SetActiveDataSet( _activeModel->GetCfdDataSet( i ) );
               }
            }
         }
      }
   }
   catch(...)
   {
      std::cerr << "|\tSomething bad happened in AddVTKDataSetEventHandler::Execute." << std::endl;
   }
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::LoadSurfaceFiles( std::string precomputedSurfaceDir )
{
   if ( precomputedSurfaceDir.empty() )// == NULL )
   {
      vprDEBUG(vesDBG,1) << "precomputedSurfaceDir == NULL" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vesDBG,1) << "Loading surface files from " 
      << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;


   boost::filesystem::path dir_path( precomputedSurfaceDir );

   if ( boost::filesystem::is_directory( dir_path ) )
   {
      std::cout << "|\tIn directory: "
              << dir_path.native_directory_string() << "\n";
      boost::filesystem::directory_iterator end_iter;
      for ( boost::filesystem::directory_iterator dir_itr( dir_path );
            dir_itr != end_iter; ++dir_itr )    
      {
         try
         {
            if ( boost::filesystem::is_directory( *dir_itr ) )
            {
               std::cout << "|\tIs a sub directory " << dir_itr->leaf() << " [directory]\n";
            }
            else
            {
               std::cout << dir_itr->leaf() << "\n";
               if ( strstr( dir_itr->leaf().c_str(), ".vtk") )
               {
                  std::string pathAndFileName;
                  pathAndFileName.assign( dir_path.leaf().c_str() );
                  pathAndFileName.append( "/" );
                  pathAndFileName.append( dir_itr->leaf().c_str() );
                  vprDEBUG(vesDBG,0) << "|\tsurface file = " << pathAndFileName
                                         << std::endl << vprDEBUG_FLUSH;

                  _activeModel->CreateCfdDataSet();
                  unsigned int numDataSets = _activeModel->GetNumberOfCfdDataSets();
                  // subtract 1 because this number was 1 base not 0 base
                  numDataSets -= 1;
                  _activeModel->GetCfdDataSet( -1 )->SetFileName( pathAndFileName );

                  // set the dcs matrix the same as the last file
                  _activeModel->GetCfdDataSet( -1 )->SetDCS( 
                                    _activeModel->GetCfdDataSet( (int)(numDataSets-1) )->GetDCS() ); 

                  // precomputed data that descends from a flowdata.vtk should
                  // automatically have the same color mapping as the "parent" 
                  _activeModel->GetCfdDataSet( -1 )->SetParent( 
                                    _activeModel->GetCfdDataSet( (int)(numDataSets-1) )->GetParent() );
               }
            }
         }
         catch ( const std::exception & ex )
         {
            std::cout << dir_itr->leaf() << " " << ex.what() << std::endl;
         }
      }
   }
   else // must be a file
   {
      std::cout << "\nFound: " << dir_path.native_file_string() << "\n";    
   }
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::Load3DTextureDirectories( std::string dirToLoad )
{
#ifdef _OSG
#ifdef VE_PATENTED
#endif
#endif
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::SetActiveVector( void )
{
if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
          == CHANGE_VECTOR )
{ 
   int vectorIndex = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
   vprDEBUG(vesDBG,0) << " CHANGE_VECTOR, vectorIndex = " << vectorIndex
      << std::endl << vprDEBUG_FLUSH;
   
   activeDataset->SetActiveVector( vectorIndex );
   //activeDataset->GetParent()->SetActiveVector( vectorIndex );
#ifdef _OSG
#ifdef VE_PATENTED
   if(_activeModel !=0)
   {
      if(_activeTDSet)
      {
         _activeTDSet->SetActiveVector(activeDataset->GetVectorName(vectorIndex));
      }
   }
#endif
#endif
}
}
//////////////////////////////////////////////////////////////////   
void CreateVisObjectEventHandler::SetActiveDataSet( void )
{
if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
     == CHANGE_STEADYSTATE_DATASET )
{
   unsigned int i = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   vprDEBUG(vesDBG,1) 
      << "CHANGE_STEADYSTATE_DATASET " << i 
      //<< ", scalarIndex = " << this->cfdSc
      //<< ", min = " << this->cfdMin 
      //<< ", max = " << this->cfdMax
      << std::endl << vprDEBUG_FLUSH;
   
   //update active texture dataset if it exists
#ifdef _OSG
#ifdef VE_PATENTED
   unsigned int nTextureDataSets = _activeModel->GetNumberOfTextureDataSets();
   if( (nTextureDataSets) && ( i < nTextureDataSets ) )
   {
      _activeTDSet = _activeModel->GetTextureDataSet(i);
      _activeModel->SetActiveTextureDataSet(_activeTDSet);
   }else{
      _activeTDSet = 0;
   }
#endif
#endif
   if ( ( i < _activeModel->GetNumberOfCfdDataSets() ) )
   {
      vprDEBUG(vesDBG,0) << "\tcfdModelHandler::PreFrameUpdate dataset = "
      << _activeModel->GetCfdDataSet( i )->GetFileName()
      << ", dcs = " << _activeModel->GetCfdDataSet( i )->GetDCS()
      << std::endl << vprDEBUG_FLUSH;
      
      int cfdType = _activeModel->GetCfdDataSet( i )->GetType();
      vprDEBUG(vesDBG,1) << "\tcfdModelHandler::PreFrameUpdate cfdType: " << cfdType
         << std::endl << vprDEBUG_FLUSH;
      
      // set the dataset as the appropriate dastaset type
      // (and the active dataset as well)
      activeDataset = _activeModel->GetCfdDataSet( i );         
      _activeModel->SetActiveDataSet( activeDataset );         
      
      vprDEBUG(vesDBG,1) << "\tcfdModelHandler::PreFrameUpdate last active dataset name = " 
         << oldDatasetName
         << std::endl << vprDEBUG_FLUSH;
      
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
      
      // update scalar bar for possible new scalar name
      updateScalarRange = true;
      // Set the current active dataset for the scalar bar
      // so that it knows how to update itself
      _scalarBar->SetActiveDataSet( activeDataset );
   }
   else
   {
      std::cerr << "ERROR: cfdModelHandler::PreFrameUpdate  requested steady state dataset " 
      << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) << " must be less than " 
      << _activeModel->GetNumberOfCfdDataSets()
      << std::endl;
   }
}
}
//////////////////////////////////////////////////////////////////////////////////////
void CreateVisObjectEventHandler::SetActiveScalar( void )
{
   // Can't be an else if because may have to update if dataset has changed beforehand
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR || 
        commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_SCALAR_RANGE ||
        updateScalarRange
        )
   { 
      int scalarIndex = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_SC );
      vprDEBUG(vesDBG,1) << "CHANGE_SCALAR || CHANGE_SCALAR_RANGE"
         << ", scalarIndex = " << scalarIndex
         << ", min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << std::endl << vprDEBUG_FLUSH;
      //update active scalar texture if it exists
#ifdef _OSG
#ifdef VE_PATENTED
      if(_activeModel !=0)
      {
         if(_activeTDSet)
         {
            _activeTDSet->SetActiveScalar(activeDataset->GetScalarName(scalarIndex));
         }
      }
#endif
#endif
      
      activeDataset->SetActiveScalar( scalarIndex );
      activeDataset->GetParent()->SetActiveScalar( scalarIndex );
      
      activeDataset->ResetScalarBarRange( 
                                          (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), 
                                          (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
      activeDataset->GetParent()->ResetScalarBarRange( 
                                                       (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ), 
                                                       (int)commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) );
   }
   
}
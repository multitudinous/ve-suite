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
 * File:          $RCSfile: cfdSteadyStateVizHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

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

#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdGlobalBase.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdObjects.h"
#include "VE_Xplorer/cfdPlanes.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdCursor.h"
#include "VE_Xplorer/cfdGraphicsObject.h"
#include "VE_Xplorer/cfdModel.h"
#include "VE_Xplorer/cfdTextOutput.h"
#include "VE_Xplorer/cfdEnvironmentHandler.h"
#include "VE_Xplorer/cfdModelHandler.h"

#include "VE_Xplorer/cfdRawNodeWriteTraverser.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include "VE_Xplorer/cfdDebug.h"
#include <vpr/vpr.h>
#include <vpr/System.h>

#include "VE_SceneGraph/cfdSwitch.h"
#include "VE_SceneGraph/cfdPfSceneManagement.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdGeode.h"
#include "VE_SceneGraph/cfdTempAnimation.h"
//This heare is WAY down here to fix compile errors on IRIX
#include "VE_Xplorer/cfdSteadyStateVizHandler.h"

#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>

vprSingletonImp( VE_Xplorer::cfdSteadyStateVizHandler );

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdSteadyStateVizHandler::cfdSteadyStateVizHandler( void )
{
   this->commandArray = 0;
   this->_activeDataSetDCS = 0;
   this->_activeObject = 0;
   this->lastSource = 0;
   this->_activeTempAnimation = 0;

   this->computeActorsAndGeodes = false;
   this->actorsAreReady = false;
   this->useLastSource = false;
   this->texturesActive = false;
   this->transientActors = true;
   this->vjTh[0] = 0;
   _param.erase();// = 0;
}

void cfdSteadyStateVizHandler::Initialize( std::string param )
{
   _param = param;
   nav = cfdEnvironmentHandler::instance()->GetNavigate();
   cursor = cfdEnvironmentHandler::instance()->GetCursor();
}

void cfdSteadyStateVizHandler::CleanUp( void )
{
   this->runIntraParallelThread = false;

   if ( this->vjTh[0] )
   {
      this->vjTh[0]->join();
      delete this->vjTh[0];
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
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::cfdTempAnimation* cfdSteadyStateVizHandler::GetActiveAnimation( void )
{
   return this->_activeTempAnimation;
}
////////////////////////////////////////////////////////////////////////////////
void cfdSteadyStateVizHandler::SetActiveVisObject( cfdObjects* tempObject )
{
   _activeObject = tempObject;
}
////////////////////////////////////////////////////////////////////////////////
void cfdSteadyStateVizHandler::SetComputeActorsAndGeodes( bool actorsAndGeodes )
{
   computeActorsAndGeodes = actorsAndGeodes;
}
////////////////////////////////////////////////////////////////////////////////
void cfdSteadyStateVizHandler::SetActorsAreReady( bool actorsReady )
{
   actorsAreReady = actorsReady;
}
////////////////////////////////////////////////////////////////////////////////
void cfdSteadyStateVizHandler::InitScene( void )
{

   // This set of thread stuff needs to be in ssvizhandler and transvizhandler
   std::cout << "|  9. Initializing......................................... Threads |" << std::endl;
   this->runIntraParallelThread = true;
   this->vjTh[0] = new vpr::Thread( new vpr::ThreadMemberFunctor< cfdSteadyStateVizHandler > ( this, &cfdSteadyStateVizHandler::CreateActorThread ) );

   //std::cout << "|  9. Initializing..................................... Text Output |" << std::endl;
   //this->textOutput = new cfdTextOutput();
}
////////////////////////////////////////////////////////////////////////////////
void cfdSteadyStateVizHandler::PreFrameUpdate( void )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) != -1 )
   {            
      vprDEBUG(vesDBG,2) 
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

   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_ACTIVE )
   {
      this->transientActors = commandArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE );
   }
   // check any virtual objects need to be updated
   if ( this->actorsAreReady && this->transientActors )
   {
      vprDEBUG(vesDBG,3) << "|\tUpdating Objects"
                                   << std::endl << vprDEBUG_FLUSH;
      bool alreadyRemoved = false;
      //for ( unsigned int i = 0; i < this->dataList.size(); i++ )
      {
         if ( _activeObject->GetUpdateFlag() )//|| 
               //this->dataList.at( i )->GetTransientGeodeFlag() )
         {
            vprDEBUG(vesDBG,2) << "|\tCreating Objects"
                                   << std::endl << vprDEBUG_FLUSH;
            // if object needs updated then already have a graphics object
            cfdGraphicsObject* temp = new cfdGraphicsObject();
            temp->SetTypeOfViz( cfdGraphicsObject::CLASSIC );
            temp->SetParentNode( _activeObject->GetActiveDataSet()->GetDCS() );
            temp->SetActiveModel( cfdModelHandler::instance()->GetActiveModel() );
            temp->SetWorldNode( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() );
            temp->SetGeodes( _activeObject->GetGeodes() );
            temp->AddGraphicsObjectToSceneGraph();
            
/*   VE_Xplorer::cfdRawNodeWriteTraverser cfdWT("test.osg");

   //set the graph
   cfdWT.setNode( this->dataList[ i ]->GetGeodes().at(0) );

   //set the "swapping" callback
   cfdWT.setCallback(1);

   //write out the file
   cfdWT.writeFile();
*/
            // search map for other object types with the same type as this one
            std::multimap< int, cfdGraphicsObject* >::iterator pos;
            for ( pos = graphicsObjects.lower_bound( _activeObject->GetObjectType() ); 
                  pos != graphicsObjects.upper_bound( _activeObject->GetObjectType() ); )
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
            graphicsObjects.insert( std::make_pair( _activeObject->GetObjectType(), temp ) );

            // Resetting these variables is very important
            _activeObject->SetUpdateFlag( false );
            this->actorsAreReady = false;
            _activeObject->ClearGeodes();
            vprDEBUG(vesDBG,2) << "|\tDone Creating Objects"
                                   << std::endl << vprDEBUG_FLUSH;

            if ( cfdModelHandler::instance()->GetActiveModel()->GetMirrorDataFlag() )
            {
               // we mirror the dataset in two places
               // once here for data viz and once in modelhandler for geom
               VE_SceneGraph::cfdGroup* temp = (VE_SceneGraph::cfdGroup*)cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet()->GetSwitchNode()->GetChild( 0 );
               cfdModelHandler::instance()->GetActiveModel()->SetMirrorNode( temp );
            }
         }
         

         // if we have selected a viz feature and it is complete the remove the text
         if ( !computeActorsAndGeodes && !alreadyRemoved )
         {
            alreadyRemoved = true;
            //cfdPfSceneManagement::instance()->GetRootNode()->RemoveChild( textOutput->add_text( "executing..." ) );
         }
      }
   }

   if ( this->commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == USE_LAST_STREAMLINE_SEEDPOINTS )
   {
      this->useLastSource = this->commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL )
   { 
      vprDEBUG(vesDBG,2) << "|\tClear All Graphics Objects From Scene Graph"
                             << std::endl << vprDEBUG_FLUSH;
      std::multimap< int, cfdGraphicsObject* >::iterator pos;
      for ( pos = graphicsObjects.begin(); pos != graphicsObjects.end(); )
      {
         pos->second->RemovecfdGeodeFromDCS();
         delete pos->second;
         graphicsObjects.erase( pos++ );
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID )== VIS_OPTION )
   {
      int visOpt = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );

      if ( visOpt == TEXTURE_BASED_VISUALIZATION )
      {
         texturesActive = true;
      }
      else if ( visOpt == CLASSIC_VISUALIZATION )
      {
         texturesActive = false;
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_DURATION )
   {
      std::multimap< int, cfdGraphicsObject* >::iterator pos;
      for (pos=graphicsObjects.begin(); pos!=graphicsObjects.end(); ++pos )
      {
         if ( pos->second->GetAnimation() )
         {
            pos->second->GetAnimation()->SetDuration( 
                  commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
         }
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

            vprDEBUG(vesDBG,0) << " Updating cfdObject..." 
               << std::endl << vprDEBUG_FLUSH;

            // May replace later , fix a later date
            //vprDEBUG(vesDBG,2) << " Memory used before update ( bytes ) : "
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
               vprDEBUG(vesDBG,1)
                 << "non-interactive object." << std::endl << vprDEBUG_FLUSH; 

               this->_activeObject->Update();      
               //this->_activeObject->SetSequence( 0 );
            }
            else if ( streamersTest != NULL )
            {
               vprDEBUG(vesDBG,1) << "interactive object." 
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
            //vprDEBUG(vesDBG,1) << " Time: " << GetTimeClock()-tt
            //                       << std::endl << vprDEBUG_FLUSH;
            //vprDEBUG(vesDBG,2) <<" Memory used after update ( bytes ) : "
            //                       << pfMemory::getArenaBytesUsed() 
            //                       << std::endl << vprDEBUG_FLUSH;

            this->_activeObject = NULL;
            this->computeActorsAndGeodes = false;   
            vprDEBUG(vesDBG,0) << "|\tDone updating cfdObject" 
               << std::endl << std::endl << vprDEBUG_FLUSH; 
            
         }
      }
   } // End of While loop
}

void cfdSteadyStateVizHandler::streamers( void )
{
   vprDEBUG(vesDBG,1) << "In streamers" << std::endl << vprDEBUG_FLUSH;
   if ( this->cursor->GetCursorID() == NONE )
   {
      this->_activeObject = NULL;
      return;
   }
   
   this->_activeObject->SetCursorType( this->cursor->GetCursorID() );
   this->_activeObject->SetNormal( this->nav->GetDirection() );
   this->_activeObject->SetOrigin( this->nav->GetObjLocation() );

   if ( this->cursor->GetCursorID() == CUBE )
   {
      this->_activeObject->SetBoxSize( this->cur_box );
   }

   if ( !this->useLastSource )
   {
      vprDEBUG(vesDBG,1) <<"creating fresh streamlines"
                             << std::endl << vprDEBUG_FLUSH;
      if ( this->lastSource != NULL )
      {
         this->lastSource->Delete();
      }

      this->lastSource = vtkPolyData::New();
      this->lastSource->DeepCopy( this->cursor->GetSourcePoints() );

      this->_activeObject->SetSourcePoints( this->lastSource );
   }
   else 
   {
      vprDEBUG(vesDBG,1) << "using transformed last source"
                             << std::endl << vprDEBUG_FLUSH;

      this->_activeObject->SetSourcePoints( this->lastSource );
   }

   this->_activeObject->Update();
   this->_activeObject = NULL;
}

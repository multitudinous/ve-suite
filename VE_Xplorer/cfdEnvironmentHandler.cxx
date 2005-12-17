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
 * File:          $RCSfile: cfdEnvironmentHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/cfdEnvironmentHandler.h"

#include "VE_Xplorer/fileIO.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdSoundHandler.h"
#include "VE_Xplorer/cfdCursor.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdReadParam.h"
#include "VE_Xplorer/cfdTeacher.h"
#include "VE_Xplorer/cfdSoundHandler.h"
#include "VE_Xplorer/cfdQuatCamHandler.h"
#include "VE_Xplorer/cfdDataSet.h"
#include "VE_Xplorer/cfdModelHandler.h"
#include "VE_SceneGraph/cfdPfSceneManagement.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_Xplorer/cfdDebug.h"

#ifdef _OSG
#include "VE_Xplorer/cfdObjectHandler.h"
#endif

#include <fstream>
#include <cstdlib>

vprSingletonImp( VE_Xplorer::cfdEnvironmentHandler );

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdEnvironmentHandler::cfdEnvironmentHandler( void )
{
   nav            = 0;
   _teacher       = 0;
   _soundHandler  = 0;
   _camHandler    = 0;
   cursor         = 0;
   _param.erase();//         = 0;
   _commandArray  = 0;
   _readParam     = 0;
   arrow          = 0;

   for ( unsigned int i = 0; i < 3; i++ )
   {
      worldScale[ i ] = 1.0f;
      worldTrans[ i ] = 0.0f;
      worldRot[ i ] = 0.0f;
   }

   this->nav = 0;
#ifdef VE_PATENTED
#ifdef _OSG
   this->objectHandler = 0;
   _activeGeomPicking = false;
#endif // _OSG
#endif // VE_PATENTED
   _readParam = 0;
   _param.erase();// = 0;

}

void cfdEnvironmentHandler::Initialize( std::string param )
{
   _param = param;
   vprDEBUG(vesDBG,1) << "|\tcfdApp::init" << std::endl << vprDEBUG_FLUSH;
   std::cout << "|  7. Initializing.............................. Navigation systems |" << std::endl;
   this->nav = new cfdNavigate();
   _readParam = new cfdReadParam();
   this->arrow = cfdModelHandler::instance()->GetArrow();
   CreateObjects();

#ifdef VE_PATENTED
#ifdef _OSG
   this->objectHandler = new cfdObjectHandler();
#endif // _OSG
#endif // VE_PATENTED

}

void cfdEnvironmentHandler::CleanUp( void )
{
   if ( this->nav )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->nav" << std::endl << vprDEBUG_FLUSH;
      delete nav;
   }
   
   if ( this->_readParam )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->_readParam" << std::endl << vprDEBUG_FLUSH;
      delete this->_readParam;
   }

   if ( this->cursor )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->cursor" << std::endl << vprDEBUG_FLUSH;
      delete this->cursor;
   }

   if ( this->_camHandler )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->_camHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_camHandler;
   }

   if ( this->_soundHandler )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->_soundHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_soundHandler;
   }

   if ( this->_teacher )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->_teacher" << std::endl << vprDEBUG_FLUSH;
      delete this->_teacher;
   }
}
/////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetCommandArray( cfdCommandArray* input )
{
   _commandArray = input;
}

cfdSoundHandler* cfdEnvironmentHandler::GetSoundHandler( void )
{
   return _soundHandler;
}

cfdTeacher* cfdEnvironmentHandler::GetTeacher( void )
{
   return _teacher;
}

cfdQuatCamHandler* cfdEnvironmentHandler::GetQuatCamHandler( void )
{
   return _camHandler;
}
////////////////////////////////////////
void cfdEnvironmentHandler::InitScene( void )
{
   std::cout << "| ***************************************************************** |" << std::endl;
   // Needs to be set by the gui fix later
   this->nav->Initialize( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() );
   this->nav->SetInitialWorldPosition( this->worldTrans, this->worldRot, this->worldScale );

   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetScaleArray( this->worldScale );

   for ( int i = 0; i < 3; i++)
   {
      this->nav->worldTrans[ i ] = this->worldTrans[ i ];
      this->nav->worldRot[ i ] = this->worldRot[ i ];
   }
   
   float tempArray[ 3 ];
   tempArray[ 0 ] = -this->nav->worldTrans[ 0 ];
   tempArray[ 1 ] = -this->nav->worldTrans[ 1 ];
   tempArray[ 2 ] = -this->nav->worldTrans[ 2 ];
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetTranslationArray( tempArray );

   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetRotationArray( this->nav->worldRot );

   // Maybe need to fix this later
   //this->cursorId = NONE;

   //
   // Initiate cursors.
   //
   std::cout << "|  8. Initializing................................. Virtual cursors |" << std::endl;
   this->cursor = new cfdCursor( this->arrow, 
                             VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS(), 
                             VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode() );
   this->cursor->Initialize( this->nav->GetCursorLocation(), this->nav->GetDirection() );

   //
   // Initiate quatcam
   //
   std::cout << "|  9. Initializing..................................... cfdQuatCams |" << std::endl;
   this->_camHandler = new cfdQuatCamHandler( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS(),
                                          this->nav, _param.c_str() );

   //
   // Initiate quatcam
   //
   std::cout << "| 10. Initializing................................... Sound Handler |" << std::endl;
   this->_soundHandler = new cfdSoundHandler( _param.c_str() );

   //
   // Initiate the Performer Stored Binary objects.
   //
   std::cout << "| 11. Initializing...................................... pfBinaries |" << std::endl;
   this->_teacher = new cfdTeacher( std::string("STORED_FILES"), 
                                 VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() );

#ifdef VE_PATENTED
#ifdef _OSG
   this->objectHandler->Initialize(this->nav);
#endif //_OSG
#endif //VE_PATENTED
}

void cfdEnvironmentHandler::PreFrameUpdate( void )
{
   // Update Navigation variables   
   vprDEBUG(vesDBG,3) << "|\tcfdEnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;

#ifdef VE_PATENTED
#ifdef _OSG
   this->objectHandler->UpdateObjectHandler();
#endif //_OSG
#endif //VE_PATENTED

#ifdef _OSG
#ifdef VE_PATENTED
   if ( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == GEOMETRY_PICKING ) 
   {
      if(_commandArray->GetCommandValue( cfdCommandArray::CFD_SC))
      {
         this->objectHandler->ActivateGeometryPicking();
      }
      else
      {
         this->objectHandler->DeactivateGeometryPicking();
      }
   }
#endif
#endif

   this->nav->SetDataValues( (int)_commandArray->GetCommandValue( cfdCommandArray::CFD_ID ), 
                        (int)_commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
   this->nav->updateNavigationFromGUI();
	
   // Need to get these values from the appropriate classes
   // the cursor will be active (based on the cursor id)
   this->cursor->SetActiveDataSet( cfdModelHandler::instance()->GetActiveDataSet() );
   this->cursor->CheckCommandId( _commandArray );
   this->cursor->Update( this->nav->GetCursorLocation(),
                           this->nav->GetDirection(), this->nav->worldTrans );

   // fix later	
   /*if ( cursorId == CUBE)
   {
       this->cursor->getExtent( this->cur_box );   //record current box cursor position
   }
*/
   _camHandler->CheckCommandId( _commandArray );
   _soundHandler->CheckCommandId( _commandArray );
   _teacher->CheckCommandId( _commandArray );
   _camHandler->PreFrameUpdate();   
}

void cfdEnvironmentHandler::CreateObjects( void )
{  
   int numObjects;
   char text[ 256 ];
   std::ifstream input;
   input.open( this->_param.c_str() );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vesDBG,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vesDBG,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 0 )
      {
         vprDEBUG(vesDBG,0) << "|\tWorld DCS parameters : "
                          << std::endl << vprDEBUG_FLUSH;
         _readParam->read_pf_DCS_parameters( input, 
                        this->worldScale, this->worldTrans, this->worldRot );
      }
      /*else if ( id == 11 )
      {
         vprDEBUG(vesDBG
,0) << " World DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;
         _readParam->read_pf_DCS_parameters( input, 
                        this->worldScale, this->worldTrans, this->worldRot );
      }*/
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
}

cfdNavigate* cfdEnvironmentHandler::GetNavigate( void )
{
   return this->nav;
}

cfdCursor* cfdEnvironmentHandler::GetCursor( void )
{
   return this->cursor;
}

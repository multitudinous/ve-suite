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
 * File:          $RCSfile: cfdEnvironmentHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdEnvironmentHandler.h"

#include "fileIO.h"
#include "cfdNavigate.h"
#include "cfdSoundHandler.h"
//#include "cfdLaser.h"
//#include "cfdMenu.h"
#include "cfdCursor.h"
#include "cfdDCS.h"
#include "cfdGroup.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#include "cfdReadParam.h"
#include "cfdTeacher.h"
#include "cfdSoundHandler.h"
#include "cfdQuatCamHandler.h"

#include <vrj/Util/Debug.h>

#include <fstream>
#include <cstdlib>

cfdEnvironmentHandler::cfdEnvironmentHandler( char* filename )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::init" << std::endl << vprDEBUG_FLUSH;
   std::cout << "|  7. Initializing.............................. Navigation systems |" << std::endl;
   this->nav = new cfdNavigate();
   _readParam = new cfdReadParam( NULL );
   _param = filename;
   CreateObjects();
}

cfdEnvironmentHandler::~cfdEnvironmentHandler( void )
{
   delete nav;
   delete _readParam;
   delete cursor;
   delete _camHandler;
   delete _soundHandler;
   delete _teacher;
}

void cfdEnvironmentHandler::SetRootNode( cfdGroup* input )
{
   this->rootNode = input;
}

void cfdEnvironmentHandler::SetWorldDCS( cfdDCS* input )
{
   this->worldDCS = input;
}

void cfdEnvironmentHandler::SetCommandArray( cfdCommandArray* input )
{
   _commandArray = input;
}

void cfdEnvironmentHandler::SetArrow( vtkPolyData* input )
{
   this->arrow = input;
}

cfdSoundHandler* cfdEnvironmentHandler::GetSoundHandler( void )
{
   return _soundHandler;
}

cfdTeacher* cfdEnvironmentHandler::GetTeacher( void )
{
   return _teacher;
}

void cfdEnvironmentHandler::InitScene( void )
{
   std::cout << "| ***************************************************************** |" << std::endl;
   // Needs to be set by the gui fix later
   this->nav->Initialize( 0.05f, this->worldDCS );
   this->nav->SetWorldLocation( this->nav->worldTrans );
   this->worldDCS->SetScaleArray( this->worldScale );

   for ( int i = 0; i < 3; i++)
   {
      this->nav->worldTrans[ i ] = this->worldTrans[ i ];
      this->nav->worldRot[ i ] = this->worldRot[ i ];
   }
   
   float tempArray[ 3 ];
   tempArray[ 0 ] = -this->nav->worldTrans[ 0 ];
   tempArray[ 1 ] = -this->nav->worldTrans[ 1 ];
   tempArray[ 2 ] = -this->nav->worldTrans[ 2 ];
   this->worldDCS->SetTranslationArray( tempArray );

   this->worldDCS->SetRotationArray( this->nav->worldRot );

   // Maybe need to fix this later
   //this->cursorId = NONE;

   //
   // Initiate cursors.
   //
   std::cout << "|  8. Initializing................................. Virtual cursors |" << std::endl;
   this->cursor = new cfdCursor( this->arrow, this->worldDCS, this->rootNode );
   this->cursor->Initialize( this->nav->GetCursorLocation(),this->nav->GetDirection() );

   //
   // Initiate quatcam
   //
   std::cout << "|  9. Initializing..................................... cfdQuatCams |" << std::endl;
   this->_camHandler = new cfdQuatCamHandler( this->worldDCS, this->nav, _param );

   //
   // Initiate quatcam
   //
   std::cout << "| 10. Initializing................................... Sound Handler |" << std::endl;
   this->_soundHandler = new cfdSoundHandler( _param );

   //
   // Initiate the Performer Stored Binary objects.
   //
   std::cout << "| 11. Initializing...................................... pfBinaries |" << std::endl;
   this->_teacher = new cfdTeacher( "STORED_FILES", this->worldDCS );
}

void cfdEnvironmentHandler::PreFrameUpdate( void )
{
   // Update Navigation variables
   
   this->nav->SetDataValues( _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ), 
                        _commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) );
   this->nav->updateNavigationFromGUI();
	
   // Need to get these values from the appropriate classes
   // fix later	
   // the cursor will be active (based on the cursor id)
	int cursorId;
   this->cursor->Update( cursorId, this->nav->GetCursorLocation(),
                           this->nav->GetDirection(), this->nav->worldTrans );

   if ( cursorId == CUBE)
   {
       this->cursor->getExtent( this->cur_box );   //record current box cursor position
   }

   _camHandler->CheckCommandId( _commandArray );
   _soundHandler->CheckCommandId( _commandArray );
   _teacher->CheckCommandId( _commandArray );
}

void cfdEnvironmentHandler::CreateObjects( void )
{  
   int numObjects;
   char text[ 256 ];
   std::ifstream input;
   input.open( this->_param );
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 11 )
      {
         vprDEBUG(vprDBG_ALL,0) << " World DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;
         _readParam->read_pf_DCS_parameters( input, 
                        this->worldScale, this->worldTrans, this->worldRot );
      }
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

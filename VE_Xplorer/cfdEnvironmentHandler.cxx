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

#include <vrj/Util/Debug.h>

#include <fstream>
#include <cstdlib>

cfdEnvironmentHandler::cfdEnvironmentHandler( char* filename )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdApp::init" << std::endl << vprDEBUG_FLUSH;
   std::cout << "|  4. Initializing.............................. Navigation systems |" << std::endl;
   this->nav = new cfdNavigate();
   _param = filename;
   _readParam = new cfdReadParam( NULL );
}

cfdEnvironmentHandler::~cfdEnvironmentHandler( void )
{
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
   //this->cfdId = -1;

   //
   // Initiate cursors.
   //
   std::cout << "|  8. Initializing................................. Virtual cursors |" << std::endl;
   this->cursor = new cfdCursor( this->arrow, this->worldDCS, this->rootNode );
   this->cursor->Initialize( this->nav->GetCursorLocation(),this->nav->GetDirection() );

   //
   // Initiate the threads.
   //
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

   if ( this->nav->digital[4]->getData() == gadget::Digital::TOGGLE_ON 
                                          || _commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL )
   { 
      // This code will need to be thought about later
      // Must fix this to clear the tree appropriately
      //reset button is triggered so delete all the objects in the scene
/*      for ( int i = 0; i < (int)this->dataList.size(); i++ )  
      {
         if ( this->dataList[ i ]->GetcfdGeode() != NULL )
         {
            vprDEBUG(vprDBG_ALL,2) << "RemoveGeodeFromDCS"
                                   << std::endl << vprDEBUG_FLUSH;
            this->dataList[ i ]->RemovecfdGeodeFromDCS();
         }
         else if ( this->dataList[ i ]->GetSequence() != NULL )
         {
            vprDEBUG(vprDBG_ALL,2) << "stop the sequence and set to NULL"
                                   << std::endl << vprDEBUG_FLUSH;
            // stop the sequence and set the active sequence to NULL
            this->dataList[ i ]->GetSequence()->StopSequence();
            // disconnect transient data from the graph
            this->dataList[ i ]->GetSequence()->ClearSequence();
            // don't update progress bar any more
            this->activeSequenceObject = NULL;  
         }
         this->dataList[ i ]->SetUpdateFlag( false );
      }

      if ( this->pfb_count != 0 )
      {
         if ( this->teacher->GetcfdDCS()->GetNumChildren() != 0 )
         {
            this->teacher->GetcfdDCS()->RemoveChild( 
                                  this->teacher->GetcfdDCS()->GetChild( 0 ) );
         }
      }

      // change all transparent geometries back to opaque
      for( int q = 0; q < this->paramReader->numGeoms; q++)
      {
         if( this->geomL[q]->transparent )
            this->geomL[q]->setOpac( 1.0 );
      }

      // HACK follows: need to do better at adding and removing cfdObjects
      // double check that no garbage is left on the scene graph...
      vprDEBUG(vprDBG_ALL,1) << "after first CLEAR_ALL attempt, ..." 
                             << std::endl << vprDEBUG_FLUSH;

      // Need to fix this later
      //clearGeodesFromNode( this->worldDCS );

      this->useLastSource = 0;
    
      this->setId( -1 );*/
   }
   // Update Scalar Bar on the scene graph
   // PLEASE NOTE !!!!!!!!!!!!!!!!
   // Only update the scene graph in pre or post frame 
   // not in intraParallelThread or in intraFrame
   /*if ( this->isTimeToUpdateScalarBar ) 
   {
      // probably need to move the scalar bar class into this handler 
      // must fix this soon
      RefreshScalarBar();
      this->isTimeToUpdateScalarBar = false;
   }   */
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

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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Xplorer/Utilities/fileIO.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include "VE_Xplorer/XplorerHandlers/cfdTrackball.h"
#include "VE_Xplorer/XplorerHandlers/cfdSoundHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdCursor.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Xplorer/XplorerHandlers/cfdTeacher.h"
#include "VE_Xplorer/XplorerHandlers/cfdSoundHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdQuatCamHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdDisplaySettings.h"
#include "VE_Xplorer/XplorerHandlers/ChangeCursorEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/StoredSceneEH.h"
#include "VE_Xplorer/XplorerHandlers/ChangeWorkingDirectoryEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/ChangeBackgroundColorEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#ifdef _OSG
#include "VE_Xplorer/XplorerHandlers/cfdObjectHandler.h"
#endif

#include <fstream>
#include <cstdlib>

vprSingletonImp( VE_Xplorer::cfdEnvironmentHandler );

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

cfdEnvironmentHandler::cfdEnvironmentHandler( void )
{
   nav = 0;
   trackball = 0;
   _teacher       = 0;
   _soundHandler  = 0;
   //_camHandler    = 0;
   cursor = 0;
   _param.erase();//         = 0;
   _commandArray  = 0;
   _readParam     = 0;
   arrow          = 0;
   displaySettings = 0;
   for ( unsigned int i = 0; i < 3; i++ )
   {
      worldScale[ i ] = 1.0f;
      worldTrans[ i ] = 0.0f;
      worldRot[ i ] = 0.0f;
   }
   this->nav = 0;
	this->trackball=0;
#ifdef VE_PATENTED
#ifdef _OSG
   this->objectHandler = 0;
   _activeGeomPicking = false;
#endif // _OSG
#endif // VE_PATENTED
   _readParam = 0;
   _param.erase();// = 0;
   desktopWidth = 0;
   desktopHeight = 0;

   _clearColor.push_back(0);
   _clearColor.push_back(0);
   _clearColor.push_back(0);
   _updateBackgroundColor = false;

   _eventHandlers[ std::string("VISUALIZATION_SETTINGS") ] = new VE_EVENTS::ChangeCursorEventHandler();
   _eventHandlers[ std::string("Stored Scenes") ] = new VE_EVENTS::StoredSceneEventHandler();
   _eventHandlers[ std::string("Change Working Directory") ] = new VE_EVENTS::ChangeWorkingDirectoryEventHandler();
   _eventHandlers[ std::string("CHANGE_BACKGROUND_COLOR") ] = new VE_EVENTS::ChangeBackgroundColorEventHandler();
}

void cfdEnvironmentHandler::Initialize( std::string param )
{
   _param = param;
   vprDEBUG(vesDBG,1) << "|\tcfdApp::init" << std::endl << vprDEBUG_FLUSH;
   std::cout << "|  7. Initializing.............................. Navigation systems |" << std::endl;
   displaySettings = new cfdDisplaySettings();
   this->nav = new cfdNavigate();
	this->trackball = new cfdTrackball();
   //_readParam = new cfdReadParam();
   this->arrow = cfdModelHandler::instance()->GetArrow();
   this->keyboard_mouse = new KeyboardMouse();
   //CreateObjects();

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

	if ( this->trackball )
	{
		vprDEBUG(vesDBG,2)
			<< "|		  deleting this->trackball" << std::endl << vprDEBUG_FLUSH;
      delete this->trackball;
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

   if(VE_Xplorer::cfdQuatCamHandler::instance())
   {
      VE_Xplorer::cfdQuatCamHandler::instance()->CleanUp();
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

   if ( this->keyboard_mouse)
   {
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->keyboard_mouse" << std::endl << vprDEBUG_FLUSH;
      delete this->keyboard_mouse;
   }

   delete displaySettings;
}
////////////////////////////////////////////////////
bool cfdEnvironmentHandler::BackgroundColorChanged()
{
   return _updateBackgroundColor;
}
/////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetCommandArray( cfdCommandArray* input )
{
   _commandArray = input;
}
////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::ResetBackgroundColorUpdateFlag()
{
   _updateBackgroundColor = false;
}
////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetBackgroundColor(std::vector<double> color)
{
   _clearColor.clear();
   for(size_t i =0; i < color.size(); i++)
   {
      _clearColor.push_back(static_cast<float>(color.at(i)));
   }
   _updateBackgroundColor = true;
}
/////////////////////////////////////////////////////////////////////
cfdSoundHandler* cfdEnvironmentHandler::GetSoundHandler( void )
{
   return _soundHandler;
}
/////////////////////////////////////////////////////////////////////
cfdTeacher* cfdEnvironmentHandler::GetTeacher( void )
{
   return _teacher;
}
/////////////////////////////////////////////////////////////////////
/*cfdQuatCamHandler* cfdEnvironmentHandler::GetQuatCamHandler( void )
{
   return _camHandler;
}*/
/////////////////////////////////////////////////////////////
std::vector<float> cfdEnvironmentHandler::GetBackgroundColor()
{
   return _clearColor;
}
/////////////////////////////////////////////////////////////////////
cfdDisplaySettings* cfdEnvironmentHandler::GetDisplaySettings( void )
{
   return displaySettings;
}
/////////////////////////////////////////////////////////////////////
cfdNavigate* cfdEnvironmentHandler::GetNavigate( void )
{
   return this->nav;
}
/////////////////////////////////////////////////////////////////////
cfdTrackball* cfdEnvironmentHandler::GetTrackball( void )
{
	return this->trackball;
}
/////////////////////////////////////////////////////////////////////


KeyboardMouse* cfdEnvironmentHandler::GetKeyboardMouse( void )
{
   return this->keyboard_mouse;
}


/////////////////////////////////////////////////////////////////////
cfdCursor* cfdEnvironmentHandler::GetCursor( void )
{
   return this->cursor;
}
/////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetDesktopSize( int width, int height )
{
   desktopWidth = width;
   desktopHeight = height;
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
   /*std::cout << "|  9. Initializing..................................... cfdQuatCams |" << std::endl;
   this->_camHandler = new cfdQuatCamHandler( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS(),
                                          , _param.c_str() );*/
   VE_Xplorer::cfdQuatCamHandler::instance()->SetNavigation(this->nav);
   VE_Xplorer::cfdQuatCamHandler::instance()->SetDCS(VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS());

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
   if ( ( desktopWidth > 0 ) && ( desktopHeight > 0 ) )
   {
      std::cout << "| 11. Initializing................................  Desktop Display |" << std::endl;
      // Create the command and data value pairs
      // to adjust the desktop settings.
      VE_XML::DataValuePair* dvpDesktopWidth = new VE_XML::DataValuePair( std::string("FLOAT") );
      dvpDesktopWidth->SetDataName( "desktop_width" );
      dvpDesktopWidth->SetDataValue( static_cast< double >( desktopWidth ) );
      VE_XML::DataValuePair* dvpDesktopHeight = new VE_XML::DataValuePair( std::string("FLOAT") );
      dvpDesktopHeight->SetDataName( "desktop_height" );
      dvpDesktopHeight->SetDataValue( static_cast< double >( desktopHeight ) );
      VE_XML::Command* displayCommand = new VE_XML::Command();
      displayCommand->SetCommandName( std::string("Juggler_Desktop_Data") );
      displayCommand->AddDataValuePair( dvpDesktopWidth );
      displayCommand->AddDataValuePair( dvpDesktopHeight );
      displaySettings->SetVECommand( displayCommand );
      displaySettings->CheckCommandId( NULL );
      delete displayCommand;
   }
}

void cfdEnvironmentHandler::PreFrameUpdate( void )
{
	// Update Navigation variables   
   vprDEBUG(vesDBG,3) << "|\tcfdEnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;

   std::map<std::string,VE_EVENTS::EventHandler*>::iterator currentEventHandler;
   if( cfdModelHandler::instance()->GetXMLCommand() )
   {
      vprDEBUG(vesDBG,3) << "Command Name : "
                           << cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() 
                           << std::endl<< vprDEBUG_FLUSH;
      currentEventHandler = _eventHandlers.find( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() );
      if(currentEventHandler != _eventHandlers.end())
      {
         vprDEBUG(vesDBG,1) << "Executing: "
                              << cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() 
                              << std::endl<< vprDEBUG_FLUSH;
         currentEventHandler->second->SetGlobalBaseObject();
         currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
      }
   }



   
   // Update "Keyboard" and "Mouse" events
   keyboard_mouse->preFrame();

   static int nav_mode=1;

   // Use spacebar( "113" ) to cycle through navigation modes
   if(keyboard_mouse->GetKey()==113){
      nav_mode++;

      if(nav_mode==2){
         nav_mode=0;
      }

      keyboard_mouse->SetKey(-1);
   }

   // Use trackball if nav_mode==1
   if(nav_mode==1){
      // Update Trackball
	   trackball->Matrix();
      
      //std::cout<<"Trackball is active!"<<std::endl;
   }



	
#ifdef _OSG
#ifdef VE_PATENTED
   this->objectHandler->UpdateObjectHandler();

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
   if( cfdModelHandler::instance()->GetActiveModel() )
   {
      this->cursor->SetActiveDataSet( cfdModelHandler::instance()->GetActiveModel()->GetActiveDataSet() );
      this->cursor->SetVECommand( cfdModelHandler::instance()->GetActiveModel()->GetVECommand() );
      this->cursor->CheckCommandId( _commandArray );
   }
   this->cursor->Update( this->nav->GetCursorLocation(),
                           this->nav->GetDirection(), this->nav->worldTrans );

   // fix later	
   /*if ( cursorId == CUBE)
   {
       this->cursor->getExtent( this->cur_box );   //record current box cursor position
   }
*/
   VE_Xplorer::cfdQuatCamHandler::instance()->CheckCommandId( _commandArray );
   _soundHandler->CheckCommandId( _commandArray );
   _teacher->CheckCommandId( _commandArray );
   displaySettings->CheckCommandId( _commandArray );
   VE_Xplorer::cfdQuatCamHandler::instance()->PreFrameUpdate();
}
///////////////////////////////////////////////////////////////
//Set values for trackball
void cfdEnvironmentHandler::SetWindowDimensions(unsigned int w, unsigned int h)
{
	_windowWidth = w;
	_windowHeight = h;
}

void cfdEnvironmentHandler::SetFrustumValues(float _top,float _bottom,float _near){
	_frustumTop=_top;
	_frustumBottom=_bottom;
	_frustumNear=_near;
}
///////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::PostFrameUpdate()
{
	//Update the values in the trackball
	trackball->Reshape(_windowWidth,_windowHeight);
	trackball->SetFOVy(_frustumTop,_frustumBottom,_frustumNear);
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
         vprDEBUG(vesDBG,0) << " World DCS parameters:"
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

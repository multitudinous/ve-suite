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
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"

#include "VE_Xplorer/Utilities/fileIO.h"

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
//#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
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
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include "VE_Xplorer/XplorerHandlers/cfdDisplaySettings.h"
#include "VE_Xplorer/XplorerHandlers/ChangeCursorEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/StoredSceneEH.h"
#include "VE_Xplorer/XplorerHandlers/ChangeWorkingDirectoryEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/ChangeBackgroundColorEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DisplayInformation.h"
#include "VE_Xplorer/XplorerHandlers/DisplayEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/ViewEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/PhysicsSimulationEventHandler.h"
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/SeedPointActivateEH.h"
#include "VE_Xplorer/XplorerHandlers/SPBoundEH.h"
#include "VE_Xplorer/XplorerHandlers/SPDimensionsEH.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#ifdef _OSG
   #include "VE_Xplorer/XplorerHandlers/cfdObjectHandler.h"
#endif

/// C/C++ libraries
#include <fstream>
#include <cstdlib>

vprSingletonImpLifetime( VE_Xplorer::cfdEnvironmentHandler, 120 );

using namespace VE_Xplorer;
using namespace VE_SceneGraph;
using namespace VE_Util;

////////////////////////////////////////////////////////////////////////////////
cfdEnvironmentHandler::cfdEnvironmentHandler( void )
{
   _teacher = 0;
   //_soundHandler = 0;
   //_camHandler = 0;
   cursor = 0;
   _param.erase();// = 0;
   _commandArray = 0;
   _readParam = 0;
   arrow = 0;
   displaySettings = 0;

   _frustumLeft = 0;
   _frustumRight = 0;
   _frustumTop = 0;
   _frustumBottom = 0;
   _frustumNear = 0;
   _frustumFar = 0;


   for( unsigned int i = 0; i < 3; i++ )
   {
      worldScale[ i ] = 1.0f;
      worldTrans[ i ] = 0.0f;
      worldRot[ i ] = 0.0f;
   }

    display_information = 0;
    this->objectHandler = 0;
    _activeGeomPicking = false;

   _readParam = 0;
   _param.erase();// = 0;
   desktopWidth = 0;
   desktopHeight = 0;

   ///create seed points drawable
   _seedPoints = new VE_Xplorer::SeedPoints(4,4,1);
   _seedPoints->Toggle(false);
   
   ///add a transform for manipulation of the seed points to sync with the active dataset
   _seedPointsDCS = new VE_SceneGraph::DCS();
   _seedPointsDCS->SetName("Seed Points DCS");
   _seedPointsDCS->addChild(_seedPoints.get());

   _eventHandlers[ std::string( "PHYSICS_SIMULATION" ) ] = new VE_EVENTS::PhysicsSimulationEventHandler();
   _eventHandlers[ std::string( "VIEW_SELECTION" ) ] = new VE_EVENTS::ViewEventHandler();
   _eventHandlers[ std::string( "VISUALIZATION_SETTINGS" ) ] = new VE_EVENTS::ChangeCursorEventHandler();
   _eventHandlers[ std::string( "Stored Scenes" ) ] = new VE_EVENTS::StoredSceneEventHandler();
   _eventHandlers[ std::string( "Change Working Directory" ) ] = new VE_EVENTS::ChangeWorkingDirectoryEventHandler();
   _eventHandlers[ std::string( "CHANGE_BACKGROUND_COLOR" ) ] = new VE_EVENTS::ChangeBackgroundColorEventHandler();
   _eventHandlers[ std::string( "DISPLAY_SELECTION" ) ] = new VE_EVENTS::DisplayEventHandler();
   _eventHandlers[ std::string( "Display Seed Points" ) ] = new VE_EVENTS::SeedPointActivateEventHandler();
   _eventHandlers[ std::string( "Seed Points Bounds" ) ] = new VE_EVENTS::SeedPointBoundsEventHandler();
   _eventHandlers[ std::string( "Seed Points Dimensions" ) ] = new VE_EVENTS::SeedPointDimensionsEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::Initialize( void )
{
   std::cout << "|  7. Initializing.............................. Navigation systems |" << std::endl;
   displaySettings = new cfdDisplaySettings();

   this->arrow = cfdModelHandler::instance()->GetArrow();
   this->objectHandler = cfdObjectHandler::instance();
}
////////////////////////////////////////////////////////////////////////////////
cfdEnvironmentHandler::~cfdEnvironmentHandler( void )
{
   if ( this->_readParam )
   {  
      //vprDEBUG(vesDBG,2)  
      //  << "|       deleting this->_readParam" << std::endl << vprDEBUG_FLUSH;
      delete this->_readParam;
   }

   if ( this->cursor )
   {  
      //vprDEBUG(vesDBG,2)  
      //  << "|       deleting this->cursor" << std::endl << vprDEBUG_FLUSH;
      delete this->cursor;
   }

   //if(VE_Xplorer::cfdQuatCamHandler::instance())
   {
      VE_Xplorer::cfdQuatCamHandler::instance()->CleanUp();
   }

   /*if ( this->_soundHandler )
   {  
      vprDEBUG(vesDBG,2)  
        << "|       deleting this->_soundHandler" << std::endl << vprDEBUG_FLUSH;
      delete this->_soundHandler;
   }*/

   if ( this->_teacher )
   {  
      //vprDEBUG(vesDBG,2)  
      //  << "|       deleting this->_teacher" << std::endl << vprDEBUG_FLUSH;
      delete this->_teacher;
   }

   if ( this->displaySettings )
   {
      //vprDEBUG(vesDBG,2)  
      //  << "|       deleting this->displaySettings" << std::endl << vprDEBUG_FLUSH;
      delete this->displaySettings;
   }

   if ( this->display_information )
   {
      //vprDEBUG(vesDBG,2)  
      //  << "|       deleting this->display_information" << std::endl << vprDEBUG_FLUSH;
      delete this->display_information;
   }

   //Delete all the devices in DeviceHandler
   VE_Xplorer::DeviceHandler::instance()->CleanUp();
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetCommandArray( cfdCommandArray* input )
{
   _commandArray = input;
}
////////////////////////////////////////////////////////////////////////////////
/*cfdSoundHandler* cfdEnvironmentHandler::GetSoundHandler( void )
{
   return _soundHandler;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdTeacher* cfdEnvironmentHandler::GetTeacher( void )
{
   return _teacher;
}
////////////////////////////////////////////////////////////////////////////////
/*cfdQuatCamHandler* cfdEnvironmentHandler::GetQuatCamHandler( void )
{
   return _camHandler;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdDisplaySettings* cfdEnvironmentHandler::GetDisplaySettings( void )
{
   return displaySettings;
}
////////////////////////////////////////////////////////////////////////////////
cfdCursor* cfdEnvironmentHandler::GetCursor( void )
{
   return this->cursor;
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
DisplayInformation* cfdEnvironmentHandler::GetDisplayInformation( void )
{
   return this->display_information;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetDesktopSize( int width, int height )
{
   desktopWidth = width;
   desktopHeight = height;
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::GetDesktopSize( int &width, int &height )
{
    width = desktopWidth;
    height = desktopHeight;
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::InitScene( void )
{
   std::cout << "| ***************************************************************** |" << std::endl;
   //
   // Initiate cursors.
   //
   std::cout << "| 8. Initializing................................. Virtual cursors |" << std::endl;
   this->cursor = new cfdCursor( this->arrow, 
                             VE_SceneGraph::SceneManager::instance()->GetWorldDCS(), 
                             VE_SceneGraph::SceneManager::instance()->GetRootNode() );
   //this->cursor->Initialize( NULL, NULL );

   //
   // Initiate quatcam
   //
   std::cout << "| 9. Initializing..................................... cfdQuatCams |" << std::endl;
   VE_Xplorer::cfdQuatCamHandler::instance()->SetDCS(VE_SceneGraph::SceneManager::instance()->GetWorldDCS());

   //
   // Initiate quatcam
   //
   //std::cout << "| 10. Initializing................................... Sound Handler |" << std::endl;
   //this->_soundHandler = new cfdSoundHandler( /*_param.c_str()*/ );

   //
   // Initiate the Performer Stored Binary objects.
   //
   std::cout << "| 11. Initializing...................................... pfBinaries |" << std::endl;
   this->_teacher = new cfdTeacher( std::string("STORED_FILES"), 
                                 VE_SceneGraph::SceneManager::instance()->GetWorldDCS() );

   //this->objectHandler->Initialize( NULL );

   if( ( desktopWidth > 0 ) && ( desktopHeight > 0 ) )
   {
      std::cout << "| 12. Initializing................................  Desktop Display |" << std::endl;
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

   //
   // Initialize DisplayInformation
   //
   std::cout << "| 13. Initializing................................. Virtual cursors |" << std::endl;
   this->display_information = new VE_Xplorer::DisplayInformation;
   std::pair< int, int > screenDims = displaySettings->GetScreenResolution();
	this->display_information->SetDisplayPositions( screenDims.first, screenDims.second );

   static_cast< VE_Xplorer::KeyboardMouse* >( 
         VE_Xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->
         SetWindowValues( screenDims.first, screenDims.second );
   static_cast< VE_Xplorer::KeyboardMouse* >( 
         VE_Xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->
         SetScreenCornerValues( displaySettings->GetScreenCornerValues() );
}
////////////////////////////////////////////////////////////////////////////////
//This function sets the dcs based on any input device
//(i.e) trackball, wand, gui nav,...
void cfdEnvironmentHandler::PreFrameUpdate( void )
{
   //Process all events for active device
   VE_Xplorer::DeviceHandler::instance()->ProcessDeviceEvents();

   VE_Xplorer::cfdQuatCamHandler::instance()->CheckCommandId( _commandArray );
   VE_Xplorer::cfdQuatCamHandler::instance()->PreFrameUpdate();

}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::LatePreFrameUpdate()
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

   displaySettings->CheckCommandId( _commandArray );
	display_information->LatePreFrame();
   vprDEBUG(vesDBG,3) << "|\tEnd cfdEnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetWindowDimensions(unsigned int w, unsigned int h)
{
	_windowWidth = w;
	_windowHeight = h;
}

void cfdEnvironmentHandler::SetFrustumValues( float _left, float _right, float _top, float _bottom, float _near, float _far )
{
   _frustumLeft = _left;
   _frustumRight = _right;
	_frustumTop = _top;
	_frustumBottom = _bottom;
	_frustumNear = _near;
   _frustumFar = _far;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdEnvironmentHandler::GetWindowWidth()
{
   return _windowWidth;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdEnvironmentHandler::GetWindowHeight()
{
   return _windowHeight;
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::SetFrameRate( float value )
{
	framerate = value;
}
////////////////////////////////////////////////////////////////////////////////
float cfdEnvironmentHandler::GetFrameRate()
{
	return framerate;
}
////////////////////////////////////////////////////////////////////////////////
void cfdEnvironmentHandler::PostFrameUpdate()
{
	//Update the values in trackball
	static_cast< VE_Xplorer::KeyboardMouse* >( 
        VE_Xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->
        SetFrustumValues( _frustumLeft, _frustumRight, _frustumTop, 
            _frustumBottom, _frustumNear, _frustumFar );
}
////////////////////////////////////////////////////////////////////////////////
VE_Xplorer::SeedPoints* cfdEnvironmentHandler::GetSeedPoints()
{
	if(_seedPoints.valid())
	{
		return _seedPoints.get();
	}

	return 0;
}
////////////////////////////////////////////////////////////////////////////////
VE_SceneGraph::DCS* cfdEnvironmentHandler::GetSeedPointsDCS()
{
	if(_seedPointsDCS.valid())
	{
		return _seedPointsDCS.get();
	}
	return 0;
}
////////////////////////////////////////////////////////////////////////////////
/*void cfdEnvironmentHandler::CreateObjects( void )
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
      else
      {
         // Skip past block
         _readParam->ContinueRead( input, id );
      }
   }
}
*/
////////////////////////////////////////////////////////////////////////////////


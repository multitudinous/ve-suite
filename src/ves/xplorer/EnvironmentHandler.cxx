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
#include <ves/xplorer/EnvironmentHandler.h>


#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/Debug.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/PhysicsSimulationEventHandler.h>

#include <ves/xplorer/device/cfdCursor.h>
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/environment/cfdSoundHandler.h>
#include <ves/xplorer/environment/cfdEnum.h>
#include <ves/xplorer/environment/cfdTeacher.h>
#include <ves/xplorer/environment/cfdQuatCamHandler.h>
#include <ves/xplorer/environment/cfdDisplaySettings.h>
#include <ves/xplorer/environment/DisplayInformation.h>

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/event/data/SeedPointActivateEH.h>
#include <ves/xplorer/event/data/SPBoundEH.h>
#include <ves/xplorer/event/data/SPDimensionsEH.h>
#include <ves/xplorer/event/device/ChangeCursorEventHandler.h>
#include <ves/xplorer/event/environment/StoredSceneEH.h>
#include <ves/xplorer/event/environment/ChangeWorkingDirectoryEventHandler.h>
#include <ves/xplorer/event/environment/ChangeBackgroundColorEventHandler.h>
#include <ves/xplorer/event/environment/DisplayEventHandler.h>
#include <ves/xplorer/event/environment/ViewEventHandler.h>
#include <ves/xplorer/event/environment/ExportDOTFileEventHandler.h>
#include <ves/xplorer/event/environment/EphemerisDataEventHandler.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/Group.h>

#include <ves/xplorer/util/fileIO.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <osgEphemeris/EphemerisModel>
#include <osg/Vec3f>
#include <osg/BoundingSphere>
#include <osg/Group>

/// C/C++ libraries
#include <fstream>
#include <cstdlib>

vprSingletonImpLifetime( ves::xplorer::EnvironmentHandler, 11 );

using namespace ves::xplorer::scenegraph;
using namespace ves::xplorer::util;

namespace ves
{
namespace xplorer
{
EnvironmentHandler::EnvironmentHandler( void )
{
   _teacher = 0;
   //_soundHandler = 0;
   //_camHandler = 0;
   cursor = 0;
   _param.erase();// = 0;
   _commandArray = 0;
   //_readParam = 0;
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
    _activeGeomPicking = false;

   //_readParam = 0;
   _param.erase();// = 0;
   desktopWidth = 0;
   desktopHeight = 0;

   ///create seed points drawable
   _seedPoints = new ves::xplorer::SeedPoints(4,4,1);
   _seedPoints->Toggle(false);
   
   ///add a transform for manipulation of the seed points 
   ///to sync with the active dataset
   _seedPointsDCS = new ves::xplorer::scenegraph::DCS();
   _seedPointsDCS->SetName("Seed Points DCS");
   _seedPointsDCS->addChild(_seedPoints.get());

   _eventHandlers[ std::string( "PHYSICS_SIMULATION" ) ] = 
       new ves::xplorer::event::PhysicsSimulationEventHandler();
   _eventHandlers[ std::string( "VIEW_SELECTION" ) ] = 
       new ves::xplorer::event::ViewEventHandler();
   _eventHandlers[ std::string( "VISUALIZATION_SETTINGS" ) ] = 
       new ves::xplorer::event::ChangeCursorEventHandler();
   _eventHandlers[ std::string( "Stored Scenes" ) ] = 
       new ves::xplorer::event::StoredSceneEventHandler();
   _eventHandlers[ std::string( "Change Working Directory" ) ] = 
       new ves::xplorer::event::ChangeWorkingDirectoryEventHandler();
   _eventHandlers[ std::string( "CHANGE_BACKGROUND_COLOR" ) ] = 
       new ves::xplorer::event::ChangeBackgroundColorEventHandler();
   _eventHandlers[ std::string( "DISPLAY_SELECTION" ) ] = 
       new ves::xplorer::event::DisplayEventHandler();
   _eventHandlers[ std::string( "Display Seed Points" ) ] = 
       new ves::xplorer::event::SeedPointActivateEventHandler();
   _eventHandlers[ std::string( "Seed Points Bounds" ) ] = 
       new ves::xplorer::event::SeedPointBoundsEventHandler();
   _eventHandlers[ std::string( "Seed Points Dimensions" ) ] = 
       new ves::xplorer::event::SeedPointDimensionsEventHandler();
   _eventHandlers[ std::string( "DOT_FILE" ) ] = 
       new ves::xplorer::event::ExportDOTFileEventHandler();
   _eventHandlers[ std::string( "Ephemeris Data" ) ] = 
       new ves::xplorer::event::EphemerisDataEventHandler();
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::Initialize( void )
{
   displaySettings = new cfdDisplaySettings();

   this->arrow = cfdModelHandler::instance()->GetArrow();
}
////////////////////////////////////////////////////////////////////////////////
EnvironmentHandler::~EnvironmentHandler( void )
{
   if ( this->cursor )
   {  
      //vprDEBUG(vesDBG,2)  
      //  << "|       deleting this->cursor" << std::endl << vprDEBUG_FLUSH;
      delete this->cursor;
   }

   //if(ves::xplorer::cfdQuatCamHandler::instance())
   {
      //ves::xplorer::cfdQuatCamHandler::instance()->CleanUp();
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
   //ves::xplorer::DeviceHandler::instance()->CleanUp();
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetCommandArray( cfdCommandArray* input )
{
   _commandArray = input;
}
////////////////////////////////////////////////////////////////////////////////
/*cfdSoundHandler* EnvironmentHandler::GetSoundHandler( void )
{
   return _soundHandler;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdTeacher* EnvironmentHandler::GetTeacher( void )
{
   return _teacher;
}
////////////////////////////////////////////////////////////////////////////////
/*cfdQuatCamHandler* EnvironmentHandler::GetQuatCamHandler( void )
{
   return _camHandler;
}*/
////////////////////////////////////////////////////////////////////////////////
cfdDisplaySettings* EnvironmentHandler::GetDisplaySettings( void )
{
   return displaySettings;
}
////////////////////////////////////////////////////////////////////////////////
cfdCursor* EnvironmentHandler::GetCursor( void )
{
   return this->cursor;
}
////////////////////////////////////////////////////////////////////////////////
#ifdef _OSG
DisplayInformation* EnvironmentHandler::GetDisplayInformation( void )
{
   return this->display_information;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetDesktopSize( int width, int height )
{
   desktopWidth = width;
   desktopHeight = height;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::GetDesktopSize( int &width, int &height )
{
    width = desktopWidth;
    height = desktopHeight;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::InitScene( void )
{
   std::cout << "| ***************************************************************** |" << std::endl;
   //
   // Initiate cursors.
   //
   std::cout << "| 8. Initializing................................. Virtual cursors |" << std::endl;
   this->cursor = new cfdCursor( this->arrow, 
                             ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS(), 
                             ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode() );
   //this->cursor->Initialize( NULL, NULL );

   //
   // Initiate quatcam
   //
   std::cout << "| 9. Initializing..................................... cfdQuatCams |" << std::endl;
   ves::xplorer::cfdQuatCamHandler::instance()->SetDCS(ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS());

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
                                 ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS() );

   if( ( desktopWidth > 0 ) && ( desktopHeight > 0 ) )
   {
      std::cout << "| 12. Initializing................................  Desktop Display |" << std::endl;
      // Create the command and data value pairs
      // to adjust the desktop settings.
      ves::open::xml::DataValuePair* dvpDesktopWidth = new ves::open::xml::DataValuePair( std::string("FLOAT") );
      dvpDesktopWidth->SetDataName( "desktop_width" );
      dvpDesktopWidth->SetDataValue( static_cast< double >( desktopWidth ) );
      ves::open::xml::DataValuePair* dvpDesktopHeight = new ves::open::xml::DataValuePair( std::string("FLOAT") );
      dvpDesktopHeight->SetDataName( "desktop_height" );
      dvpDesktopHeight->SetDataValue( static_cast< double >( desktopHeight ) );
      ves::open::xml::Command* displayCommand = new ves::open::xml::Command();
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
   this->display_information = new ves::xplorer::DisplayInformation;
   std::pair< int, int > screenDims = displaySettings->GetScreenResolution();
   this->display_information->SetDisplayPositions( screenDims.first, screenDims.second );

   static_cast< ves::xplorer::KeyboardMouse* >( 
         ves::xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->
         SetWindowValues( screenDims.first, screenDims.second );
   static_cast< ves::xplorer::KeyboardMouse* >( 
         ves::xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->
         SetScreenCornerValues( displaySettings->GetScreenCornerValues() );
}
////////////////////////////////////////////////////////////////////////////////
//This function sets the dcs based on any input device
//(i.e) trackball, wand, gui nav,...
void EnvironmentHandler::PreFrameUpdate( void )
{
   //Process all events for active device
   ves::xplorer::DeviceHandler::instance()->ProcessDeviceEvents();

   ves::xplorer::cfdQuatCamHandler::instance()->CheckCommandId( _commandArray );
   ves::xplorer::cfdQuatCamHandler::instance()->PreFrameUpdate();

}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::LatePreFrameUpdate()
{
   // Update Navigation variables   
   vprDEBUG(vesDBG,3) << "|\tEnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;

   std::map<std::string,ves::xplorer::event::EventHandler*>::iterator currentEventHandler;
   if( cfdModelHandler::instance()->GetXMLCommand() )
   {
      vprDEBUG(vesDBG,3) << "|\tEnvironmentHandler::LatePreFrameUpdate Command Name : "
                           << cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() 
                           << std::endl<< vprDEBUG_FLUSH;
      currentEventHandler = _eventHandlers.find( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() );
      if(currentEventHandler != _eventHandlers.end())
      {
         vprDEBUG(vesDBG,1) << "|\tEnvironmentHandler::LatePreFrameUpdate Executing: "
                              << cfdModelHandler::instance()->GetXMLCommand()->GetCommandName() 
                              << std::endl<< vprDEBUG_FLUSH;
         currentEventHandler->second->SetGlobalBaseObject();
         currentEventHandler->second->Execute( cfdModelHandler::instance()->GetXMLCommand() );
      }
   }

   displaySettings->CheckCommandId( _commandArray );
   display_information->LatePreFrame();
   vprDEBUG(vesDBG,3) << "|\tEnd EnvironmentHandler::PreFrameUpdate " << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetWindowDimensions(unsigned int w, unsigned int h)
{
   _windowWidth = w;
   _windowHeight = h;
}

void EnvironmentHandler::SetFrustumValues( float _left, float _right, float _top, float _bottom, float _near, float _far )
{
   _frustumLeft = _left;
   _frustumRight = _right;
   _frustumTop = _top;
   _frustumBottom = _bottom;
   _frustumNear = _near;
   _frustumFar = _far;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int EnvironmentHandler::GetWindowWidth()
{
   return _windowWidth;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int EnvironmentHandler::GetWindowHeight()
{
   return _windowHeight;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::SetFrameRate( float value )
{
   framerate = value;
}
////////////////////////////////////////////////////////////////////////////////
float EnvironmentHandler::GetFrameRate()
{
   return framerate;
}
////////////////////////////////////////////////////////////////////////////////
void EnvironmentHandler::PostFrameUpdate()
{
   //Update the values in trackball
   static_cast< ves::xplorer::KeyboardMouse* >( 
        ves::xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->
        SetFrustumValues( _frustumLeft, _frustumRight, _frustumTop, 
            _frustumBottom, _frustumNear, _frustumFar );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::SeedPoints* EnvironmentHandler::GetSeedPoints()
{
   if(_seedPoints.valid())
   {
      return _seedPoints.get();
   }

   return 0;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* EnvironmentHandler::GetSeedPointsDCS()
{
    if(_seedPointsDCS.valid())
    {
        return _seedPointsDCS.get();
    }
    return 0;
}
/////////////////////////////////////////////////////////////////////////////////////////////////
osgEphemeris::EphemerisModel* EnvironmentHandler::GetEphemerisModel(bool createIfDoesNotExist)
{
    if(!m_ephemerisModel.valid() && createIfDoesNotExist)
    {
        m_ephemerisModel = new osgEphemeris::EphemerisModel();
      osg::ref_ptr<osg::Group> worldDCSParent = 
         ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->GetParent(0);

        /*if(worldDCS->getBound().valid())
        {
            m_ephemerisModel->setSkyDomeRadius( worldDCS->getBound().radius()*2 );
            m_ephemerisModel->setSkyDomeCenter( worldDCS->getBound().center() );
         m_ephemerisModel->setMembers(osgEphemeris::EphemerisModel::ALL_MEMBERS);
        }
        else*/
        {
            m_ephemerisModel->setSkyDomeRadius( 5000. );
            m_ephemerisModel->setAutoDateTime(false);
            m_ephemerisModel->setSkyDomeCenter( osg::Vec3f(0.,0.,0.) );
            m_ephemerisModel->setSunLightNum(2);
            m_ephemerisModel->setMembers(osgEphemeris::EphemerisModel::ALL_MEMBERS);
        }
        worldDCSParent->addChild(m_ephemerisModel.get()); 
    }
    return m_ephemerisModel.get();
}
////////////////////////////////////////////////////////////////////////////////
/*void EnvironmentHandler::CreateObjects( void )
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

} // end xplorer
} // end ves

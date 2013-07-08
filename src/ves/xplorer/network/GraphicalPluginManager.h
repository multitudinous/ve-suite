/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef VES_XPLORER_NETWORK_GraphicalPluginManager_H
#define VES_XPLORER_NETWORK_GraphicalPluginManager_H

/*!\file GraphicalPluginManager.h
 * GraphicalPluginManager API
 * \class ves::xplorer::GraphicalPluginManager
 *
 */

// --- VE-Suite Includes --- //
#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/plugin/PluginBasePtr.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/model/SystemPtr.h>

#include <switchwire/ScopedConnectionList.h>

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>

// --- OSG Includes --- //
namespace osg
{
class Group;
}

// --- STL Includes --- //
#include <map>
#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace event
{
class EventHandler;
}

namespace scenegraph
{
class Group;
class DCS;
}

namespace network
{
class cfdVEAvailModules;
class NetworkSystemView;

class VE_XPLORER_NETWORK_EXPORTS GraphicalPluginManager : public ves::xplorer::GlobalBase
{
private:
    //Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< GraphicalPluginManager >;
    //GraphicalPluginManager(const GraphicalPluginManager& o) { ; }
    //GraphicalPluginManager& operator=(const GraphicalPluginManager& o) { ; }
    //this class should be a singleton
    ///Constructor
    GraphicalPluginManager();

    ///
    vprSingletonHeader( GraphicalPluginManager );

protected:
    ///Destructor
    virtual ~GraphicalPluginManager();

public:
    ///
    void Initialize();

    ///Functions that operate on the Executive
    void GetNetwork();

    ///
    void GetOutput( std::string name );

    ///
    void GetEverything();

    ///
    void HowToUse( std::string name );

    ///Get intial module information from the executive
    void InitModules();

    ///Update function called from within latePreFrame
    void UpdateModules();

    ///Update function called from within latePreFrame
    void PreFrameUpdate();

    ///Update function called from within latePreFrame
    void PostFrameUpdate();

    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand()
    {
        ;
    }

    ///This function returns the map of the current plugins
    ///so that evehenthandlers can manipulate the plugins while
    ///with commands from the gui
    std::map< std::string, ves::xplorer::plugin::PluginBasePtr >* GetTheCurrentPlugins();

    ///Get available plugins object
    cfdVEAvailModules* GetAvailablePlugins();

    ///Laod data from CE
    void LoadDataFromCE();

    ///Get the current network string being used by GraphicalPluginManager
    std::string GetCurrentNetwork();

    ///Set the current network string
    void SetCurrentNetwork( const std::string& network );

    ///Get the current network system view being used by GraphicalPluginManager
    NetworkSystemView* GetNetworkSystemView();

    ///Delete the network system view
    void DeleteNetworkSystemView();

    ///Unregister in the executive from the ce. This should be called before the
    /// destrucutor is called.
    void UnRegisterExecutive();

    ///Clean up plugins
    void UnloadPlugins();

    ///Discover plugins when changing directories
    void DiscoverPlugins( std::string const& fileName );

    ///Remove a plugin from the plugin map and the event handler map
    void RemovePlugin( std::string const& pluginId );

    std::map< std::string, ves::open::xml::model::SystemPtr > GetSystemsMap() const;

private:
    ///Recusive function to find all sub-systems
    void ParseSystem(
        ves::open::xml::model::SystemPtr system,
        bool getResults = false,
        osg::Group* parentNode = 0 );

    ///Loading the Available Modules
    cfdVEAvailModules* mAvailableModules;

    ///The raw xml network data from ce
    std::string veNetwork;

    ///Network buffer used to store network string between call to
    /// SetCurrentNetwork and call to GetNetwork
    std::string mNetworkBuffer;

    ///_name_map : maps a module id to it's module name.
    std::map< std::string, std::string> _id_map;

    ///map of all the systems
    std::map< std::string, ves::open::xml::model::SystemPtr > mIDToSystem;

    ///id of the top most system
    std::string mTopSystemID;

    ///_name_map : maps a module name to it's module id.
    std::map< std::string, ves::xplorer::plugin::PluginBasePtr > mPluginsMap;

    ///map to hold unique plugin command names and associated plugin pointers
    std::map< std::string, std::map< std::string, ves::xplorer::plugin::PluginBasePtr > > pluginEHMap;

    ///Network View
    NetworkSystemView* netSystemView;

    ///The event handler for commands.
    std::map< std::string, ves::xplorer::event::EventHandler* > _eventHandlers;

    ///The GUID for the executive
    std::string m_UINAME;

    ///Wire up the slots
    switchwire::ScopedConnectionList m_connections;

};
} //end network
} //end xplorer
} //end ves

#endif //VES_XPLORER_NETWORK_GraphicalPluginManager_H

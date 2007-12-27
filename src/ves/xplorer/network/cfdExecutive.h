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
#ifndef CFD_EXECUTIVE_H
#define CFD_EXECUTIVE_H
/*!\file cfdExecutive.h
cfdExecutive API
*/
/*!\class ves::xplorer::cfdExecutive
*
*/

#include <ves/xplorer/GlobalBase.h>

#include <vpr/Util/Singleton.h>

#include <ves/xplorer/scenegraph/Group.h>

#include <ves/open/xml/model/ModelPtr.h>

#include <map>
#include <string>
#include <vector>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class Group;
}
}
}

namespace ves
{
namespace xplorer
{
class cfdVjObsWrapper;
}
}

namespace ves
{
namespace xplorer
{
namespace plugin
{
class cfdVEBaseClass;
}
}
}

namespace ves
{
namespace xplorer
{
namespace event
{
class EventHandler;
}
}
}

namespace Body
{
class Executive;
}
namespace CosNaming
{
class NamingContext;
}
namespace PortableServer
{
class POA;
}

namespace ves
{
namespace xplorer
{
namespace network
{
class cfdVEAvailModules;
class Body_UI_i;
class VE_XPLORER_NETWORK_EXPORTS cfdExecutive : public ves::xplorer::GlobalBase
{
private:
    // Required so that vpr::Singleton can instantiate this class.
    //friend class vpr::Singleton< cfdExecutive >;
    //cfdExecutive(const cfdExecutive& o) { ; }
    //cfdExecutive& operator=(const cfdExecutive& o) { ; }
    // this class should be a singleton
    // constructor
    cfdExecutive():
        m_avModules( 0 ),
        ui_i( 0 ),
        naming_context( 0 ),
        _exec( 0 )
    {
        ;
    }

    // destructor
    virtual ~cfdExecutive( void );
    vprSingletonHeader( cfdExecutive );
public:
    void Initialize( CosNaming::NamingContext*, PortableServer::POA* );

    ///Functions that operate on the Executive
    void GetNetwork( void );
    void GetOutput( std::string name );
    void GetEverything( void );
    void HowToUse( std::string name );
    ///Get intial module information from the executive
    void InitModules( void );
    ///Update function called from within latePreFrame
    void UpdateModules( void );
    ///Update function called from within latePreFrame
    void PreFrameUpdate( void );
    ///Update function called from within latePreFrame
    void PostFrameUpdate( void );
    ///Function called within preFrame to allow cfdExecutive
    ///to have access to scalar information
    void UnbindORB( void );
    ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
    virtual void UpdateCommand()
    {
        ;
    }
    ///This function returns the map of the current plugins
    ///so that evehenthandlers can manipulate the plugins while
    ///with commands from the gui
    std::map<int, ves::xplorer::plugin::cfdVEBaseClass* >* GetTheCurrentPlugins( void );
    ///Get available plugins object
    cfdVEAvailModules* GetAvailablePlugins( void );
    ///Accessor for ehs to use
    Body_UI_i* GetCORBAInterface();
    ///Laod data from CE
    void LoadDataFromCE( void );
    ///Get the current network string being used by cfdExecutive
    std::string GetCurrentNetwork();
    ///Unregister in the executive from the ce. This should be called before the
    /// destrucutor is called.
    void UnRegisterExecutive();
    
private:
    ///Loading the Available Modules
    cfdVEAvailModules* m_avModules;
    std::string veNetwork;

    Body_UI_i* ui_i;
    osg::ref_ptr< ves::xplorer::scenegraph::Group > _masterNode;

    // _name_map : maps a module id to it's module name.
    std::map< int, std::string> _id_map;
    std::map< int, ves::open::xml::model::ModelPtr > idToModel;

    // _name_map : maps a module name to it's module id.
    std::map<int, ves::xplorer::plugin::cfdVEBaseClass* > _plugins;

    // map to hold unique plugin command names and associated plugin pointers
    std::map< int, std::map< std::string, ves::xplorer::plugin::cfdVEBaseClass* > > pluginEHMap;

    std::map< std::string, ves::xplorer::event::EventHandler*> _eventHandlers;///<The event handler for commands.

    ///the Computational Engine
    CosNaming::NamingContext* naming_context;
    Body::Executive* _exec;
};
}
}
}
#endif

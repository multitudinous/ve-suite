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

#ifndef VES_XPLORER_NETWORK_PLUGINLOADER_H
#define VES_XPLORER_NETWORK_PLUGINLOADER_H
/*!\file cfdVEPluginLoader.h
 *       cfdVEPluginLoader API
 * \class ves::xplorer::cfdVEPluginLoader
 *
 */

#include <vpr/DynLoad/LibraryFinder.h>

#include <map>
#include <string>
#include <ves/VEConfig.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <boost/signals2/signal.hpp>

namespace ves
{
namespace xplorer
{
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS PluginLoader
{
public:

    PluginLoader();
    ~PluginLoader();

    void LoadPlugins();
    void ScanAndLoad();
    ves::xplorer::plugin::PluginBase* CreateNewPlugin( unsigned int );

    //Load all the dlls in the given dir

    void RegisterPlugins();

    int GetNumberOfPlugins();

    ves::xplorer::plugin::PluginBase* CreateObject( std::string );

    //Keep the list of the first intance of each plugin
    std::map< int, ves::xplorer::plugin::PluginBase* > plugins;

    vpr::LibraryFinder::LibraryList libs;

    typedef boost::signals2::signal< void ( const std::string&,
                      ves::xplorer::plugin::PluginBase* ) > createPluginSignal_type;
    createPluginSignal_type m_createUIPlugin;
};
}
}
}
#endif

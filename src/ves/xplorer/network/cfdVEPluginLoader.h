/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef CFD_PLUGINLOADER_H
#define CFD_PLUGINLOADER_H
/*!\file cfdVEPluginLoader.h
cfdVEPluginLoader API
*/
/*!\class ves::xplorer::cfdVEPluginLoader
*
*/

#include <vpr/DynLoad/LibraryFinder.h>

#include <map>
#include <ves/VEConfig.h>
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
namespace network
{
class VE_XPLORER_NETWORK_EXPORTS cfdVEPluginLoader
{
public:

    cfdVEPluginLoader();
    ~cfdVEPluginLoader();

    void LoadPlugins( void );
    void ScanAndLoad( void );
    ves::xplorer::plugin::cfdVEBaseClass* CreateNewPlugin( unsigned int );

    //Load all the dlls in the given dir

    void RegisterPlugins();

    int GetNumberOfPlugins();

    ves::xplorer::plugin::cfdVEBaseClass* CreateObject( std::string );

    //Keep the list of the first intance of each plugin
    std::map< int, ves::xplorer::plugin::cfdVEBaseClass* > plugins;

    vpr::LibraryFinder::LibraryList libs;
};
}
}
}
#endif

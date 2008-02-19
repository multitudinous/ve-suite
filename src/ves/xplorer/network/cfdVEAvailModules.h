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
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CFD_AVAIL_MODULES_H
#define CFD_AVAIL_MODULES_H
/*!\file cfdVEAvailModules.h
cfdVEAvailModules API
*/
/*!\class ves::xplorer::cfdVEAvailModules
*
*/

#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace network
{
class cfdVEPluginLoader;

class VE_XPLORER_NETWORK_EXPORTS cfdVEAvailModules
{
public:
    ///Constructor
    cfdVEAvailModules( void );
    ///Destructor
    ~cfdVEAvailModules( void );
    //Load all the modules from the dlls
    bool LoadModules();
    ///Get the plugin loader to load available plugins
    cfdVEPluginLoader* GetLoader( void );
    ///Reset the plugin loader so that plugins can be
    ///loaded as required by the user
    void ResetPluginLoader( void );

protected:
    cfdVEPluginLoader* pl_loader;
};
}
}
}
#endif

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

#pragma once

#include <string>

#include <ves/xplorer/eventmanager/ScopedConnectionList.h>
#include <ves/util/SimpleDataTypeSignalSignatures.h>

namespace ves
{
namespace conductor
{
/**
 * NetworkLoader unifies the logic to load a network. This logic was previously
 * spread across AppFrame, Canvas, Network, AvailableModules, and UIPluginBase.
 * This class is intended to be completely separate of the UI, and should compile,
 * link, and be functional even in the absence of a UI library (Qt, Wx, etc.)
**/
class NetworkLoader
{
public:
    // This is the replacement constructor for this object. It is not possible
    // to create an instance on the stack. It must be created on the heap.
    // Notice the destructor is private too. This oject autodeletes when it is
    // done processing.
    static NetworkLoader* createNetworkLoader()
    {
        return new NetworkLoader;
    }

    /**
     * Load the .ves file specified by @c fileName
     * This function will activate the first model loaded by default.
    **/
    void LoadVesFile( const std::string& fileName );

private:
    ///Constuctor
    NetworkLoader();
    ///Destructor
    ~NetworkLoader();

    ///Set the active model
    void OnActiveModelChanged( const std::string& modelID );
    /// Holds the filename we're loading so we can later emit a signal
    /// indicating loading is done.
    std::string m_filename;
    ///Signals tools
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;
    ///The signal to tell whether we have a db or not
    ves::util::BoolSignal_type m_dbPresent;
    ///The signal for reseting the view
    ves::util::VoidSignal_type m_navReset;
};

} // namespace conductor
} // namespace ves

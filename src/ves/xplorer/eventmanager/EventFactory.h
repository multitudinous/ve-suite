#if 1
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
#include <map>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //

#include <switchwire/Event.h>
#include <switchwire/EventManager.h>

// --- VES includes --- //
#include <ves/VEConfig.h>
#include <ves/xplorer/Logging.h>

#include <ves/util/SimpleDataTypeSignalSignatures.h>

namespace ves
{
namespace xplorer
{
namespace eventmanager
{
class VE_EVENTMANAGER_EXPORTS EventFactory
{
public:

    /// Get the signal with name @c signalName. Calling code will have to
    /// perform an appropriate reinterpret_cast on the returned pointer.
    switchwire::EventBase* GetSignal( const std::string& signalName );

    /// Get the signal with name @c signalName and signature as specified in
    /// the template parameter. This hides the necessity for a reinterpret_cast
    /// for cleaner code. Example signature: void( int )
    template< typename Signature >
    switchwire::Event< Signature >* GetSignalBySignature( const std::string& signalName )
    {
        return reinterpret_cast< switchwire::Event< Signature >* >
                ( GetSignal( signalName ) );
    }

    /// Get the signal with name @c signalName and event type as specified in
    /// the template parameter. This hides the necessity for a reinterpret_cast
    /// for cleaner code. Example signal type:
    /// switchwire::Event< void( int ) >
    template< typename SignalType >
    SignalType* GetSignalByType( const std::string& signalName )
    {
        return reinterpret_cast< SignalType* >
                ( GetSignal( signalName ) );
    }

private:

    /// Constructor
    EventFactory();

    /// Destructor
    virtual ~EventFactory();

    /// Singleton declarations
    vprSingletonHeader( EventFactory );

    /// Holds the signals
    std::map< std::string, switchwire::EventBase* > m_signals;

    // ------------------------------------
    // Put signal type declarations here.

    /// Signal to generate deleting a viz feature
    /// The delete viz signal; pass PropertySet UUID as argument.
    ves::util::StringSignal_type m_deleteVizSignal;
    /// The add viz signal
    /// Pass PropertySet UUID as arg1, tablename as arg2
    ves::util::TwoStringSignal_type m_addVizSignal;
    /// Signal indicating a new ves file is about to be loaded.
    ves::util::StringSignal_type m_vesFileLoadingSignal;
    /// Signal indicating a new ves file has successfully loaded.
    ves::util::StringSignal_type m_vesFileLoadedSignal;
    /// Signal indicating working directory has changed
    ves::util::StringSignal_type m_workingDirChangedSignal;
    ///Delete a CAD file that is loaded
    ves::util::ThreeStringSignal_type m_deleteCADNodeSignal;
    ///Add multi body dynamics data to a cad file
    ves::util::ThreeStringSignal_type m_dynamicsDataCADNodeSignal;
    ///Change the active model in xplorer by specifing a uuid
    ves::util::StringSignal_type m_changeActiveModelSignal;
    ///
    ves::util::BoolAndDoubleVectorSignal_type m_changeBackgroundColorSignal;
	///
    ves::util::TwoDoubleVectorsSignal_type m_updateCameraSignal;
    ///Update network -- main slot lives in xplorer::network::UpdateNetworkEventHandler
    ves::util::VoidSignal_type m_updateNetworkSignal;
    /// Add a texture dataset - Pass PropertySet UUID as arg1, texture directory as arg2
    ves::util::TwoStringSignal_type m_addTBETScalarSignal;
    ///Delete a dataset 
    ves::util::StringSignal_type m_deleteDataSetSignal;
    /// Scenegraph has changed
    ves::util::VoidSignal_type m_scenegraphChangedSignal;
    /// SetNavigationData
    switchwire::Event< void( std::vector< double >&, std::vector< double >& ) >
                                                      m_setNavigationDataSignal;
    /// CenterPointUpdate
    ves::util::StringSignal_type m_CenterPointUpdate;
    /// SetResetStartPosition
    ves::util::VoidSignal_type m_setResetStartPosition;
    //--------------------------------------

    /// Logging tools
    Poco::Logger& m_logger;
    ves::xplorer::LogStreamPtr m_logStream;
};

}
}
}
#endif

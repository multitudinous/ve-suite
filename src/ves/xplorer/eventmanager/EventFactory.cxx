/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#if 1
#include "EventFactory.h"

namespace ves
{
namespace xplorer
{
namespace eventmanager
{

vprSingletonImp( EventFactory );

EventFactory::EventFactory():
    m_logger( Poco::Logger::get( "xplorer.EventFactory" ) ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    LOG_TRACE( "ctor" );

    EventManager* evm = EventManager::instance();

    //Delete viz feature
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::StringSignal_type >( &m_deleteVizSignal );
        evm->RegisterSignal( swb, "VizBasePropertySet.DeleteVizFeature",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VizBasePropertySet.DeleteVizFeature" ] = swb;
    }

    //Add viz feature
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::TwoStringSignal_type >( &m_addVizSignal );
        evm->RegisterSignal( swb, "VizBasePropertySet.AddVizFeature",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VizBasePropertySet.AddVizFeature" ] = swb;
    }

    //Ves File Loading
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::StringSignal_type >( &m_vesFileLoadingSignal );
        evm->RegisterSignal( swb, "VesFileLoading",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VesFileLoading" ] = swb;
    }

    //Ves File Loaded
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::StringSignal_type >( &m_vesFileLoadedSignal );
        evm->RegisterSignal( swb, "VesFileLoaded",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "VesFileLoaded" ] = swb;
    }
    //Working Directory has changed
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::StringSignal_type >( &m_workingDirChangedSignal );
        evm->RegisterSignal( swb, "WorkingDirectoryChanged",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "WorkingDirectoryChanged" ] = swb;
    }
    // Delete a CAD node
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::ThreeStringSignal_type >( &m_deleteCADNodeSignal );
        evm->RegisterSignal( swb, "DeleteCADNode",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "DeleteCADNode" ] = swb;
    }
    //Change the active model
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::StringSignal_type >( &m_changeActiveModelSignal );
        evm->RegisterSignal( swb, "ChangeActiveModel",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "ChangeActiveModel" ] = swb;
    }
    //Add multi body dynamics data to a CAD file
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::ThreeStringSignal_type >( &m_dynamicsDataCADNodeSignal );
        evm->RegisterSignal( swb, "CADPropertySet.CADAnimation",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "CADPropertySet.CADAnimation" ] = swb;
    }
    ///Signal for Background Color
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::BoolAndDoubleVectorSignal_type >( &m_changeBackgroundColorSignal );
        evm->RegisterSignal( swb, "PreferencesPropertySet.UsePreferredBackgroundColor",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "PreferencesPropertySet.UsePreferredBackgroundColor" ] = swb;
    }
    // Update Network
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::VoidSignal_type >( &m_updateNetworkSignal );
        evm->RegisterSignal( swb, "UpdateNetwork",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "UpdateNetwork" ] = swb;
    }
    // Add a texture dataset
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::TwoStringSignal_type >( &m_addTBETScalarSignal );
        evm->RegisterSignal( swb, "DatasetPropertySet.TBETAddScalarSignal",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "DatasetPropertySet.TBETAddScalarSignal" ] = swb;
    }

    // Scenegraph has changed
    {
        SignalWrapperBase* swb =
            new SignalWrapper< ves::util::VoidSignal_type >( &m_scenegraphChangedSignal );
        evm->RegisterSignal( swb, "ScenegraphChanged",
                             ves::xplorer::eventmanager::EventManager::unspecified_SignalType );
        m_signals[ "ScenegraphChanged" ] = swb;
    }
}

EventFactory::~EventFactory()
{

}

SignalWrapperBase* EventFactory::GetSignal( const std::string& signalName )
{
    std::map< std::string, SignalWrapperBase* >::const_iterator iter =
        m_signals.find( signalName );
    if( iter != m_signals.end() )
    {
        return iter->second;
    }
    else
    {
        return 0;
    }
}

}
}
}
#endif

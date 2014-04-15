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

    switchwire::EventManager* evm = switchwire::EventManager::instance();

    //Delete viz feature
    evm->RegisterSignal( &m_deleteVizSignal, "VizBasePropertySet.DeleteVizFeature",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "VizBasePropertySet.DeleteVizFeature" ] = &m_deleteVizSignal;


    //Add viz feature
    evm->RegisterSignal( &m_addVizSignal, "VizBasePropertySet.AddVizFeature",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "VizBasePropertySet.AddVizFeature" ] = &m_addVizSignal;


    //Ves File Loading
    evm->RegisterSignal( &m_vesFileLoadingSignal, "VesFileLoading",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "VesFileLoading" ] = &m_vesFileLoadingSignal;


    //Ves File Loaded
    evm->RegisterSignal( &m_vesFileLoadedSignal, "VesFileLoaded",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "VesFileLoaded" ] = &m_vesFileLoadedSignal;

    //Working Directory has changed
    evm->RegisterSignal( &m_workingDirChangedSignal, "WorkingDirectoryChanged",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "WorkingDirectoryChanged" ] = &m_workingDirChangedSignal;

    // Delete a CAD node
    evm->RegisterSignal( &m_deleteCADNodeSignal, "DeleteCADNode",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "DeleteCADNode" ] = &m_deleteCADNodeSignal;

    //Change the active model
    evm->RegisterSignal( &m_changeActiveModelSignal, "ChangeActiveModel",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "ChangeActiveModel" ] = &m_changeActiveModelSignal;

    //Add multi body dynamics data to a CAD file
    evm->RegisterSignal( &m_dynamicsDataCADNodeSignal, "CADPropertySet.CADAnimation",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "CADPropertySet.CADAnimation" ] = &m_dynamicsDataCADNodeSignal;

    ///Signal for Background Color
    evm->RegisterSignal( &m_changeBackgroundColorSignal, "PreferencesPropertySet.UsePreferredBackgroundColor",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "PreferencesPropertySet.UsePreferredBackgroundColor" ] = &m_changeBackgroundColorSignal;

	///Signal for Camera Update
    evm->RegisterSignal( &m_updateCameraSignal, "PreferencesPropertySet.UpdateCamera",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "PreferencesPropertySet.UpdateCamera" ] = &m_updateCameraSignal;
	
    // Update Network
    evm->RegisterSignal( &m_updateNetworkSignal, "UpdateNetwork",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "UpdateNetwork" ] = &m_updateNetworkSignal;

    // Add a texture dataset
    evm->RegisterSignal( &m_addTBETScalarSignal, "DatasetPropertySet.TBETAddScalarSignal",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "DatasetPropertySet.TBETAddScalarSignal" ] = &m_addTBETScalarSignal;

    // Delete a dataset
    evm->RegisterSignal( &m_deleteDataSetSignal, "DeleteDataSet",
                        switchwire::EventManager::unspecified_SignalType );
    m_signals[ "DeleteDataSet" ] = &m_deleteDataSetSignal;
    

    // Scenegraph has changed
    evm->RegisterSignal( &m_scenegraphChangedSignal, "ScenegraphChanged",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "ScenegraphChanged" ] = &m_scenegraphChangedSignal;

    // Set navigation data
    evm->RegisterSignal( &m_setNavigationDataSignal, "SetNavigationData",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "SetNavigationData" ] = &m_setNavigationDataSignal;

    // Center point update
    evm->RegisterSignal( &m_CenterPointUpdate, "CenterPointUpdate",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "CenterPointUpdate" ] = &m_CenterPointUpdate;

    // SetResetStartSignal
    evm->RegisterSignal( &m_setResetStartPosition, "SetResetStartPosition",
                         switchwire::EventManager::unspecified_SignalType );
    m_signals[ "SetResetStartPosition" ] = &m_setResetStartPosition;
}

EventFactory::~EventFactory()
{

}

switchwire::EventBase* EventFactory::GetSignal( const std::string& signalName )
{
    std::map< std::string, switchwire::EventBase* >::const_iterator iter =
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

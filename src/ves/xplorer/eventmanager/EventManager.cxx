/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <Poco/Data/Session.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/DataException.h>

#include <ves/xplorer/eventmanager/EventManager.h>

//#include <ves/xplorer/Debug.h>

using boost::shared_ptr;
using boost::weak_ptr;
using boost::signals2::scoped_connection;
using boost::signals2::shared_connection_block;

namespace ves
{
namespace xplorer
{
namespace eventmanager
{

vprSingletonImp( EventManager );

////////////////////////////////////////////////////////////////////////////////
EventManager::EventManager():
    mMonotonicID(0)//,
    //m_Logger( Poco::Logger::get("xplorer.EventManager") )
{
    //DECLARE_LOGGER;
    //CREATE_LOG_STREAM;
    //LOG_TRACE( "ctor" );

    // Open an in-memory database to allow efficient searches of existing signals
    Poco::Data::SQLite::Connector::registerConnector();
    mSession = new Poco::Data::Session( "SQLite", ":memory:" );
    ( *mSession ) << "CREATE TABLE signals (id INTEGER PRIMARY KEY, name TEXT, type INTEGER)",
            Poco::Data::now;

    // Create a table to store slots that have requested connection to a certain signal
    // or signal pattern that hasn't been registered yet
    ( *mSession ) << "CREATE TABLE slots (id INTEGER PRIMARY KEY, mapID INTEGER, pattern TEXT, type INTEGER, priority INTEGER)",
            Poco::Data::now;
}
////////////////////////////////////////////////////////////////////////////////
EventManager::~EventManager()
{
    //LOG_TRACE( "dtor" );

    Poco::Data::SQLite::Connector::unregisterConnector();

    // Delete all our signals
    {
        std::map<std::string, SignalWrapperBase*>::const_iterator iter = mSignals.begin();
        std::map<std::string, SignalWrapperBase*>::const_iterator max = mSignals.end();

        while( iter != max )
        {
            delete ( iter->second );
            ++iter;
        }
    }

    // Delete all our stored slots
    {
        std::map< int, SlotWrapperBase* >::const_iterator iter = mExactSlotMap.begin();
        std::map< int, SlotWrapperBase* >::const_iterator max = mExactSlotMap.end();

        while( iter != max )
        {
            delete ( iter->second );
            ++iter;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::RegisterSignal( SignalWrapperBase* sig, const std::string& sigName, SignalType sigType )
{
    //LOG_DEBUG( "RegisterSignal: " << sigName );

    // Add this signal to the lookup table
    try
    {
        bool exists = false;
        ( *mSession ) << "SELECT 1 FROM signals WHERE name=:name",
                Poco::Data::use( sigName ),
                Poco::Data::into( exists ),
                Poco::Data::now;

        if( exists )
        {
            //LOG_WARNING( "RegisterSignal: " << sigName << " will hide previous signal with same name" );
            std::string warning( "RegisterSignal: " );

            ( *mSession ) << "UPDATE signals SET type=:type WHERE name=:name",
                    Poco::Data::use( sigType ),
                    Poco::Data::use( sigName ),
                    Poco::Data::now;
        }
        else
        {
            //LOG_DEBUG( "RegisterSignal: Registering new signal " << sigName );

            ( *mSession ) << "INSERT INTO signals (name, type) VALUES (:name,:type)",
                    Poco::Data::use( sigName ),
                    Poco::Data::use( sigType ),
                    Poco::Data::now;
        }
    }
    catch( Poco::Data::DataException& ex )
    {
         //LOG_ERROR( ex.displayText() );
    }

    // Store the signal in the signal map
    mSignals[sigName] = sig;

    ConnectToPreviousSlots( sigName );
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::ConnectToPreviousSlots( const std::string& sigName )
{
    //LOG_TRACE( "ConnectToPreviousSlots" );

    std::vector< int > ids;
    std::vector< int > priorities;
    GetSlotMatches( sigName, ids, priorities );

    // Iterate through result set and attempt to connect to the matching slots
    std::vector< int >::iterator idsIter = ids.begin();
    std::vector< int >::iterator prioritiesIter = priorities.begin();
    while( idsIter != ids.end() )
    {
        SlotWrapperBase* slot = mExactSlotMap[ *idsIter ];
        weak_ptr< ScopedConnectionList > wConnectionsPtr
                = mExactSlotConnections[ *idsIter ];

        if( shared_ptr< ScopedConnectionList > sConnectionsPtr = wConnectionsPtr.lock() )
        {
           _ConnectSignal( sigName, slot, *(sConnectionsPtr.get()), *prioritiesIter, false );
        }
        else
        {
            // If we were unable to lock the weak ptr, the underlying object
            // must have been destroyed. Remove this entry from the database
            // so we don't have to deal with it in future.
            try
            {
                ( *mSession ) << "DELETE FROM slots WHERE mapID=:id",
                        Poco::Data::use( *idsIter ),
                        Poco::Data::now;
            }
            catch( Poco::Data::DataException& ex )
            {
                std::cout << ex.displayText() << std::endl;
            }

            // We can also remove this entry from mExactSlotMap and free up
            // associated memory
            std::map< int, SlotWrapperBase* >::iterator slotIter =
                    mExactSlotMap.find( *idsIter );
            if( slotIter != mExactSlotMap.end() )
            {
                delete( slotIter->second );
                mExactSlotMap.erase( slotIter );
            }
        }
        ++idsIter;
        ++prioritiesIter;
    }
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::ConnectSignal( const std::string& sigName,
                                  SlotWrapperBase* slot,
                                  ScopedConnectionList& connections,
                                  int priority )
{
    _ConnectSignal( sigName, slot, connections, priority, true );
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::_ConnectSignal( const std::string& sigName,
                                  SlotWrapperBase* slot,
                                  ScopedConnectionList& connections,
                                  int priority,
                                  bool store )
{
    //LOG_TRACE( "_ConnectSignal" );
    // Find the appropriate SignalWrapperBase
    std::map< std::string, SignalWrapperBase* >::const_iterator iter = mSignals.find( sigName );
    if( iter != mSignals.end() )
    {
        //LOG_DEBUG( "_ConnectSignal: Connecting " << slot << " to signal "
        //        << sigName << " (" << iter->second->GetSignalAddress() << ")" );
        // Tell the SignalWrapper to connect its signal to this slot
        SignalWrapperBase* signalWrapper = iter->second;
        if( signalWrapper->ConnectSlot( slot, connections, priority ) )
        {
            //LOG_DEBUG( "_ConnectSignal: Connection successful" );
            //Connection was successful; store the details
            StoreConnection( connections, signalWrapper );

            // Check whether there is currently a strong monopoly on this signal.
            // If so, immediately block the connection that was just made.
            StrongMonopolies_type::iterator mIter = mStrongMonopolies.find( signalWrapper );
            if( mIter != mStrongMonopolies.end() )
            {
                if( shared_ptr< ConnectionMonopoly > monopoly = mIter->second.lock() )
                {
                    shared_ptr< shared_connection_block >
                            blocker( new shared_connection_block( *( connections.GetLastConnection() ) ) );
                    monopoly->AddBlocker( blocker );
                }
                else
                {
                    // Monopoly must have already been ended; remove this entry.
                    mStrongMonopolies.erase( mIter );
                }
            }
        }
        else
        {
            //LOG_ERROR( "_ConnectSignal: Connection to " << sigName << " failed" );
        }
    }

    // Copy this slot off for later async connections
    if( store )
    {
        StoreSlot( sigName, slot, connections, EventManager::unspecified_SignalType, priority );
    }
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::ConnectSignals( const std::string& stringToMatch,
                                   SlotWrapperBase* slot,
                                   ScopedConnectionList& connections,
                                   SignalType sigType,
                                   int priority )
{
    //LOG_DEBUG( "ConnectSignals: " << stringToMatch << " " << slot );
    std::vector< std::string > names;
    GetMatches( stringToMatch, sigType, names );

    // Iterate through result set and attempt to connect to the matching signals
    std::vector< std::string >::iterator namesIter = names.begin();
    while( namesIter != names.end() )
    {
        // Connect to the signal, but don't store slot details for each connection
        _ConnectSignal( ( *namesIter ), slot, connections, priority, false );
        namesIter++;
    }

    // Store slot details for general pattern
    StoreSlot( stringToMatch, slot, connections, sigType, priority );
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::StoreSlot( const std::string& sigName,
                              SlotWrapperBase* slot,
                              ScopedConnectionList& connections,
                              int type,
                              int priority )
{
    //LOG_TRACE( "StoreSlot " << sigName << " " << slot );
    mExactSlotMap[ mMonotonicID ] = slot;

    mExactSlotConnections[ mMonotonicID ] = connections.GetWeakPtr();

    // Add this slot to the lookup table
    try
    {
        ( *mSession ) << "INSERT INTO slots (mapID, pattern, type, priority) VALUES (:id,:pattern,:type,:priority)",
                Poco::Data::use( mMonotonicID ),
                Poco::Data::use( sigName ),
                Poco::Data::use( type ),
                Poco::Data::use( priority ),
                Poco::Data::now;
    }
    catch( Poco::Data::DataException& ex )
    {
         //LOG_ERROR( ex.displayText() );
    }

    // Increment the ID so we never have name clashes when things get deleted
    // from the middle.
    ++mMonotonicID;
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::GetMatches( const std::string stringToMatch, SignalType sigType, std::vector< std::string >& names )
{
    //LOG_TRACE( "GetMatches: " << stringToMatch << " " << sigType );
    try
    {
        Poco::Data::Statement statement( *mSession );
        statement << "SELECT name FROM signals WHERE name LIKE :name",
                Poco::Data::use( stringToMatch );
        if( sigType != any_SignalType )
        {
            statement << " AND type=:type",
                    Poco::Data::use( sigType );
        }
        statement, Poco::Data::into( names );
        statement.execute();
    }
    catch( Poco::Data::DataException& ex )
    {
        //LOG_ERROR( ex.displayText() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::GetSlotMatches( const std::string& sigName, std::vector< int >& ids, std::vector< int >& priorities )
{
    //LOG_TRACE( "GetSlotMatches: " << sigName );
    // TODO: Needs slightly more subtle matching that includes signal type
    try
    {
        Poco::Data::Statement statement( *mSession );
        statement << "SELECT mapID FROM slots WHERE :pattern LIKE pattern",
                Poco::Data::use( sigName ),
                Poco::Data::into( ids );
        statement.execute();
        int priority = 3;
        for( size_t count = 0; count < ids.size(); ++count )
        {
            ( *mSession ) << "SELECT priority FROM slots WHERE mapID=:id",
            Poco::Data::use( ids.at( count ) ),
            Poco::Data::into( priority ),
            Poco::Data::now;
            priorities.push_back( priority );
        }
    }
    catch( Poco::Data::DataException& ex )
    {
         //LOG_ERROR( ex.displayText() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void EventManager::StoreConnection( ScopedConnectionList& connections, SignalWrapperBase* sigWrapper )
{
    //LOG_TRACE( "StoreConnection" );
    // Only store the connection if it represents an active connection
    boost::shared_ptr< boost::signals2::scoped_connection > connection = connections.GetLastConnection();
    if( connection->connected() )
    {
        weak_ptr< scoped_connection > weakConnection( connection );
        mConnections[ weakConnection ] = sigWrapper;
    }
}
////////////////////////////////////////////////////////////////////////////////
shared_ptr< ConnectionMonopoly > EventManager::MonopolizeConnectionWeak( shared_ptr< scoped_connection > connection )
{
    //LOG_TRACE( "MonopolizeConnectionWeak" );
    shared_ptr< ConnectionMonopoly > monopoly( new ConnectionMonopoly );

    // Determine which SignalWrapper this connection is associated with
    ConnectionMap_type::iterator iter = mConnections.find( connection );

    if( iter != mConnections.end() )
    {
        SignalWrapperBase* signalWrapper = iter->second;
        // Get the list of all connections from the signal wrapper and set up blocks
        // on all connections besides the one passed in here
        std::list< weak_ptr< scoped_connection > > connections = signalWrapper->GetConnections();

        std::list< weak_ptr< scoped_connection > >::iterator connectionsIter = connections.begin();
        while( connectionsIter != connections.end() )
        {
            weak_ptr< scoped_connection > wCurrentConnection = ( *connectionsIter );
            if( shared_ptr< scoped_connection > sCurrentConnection = wCurrentConnection.lock() )
            {
                if( sCurrentConnection != connection )
                {
                    shared_ptr< shared_connection_block >
                            blocker( new shared_connection_block( *sCurrentConnection ) );
                    monopoly->AddBlocker( blocker );
                }

                connectionsIter++;
            }
            else
            {
                // Failed to lock weak_ptr, which means the underlying shared_ptr
                // has gone out of scope. Jettison the expired entry so we don't
                // need to iterate through it next time.
                std::list< weak_ptr< scoped_connection > >::iterator oldIter = connectionsIter;
                connectionsIter++;
                connections.erase( oldIter );
            }
        }
    }

    return monopoly;
}
////////////////////////////////////////////////////////////////////////////////
shared_ptr< ConnectionMonopoly > EventManager::MonopolizeConnectionStrong( shared_ptr< scoped_connection > connection )
{
    //LOG_TRACE( "MonopolizeConnectionStrong" );
    // Determine which SignalWrapper this connection is associated with
    ConnectionMap_type::iterator iter = mConnections.find( connection );

    shared_ptr< ConnectionMonopoly > monopoly = MonopolizeConnectionWeak( connection );

    if( iter != mConnections.end() )
    {
        SignalWrapperBase* signalWrapper = iter->second;

        // Store a weak ptr to the monopoly and which SignalWrapper it affects so
        // the monopoly can be updated whenever a new slot connects to the signal
        weak_ptr< ConnectionMonopoly > cmPtr( monopoly );
        mStrongMonopolies[signalWrapper] = cmPtr;
    }

    return monopoly;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace eventmanager
} // namespace xplorer
} // namespace ves

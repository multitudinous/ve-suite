/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include<Poco/Data/Session.h>
#include<Poco/Data/SQLite/Connector.h>
#include <Poco/Data/DataException.h>

#include <ves/xplorer/eventmanager/EventManager.h>

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

EventManager::EventManager( )
{
    // Open an in-memory database to allow efficient searches of existing signals
    Poco::Data::SQLite::Connector::registerConnector( );
    mSession = new Poco::Data::Session( "SQLite", ":memory:" );
    ( *mSession ) << "CREATE TABLE signals (id INTEGER PRIMARY KEY, name TEXT, type INTEGER)",
            Poco::Data::now;
}
////////////////////////////////////////////////////////////////////////////////

EventManager::~EventManager( )
{
    Poco::Data::SQLite::Connector::unregisterConnector( );

    std::map<std::string, SignalWrapperBase*>::const_iterator iter = mSignals.begin( );
    std::map<std::string, SignalWrapperBase*>::const_iterator max = mSignals.end( );

    while( iter != max )
    {
        delete ( iter->second );
        iter++;
    }
}
////////////////////////////////////////////////////////////////////////////////

void EventManager::RegisterSignal( SignalWrapperBase* sig, const std::string& sigName, SignalType sigType )
{
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
            ( *mSession ) << "UPDATE signals SET type=:type WHERE name=:name",
                    Poco::Data::use( sigType ),
                    Poco::Data::use( sigName ),
                    Poco::Data::now;
        }
        else
        {
            ( *mSession ) << "INSERT INTO signals (name, type) VALUES (:name,:type)",
                    Poco::Data::use( sigName ),
                    Poco::Data::use( sigType ),
                    Poco::Data::now;
        }
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText( ) << std::endl;
    }

    // Store the signal in the signal map
    mSignals[sigName] = sig;
}
////////////////////////////////////////////////////////////////////////////////

void EventManager::ConnectSignal( const std::string& sigName,
                                  SlotWrapperBase* slot,
                                  ScopedConnectionList& connections,
                                  Priority priority )
{
    // Find the appropriate SignalWrapperBase
    std::map<std::string, SignalWrapperBase*>::const_iterator iter = mSignals.find( sigName );
    if( iter != mSignals.end( ) )
    {
        // Tell the SignalWrapper to connect its signal to this slot
        SignalWrapperBase* signalWrapper = iter->second;
        if( signalWrapper->ConnectSlot( slot, connections, priority ) )
        {
            //Connection was successful; store the details
            StoreConnection( connections, signalWrapper );

            // Check whether there is currently a strong monopoly on this signal.
            // If so, immediately block the connection that was just made.
            StrongMonopolies_type::iterator mIter = mStrongMonopolies.find( signalWrapper );
            if( mIter != mStrongMonopolies.end( ) )
            {
                if( shared_ptr< ConnectionMonopoly > monopoly = mIter->second.lock( ) )
                {
                    shared_ptr< shared_connection_block >
                            blocker( new shared_connection_block( *( connections.GetLastConnection( ) ) ) );
                    monopoly->AddBlocker( blocker );
                }
                else
                {
                    // Monopoly must have already been ended; remove this entry.
                    mStrongMonopolies.erase( mIter );
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////

void EventManager::ConnectSignals( const std::string& stringToMatch,
                                   SlotWrapperBase* slot,
                                   ScopedConnectionList& connections,
                                   SignalType sigType,
                                   Priority priority )
{
    std::vector< std::string > names;
    GetMatches( stringToMatch, sigType, names );

    // Iterate through result set and attempt to connect to the matching signals
    std::vector< std::string >::iterator namesIter = names.begin( );
    while( namesIter != names.end( ) )
    {
        ConnectSignal( ( *namesIter ), slot, connections, priority );
        namesIter++;
    }
}
////////////////////////////////////////////////////////////////////////////////

void EventManager::GetMatches( const std::string stringToMatch, SignalType sigType, std::vector< std::string >& names )
{
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
        statement.execute( );
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText( ) << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////

void EventManager::StoreConnection( ScopedConnectionList& connections, SignalWrapperBase* sigWrapper )
{
    // Only store the connection if it represents an active connection
    boost::shared_ptr< boost::signals2::scoped_connection > connection = connections.GetLastConnection( );
    if( connection->connected( ) )
    {
        weak_ptr< scoped_connection> weakConnection( connection );
        mConnections[ weakConnection ] = sigWrapper;
    }
}
////////////////////////////////////////////////////////////////////////////////

shared_ptr< ConnectionMonopoly > EventManager::MonopolizeConnectionWeak( shared_ptr< scoped_connection > connection )
{
    shared_ptr< ConnectionMonopoly > monopoly( new ConnectionMonopoly );

    // Determine which SignalWrapper this connection is associated with
    ConnectionMap_type::iterator iter = mConnections.find( connection );

    if( iter != mConnections.end( ) )
    {
        SignalWrapperBase* signalWrapper = iter->second;
        // Get the list of all connections from the signal wrapper and set up blocks
        // on all connections besides the one passed in here
        std::list< weak_ptr< scoped_connection > > connections = signalWrapper->GetConnections( );

        std::list< weak_ptr< scoped_connection > >::iterator connectionsIter = connections.begin( );
        while( connectionsIter != connections.end( ) )
        {
            weak_ptr< scoped_connection > wCurrentConnection = ( *connectionsIter );
            if( shared_ptr< scoped_connection > sCurrentConnection = wCurrentConnection.lock( ) )
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
    // Determine which SignalWrapper this connection is associated with
    ConnectionMap_type::iterator iter = mConnections.find( connection );

    shared_ptr< ConnectionMonopoly > monopoly = MonopolizeConnectionWeak( connection );

    if( iter != mConnections.end( ) )
    {
        SignalWrapperBase* signalWrapper = iter->second;

        // Store a weak ptr to the monopoly and which SignalWrapper it affects so
        // the monopoly can be updated whenever a new slot connects to the signal
        weak_ptr< ConnectionMonopoly > cmPtr( monopoly );
        mStrongMonopolies[signalWrapper] = cmPtr;
    }

    return monopoly;
}

} // namespace eventmanager
} // namespace xplorer
} // namespace ves

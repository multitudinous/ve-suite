/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#include<string>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>

// --- Boost includes --- //
#include <boost/noncopyable.hpp>
#include<boost/signals2/signal.hpp>
#include<boost/weak_ptr.hpp>
#include<boost/shared_ptr.hpp>

#include <ves/xplorer/eventmanager/SignalWrapper.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>
#include <ves/xplorer/eventmanager/ConnectionMonopoly.h>

#include <ves/VEConfig.h>

namespace Poco
{
namespace Data
{
class Session;
}
}

namespace ves
{
namespace xplorer
{
namespace eventmanager
{

/**
 * EventManager provides a centralized place to manage and connect signals and slots
 * based on the boost::signals2 library. Objects desiring to make a signal (event)
 * available to listeners should register the signal with EventManager
 * via EventManager::RegisterSignal. Listener objects desiring to receive a
 * particular events hould call EventManager::ConnectSignal or
 * EventManager::ConnectSignals, specifying the name of the signal to which they
 * would like to connect (or some other valid search criterion) and should
 * provide a slot of the appropriate type.
**/
class VE_EVENTMANAGER_EXPORTS EventManager
{
public:

    /**
     * SignalType is intended as a means to differentiate signals with
     * the same signature but different intended purposes. The various
     * SignalTypeS are <b>not</b> associated with a particular signature. The
     * presence of this enum allows connecting slots to request a connection to
     * a particular type of signal, even if they don't know the signal's name
     * (or perhaps want to be connected to all signals of a certain type.)
     **/
    enum SignalType
    {
        any_SignalType, ///< Intended only for use in calls to ConnectSignals for matching any of the other SignalTypeS
        button_SignalType, ///< Mouse, wand, or other device button press/release/scroll/etc
        computation_SignalType, ///< Beginning or end of computation
        file_SignalType, ///< File loading, writing, etc.
        input_SignalType, ///< General input event
        keyboard_SignalType, ///< Key press or release
        mouse_SignalType, ///< Mouse movement
        unspecified_SignalType ///< Unspecified, or doesn't fit a defined category -- this is the default in calls to RegisterSignal
    };

    /**
     * Listeners (slots) can be connected to an event (signal) with one of five
     * priorities. Unless a priority is explicitly given when making a
     * connection, normal_Priority is used. Listeners should generally use the
     * default normal_Priority unless a different priority is actually required.
     * Priorities represent slot groups. The groups are called in order from
     * highest to lowest. Slots within each group are called in the order in
     * which they were connected (FIFO).
     **/
    enum Priority
    {
        highest_Priority, ///< Called first
        high_Priority, ///< Called after highest_Priority
        normal_Priority, ///< Called after high_Priority
        low_Priority, ///< Called after normal_Priority
        lowest_Priority ///< Called last
    };

    /**
     * Registers a signal for exposure to other objects.
     * @param sig Pointer to a templated SignalWrapper which contains the
     * signal. Since SignalWrapper inherits from SignalWrapperBase, there is
     * no need to explicitly cast to SignalWrapperBase*.
     * @param sigName A unique name chosen by the caller. EventManager makes no
     * attempt to check or ensure uniqueness of the name. The reccommended way
     * to ensure uniqueness is to prepend a "meaningful" name with either a
     * uuid or a pointer cast as a string; eg.
     * @code
     *     std::string mySigName = boost::lexical_cast<std::string>( this );
     *     mySigName += "DeviceEightKeyboardSignal";
     * @endcode
     * It is best to refrain from using the characters "%" and "_" in sigName,
     * as these characters are used as wildcards in SQL searches. Having these
     * characters in the signal name will force match strings passed to
     * EventManager::ConnectSignal and EventManager::ConnectSignals to contain
     * special commands to escape these characters. See any SQL documentation to
     * determine the required escape commands for your need.
     * @param sigType The type of signal. Defaults to unspecified_SignalType.
     *
     * Example usage:
     * @code
     * boost::signals2::signal<void (int)> mySignal;
     *
     * RegisterSignal( new SignalWrapper<void (int)>( &mySignal ),
     *                 "some-unique-prefix.mySignal",
     *                 computation_SignalType );
     * @endcode
     * @see SignalWrapper
     **/
    void RegisterSignal( SignalWrapperBase* sig,
                         const std::string& sigName,
                         SignalType sigType = unspecified_SignalType );

    /**
     * Request a weak monoploy on a signal.
     * @param connection Shared pointer to the scoped_connection object associated with the
     * signal that should be monopolized.
     *
     * A weak monopoly means that all <em>existing</em> connections to the signal except the
     * connection passed in will be blocked until the monopoly is destroyed.
     * The monopoly does not affect new connections made on the signal after
     * the monopoly is set up. For example, if A, B, and C are connected to a
     * signal, B and C will be blocked when A creates a monopoly. However, if
     * D later connects to the signal, it will not be blocked even though B and
     * C still are.
     *
     * This method returns a boost::shared_ptr to a ConnectionMonopoly that the
     * caller should keep in scope as long as it want the monopoly to remain in
     * effect. To end the monoply, the caller should either call
     * boost::shared_ptr::reset on the ptr returned by this method, or should
     * allow the returned ptr to go out of scope.
     *
     * Blocked listeners are entirely at the mercy of the monopolist
     * as there is no way for a blocked listener to unblock its own connection.
     **/
    boost::shared_ptr< ConnectionMonopoly > MonopolizeConnectionWeak( boost::shared_ptr< boost::signals2::scoped_connection > connection );

    /**
     * Request a strong monopoly on a signal.
     * @param connection Shared pointer to the scoped_connection object associated with the
     * signal that should be monopolized.
     * 
     * A strong monopoly is similar to a weak monopoly as described in
     * EventManager::MonopolizeConnectionWeak, with the difference that new connections to the
     * signal will be affected as well as existing connections. In the example
     * from EventManager::MonopolizeConnectionWeak, D will successfully connect to the signal
     * but will not receive the signal until A ends its monopoly. There is a
     * slight chance that if the signal is fired at just the right time,
     * D will be called once, directly after it makes the connection but before
     * its block is in place. This is unavoidable because a connection must
     * be made before the connection can be blocked.
     * @see MonopolizeConnectionWeak
     * @see ConnectionMonopoly
     **/
    boost::shared_ptr< ConnectionMonopoly > MonopolizeConnectionStrong( boost::shared_ptr< boost::signals2::scoped_connection > connection );

    /**
     * Connects to the <em>single</em> signal whose name exactly matches sigName.
     * @param sigName The exact name of the signal to which to connect
     * @param slot Pointer to a templated SlotWrapper that contains the slot
     * to be connected to the signal. SlotWrapper inherits from SlotWrapperBase,
     * so there is no need to explicitly cast SlotWrapper as SlotWrapperBase.
     * @param connections Reference to the ScopedConnectionList that will hold
     * the resulting scoped_connection. The signal-slot connection will remain valid
     * as long as the scoped_connection object held in connections remains in scope.
     * @param priority The calling priority for this connection. Defaults to normal_Priority.
     *
     * Example:
     * @code
     * class Receiver
     * {
     * public:
     *     void MyIntSlot( int data )
     *     {
     *          std::cout << data << std::endl;
     *     }
     *     ScopedConnectionList mConnections;
     * }
     *
     * Receiver rec;
     *
     * // A proper connection will require at least three lines of code. The
     * // typedef below is the only optional part (but increases readability).
     * typedef boost::signals2::signal<void (int ) > signal_t;
     *
     * // Use boost::bind to make a functor out of the member method that is the slot
     * signal_t::slot_type slotFunctor( boost::bind( &Receiver::MyIntSlot, &rec, _1 ) );
     *
     * // Wrap the functored slot in a slotwrapper
     * SlotWrapper< signal_t > slotWrapper( slotFunctor );
     * 
     * // Call into EventManager to connect this instance of Receiver::MyIntSlot
     * // to the signal named "ExactSignalName"
     * ConnectSignal( "ExactSignalName", &slotWrapper, rec.mConnections, normal_Priority );
     * @endcode
     *
     * In the above example, it is <b>critical</b> to create slotFunctor as an lvalue
     * on the stack before passing it to the slotwrapper. If we were to try to
     * put the creation of slotFunctor inside the call to SlotWrapper's ctor,
     * the functor would be an rvalue and the reference that SlotWrapper expects
     *  would be invalid immediately after the call. Doing so would lead to a
     * connection to a non-existent function and an exception of type
     * boost::function: empty function.
     * @code
     * // This will compile, but won't work. Don't do this:
     * SlotWrapper< signal_t > slotWrapper( boost::bind( &Receiver::MyIntSlot, &rec, _1 ) );
     *
     * // You MUST use multiple steps like this:
     * signal_t::slot_type slotFunctor( boost::bind( &Receiver::MyIntSlot, &rec, _1 ) );
     * SlotWrapper< signal_t > slotWrapper( slotFunctor );
     * @endcode
     * @see ConnectSignals
     **/
    void ConnectSignal( const std::string& sigName,
                        SlotWrapperBase* slot,
                        ScopedConnectionList& connections,
                        Priority priority = normal_Priority );

    /**
     * Connects to <em>all</em> signals whose name contains stringToMatch, whose signal type
     * matches sigType, and whose signature matches that of the slot. Note carefully
     * that last part: a connection can be successfully made <em>only</em> if the slot and
     * signal signature match exactly.
     * @param stringToMatch A substring of a signal name to look for. If
     * stringToMatch is "%", then any signal name will match. The "%"
     * character is the wildcard for matches, so to match all signals whose
     * name contains the substring "xplorer", the string "%xplorer%" should be
     * supplied for stringToMatch. Underscore "_" is special as well, and is a
     * placeholder for a single character.
     * @param slot Pointer to a templated SlotWrapper which contains the slot
     * which should be connected to the signal(s).
     * @param connections Reference to a ScopedConnectionList which will manage
     * the lifetime of this connection.
     * @param sigType The type of signal(s) to which the slot should be connected
     * @param priority The priority of the resulting connection.
     * @see ConnectSignal
     * @see SlotWrapper
     **/
    void ConnectSignals( const std::string& stringToMatch,
                         SlotWrapperBase* slot,
                         ScopedConnectionList& connections,
                         SignalType sigType = any_SignalType,
                         Priority priority = normal_Priority );

private:
    
    /// Constructor
    EventManager( );
    
    /// Destructor
    virtual ~EventManager( );

    /// Singleton declarations
    vprSingletonHeader( EventManager );
    ///
    /// Helper function that finds signals matching certain criteria in the signals database.
    void GetMatches( const std::string stringToMatch,
                     SignalType sigType,
                     std::vector< std::string >& names );

    ///
    /// Stores a weak_ptr to the connection and a pointer to the affected SignalWrapper
    /// after a connection has been made.
    /// This allows mConnections mConnections to be searched for a specific connection
    /// and thereby figure out which signal it was connected to.
    void StoreConnection( ScopedConnectionList& connections,
                          SignalWrapperBase* sigWrapper );

    ///
    /// Holds the signal name and its corresponding SignalWrapper*.
    /// Used for connecting a slot to a signal when the exact signal name is known.
    std::map< std::string, SignalWrapperBase* > mSignals;

    ///
    /// Holds a weak_ptr to a connection and a raw ptr to the corresponding
    /// SignalWrapper. Used for dealing with signal monopolies.
    typedef std::map< boost::weak_ptr< boost::signals2::scoped_connection >, SignalWrapperBase* > ConnectionMap_type;
    ConnectionMap_type mConnections;

    ///
    /// Holds a weak_ptr to a ConnectionMonopoly and a raw ptr to the corresponding
    /// SignalWrapper. Used for dealing with strong signal monopolies.
    typedef std::map< SignalWrapperBase*, boost::weak_ptr< ConnectionMonopoly > > StrongMonopolies_type;
    StrongMonopolies_type mStrongMonopolies;

    // Pointer to in-memory database session that holds details about signals
    // to allow for efficient searches
    Poco::Data::Session* mSession;
};


} // namespace eventmanager
} // namespace xplorer
} // namespace ves

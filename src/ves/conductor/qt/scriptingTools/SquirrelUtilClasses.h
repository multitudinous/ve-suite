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

// !!WARNING!! This header is not intended to be included by any file other
// than SquirrelConnection.cxx. This file contains a few convenience classes
// designed to encapsulate complex behaviors and/or reduce the total number of
// C++ classes that must be exposed to the Squirrel engine.

#include <ves/xplorer/Logging.h>

#include <switchwire/ScopedConnectionList.h>
#include <switchwire/ConnectSignals.h>
#include <propertystore/PropertySetPtr.h>

#include <sqrat.h>
#include <Poco/ConsoleChannel.h>

#include <gadget/Type/DigitalData.h>

#include <boost/thread/mutex.hpp>

namespace crunchstore
{
class SQLiteTransactionKey;
}

namespace ves
{
namespace xplorer
{
namespace data
{
class CADPropertySet;
}
}
namespace conductor
{
////////////////////////////////////////////////////////////////////////////////
/// \brief The TweakStore class
/// Allows an easy way to start and stop bulk transactions. You will generally
/// create an instance of TweakStore in your script and then call its
/// OpenTransaction method. For each operation you want done as part of the
/// transaction, you will pass in the TweakStore object. When you're done with
/// the transaction, call the CloseTransaction method of this object.
class TweakStore
{
public:
    TweakStore();

    ~TweakStore();

    TweakStore( const TweakStore& rhs );

    /// Begins a bulk transaction
    void OpenTransaction();

    /// Ends a bulk transaction
    void CloseTransaction();

    /// Returns the transaction key required by crunchstore to manage bulk
    /// transactions. You should never need to call this method from inside a
    /// script.
    crunchstore::SQLiteTransactionKey GetKey() const;

private:
    /// Stores the transaction key required by crunchstore
    crunchstore::SQLiteTransactionKey* m_transactionKey;
};
////////////////////////////////////////////////////////////////////////////////
/// \brief The VizPropertySetWrapper class
/// Allows creation of Viz features without having to expose each of the
/// various Viz PropertySet classes.
class VizPropertySetWrapper
{
public:

    /// Creates a new viz feature of the type specified by @c featureType.
    /// Valid featureType strings are Contours, Vectors, Streamlines,
    /// Isosurfaces, Texture-based, and Polydata.
    void CreateNewFeature( const std::string& featureType );

    /// Sets the value of property @c key to the bool @c value.
    void SetBoolPropertyValue( const std::string& key, bool value );

    /// Sets the value of property @c key to the int @c value.
    void SetIntPropertyValue( const std::string& key, int value );

    /// Sets the value of property @c key to the float @c value.
    void SetFloatPropertyValue( const std::string& key, float value );

    /// Sets the value of property @c key to the double @c value.
    void SetDoublePropertyValue( const std::string& key, double value );

    /// Sets the value of property @c key to the string @c value.
    void SetStringPropertyValue( const std::string& key, std::string value );

    /// Returns the UUID of the viz feature.
    std::string GetUUIDAsString();

    /// Saves the viz feature.
    void Save();

    /// Saves the viz feature as part of the transaction opened using the
    /// passed TweakStore object
    // Trying to use a const reference to a TweakStore object works fine in
    // C++ but somehow accesses invalid memory when used in Squirrel. Normal
    // pointers work just fine though.
    void BulkSave( TweakStore* tweakstore );

private:
    propertystore::PropertySetPtr m_set;
};
////////////////////////////////////////////////////////////////////////////////
/** \brief The Sleeper class
  * Allows the script to sleep, or pause, for a specified number of
  * milliseconds. This will allow other threads in the main application to get
  * things done while the script is paused. In a script, you'll do something
  * like this:
  * @code
  * // ... do some stuff ...
  * // Now, sleep for one second
  * MySleeper <- Sleeper;
  * MySleeper.Sleep( 1000 );
  * // ... sleep is over, do more stuff ...
  * @endcode
**/
class Sleeper
{
public:
    /// Sleep for @c time milliseconds
    static void Sleep( unsigned long time );
};

////////////////////////////////////////////////////////////////////////////////
/** \brief The Logger class
  * Allows the script to log messages to std::clog (which is typically mapped to
  * std::cerr). Useful for debugging scripts.
  * @code
  * local logger = Logger();
  * logger.info( "hello from Squirrel" );
  * @endcode
**/
class Logger
{
public:
    Logger();

    void Info( const std::string& message );

    void Notice( const std::string& message );

    void Warning( const std::string& message );

    void Error( const std::string& message );

private:
    Poco::Logger& m_logger;
    ves::xplorer::LogStreamPtr m_logStream;
};

// see http://squirrel-lang.org/forums/default.aspx?g=posts&m=7099
class BaseObject
{
public:
    BaseObject();

protected:
    Sqrat::Var< Sqrat::Object& > m_instance;
};

class BaseEvent
{
public:
    BaseEvent();
};

// forward declaration
class BaseContext;

////////////////////////////////////////////////////////////////////////////////
/** \brief The BaseState class
  * Provides a framework for implementing the State design pattern
  * https://en.wikipedia.org/wiki/State_pattern
**/
class BaseState : public BaseObject
{
public:
    BaseState();

    void _OnEnter( BaseContext* context );

    void OnEnter( BaseContext* context );

    void _OnExit( BaseContext* context );

    void OnExit( BaseContext* context );

    Sqrat::SharedPtr< BaseState > _OnEvent( BaseContext* context, BaseEvent* event );

    Sqrat::SharedPtr< BaseState > OnEvent( BaseContext* context, BaseEvent* event );
};

////////////////////////////////////////////////////////////////////////////////
/** \brief The BaseContext class
  * Provides a framework for implementing the State design pattern
  * https://en.wikipedia.org/wiki/State_pattern
**/
class BaseContext
{
public:
    BaseContext();

    void SetInitialState( BaseState* state );

    void HandleEvent( BaseEvent* event );

protected:
    Sqrat::SharedPtr< BaseState > m_state;
};
////////////////////////////////////////////////////////////////////////////////
class CADPropertySetWrapper
{
public:
    CADPropertySetWrapper();

    void SetBoolPropertyValue( const std::string& key, bool value );

    bool GetBoolPropertyValue( const std::string& key );

    void SetIntPropertyValue( const std::string& key, int value );

    int GetIntPropertyValue( const std::string& key );

    void SetFloatPropertyValue( const std::string& key, float value );

    float GetFloatPropertyValue( const std::string& key );

    void SetDoublePropertyValue( const std::string& key, double value );

    double GetDoublePropertyValue( const std::string& key );

    void SetStringPropertyValue( const std::string& key, std::string value );

    std::string GetStringPropertyValue( const std::string& key );

    void SetUUID( const std::string& uuid );

    bool Load();

    void EmitValueChangedSignals();

    void EnableLiveProperties( bool live );

private:
    propertystore::PropertySetPtr m_set;
};

template< typename ArgType >
class SynchronizedSignalReceiver
{
public:
    SynchronizedSignalReceiver()
        : m_dataIsPending( false )
    {
        ;
    }

    // Sqrat's allocators expect bindable C++ objects to be copy-constructable
    SynchronizedSignalReceiver( const SynchronizedSignalReceiver& other )
        : m_dataIsPending( false )
    {
        ; // don't copy anything from the other object
    }

    void ConnectToSignal( const std::string& signal_name )
    {
        typedef boost::signals2::signal< void( ArgType ) > signal_t;

        typename signal_t::slot_type* slot_functor =
            new typename signal_t::slot_type( boost::bind( &SynchronizedSignalReceiver< ArgType >::_Slot, this, _1 ) );

        switchwire::SlotWrapperBasePtr slot_wrapper( new switchwire::SlotWrapper< signal_t >( slot_functor ) );

        switchwire::EventManager::instance()->ConnectSignal( signal_name,
                                                             slot_wrapper,
                                                             this->m_connections );
    }

    void Disconnect()
    {
        m_connections.DropConnections();
    }

    bool Pending()
    {
        boost::mutex::scoped_lock scoped_lock( m_lock );
        return m_dataIsPending;
    }

    ArgType Pop()
    {
        boost::mutex::scoped_lock scoped_lock( m_lock );
        m_dataIsPending = false;
        return m_data;
    }

    void _Slot( ArgType data )
    {
        boost::mutex::scoped_lock scoped_lock( m_lock );
        m_data = data;
        m_dataIsPending = true;
    }

protected:
    ArgType m_data;
    bool m_dataIsPending;

    boost::mutex m_lock;

    switchwire::ScopedConnectionList m_connections;
};
}} //ves::conductor

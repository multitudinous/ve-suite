#pragma once
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
 * Date modified: $Date: 2012-10-29 18:37:56 -0500 (Mon, 29 Oct 2012) $
 * Version:       $Rev: 17252 $
 * Author:        $Author: mccdo $
 * Id:            $Id: EventFactory.h 17252 2012-10-29 23:37:56Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/


// !!WARNING!! This header is not intended to be included by any file other
// than SquirrelConnection.cxx. This file contains a few convenience classes
// designed to encapsulate complex behaviors and/or reduce the total number of
// C++ classes that must be exposed to the Squirrel engine.

#include <ves/xplorer/data/ContourPlanePropertySet.h>
#include <ves/xplorer/data/DatabaseManager.h>
#include <ves/conductor/qt/VisFeatureManager.h>

#include <crunchstore/SQLiteTransactionKey.h>

#include <vpr/System.h>

namespace ves
{
namespace xplorer
{
namespace eventmanager
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
    TweakStore()
        : m_transactionKey( 0 )
    {
        ;
    }

    ~TweakStore()
    {
        delete m_transactionKey;
    }

    TweakStore( const TweakStore& rhs )
    {
        m_transactionKey = rhs.m_transactionKey;
    }

    /// Begins a bulk transaction
    void OpenTransaction()
    {
        m_transactionKey = new crunchstore::SQLiteTransactionKey( ves::xplorer::data::DatabaseManager::instance()->OpenBulkMode() );
    }

    /// Ends a bulk transaction
    void CloseTransaction()
    {
        ves::xplorer::data::DatabaseManager::instance()->CloseBulkMode( *m_transactionKey );
    }

    /// Returns the transaction key required by crunchstore to manage bulk
    /// transactions. You should never need to call this method from inside a
    /// script.
    crunchstore::SQLiteTransactionKey GetKey() const
    {
        return *m_transactionKey;
    }

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
    void CreateNewFeature( const std::string& featureType )
    {
        m_set = ves::conductor::VisFeatureManager::instance()->CreateNewFeature( featureType );
    }

    /// Sets the value of property @c key to the bool @c value.
    void SetBoolPropertyValue( const std::string& key, bool value )
    {
        m_set->SetPropertyValue( key, value );
    }

    /// Sets the value of property @c key to the int @c value.
    void SetIntPropertyValue( const std::string& key, int value )
    {
        m_set->SetPropertyValue( key, value );
    }

    /// Sets the value of property @c key to the float @c value.
    void SetFloatPropertyValue( const std::string& key, float value )
    {
        m_set->SetPropertyValue( key, value );
    }

    /// Sets the value of property @c key to the double @c value.
    void SetDoublePropertyValue( const std::string& key, double value )
    {
        m_set->SetPropertyValue( key, value );
    }

    /// Sets the value of property @c key to the string @c value.
    void SetStringPropertyValue( const std::string& key, std::string value )
    {
        m_set->SetPropertyValue( key, value );
    }

    /// Returns the UUID of the viz feature.
    std::string GetUUIDAsString()
    {
        return m_set->GetUUIDAsString();
    }

    /// Saves the viz feature.
    void Save()
    {
        m_set->Save();
    }

    /// Saves the viz feature as part of the transaction opened using the
    /// passed TweakStore object
    // Trying to use a const reference to a TweakStore object works fine in
    // C++ but somehow accesses invalid memory when used in Squirrel. Normal
    // pointers work just fine though.
    void BulkSave( TweakStore* tweakstore )
    {
        m_set->Save( tweakstore->GetKey() );
    }

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
    void Sleep( unsigned long time )
    {
        vpr::System::msleep( time );
    }
};

}}} //ves::xplorer::eventmanager

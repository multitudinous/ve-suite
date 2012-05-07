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
#include <map>
#include <string>
#include <vector>

#include <boost/any.hpp>
#include <boost/uuid/uuid.hpp>

#include <Poco/Types.h>

#include <ves/VEConfig.h>

#include <ves/xplorer/data/MakeLivePtr.h>

#include <Persistence/Persistable.h>

#include <ves/xplorer/data/PropertySetPtr.h>
#include <ves/xplorer/data/PropertyPtr.h>
#include <ves/xplorer/Logging.h>

// Forward declarations
namespace Poco
{
class Timer;
namespace Data
{
class Session;
class Statement;
} // namespace Data
} // namespace Poco

namespace ves
{
namespace xplorer
{
namespace data
{
/*!\file PropertySet.h
 * \class ves::xplorer::data::PropertySet
 * PropertySet is a base class for collections of properties
 * containing methods to read and write values and attributes of
 * properties, as well as to get information about changes to other properties
 * that may occur when a property value or attribute is changed.
 * \namespace ves::xplorer::data
 *
 */

    class VE_DATA_EXPORTS PropertySet: public Persistence::Persistable
{
public:
    typedef std::vector<std::string> PSVectorOfStrings;


    /// Constructor
    PropertySet();


    /// Copy constructor
    PropertySet( const PropertySet& orig );


    /// Destructor
    virtual ~PropertySet();

    /// Method to allow factory creation. Derived classes should override this
    /// to return an object of the derived type wrapped in a PropertySetPtr.
    virtual PropertySetPtr CreateNew();


    /// Add a new property named propertyName with main value value to this set
    virtual void AddProperty( const std::string& propertyName,
                              boost::any value,
                              std::string uiLabel = "" );

    virtual boost::any GetPropertyValue( const std::string& propertyName );

    /// Returns a vector containing the identifying names of all attributes
    /// owned by property propertyName
    virtual const PSVectorOfStrings& GetPropertyAttributeList( std::string const&
                                                            propertyName );


    /// Returns value of the property attribute identified by
    /// attributeName and owned by property propertyName
    virtual Persistence::DatumPtr GetPropertyAttribute( std::string const& propertyName,
                                             std::string const& attributeName ) const;

    /// Returns the value of the attribute identified by attributeName of the
    /// property identified by propertName, converted
    /// to type T. Example: propertySet.GetPropertyAttributeValue< double >
    ///                                  ( "SomeProperty", "ADoubleAttribute" );
    template <typename T>
            T GetPropertyAttributeValue( std::string const& propertyName,
                                         std::string const& attributeName ) const
    {
        T val = GetPropertyAttribute( propertyName, attributeName )->extract<T>();
        return val;
    }


    /// Returns true if the property with name propertyName is currently enabled.
    virtual bool GetPropertyEnabled( std::string const& propertyName ) const;


    /// Sets the property attribute given by attributeName of the property
    /// propertyName to value.
    virtual void SetPropertyAttribute( std::string const& propertyName,
                                       std::string const& attributeName,
                                       boost::any value );

    /// Sets the enabled status of the property identified by propertyName.
    /// If the second argument is true, the property is enabled. If the second
    /// argument is false, the property is disabled.
    virtual void SetPropertyEnabled( std::string const& propertyName,
                                     bool enabled );

    /// Returns a list containing names of properties that have undergone a
    /// state change of some sort due to the most recent operation on a
    /// property in this set.
    virtual PSVectorOfStrings const& GetChanges();

    /// Clears the internal list of all property changes that have occurred
    /// recently. The maximum size of the internal list is equal to the number
    /// of properties in the set.
    virtual void ClearAccumulatedChanges();


    ///
    /// Toggles live properties if this set has any that are not live by default.
    /// Add MakeLive directives for any such properties inside an overridden
    /// version of this method. To enable autosaving of live properties, call
    /// PropertySet::EnableLiveProperties( live ) from inside the overridden
    /// version. Caution: Since PropertySet is an Object Relational Mapper,
    /// the *entire* propertyset, not just the changed property, is saved out
    /// every time an autosave occurs. This may be undesirable behavior for
    /// some propertysets. To ensure that autosaving does not occur, override
    /// this method in your derived class and do not call
    /// PropertySet::EnableLiveProperties() from within it.
    virtual void EnableLiveProperties( bool live );


    virtual bool Load();

    virtual bool LoadByKey( const std::string& KeyName, boost::any KeyValue );

    ///A series of methods to write a current PropertySet to the db
    virtual bool Save();

    ///Strictly writes out PropertySet to db; prevents derived-class overrides
    ///from performing other operations in the process.
    void SaveNoOverride();

    ///A series of methods to delete the current PropertySet from the DB
    virtual bool Remove();

protected:

    /// Slot that should be connected to any state-change signal emitted by any
    /// property in this set. This connection is taken care of in the 
    /// AddProperty method through a call to _connectChanges. Derived classes
    /// that override AddProperty should be sure to call _connectChanges if
    /// change accumulation is desired.
    /// ChangeAccumulator will accumulate a list of all properties
    /// that have undergone a change in value, attributes, or enabled state
    /// since the last call to GetChanges or the last call to
    /// ClearAccumulatedChanges
    virtual void ChangeAccumulator( PropertyPtr property );

    unsigned int GetBoostAnyVectorSize( const boost::any& value );

    ///
    /// Internal function that connects default signals to the change accumulator
    virtual void _connectChanges( PropertyPtr property );


    /// Callback method set up when argument to EnableLiveProperties is true.
    /// Checks whether a live property has changed and saves out the PropertySet
    /// if so.
    void SaveLiveProperties( Poco::Timer& timer );


    PSVectorOfStrings m_accumulatedChanges;

    ///The list of live objects for this property set
    std::vector< MakeLiveBasePtr > m_liveObjects;
    ///Determine whether the live properties should be live
    bool m_isLive;
private:
    // Empty string to use when need to return an empty string by reference
    std::string emptyString;
    // Empty PSVectorOfStrings to use when need to return an empty PSVOS by reference
    PSVectorOfStrings emptyPSVectorOfStrings;
    PSVectorOfStrings m_accumulatedChangesReturnable;

    Poco::Timer* m_timer;
    bool m_writeDirty; ///< Data has changed since last write to db.
    bool m_liveWriteDirty; ///< Data in a live property has changed since last write to db

    Poco::Logger& m_logger;
    ves::xplorer::LogStreamPtr m_logStream;

};

} // namespace data
} // namespace xplorer
} // namespace ves

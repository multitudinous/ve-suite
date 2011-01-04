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
#pragma once
#include <ves/xplorer/data/PropertySetPtr.h>
#include <ves/xplorer/data/PropertyPtr.h>

#include <map>
#include <string>
#include <vector>

#include <boost/any.hpp>
#include <boost/uuid/uuid.hpp>

#include <Poco/Types.h>

#include <ves/VEConfig.h>

// Forward declarations
namespace Poco
{
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
 *
 */

/*!\class ves::xplorer::data::PropertySet
 * PropertySet is a base class for collections of properties
 * containing methods to read and write values and attributes of
 * properties, as well as to get information about changes to other properties
 * that may occur when a property value or attribute is changed.
 */

/*!\namespace ves::xplorer::data
 *
 */

class VE_DATA_EXPORTS PropertySet
{
public:
    typedef std::map< std::string, PropertyPtr > PropertyMap;
    typedef std::vector<std::string> PSVectorOfStrings;

    ///
    /// Constructor
    PropertySet();

    ///
    /// Copy constructor
    PropertySet( const PropertySet& orig );

    ///
    /// Destructor
    virtual ~PropertySet();

    ///
    /// Add a new property named propertyName with main value value to this set
    virtual void AddProperty( const std::string& propertyName,
                              boost::any value,
                              std::string uiLabel = "" );

    ///
    /// Returns true if this property set owns a property with name propertyName,
    /// false if not.
    virtual bool PropertyExists( const std::string& propertyName ) const;

    ///
    /// Returns a vector containing the identifying names of all properties
    /// contained in this set. Names are in the order in which they were added
    /// to the property set.
    virtual const PSVectorOfStrings& GetPropertyList();

    ///
    /// Returns a pointer to the property identified by propertyName
    virtual PropertyPtr GetProperty( const std::string& propertyName ) const;

    ///
    /// Returns the boost::any main value of the property identified by
    /// propertyName
    virtual boost::any GetPropertyValue( const std::string& propertyName ) const;

    ///
    /// Returns a vector containing the identifying names of all attributes
    /// owned by property propertyName
    virtual const PSVectorOfStrings& GetPropertyAttributeList( const std::string&
                                                            propertyName );

    ///
    /// Returns boost::any value of the property attribute identified by
    /// attributeName and owned by property propertyName
    virtual boost::any GetPropertyAttribute( const std::string& propertyName,
                                             const std::string& attributeName );

    ///
    /// Returns true if the property with name propertyName is currently enabled.
    virtual bool GetPropertyEnabled( const std::string& propertyName ) const;

    ///
    /// Sets the main value of the property indentified by propertyName to value.
    virtual bool SetPropertyValue( const std::string& propertyName,
                                   boost::any value );

    ///
    /// Sets the property attribute given by attributeName of the property
    /// propertyName to value.
    virtual void SetPropertyAttribute( const std::string& propertyName,
                                       const std::string& attributeName,
                                       boost::any value );

    ///
    /// Sets the enabled status of the property identified by propertyName.
    /// If the second argument is true, the property is enabled. If the second
    /// argument is false, the property is disabled.
    virtual void SetPropertyEnabled( const std::string& propertyName,
                                     bool enabled );

    ///
    /// Returns a list containing names of properties that have undergone a
    /// state change of some sort due to the most recent operation on a
    /// property in this set.
    virtual const PSVectorOfStrings& GetChanges();

    ///
    /// Clears the internal list of all property changes that have occurred
    /// recently. The maximum size of the internal list is equal to the number
    /// of properties in the set.
    virtual void ClearAccumulatedChanges();

    ///
    /// Sets the name of the table this property set tries to read from
    /// and write to.
    virtual void SetTableName( const std::string& TableName );

    ///
    /// Returns the name of the table this property set reads from and writes
    /// to.
    virtual const std::string& GetTableName() const;

    ///
    /// Sets the record ID for this property. This should generally be used only
    /// to identify this property set before asking it to read data with a 
    /// matching record ID from the database.
    virtual void SetRecordID( long unsigned int id );

    ///
    /// Returns the record ID of this property set. The record ID is how this
    /// property set is distinguished from others of a similar type in the
    /// database.
    virtual unsigned int GetRecordID() const;

    virtual void SetUUID( const std::string& uuid );
    virtual void SetUUID( boost::uuids::uuid& uuid );
    virtual const boost::uuids::uuid& GetUUID() const;
    virtual std::string GetUUIDAsString() const;

    virtual bool LoadFromDatabase();
    virtual bool LoadFromDatabase( Poco::Data::Session* session );
    virtual bool LoadFromDatabase( Poco::Data::Session* session, const std::string& TableName );
    //virtual bool LoadFromDatabase( Poco::Data::Session* session, const std::string& TableName, Poco::UInt32 ID );
    virtual bool LoadFromDatabase( Poco::Data::Session* session, const std::string& TableName, const std::string& UUID );

    virtual bool LoadFromDatabase( const std::string& DatabaseName );
    virtual bool LoadFromDatabase( const std::string& DatabaseName, const std::string& TableName );
    //virtual bool LoadFromDatabase( const std::string& DatabaseName, const std::string& TableName, unsigned int ID );
    virtual bool LoadFromDatabase( const std::string& DatabaseName, const std::string& TableName, const std::string& UUID );

    virtual bool LoadByKey( const std::string& KeyName, boost::any KeyValue );
    virtual bool LoadByKey( Poco::Data::Session* session, const std::string& KeyName, boost::any KeyValue );
    virtual bool LoadByKey( const std::string& DatabaseName, const std::string& KeyName, boost::any KeyValue );

    virtual bool WriteToDatabase();
    virtual bool WriteToDatabase( Poco::Data::Session* session );
    virtual bool WriteToDatabase( Poco::Data::Session* session, const std::string& TableName );
    virtual bool WriteToDatabase( Poco::Data::Session* session, const std::string& TableName, Poco::Data::Statement& statement );
    virtual bool WriteToDatabase( const std::string& DatabaseName );
    virtual bool WriteToDatabase( const std::string& DatabaseName, const std::string& TableName );

    virtual bool DeleteFromDatabase();
    virtual bool DeleteFromDatabase( Poco::Data::Session* session );
    virtual bool DeleteFromDatabase( Poco::Data::Session* session, const std::string& TableName );
    virtual bool DeleteFromDatabase( const std::string& DatabaseName );
    virtual bool DeleteFromDatabase( const std::string& DatabaseName, const std::string& TableName );

protected:

    ///
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

    ///
    /// Internal function that looks through the property set and builds an
    /// appropriate string for creating an sqlite table for storing the data
    /// contained in this property set. If the default function is not doing
    /// what you need, override this function to create a custom table.
    virtual std::string _buildColumnHeaderString();

    ///
    /// Tests for presence of characters disallowed in database column names in
    /// string value. For sqlite, allowed characters are digits 0-9, lower- and
    /// upper-case letters, and the underscore. All other characters are illegal.
    bool _containsIllegalCharacter( const std::string& value );

    ///
    /// Helper function to determine whether a given TableName exists in the db.
    bool _tableExists( Poco::Data::Session* session, const std::string& TableName );

    PropertyMap mPropertyMap; /// Map holding the collection of properties.
    PSVectorOfStrings mAccumulatedChanges;
    PSVectorOfStrings mPropertyList; /// Maintains a list of available properties
    // sorted by order of addition

    std::string mTableName;
    Poco::UInt32 mID;

    boost::uuids::uuid mUUID;
    std::string mUUIDString;

private:
    // Empty string to use when need to return an empty string by reference
    std::string emptyString;
    // Empty PSVectorOfStrings to use when need to return an empty PSVOS by reference
    PSVectorOfStrings emptyPSVectorOfStrings;
    PSVectorOfStrings mAccumulatedChangesReturnable;
    //std::string _toString( int value );
    //std::string _toString( size_t value );
    //std::string _toString( long unsigned int value );
};

} // namespace data
} // namespace xplorer
} // namespace ves

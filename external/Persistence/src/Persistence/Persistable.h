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

#include <Persistence/Datum.h>
#include <Persistence/PersistablePtr.h>

// Defines VE_DATA_EXPORTS
#include <Persistence/ExportConfig.h>

namespace Persistence
{

class PERSISTENCE_EXPORT Persistable
{
public:

    Persistable();

    /// Construct and set typename in one step
    Persistable( const std::string& typeName );

    /// Copy constructor
    Persistable( const Persistable& orig );

    virtual ~Persistable();

// UUID operations --
    /// Sets the UUID via a string; the UUID allows this persistable to be
    /// uniquely identified in the data store.
    virtual void SetUUID( std::string const& uuid );

    /// Sets the UUID via a boost::uuid
    virtual void SetUUID( boost::uuids::uuid const& uuid );

    /// Gets the UUID as a boost::uuid
    virtual boost::uuids::uuid const& GetUUID() const;

    /// Gets the UUID as a string
    virtual std::string const& GetUUIDAsString() const;
// --/


// TypeName operations --
    /** Sets the typename of the persistable. The application is responsible for
      * setting up an appropriate typename before saving or loading a
      * persistable. The typename defaults to the UUID of the persistable with
      * the dashes removed if left unset. It is not reccommended to leave this
      * default unchanged, as in most relational databases, this will result in
      * a new table for every single persistable -- and many RDBMS have a limit
      * on the number of tables allowed.
      *
      * The type should ideally be a unique human-readable name that
      * identifies "type" or "class" of data being stored. For example, a
      * retail sales system might have types corresponding to "customers",
      * "products", "stores", etc. In many cases, the typename will be the same
      * or similar to the classname of the data being stored. In all cases, any
      * two PersistableS with the same typename must have exactly the same
      * data fields -- in (data)type, number, and name. This is because some
      * common back-end store implementations (notably those that use SQL)
      * will use the typename as the tablename in the database.
      **/
    virtual void SetTypeName( std::string name );

    /** Returns the typename.
      **/
    virtual std::string const& GetTypeName() const;
// --/


// datum operations --
    /// Adds a new datum named datumName with main value value
    virtual void AddDatum( const std::string& datumName,
                              boost::any value );

    /// Adds a new datum named datumName from a pre-created datum object
    virtual void AddDatumPtr( const std::string& datumName,
                              DatumPtr datum );

    /// Returns a pointer to the datum identified by datumName
    virtual DatumPtr GetDatum( std::string const& datumName ) const;

    /// Sets the main value of the datum identified by datumName to value.
    virtual void SetDatumValue( std::string const& datumName,
                                boost::any value );

    /// Returns the main value of the datum identified by datumName, converted
    /// to type T. Example: persist.GetDatumValue< double >( "MyDouble" );
    template <typename T>
            T GetDatumValue( std::string const& datumName ) const
    {
        return GetDatum( datumName )->extract<T>();
    }

    /// Returns true if this Persistable owns a datum with name datumName,
    /// false if not.
    virtual bool DatumExists( std::string const& datumName ) const;

    /// Returns a vector containing the identifying names of all data
    /// contained in this set. Names are in the order in which they were added
    /// to the Persistable.
    virtual const std::vector<std::string>& GetDataList() const;
// --/

protected:

    /// Typedef for datum map to make it easier to switch to a different
    /// underlying type in future if needed.
    typedef std::map< std::string, DatumPtr > DataMap;

    /// Map holding the collection of data.
    DataMap m_dataMap;

    /// Maintains a list of available data sorted by order of addition
    std::vector< std::string > m_dataList;

    /// The uuid
    boost::uuids::uuid m_UUID;
    /// The uuid as a std::string
    std::string m_UUIDString;

private:
    /// Holds the typename
    std::string m_typename;

    /// Empty string to use when need to return an empty string by reference
    std::string emptyString;

    /// Empty vector of strings to use whenever we need to return one by
    /// reference
    std::vector<std::string> emptyVectorOfStrings;

};

} // namespace Persistence

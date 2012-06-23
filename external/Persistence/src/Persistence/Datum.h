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
//#include <ves/xplorer/data/PropertyPtr.h>

#include <string>
#include <vector>
#include <map>

#include <boost/any.hpp>

#include <Persistence/ExportConfig.h>
#include <Persistence/DatumPtr.h>

// TODO: Check through const status everywhere

/// @class Datum holds a main value in a boost::any type. It is a thin
/// wrapper around boost::any that provides some convenient type-checking
/// methods and serves a base class for other, more complicated classes that
/// can be held by a Persistable.
///
/// Main Value
/// Since the main value is a boost::any type, it can contain any single-valued
/// type available, including user-defined classes, stl container objects, and
/// PODs. Since part of Datum's intent is to contain data that can be stored
/// in a persistent store, there may be store-dependent limitations on the
/// allowable value types. This does not affect Datum per se, but may affect the
/// ability to store or serialize the data it contains. Values that should be
/// treated as binary large objects (BLOBs) must passed as a
/// std::vector< char >.
///
/// Special Types of main value
///
/// Strings: The main value can hold std::string values, but these must be
/// passed in one of the following ways. 
/// 1) As a pointer to an existing string (not recommended )
/// 2) As a copyable reference to an existing string (recommended)
/// 3) Via in-place construction of a new string object (recommended), eg.
///    SetValue( std::string("My string") );
///
/// BLOBs: BLOB values must be passed in as a std::vector< char >. No other
/// data type will be treated as a BLOB. Reasons:
/// 1) char vectors function exactly like ByteArrays in other languages, so make
///    sense as a way to deal with binary data
/// 2) char vectors provide an easy way to provide the size of the binary data
///    along with the data, without having to know anything about the contents
///    of the data itself. Such size information would have to be provided
///    separately if datum were to accept raw pointers to binary data (and a
///    deep copy would have to be performed, too)
/// 3) char vectors will usually have a straightforward representation
///    in any data store that doesn't natively understand BLOBs, and for stores
///    that do, it is easy to convert a char vector into whatever sort of
///    array of bytes is required by the store adaptor.

namespace Persistence
{

class PERSISTENCE_EXPORT Datum
{
public:

    typedef std::vector<std::string> PSVectorOfStrings;
    ///
    /// Create an instance of Datum.
    /// @param value The main value to be held by this datum.
    Datum( boost::any value );

    /// Copy constructor
    Datum( const Datum& orig );

    ///
    /// Destructor
    virtual ~Datum();

    ///
    /// Returns the main value associated with this datum
    virtual boost::any GetValue() const;

    template <typename T>
    T extract() const
    {
        if( m_value.type() == typeid(T) )
        {
            T result = boost::any_cast<T>(m_value);
            return result;
        }
        else
        {
            std::string e("Unable to cast ");
            e.append( m_value.type().name() );
            e += " to ";
            e.append( typeid(T).name() );
            throw e.c_str();
        }
    }

    ///
    /// Set the main value.
    /// @param value The new contents of the main value.
    virtual bool SetValue( boost::any value );

    ///
    /// Basic typechecking methods
    bool IsBool() const;
    bool IsInt() const;
    bool IsFloat() const;
    bool IsDouble() const;
    bool IsString() const;
    bool IsIntVector() const;
    bool IsFloatVector() const;
    bool IsDoubleVector() const;
    bool IsStringVector() const;
    /// BLOBs have type std::vector< char > -- a containerized byte array.
    bool IsBLOB() const;

    /// Returns true is the held value is one of the following:
    /// std::vector<int>
    /// std::vector<float>
    /// std::vector<double>
    /// std::vector<std::string>
    /// Checking is limited to these types because these are the only ones
    /// that have an easy, direct representation in most relational databases.
    bool IsVectorized() const;

    ///
    /// Convenience versions of the typechecking methods
    bool IsBool( const boost::any& value ) const;
    bool IsInt( const boost::any& value ) const;
    bool IsFloat( const boost::any& value ) const;
    bool IsDouble( const boost::any& value ) const;
    bool IsString( const boost::any& value ) const;
    bool IsIntVector( const boost::any& value ) const;
    bool IsFloatVector( const boost::any& value ) const;
    bool IsDoubleVector( const boost::any& value ) const;
    bool IsStringVector( const boost::any& value ) const;
    bool IsBLOB( const boost::any& value ) const;
    bool IsVectorized( const boost::any& value ) const;

protected:
    ///
    /// Stores the main value associated with this datum
    boost::any m_value;
};

} // namespace Persistence

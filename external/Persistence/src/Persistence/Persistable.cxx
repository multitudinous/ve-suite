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
#include <Persistence/Persistable.h>

#include <Persistence/Datum.h>

#include <sstream>
#include <algorithm>
#include <stdexcept>
#include <string>

#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/algorithm/string/erase.hpp>

namespace Persistence
{

////////////////////////////////////////////////////////////////////////////////
Persistable::Persistable():
    m_UUID( boost::uuids::random_generator()() )
{
    m_UUIDString = boost::lexical_cast< std::string >( m_UUID );
    m_typename = m_UUIDString;
    boost::algorithm::erase_all( m_typename, "-" );
}
////////////////////////////////////////////////////////////////////////////////
Persistable::Persistable( const std::string& typeName ):
    m_UUID( boost::uuids::random_generator()() )
{
    m_UUIDString = boost::lexical_cast< std::string >( m_UUID );
    m_typename = typeName;
}
////////////////////////////////////////////////////////////////////////////////
Persistable::Persistable( const Persistable& orig ):
        m_dataList( orig.m_dataList ),
        m_UUID( orig.m_UUID ),
        m_UUIDString( orig.m_UUIDString ),
        m_typename( orig.m_typename )
{
    // Since DataMap holds smart pointers to instances of Datum, we have to
    // create new instances of the underlying Datum
    DataMap::const_iterator it = orig.m_dataMap.begin();
    while( it != orig.m_dataMap.end() )
    {
        DatumPtr datum( new Datum( *(it->second) ) );
        m_dataMap[ it->first ] = datum;
        ++it;
    }
}
////////////////////////////////////////////////////////////////////////////////
Persistable::~Persistable()
{

}
////////////////////////////////////////////////////////////////////////////////
void Persistable::SetUUID( std::string const& uuid )
{
    m_UUIDString = uuid;
    boost::uuids::string_generator gen;
    m_UUID = gen( uuid );
}
////////////////////////////////////////////////////////////////////////////////
void Persistable::SetUUID( boost::uuids::uuid const& uuid )
{
    m_UUID = uuid;

    std::stringstream ss;
    ss << m_UUID;
    m_UUIDString = ss.str();
}
////////////////////////////////////////////////////////////////////////////////
boost::uuids::uuid const& Persistable::GetUUID() const
{
    return m_UUID;
}
////////////////////////////////////////////////////////////////////////////////
std::string const& Persistable::GetUUIDAsString() const
{
    return m_UUIDString;
}
////////////////////////////////////////////////////////////////////////////////
void Persistable::SetTypeName( std::string name )
{
    m_typename = name;
}
////////////////////////////////////////////////////////////////////////////////
std::string const& Persistable::GetTypeName() const
{
    return m_typename;
}
////////////////////////////////////////////////////////////////////////////////
bool Persistable::DatumExists( std::string const& datumName ) const
{
    bool result = false;

    DataMap::const_iterator iterator = m_dataMap.find( datumName );
    if( iterator != m_dataMap.end() )
    {
        result = true;
    }
    return result;
}
////////////////////////////////////////////////////////////////////////////////
void Persistable::AddDatum( const std::string& datumName,
                          boost::any value )
{
    // No-op if datum with this name already exists
    if( DatumExists( datumName ) )
    {
        return;
    }

    DatumPtr datum = DatumPtr( new Datum( value ) );
    AddDatumPtr( datumName, datum );
}
////////////////////////////////////////////////////////////////////////////////
void Persistable::AddDatumPtr( const std::string& datumName,
                          DatumPtr datum )
{
    // No-op if datum with this name already exists
    if( DatumExists( datumName ) )
    {
        return;
    }

    m_dataMap[ datumName ] = datum;

    m_dataList.push_back( datumName );
}

////////////////////////////////////////////////////////////////////////////////
DatumPtr Persistable::GetDatum( std::string const& datumName ) const
{
    DataMap::const_iterator iterator = m_dataMap.find( datumName );
    if( iterator != m_dataMap.end() )
    {
        return (*iterator ).second;
    }
    else
    {
        std::string error( "Persistable::GetDatum: No datum named " );
        error += datumName;
        throw std::runtime_error( error );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Persistable::SetDatumValue( std::string const& datumName,
                                 boost::any value )
{
    DataMap::const_iterator iterator = m_dataMap.find( datumName );
    if( iterator != m_dataMap.end() )
    {
        ( *iterator ).second->SetValue( value );
    }
}
////////////////////////////////////////////////////////////////////////////////
const std::vector<std::string>& Persistable::GetDataList() const
{
    return m_dataList;
}
////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence

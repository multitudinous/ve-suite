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
#include <Persistence/Datum.h>

#include <boost/concept_check.hpp>

namespace Persistence
{
////////////////////////////////////////////////////////////////////////////////
Datum::Datum( boost::any value )
    :
    m_value( value )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Datum::Datum( const Datum& orig ):
    m_value( orig.m_value )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Datum::~Datum()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
boost::any Datum::GetValue() const
{
    return m_value;
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::SetValue( boost::any value )
{
    m_value = value;
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsBool() const
{
    return IsBool( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsInt() const
{
    return IsInt( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsFloat() const
{
    return IsFloat( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsDouble() const
{
    return IsDouble( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsString() const
{
    return IsString( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsIntVector() const
{
    return IsIntVector( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsFloatVector() const
{
    return IsFloatVector( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsDoubleVector() const
{
    return IsDoubleVector( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsBLOB() const
{
    return IsBLOB( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsStringVector() const
{
    return IsStringVector( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsVectorized() const
{
    return IsVectorized( m_value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsBool( const boost::any& value ) const
{
    return value.type() == typeid ( bool );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsInt( const boost::any& value ) const
{
    return value.type() == typeid ( int );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsFloat( const boost::any& value ) const
{
    return value.type() == typeid ( float );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsDouble( const boost::any& value ) const
{
    return value.type() == typeid ( double );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsString( const boost::any& value ) const
{
    return boost::any_cast<std::string > ( &value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsIntVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< int > >( &value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsFloatVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< float > >( &value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsDoubleVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< double > >( &value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsStringVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< std::string > >( &value );
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsBLOB( const boost::any& value ) const
{
    // If it's not any of our known types, it's a BLOB.
    if( !( (IsBool( value)) || (IsInt( value )) || (IsFloat( value )) ||
           (IsDouble( value )) || (IsString( value )) || (IsVectorized( value ))
        ) )
    {
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
bool Datum::IsVectorized( const boost::any& value ) const
{
    if( ( IsIntVector( value ) ) ||
            ( IsFloatVector( value ) ) ||
            ( IsDoubleVector( value ) ) ||
            ( IsStringVector( value ) )
            )
    {
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence

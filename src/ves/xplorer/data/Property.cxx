/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include <ves/xplorer/data/Property.h>

namespace ves
{
namespace xplorer
{
namespace data
{

Property::Property( boost::any value, bool enabled )
{
    mValue = value;
    mEnabled = enabled;
    mHasMinimum = false;
    mHasMaximum = false;
    mIsEnum = false;
}

////////////////////////////////////////////////////////////////////////////////

Property::~Property( )
{
}

////////////////////////////////////////////////////////////////////////////////

boost::any Property::GetValue( ) const
{
    return mValue;
}

////////////////////////////////////////////////////////////////////////////////

bool Property::SetValue( boost::any value )
{
    // Are changes enabled?
    if( !mEnabled )
    {
        return false;
    }

    // If we already have this value, don't actually do anything except let
    // the caller know the value was set. Note that SignalValueChanged is not
    // emitted and there is no request for validation.
    if( _valuesEqual( value, mValue ) )
    {
        return true;
    }

    // Does external validator allow this change?
    if( !SignalRequestValidation.empty( ) )
    {
        if( !SignalRequestValidation( this, value ) )
        {
            return false;
        }
    }

    // Does value fit in set range?
    if( mHasMinimum )
    {
        if( _compareNumeric( value, '<', mMinimum ) )
        {
            return false;
        }
    }
    if( mHasMaximum )
    {
        if( _compareNumeric( value, '>', mMaximum ) )
        {
            return false;
        }
    }

    // If value is really an enumerated type, is it an allowed value?
    if( mIsEnum )
    {
        if( !_checkEnumValue( value ) )
        {
            return false;
        }
    }

    // If we've passed all the tests, set the new value
    if( !mIsEnum )
    {
        mValue = value;
    }
    else // We have an enum and we must do extra stuff to ensure that the
        // correct value gets set and associated attributes stay synchronized
    {
        _doExtraEnumSetValueProcessing( value );
    }

    // Tell the world the value has changed
    if( !SignalValueChanged.empty( ) )
    {
        SignalValueChanged( this );
    }
    return true;
}

////////////////////////////////////////////////////////////////////////////////

void Property::SetEnabled( Property* caller )
{
    // Don't do anything if we're already enabled
    if( !mEnabled )
    {
        mEnabled = true;

        // Tell the world we're enabled
        if( !SignalEnabled.empty( ) )
        {
            SignalEnabled( this );
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

void Property::SetDisabled( Property* caller )
{
    // Don't do anything if we're already disabled
    if( mEnabled )
    {
        mEnabled = false;

        // Tell the world we're disabled
        if( !SignalDisabled.empty( ) )
        {
            SignalDisabled( this );
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

bool Property::GetEnabled( ) const
{
    return mEnabled;
}

////////////////////////////////////////////////////////////////////////////////

void Property::SetAttribute( const std::string& attributeName,
                             boost::any attributeValue )
{
    // Check whether this attribute exists, and if so, compare its value to the
    // one being passed in. If the two are the same, don't change anything and
    // don't emit any signals.
    if( AttributeExists( attributeName ) )
    {
        boost::any existingValue = GetAttribute( attributeName );
        if( _valuesEqual( existingValue, attributeValue ) )
        {
            return;
        }
    }

    // Implicitly creates a new key-value pair if the key doesn't already exist
    mAttributeMap[attributeName] = attributeValue;

    // Check for minimumValue and maximumValue attributes and cache them
    // in mMinimum and mMaximum.
    if( attributeName == std::string( "minimumValue" ) )
    {
        mHasMinimum = true;
        _cacheDoubleValue( &mMinimum, attributeValue );
    }

    if( attributeName == std::string( "maximumValue" ) )
    {
        mHasMaximum = true;
        _cacheDoubleValue( &mMaximum, attributeValue );
    }

    // Check for Enum status, and set internal flag
    if( attributeName == std::string( "enumValues" ) )
    {
        // Start off false, and only set true if everything checks out
        mIsEnum = false;

        // Main value MUST be integer type to be considered enum
        if( IsInt( ) )
        {
            // If attributeValue is empty, we don't consider this a proper enum
            if( !attributeValue.empty( ) )
            {
                PSVectorOfStrings *enumValues =
                        boost::any_cast<PSVectorOfStrings > ( &attributeValue );
                // If cast was successful, continue...
                if( enumValues )
                {
                    // Vector must contain at least one string to be proper enum
                    if( enumValues->size( ) > 0 )
                    {
                        // Ladies and gentlemen, we have an enum!
                        mIsEnum = true;

                        // If mValue is outside range allowed by enumValues,
                        // set it to zero.
                        int mainValue = boost::any_cast<int>( mValue );
                        int size = static_cast < int > ( enumValues->size( ) );
                        if( ( mainValue < 0 ) || ( mainValue >= size ) )
                        {
                            SetValue( 0 );
                            // Redo this since we need true current value below
                            mainValue = boost::any_cast<int>( mValue );
                        }

                        // Add/Update the enumCurrentString attribute
                        std::string enumString = ( *enumValues )[mainValue];
                        SetAttribute( "enumCurrentString", enumString );

                        // Add/update the enumSize attribute
                        SetAttribute( "enumSize", size );
                    }
                }
            }
        }
    } // attr = enumValues

    // Tell the world an attribute has changed
    if( !SignalAttributeChanged.empty( ) )
    {
        SignalAttributeChanged( this );
    }
}

////////////////////////////////////////////////////////////////////////////////

boost::any Property::GetAttribute( const std::string& attributeName ) const
{
    // If the attribute doesn't exist, return empty value and get out
    if( !AttributeExists( attributeName ) )
    {
        return boost::any( );
    }

    AttributeMap::const_iterator iterator = mAttributeMap.find( attributeName );
    return (*iterator ).second;
}

////////////////////////////////////////////////////////////////////////////////

const Property::PSVectorOfStrings& Property::GetAttributeList( ) const
{
    mAttributeList.clear( );
    for( AttributeMap::const_iterator iterator = mAttributeMap.begin( );
            iterator != mAttributeMap.end( );
            iterator++ )
    {
        mAttributeList.push_back( ( *iterator ).first );
    }
    return mAttributeList;
}

////////////////////////////////////////////////////////////////////////////////

bool Property::AttributeExists( const std::string& attributeName ) const
{
    bool result = false;

    AttributeMap::const_iterator iterator = mAttributeMap.find( attributeName );
    if( iterator != mAttributeMap.end( ) )
    {
        result = true;
    }
    return result;
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsBool( ) const
{
    return IsBool( mValue );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsInt( ) const
{
    return IsInt( mValue );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsFloat( ) const
{
    return IsFloat( mValue );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsDouble( ) const
{
    return IsDouble( mValue );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsString( ) const
{
    return IsString( mValue );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsEnum( )
{
    return mIsEnum;
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsIntVector( ) const
{
    return IsIntVector( mValue );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsFloatVector( ) const
{
    return IsFloatVector( mValue );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsDoubleVector( ) const
{
    return IsDoubleVector( mValue );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsStringVector( ) const
{
    return IsStringVector( mValue );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsVectorized( ) const
{
    return IsVectorized( mValue );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsBool( const boost::any& value ) const
{
    return value.type( ) == typeid ( bool );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsInt( const boost::any& value ) const
{
    return value.type( ) == typeid ( int );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsFloat( const boost::any& value ) const
{
    return value.type( ) == typeid ( float );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsDouble( const boost::any& value ) const
{
    return value.type( ) == typeid ( double );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsString( const boost::any& value ) const
{
    return boost::any_cast<std::string > ( &value );
}

////////////////////////////////////////////////////////////////////////////////

bool Property::IsIntVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< int > >( &value );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsFloatVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< float > >( &value );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsDoubleVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< double > >( &value );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsStringVector( const boost::any& value ) const
{
    return boost::any_cast< std::vector< std::string > >( &value );
}
////////////////////////////////////////////////////////////////////////////////

bool Property::IsVectorized( const boost::any& value ) const
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

void Property::_cacheDoubleValue( double* store, boost::any value )
{
    if( IsInt( value ) )
    {
        *store = static_cast < double > ( boost::any_cast<int>( value ) );
    }
    else if( IsFloat( value ) )
    {
        *store = static_cast < double > ( boost::any_cast<float>( value ) );
    }
    else if( IsDouble( value ) )
    {
        *store = boost::any_cast<double>( value );
    }
}

////////////////////////////////////////////////////////////////////////////////

bool Property::_compareNumeric( boost::any left, char operation, double right )
{
    double m_castLeft = 0;
    bool result = false;

    if( IsInt( left ) )
    {
        m_castLeft = static_cast < double > ( boost::any_cast<int>( left ) );
    }
    else if( IsFloat( left ) )
    {
        m_castLeft = static_cast < double > ( boost::any_cast<float>( left ) );
    }
    else if( IsDouble( ) )
    {
        m_castLeft = boost::any_cast<double>( left );
    }
    else // Not an int, float, or double, but we think it's numeric....
        // What are we dealing with?
    {
        return false;
    }

    switch( operation )
    {
    case '<':
        result = ( m_castLeft < right );
        break;
    case '>':
        result = ( m_castLeft > right );
        break;
    case '=':
        result = ( m_castLeft == right );
        break;
    default:
        result = false;
    }

    return result;
}

bool Property::_checkEnumValue( boost::any value )
{
    boost::any enumVector = GetAttribute( "enumValues" );
    // If there's nothing in the enumValues attribute (which really should
    // never happen, but just in case...), skip the rest of this test and
    // **ALLOW THE VALUE TO BE SET**
    if( !enumVector.empty( ) )
    {
        // Attempt to cast enumVector as a PSVectorOfStrings
        PSVectorOfStrings *castEnumVector =
                boost::any_cast< PSVectorOfStrings > ( &enumVector );
        // If the cast returned NULL, fail out
        if( !castEnumVector )
        {
            return false;
        }
        // There are two ways to set the value of an enum: via its integer
        // index, or via its string name. Take each possibility in turn.
        if( IsInt( value ) )
        {
            // Get the size of the vector
            size_t max = castEnumVector->size( );
            // Adjust max since maximum index is always one less than size
            max -= 1;
            // If value is outside of allowable index range, disallow set
            if( ( _compareNumeric( value, '<', 0 ) ) ||
                    ( _compareNumeric( value, '>', max ) ) )
            {
                return false;
            }
        }
        else if( IsString( value ) )
        {
            // Check whether value is one of the allowable strings
            std::string castValue = boost::any_cast<std::string > ( value );
            boost::any enumValues = GetAttribute( "enumValues" );
            PSVectorOfStrings castEnumValues =
                    boost::any_cast< PSVectorOfStrings > ( enumValues );
            PSVectorOfStrings::const_iterator iterator = castEnumValues.begin( );
            PSVectorOfStrings::const_iterator end = castEnumValues.end( );
            bool found = false;
            while( ( !found ) && ( iterator != end ) )
            {
                if( ( *iterator ) == castValue )
                {
                    found = true;
                }
                iterator++;
            }
            if( !found )
            {
                return false;
            }
        }
    }
    return true;
}

void Property::_doExtraEnumSetValueProcessing( boost::any value )
{
    if( IsInt( value ) )
    {
        // We have the native type (int), so go ahead and set the value
        mValue = value;
    }
    else // Must have a std::string
    {
        // Convert the string to the proper index to store in main value
        std::string castValue = boost::any_cast<std::string > ( value );
        boost::any enumValues = GetAttribute( "enumValues" );
        PSVectorOfStrings castEnumValues =
                boost::any_cast< PSVectorOfStrings > ( enumValues );
        int max = static_cast < int > ( castEnumValues.size( ) );
        max -= 1;
        bool found = false;
        int count = -1;
        while( ( !found ) && ( count < max ) )
        {
            count++;
            if( castEnumValues[count] == castValue )
            {
                found = true;
            }
        }
        mValue = count;
    }

    // Set the enumCurrentString attribute
    int mainValue = boost::any_cast<int>( mValue );
    boost::any enumValues = GetAttribute( "enumValues" );
    PSVectorOfStrings castEnumValues =
            boost::any_cast< PSVectorOfStrings > ( enumValues );
    std::string enumString = castEnumValues[mainValue];
    SetAttribute( "enumCurrentString", enumString );
}

bool Property::_valuesEqual( boost::any& one, boost::any& two )
{
    bool result = false;
    // Check whether the two values are of the same type
    if( IsInt( one ) && IsInt( two ) )
    {
        int castOne = boost::any_cast<int>( one );
        int castTwo = boost::any_cast<int>( two );
        if( castOne == castTwo )
        {
            result = true;
        }
    }
    else if( IsFloat( one ) && IsFloat( two ) )
    {
        float castOne = boost::any_cast<float>( one );
        float castTwo = boost::any_cast<float>( two );
        if( castOne == castTwo )
        {
            result = true;
        }
    }
    else if( IsDouble( one ) && IsDouble( two ) )
    {
        double castOne = boost::any_cast<double>( one );
        double castTwo = boost::any_cast<double>( two );
        if( castOne == castTwo )
        {
            result = true;
        }
    }
    else if( IsBool( one ) && IsBool( two ) )
    {
        bool castOne = boost::any_cast<bool>( one );
        bool castTwo = boost::any_cast<bool>( two );
        if( castOne == castTwo )
        {
            result = true;
        }
    }
    else if( IsString( one ) && IsString( two ) )
    {
        std::string castOne = boost::any_cast<std::string > ( one );
        std::string castTwo = boost::any_cast<std::string > ( two );
        if( castOne == castTwo )
        {
            result = true;
        }
    }

    return result;
}

} // namespace data
} // namespace xplorer
} // namespace ves

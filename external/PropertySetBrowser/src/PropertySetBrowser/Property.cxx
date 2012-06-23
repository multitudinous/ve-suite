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
#include "Property.h"

#include <boost/concept_check.hpp>

namespace PropertySetBrowser
{
////////////////////////////////////////////////////////////////////////////////
Property::Property( boost::any value, bool enabled )
    :
    Datum( value ),
    mEnabled( enabled ),
    mHasMinimum( false ),
    mHasMaximum( false ),
    mIsEnum( false )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Property::~Property()
{
    ;
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
    if( _valuesEqual( value, m_value ) )
    {
        return true;
    }

    // Does external validator allow this change?
    if( !SignalRequestValidation.empty() )
    {
        bool valid = *SignalRequestValidation( shared_from_this(), value );
        if( !valid )
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
        Persistence::Datum::SetValue( value );
    }
    else // We have an enum and we must do extra stuff to ensure that the
        // correct value gets set and associated attributes stay synchronized
    {
        _doExtraEnumSetValueProcessing( value );
    }

    // Tell the world the value has changed
    if( !SignalValueChanged.empty() )
    {
        //PropertyPtr tempPtr = PropertyPtr( this );
        SignalValueChanged( shared_from_this() );
    }
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Property::SetEnabled( PropertyPtr caller )
{
    boost::ignore_unused_variable_warning( caller );
    // Don't do anything if we're already enabled
    if( !mEnabled )
    {
        mEnabled = true;

        // Tell the world we're enabled
        if( !SignalEnabled.empty() )
        {
            //PropertyPtr tempPtr = PropertyPtr( this );
            SignalEnabled( shared_from_this() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Property::SetDisabled( PropertyPtr caller )
{
    boost::ignore_unused_variable_warning( caller );
    // Don't do anything if we're already disabled
    if( mEnabled )
    {
        mEnabled = false;

        // Tell the world we're disabled
        if( !SignalDisabled.empty() )
        {
            //PropertyPtr tempPtr = PropertyPtr( this );
            SignalDisabled( shared_from_this() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool Property::GetEnabled() const
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
        boost::any existingValue = GetAttribute( attributeName )->GetValue();
        if( _valuesEqual( existingValue, attributeValue ) )
        {
            return;
        }
    }

    if( !mAttributes.DatumExists( attributeName ) )
    {
        mAttributes.AddDatum( attributeName, attributeValue );
    }
    else
    {
        mAttributes.SetDatumValue( attributeName, attributeValue );
    }

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

        // Main value MUST be string type to be considered enum
        if( IsString() )
        {
            // If attributeValue is empty, we don't consider this a proper enum
            if( !attributeValue.empty() )
            {
                PSVectorOfStrings* enumValues =
                        boost::any_cast<PSVectorOfStrings > ( &attributeValue );
                // If cast was successful, continue...
                if( enumValues )
                {
                    // Vector must contain at least one string to be proper enum
                    if( enumValues->size() > 0 )
                    {
                        mIsEnum = true;

                        // If mValue is not equal to a valid enum value,
                        // set mValue to first allowable string
                        int index = 0;
                        std::string mainValue = boost::any_cast<std::string>( m_value );
                        PSVectorOfStrings::const_iterator it = enumValues->begin();
                        while( it != enumValues->end() )
                        {
                            if( *it == mainValue )
                            {
                                break;
                            }
                            ++it;
                            ++index;
                        }
                        if( it == enumValues->end() )
                        {
                            index = 0;
                            mainValue = enumValues->at( 0 );
                            SetValue( mainValue );
                        }

                        // Add/Update the enumCurrentIndex attribute
                        SetAttribute( "enumCurrentIndex", index );

                        // Add/update the enumSize attribute
                        int size = enumValues->size();
                        SetAttribute( "enumSize", size );
                    }
                }
            }
        }
    } // attr = enumValues

    // Tell the world an attribute has changed
    if( !SignalAttributeChanged.empty() )
    {
        //PropertyPtr tempPtr = PropertyPtr( this );
        SignalAttributeChanged( shared_from_this() );
    }
}
////////////////////////////////////////////////////////////////////////////////
Persistence::DatumPtr Property::GetAttribute( const std::string& attributeName ) const
{
    return mAttributes.GetDatum( attributeName );
}
////////////////////////////////////////////////////////////////////////////////
const Property::PSVectorOfStrings& Property::GetAttributeList() const
{
    return mAttributes.GetDataList();
}
////////////////////////////////////////////////////////////////////////////////
bool Property::AttributeExists( const std::string& attributeName ) const
{
    return mAttributes.DatumExists( attributeName );
}
////////////////////////////////////////////////////////////////////////////////
bool Property::IsEnum()
{
    return mIsEnum;
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
    else if( IsDouble() )
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
////////////////////////////////////////////////////////////////////////////////
bool Property::_checkEnumValue( boost::any value )
{
    PSVectorOfStrings castEnumVector;
    try
    {
        castEnumVector = GetAttribute( "enumValues" )->extract< PSVectorOfStrings >();
    }
    catch(...)
    {
        return true;
    }

    // There are two ways to set the value of an enum: via its integer
    // index, or via its string name. Take each possibility in turn.
    if( IsInt( value ) )
    {
        // Get the size of the vector
        size_t max = castEnumVector.size();
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
        PSVectorOfStrings::const_iterator iterator = castEnumVector.begin();
        PSVectorOfStrings::const_iterator end = castEnumVector.end();
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

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Property::_doExtraEnumSetValueProcessing( boost::any value )
{
    int index = 0;
    if( IsInt( value ) )
    {
        // Find the associated string value and set it
        int castValue = boost::any_cast< int > ( value );
        PSVectorOfStrings castEnumValues =
                GetAttribute( "enumValues" )->extract< PSVectorOfStrings >();
        if( castValue < ( castEnumValues.size() - 1 ) )
        {
            m_value = castEnumValues.at( castValue );
            index = castValue;
        }
    }
    else // Must have a std::string
    {
        // Convert the string to the proper index to store in main value
        std::string castValue = boost::any_cast<std::string > ( value );
        PSVectorOfStrings castEnumValues =
                GetAttribute( "enumValues" )->extract< PSVectorOfStrings >();
        int max = static_cast < int > ( castEnumValues.size() );
        max -= 1;
        bool found = false;
        int count = -1;
        while( ( !found ) && ( count < max ) )
        {
            count++;
            if( castEnumValues[count] == castValue )
            {
                found = true;
                index = count;
            }
        }

        if( found )
        {
            m_value = value;
        }
        else
        {
            std::string nullstring;
            m_value = nullstring;
        }
    }

    SetAttribute( "enumCurrentIndex", index );
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
} // namespace

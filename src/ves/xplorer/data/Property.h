/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/xplorer/data/PropertyPtr.h>

#include <string>
#include <vector>
#include <map>

#include <boost/any.hpp>
#include <boost/signal.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <ves/VEConfig.h>

// TODO: Check through const status everywhere
// TODO: Add in DB read/write functionality
// TODO: Finish doxygen comments

/// @class Property holds a main value in a boost::any type.
///
/// Data Validation
/// Data validation when setting the main value (after initial set during
/// construction) is accomplished via the SignalRequestValidation signal,
/// which expects a boolean return value. This mechanism allows the owner of
/// the property to hand off references or pointers to the property while still
/// maintaining control over the allowed values, or to connect the property
/// to some external validation routine supplied by another object.
///
/// Main Value
/// Since the main value is a boost::any type, it can contain any single-valued
/// type available, including user-defined classes, stl container objects, and
/// PODs.
///
/// Special Types of main value
///
/// Strings: The main value can easily hold std::sting values, but these must be
/// passed in one of the following ways. 
/// 1) As a pointer to an existing string (not recommended )
/// 2) As a copyable reference to an existing string (recommended)
/// 3) Via in-place construction of a new string object (recommended), eg.
/// SetValue( std::string("My string") );
///
/// Enum types: C/C++ enumerated types cannot be used directly since there is no
/// way to allow compile-time access to the enumerator definition without
/// subclassing Property (which is one way to handle enum types if the following
/// pseudo-enum does not meet your requirements).
/// Setting up a pseudo-enum type works as follows.
/// 1) The main value MUST be an integer, and it must be set before meeting
/// the other requirements.
/// 2) Call SetAttribute with the attribute name "enumValues" and a NON-EMPTY
/// Property::PSVectorOfStrings representing the "names" of the allowable enum
/// types. An attribute named "enumSize" and containing the number of items
/// in the enumValues vector will be automatically added to the property's
/// attributes. This attribute can be queried by callers who don't want to
/// or cannot deal with the PSVectorOfStrings directly. Another attribute called
/// "enumCurrentString" will also be added. Its value will always contain
/// the string referred to by enumValues[mainValue]. If the pre-existing main
/// value is outside the allowable range, it will be reset to zero.
/// 3) On all future calls to SetValue, the (integer) value will be restricted
/// to lying within the index range of the enumValues vector. GetValue will
/// return an integer that is an index into the enumValues vector.
/// 4) SetValue may also be called with a std::string that is equal to one
/// of the strings in the enumValues vector, and this will be converted to the
/// proper integer value.
/// 5) Although not recommended practice, it is possible to convert a
/// pseudo-enum back to a normal integer by calling
/// SetAttribute( "enumValues", val ), where val is anything besides a valid
/// Property::PSVectorOfStrings.

namespace ves
{
namespace xplorer
{
namespace data
{


class VE_DATA_EXPORTS Property : public boost::signals::trackable, public boost::enable_shared_from_this< Property >
{
public:

    typedef std::vector<std::string> PSVectorOfStrings;
    typedef std::map<std::string, boost::any> AttributeMap;
    ///
    /// Create an instance of Property.
    /// @param value The main value to be held by this property.
    /// @param enabled Whether this property should be created with access to
    ///        SetValue turned on. Defaults to true.
    /// The signals SignalValueChanged, SignalEnabled, and SignalDisabled are
    /// NOT emitted at construction, since there is no way for there to be a
    /// listener to these signals until after creation is complete.
    Property( boost::any value, bool enabled = true );

    ///
    /// Destructor
    virtual ~Property( );

    ///
    /// Returns the main value associated with this property
    boost::any GetValue( ) const;

    ///
    /// Set the main value.
    /// @param value The new contents of the main value.
    /// This method does a few things before actually changing the value:
    /// 1. It checks whether the property is enabled. If not, no change happens.
    /// 2. It emits a SignalRequestValidation signal, and waits for the bool
    ///    return value. If signal returns true, we proceed.
    ///    If false, the value is not changed.
    /// 3. We look through the AttributeMap for some common internal settings
    ///    such as minimumValue, maximumValue, etc. and do basic checking against
    ///    these attributes. If we pass these tests, the value is changed. If
    ///    any of those tests fail, we don't change the value.
    bool SetValue( boost::any value );

    ///
    /// Adds or sets a secondary attribute.
    /// @param attributeName The unique name associated with this attribute.
    /// @param attributeValue The value to give the attribute. If the value is
    ///        to be a std::string, it must be passed either as a copyable
    ///        instance of a std::string, or with in-place construction; ie, as
    ///        std::string("My String").
    /// Examples of secondary attributes are a UI_Label, a minimum allowable
    /// value, a maximum allowable value, etc. This allows great extensibility
    /// because pretty much any extra data that needs to travel with this
    /// property can be shoved in as an attribute.
    void SetAttribute( const std::string& attributeName, boost::any attributeValue );

    ///
    /// Returns the associated value of a given attribute
    boost::any GetAttribute( const std::string& attributeName ) const;

    ///
    /// Returns a list of all attribute names owned by this property
    const PSVectorOfStrings& GetAttributeList( ) const;

    ///
    /// Returns true if this property has an attribute named attributeName
    bool AttributeExists( const std::string& attributeName ) const;

    ///
    /// Enables access to SetValue
    /// This can be connected as a slot to signals emitted by another instance
    /// of this object (caller).The parameter caller is ignored by these
    /// methods; it appears here only so that it can match the signature of all
    /// signals emitted by this class.
    void SetEnabled( PropertyPtr caller = PropertyPtr() );

    ///
    /// Disables access to SetValue
    /// See notes for SetEnabled
    void SetDisabled( PropertyPtr caller = PropertyPtr() );

    ///
    /// Returns whether this property is enabled. If the property is enabled,
    /// its main value can be changed through calls to SetValue. If it is not
    /// enabled, no changes are allowed to the main value via SetValue.
    bool GetEnabled( ) const;

    ///
    /// Property emits this signal anytime something attempts to change its value.
    /// A validation slot can subscribe to this and return true to allow the
    /// change, or false to disallow it.
    boost::signal< bool ( PropertyPtr, boost::any ) > SignalRequestValidation;

    ///
    /// Signals to which other objects can subscribe to get basic state updates
    /// about this property. A pointer to this instance is passed in the signal
    /// since many slots will be concerned with the identity of the caller in
    /// order to choose the appropriate action.
    boost::signal< void ( PropertyPtr ) > SignalValueChanged;
    boost::signal< void ( PropertyPtr ) > SignalAttributeChanged;
    boost::signal< void ( PropertyPtr ) > SignalEnabled;
    boost::signal< void ( PropertyPtr ) > SignalDisabled;

    ///
    /// Basic typechecking methods
    bool IsBool( ) const;
    bool IsInt( ) const;
    bool IsFloat( ) const;
    bool IsDouble( ) const;
    bool IsString( ) const;
    bool IsEnum( );
    bool IsIntVector( ) const;
    bool IsFloatVector( ) const;
    bool IsDoubleVector( ) const;
    bool IsStringVector( ) const;
    bool IsVectorized( ) const;

    ///
    /// Convenience versions of the typechecking methods
    /// Notice there is no conveneince version of IsEnum since enum
    /// status is held as a Property Attribute and is not directly discoverable
    /// from the boost::any value.
    bool IsBool( const boost::any& value ) const;
    bool IsInt( const boost::any& value ) const;
    bool IsFloat( const boost::any& value ) const;
    bool IsDouble( const boost::any& value ) const;
    bool IsString( const boost::any& value ) const;
    bool IsIntVector( const boost::any& value ) const;
    bool IsFloatVector( const boost::any& value ) const;
    bool IsDoubleVector( const boost::any& value ) const;
    bool IsStringVector( const boost::any& value ) const;
    bool IsVectorized( const boost::any& value ) const;

private:

    ///
    /// Extracts a numeric quantity from value, casts it as a double type,
    /// and puts it into the double variable pointed to by store.
    void _cacheDoubleValue( double* store, boost::any value );

    ///
    /// Performs numerical comparisons of the values in left and right.
    /// This handles extracting the proper type from the boost::any value in
    /// passed in as left. Operation may be one of '<', '=', '>'.
    bool _compareNumeric( boost::any left, char operation, double right );

    // TODO: document
    bool _checkEnumValue( boost::any value );
    void _doExtraEnumSetValueProcessing( boost::any value );

    ///
    /// Checks whether the values passed in as boost::any types are equal.
    /// Values are considered to be equal only if both their types and their
    /// values match. For example,
    /// _valuesEqual( boost::any(2), boost::any(2) )
    /// would return true, but
    /// _valuesEqual( boost::any(2), boost::any(2.) )
    /// would return false because the types do not match: the first argument is
    /// an integer and the second is a floating point value.
    bool _valuesEqual(boost::any & one, boost::any & two);

    ///
    /// Stores the main value associated with this property
    boost::any mValue; 

    ///
    /// Is the property currently enabled? Default true. If false,
    /// no changes to mValue are allowed.
    bool mEnabled; 

    ///
    /// Min, Max, UI_Label, etc. are just attributes put into the attribute map.
    /// This allows the class to be extremely flexible in terms of what
    /// extra information it can hold.
    AttributeMap mAttributeMap;

    ///
    /// Stores whether this property has a minimum value so that we need not
    /// search through the attribute map each time we need to know this
    /// information
    bool mHasMinimum;

    ///
    /// Stores whether this property has a maximum value so that we need not
    /// search through the attribute map each time we need to know this
    /// information
    bool mHasMaximum;

    ///
    /// Cached value of minimumValue attribute.
    /// Note it will always be cast to type double!
    double mMinimum;

    ///
    /// Cached value of maximumValue attribute.
    /// Note it will always be cast to type double!
    double mMaximum; 

    ///
    /// Set true whenever mValue.type == int AND there exists an
    /// attribute with key "enumValues" and value of type
    /// std::vector<std::string>.
    /// This member variable is set so that we don't have to look through
    /// the attribute map every time we want to check whether this property is
    /// an enum.
    bool mIsEnum; 

    mutable PSVectorOfStrings mAttributeList;
};

} // namespace data
} // namespace xplorer
} // namespace ves

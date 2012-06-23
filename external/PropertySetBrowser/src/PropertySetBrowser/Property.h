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
#include <Persistence/Datum.h>
#include <Persistence/Persistable.h>

#include <string>
#include <vector>
#include <map>

#include <boost/any.hpp>
#include <boost/signals2/signal.hpp>
#include <boost/enable_shared_from_this.hpp>

#include <PropertySetBrowser/Exports.h>
#include <PropertySetBrowser/PropertyPtr.h>

// TODO: Check through const status everywhere
/// @file Property.h
/// @namespace PropertySetBrowser
/// @class Property holds a main value in a boost::any type, and can also
/// hold attributes that give extra information about the main value or
/// that alter the way the main value is used.
///
/// Attributes
/// Attributes are key-value pairs in which the key is a std::string and the
/// value is a boost::any. This allows attributes to store more or less anything
/// you need to travel along with the main value of a property. There are a
/// few special attributes that are baked into the PropertySetBrowser library.
/// * "userVisible" -- adding this attribute to a property and setting it to
///    false indicated to the PropertyBrowser widget that this property should
///    not be displayed to the user. Setting the value to true indicates that
///    the property *should* be displayed.
/// * "minimumValue" -- adding this attribute to a numerical property will
///    constrain the main value so that it must be equal to or greater than the
///    minimum value.
/// * "maximumValue" -- adding this attribute to a numerical property will
///    constrain the main value so that it must be equal to or less than the
///    maximum value.
/// * "enumValues", "enumSize", "enumCurrentIndex" -- see the documention below
///    regarding special types of main value for a description of these
///    attributes.
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
/// Strings: The main value can easily hold std::string values, but these must be
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
/// 1) The main value MUST be an std::string, and it must be set before meeting
/// the other requirements.
/// 2) Call SetAttribute with the attribute name "enumValues" and a NON-EMPTY
/// Property::PSVectorOfStrings representing the "names" of the allowable enum
/// types. An attribute named "enumSize" and containing the number of items
/// in the enumValues vector will be automatically added to the property's
/// attributes. This attribute can be queried by callers who don't want to
/// or cannot deal with the PSVectorOfStrings directly. Another attribute called
/// "enumCurrentIndex" will also be added. Its value will always contain
/// the integer index referred to by enumValues[mainValue]. If the pre-existing main
/// value is outside the allowable range, it will be reset to zero.
/// 3) SetValue may be called with a string argument or an integer argument. A
/// string argument should correspond to one of the values of the enumValues,
/// and in integer argument should correspond to a valid index, that is it
/// should lie on the interval [0, enumSize).
/// 4) Although not recommended practice, it is possible to convert a
/// pseudo-enum back to a normal integer by calling
/// SetAttribute( "enumValues", val ), where val is anything besides a valid
/// Property::PSVectorOfStrings.

namespace PropertySetBrowser
{


class PROPERTYSETBROWSER_EXPORT Property : public boost::signals2::trackable,
                                 public Persistence::Datum,
                                 public boost::enable_shared_from_this< Property >
{
public:

    typedef std::vector<std::string> PSVectorOfStrings;
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
    virtual ~Property();

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
    Persistence::DatumPtr GetAttribute( const std::string& attributeName ) const;

    ///
    /// Returns a list of all attribute names owned by this property
    const PSVectorOfStrings& GetAttributeList() const;

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
    bool GetEnabled() const;

    ///
    /// Property emits this signal anytime something attempts to change its value.
    /// A validation slot can subscribe to this and return true to allow the
    /// change, or false to disallow it.
    boost::signals2::signal< bool ( PropertyPtr, boost::any ) > SignalRequestValidation;

    ///
    /// Signals to which other objects can subscribe to get basic state updates
    /// about this property. A pointer to this instance is passed in the signal
    /// since many slots will be concerned with the identity of the caller in
    /// order to choose the appropriate action.
    boost::signals2::signal< void ( PropertyPtr ) > SignalValueChanged;
    boost::signals2::signal< void ( PropertyPtr ) > SignalAttributeChanged;
    boost::signals2::signal< void ( PropertyPtr ) > SignalEnabled;
    boost::signals2::signal< void ( PropertyPtr ) > SignalDisabled;

    ///
    /// Extensions to typechecking methods in Persistable base class
    bool IsEnum();

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
    /// Is the property currently enabled? Default true. If false,
    /// no changes to m_value are allowed.
    bool mEnabled; 

    ///
    /// Min, Max, UI_Label, etc. are just attributes stored in a Persistable.
    /// This allows the class to be extremely flexible in terms of what
    /// extra information it can hold.
    Persistence::Persistable mAttributes;

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
    /// Set true whenever m_value.type == int AND there exists an
    /// attribute with key "enumValues" and value of type
    /// std::vector<std::string>.
    /// This member variable is set so that we don't have to look through
    /// the attribute map every time we want to check whether this property is
    /// an enum.
    bool mIsEnum; 

    /// Is used to return a reference to a list of all available attributes
    /// for this property. Mutable so the method GetAttributeList() can be
    /// const, since conceptually it is. This list is only used for this single
    /// purpose, and is only updated when GetAttributeList is called.
    mutable PSVectorOfStrings mAttributeList;
};

} // namespace

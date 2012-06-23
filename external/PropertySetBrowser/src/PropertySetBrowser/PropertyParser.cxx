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
#include <PropertySetBrowser/PropertyParser.h>

#include <iostream>
#include <cmath>

namespace PropertySetBrowser
{
////////////////////////////////////////////////////////////////////////////////
PropertyParser::PropertyParser( QObject* parent ) :
    QObject( parent ),
    m_ignoreValueChanges( false ),
    m_logger( Poco::Logger::get("PropertyParser") ),
    m_logStream( LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    // Create a standard set of property managers
    mDoubleManager = new QtDoublePropertyManager( this );
    mStringManager = new QtStringPropertyManager( this );
    mBooleanManager = new QtBoolPropertyManager( this );
    mEnumManager = new QtEnumPropertyManager( this );
    mGroupManager = new QtGroupPropertyManager( this );
    mIntManager = new QtIntPropertyManager( this );

    // Connect managers' valueChanged signals to our slots
    connect( mBooleanManager, SIGNAL( valueChanged( QtProperty*, bool ) ),
             this, SLOT( BoolValueChanged( QtProperty*, bool ) ) );
    connect( mIntManager, SIGNAL( valueChanged( QtProperty*, int ) ),
             this, SLOT( IntValueChanged( QtProperty*, int ) ) );
    connect( mDoubleManager, SIGNAL( valueChanged( QtProperty*, double ) ),
             this, SLOT( DoubleValueChanged( QtProperty*, double ) ) );
    connect( mEnumManager, SIGNAL( valueChanged( QtProperty*, int ) ),
             this, SLOT( IntValueChanged( QtProperty*, int ) ) );
    connect( mStringManager, SIGNAL( valueChanged( QtProperty*, QString ) ),
             this, SLOT( StringValueChanged( QtProperty*, QString ) ) );
}
////////////////////////////////////////////////////////////////////////////////
PropertyParser::~PropertyParser()
{
    // Delete items
    size_t max = mItems.size();
    for( size_t index = 0; index < max; index++ )
    {
        delete mItems[index];
    }

    mItems.clear();
    mProperties.clear();
    mTreedItems.clear();
    mGroupManager->clear();
    mBooleanManager->clear();
    mEnumManager->clear();
    mIntManager->clear();
    mDoubleManager->clear();
    mStringManager->clear();

    std::map< std::string, ExternalStringSelectManager* >::iterator itr =
            m_customStringManagers.begin();
    while( itr != m_customStringManagers.end() )
    {
        itr->second->clear();
        delete itr->second;
        ++itr;
    }

    delete mGroupManager;
    delete mBooleanManager;
    delete mEnumManager;
    delete mIntManager;
    delete mDoubleManager;
    delete mStringManager;
}
////////////////////////////////////////////////////////////////////////////////
QtDoublePropertyManager* PropertyParser::GetDoubleManager()
{
    return mDoubleManager;
}
////////////////////////////////////////////////////////////////////////////////
QtStringPropertyManager* PropertyParser::GetStringManager()
{
    return mStringManager;
}
////////////////////////////////////////////////////////////////////////////////
QtBoolPropertyManager* PropertyParser::GetBoolManager()
{
    return mBooleanManager;
}
////////////////////////////////////////////////////////////////////////////////
QtEnumPropertyManager* PropertyParser::GetEnumManager()
{
    return mEnumManager;
}
////////////////////////////////////////////////////////////////////////////////
QtGroupPropertyManager* PropertyParser::GetGroupManager()
{
    return mGroupManager;
}
////////////////////////////////////////////////////////////////////////////////
QtIntPropertyManager* PropertyParser::GetIntManager()
{
    return mIntManager;
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::AddCustomExternalStringManager(
        ExternalStringSelectManager* manager,
        const std::string& attributeFlag )
{
    // Reparent the manager to keep the Qt hierarchy correct
    manager->setParent(this);
    connect( manager, SIGNAL(valueChanged(QtProperty*,QString)),
                 this, SLOT(StringValueChanged(QtProperty*,QString)));
    m_customStringManagers[ attributeFlag ] = manager;
}
////////////////////////////////////////////////////////////////////////////////
PropertyParser::ItemVector* PropertyParser::GetItems()
{
    return &mTreedItems;
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::ParsePropertySet( PropertySetPtr set )
{
    LOG_TRACE( "ParsePropertySet" );

    // Delete old items, etc. before adding new
    size_t max = mItems.size();
    for( size_t index = 0; index < max; index++ )
    {
        delete mItems[index];
    }
    mItems.clear();
    mProperties.clear();
    mTreedItems.clear();
    mGroupManager->clear();
    mBooleanManager->clear();
    mEnumManager->clear();
    mIntManager->clear();
    mDoubleManager->clear();
    mStringManager->clear();

    std::map< std::string, ExternalStringSelectManager* >::iterator itr =
            m_customStringManagers.begin();
    while( itr != m_customStringManagers.end() )
    {
        itr->second->clear();
        ++itr;
    }

    mSet = set;

    // If we were passed a null set our slate has been cleaned above and we 
    // stop here.
    if( !mSet )
    {
        return;
    }
    // Ask the set for a list of its properties
    mPropertyNames = set->GetDataList();

    // Walk through properties list and store a pointer to each underlying property
    { // Bracket used to scope iterator and end
        PropertySet::PSVectorOfStrings::iterator iterator;
        //PropertySet::PSVectorOfStrings::iterator end = mPropertyNames.end();
        for( iterator = mPropertyNames.begin(); iterator != mPropertyNames.end();  )
        {
            // If the userVisible attribute is false, do not add this property
            // to any of our lists.
            bool show = true;
            if( set->HasPropertyAttribute( *iterator, "userVisible" ) )
            {
                show = set->GetPropertyAttribute( *iterator, "userVisible" )->
                        extract<bool>();
            }
            if( show )
            {
                //mProperties.push_back( PropertyPtr(static_cast<Property*>(set->GetDatum( ( *iterator ) ).get())) );
                mProperties.push_back( PropertyPtr(
                                        boost::dynamic_pointer_cast<Property>
                                        ( set->GetDatum( ( *iterator ) ) ) ) );
                LOG_TRACE( "Adding property named " << (*iterator) );
                ++iterator;
            }
            else
            {
                LOG_TRACE( "Not adding property named " << (*iterator) );
                mPropertyNames.erase( iterator );
                // No need to increment iterator here since everything past the
                // deleted iterator will fall back in the vector
            }
        }
    }

    // Look at each property in turn and create an appropriate browser item
    // for it
    PropertyVector::iterator iterator;
    PropertyVector::iterator end = mProperties.end();
    PropertyPtr property;
    for( iterator = mProperties.begin(); iterator != end; iterator++ )
    {
        property = ( *iterator );
        QtProperty* item = NULL;

        std::string propertyLabel = ( property->GetAttribute( "uiLabel" )
                                      ->extract<std::string>() );

        // Convert label to type needed by Qt functions
        QString label = QString::fromStdString( propertyLabel );

        boost::any value = property->GetValue();

        // Check whether this property should be visible to users
        bool show = true;
        if( property->AttributeExists( "userVisible" ) )
        {
            show = property->GetAttribute( "userVisible" )->extract<bool>();
        }

        // Do type-specific item creation operations
        if( show )
        {
            if( value.empty() )
            {
                // Empty value...is this property intended to be a group?
                if( property->AttributeExists( "isUIGroupOnly" ) )
                {
                    item = mGroupManager->addProperty( label );
                }
            }
            else if( property->IsBool() )
            {
                item = mBooleanManager->addProperty( label );
            }
            else if( property->IsInt() )
            {
                item = mIntManager->addProperty( label );
            }
            else if( property->IsFloat() )
            {
                // Qt's property browser doesn't handle floats directly,
                // so all floats must be cast to double. We cast back and forth
                // in gets and sets to make sure that the underlying data type
                // remains float.
                item = mDoubleManager->addProperty( label );
            }
            else if( property->IsDouble() )
            {
                item = mDoubleManager->addProperty( label );
            }
            // We must be careful to ask about Enum status *BEFORE* string status,
            // since enums *ARE* strings. But not all strings are Enums....
            else if( property->IsEnum() )
            {
                item = mEnumManager->addProperty( label );
            }
            else if( property->IsString() )
            {
                LOG_DEBUG( "Looping through custom string managers" );
                bool found = false;
                std::map< std::string, ExternalStringSelectManager* >::iterator itr =
                        m_customStringManagers.begin();
                while( itr != m_customStringManagers.end() )
                {
                    std::string str = itr->first;
                    if( (property->AttributeExists( str )) &&
                            ( property->GetAttribute( str ))->extract<bool>() )
                    {
                        found = true;
                        item = itr->second->addProperty( label );
                    }
                    ++itr;
                }
                if( !found )
                {
                    item = mStringManager->addProperty( label );
                }

            }

            // These are done for all items
            if( item )
            {
                mItems.push_back( item );
                _refreshItem( _getItemIndex( item ) );
            }
        }
    }

    // Set up specified hierarchy of elements
    _createHierarchy();
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::RefreshAll()
{
    LOG_TRACE( "RefreshAll" );
    int max = static_cast < int > ( mProperties.size() );
    for( int index = 0; index < max; index++ )
    {
        _refreshItem( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::_refreshItem( int index )
{
    // This log message is usually too verbose to be useful
    //LOG_TRACE( "_refreshItem " << index );
    if( index >= static_cast < int > ( mItems.size() ) )
    {
        // We haven't set this item up yet!
        return;
    }

    PropertyPtr property = mProperties[index];
    QtProperty* item = mItems[index];

    boost::any value = property->GetValue();

    bool hasMin = false;
    bool hasMax = false;
    double min = 0.0;
    double max = 0.0;
    _extractMinMaxValues( property, &min, &max, &hasMin, &hasMax );

    LOG_TRACE( "Refreshing " << property->GetAttribute("nameInSet")->extract<std::string>() );

    // Block signals from all Qt...Manager instances so that altering range
    // or other non-value settings of an item does not trigger a value change.
    //_blockManagerSignals( true );
    m_ignoreValueChanges = true;

    // Do type-specific extra operations such as setting min/max
    if( property->IsEnum() )
    {
        // Update the list of valid choices
        QStringList qEnumNames;
        Property::PSVectorOfStrings enumNames =
                property->GetAttribute( "enumValues" )
                ->extract< Property::PSVectorOfStrings >() ;
        Property::PSVectorOfStrings::iterator iterator;
        Property::PSVectorOfStrings::iterator end = enumNames.end();
        for( iterator = enumNames.begin(); iterator != end; iterator++ )
        {
            qEnumNames << QString::fromStdString( ( *iterator ) );
        }
        mEnumManager->setEnumNames( item, qEnumNames );
    }
    else if( property->IsInt() )
    {
        if( hasMin )
        {
            mIntManager->setMinimum( item, static_cast < int > ( min ) );
        }
        if( hasMax )
        {
            mIntManager->setMaximum( item, static_cast < int > ( max ) );
        }
        //int currentMin = mIntManager->minimum( item );
        //int currentMax = mIntManager->maximum( item );
        //int step = static_cast < int > ( ( currentMax - currentMin ) / 100.0 );
        int step = 1;
        if( property->AttributeExists("StepSize") )
        {
            int tStep = property->GetAttribute( "StepSize" )->extract<int>();
            // Require positive, non-zero step size
            if( tStep > 0 )
            {
                step = tStep;
            }
        }
        mIntManager->setSingleStep( item, step );
    }
    else if( property->IsFloat() )
    {
        if( hasMin )
        {
            mDoubleManager->setMinimum( item, min );
        }
        if( hasMax )
        {
            mDoubleManager->setMaximum( item, max );
        }

        int precision = 2;
        if( property->AttributeExists("DisplayPrecision") )
        {
            precision = property->GetAttribute( "DisplayPrecision" )->extract<int>();
        }
        mDoubleManager->setDecimals( item, precision );

        // Pressing arrow keys or spinner arrows should change the values in
        // the least significant figure.
        double step = std::pow( double(10), double(-1 * precision) );
        mDoubleManager->setSingleStep( item, step );
    }
    else if( property->IsDouble() )
    {
        if( hasMin )
        {
            mDoubleManager->setMinimum( item, min );
        }
        if( hasMax )
        {
            mDoubleManager->setMaximum( item, max );
        }

        int precision = 2;
        if( property->AttributeExists("DisplayPrecision") )
        {
            precision = property->GetAttribute( "DisplayPrecision" )->extract<int>();
        }
        mDoubleManager->setDecimals( item, precision );

        // Pressing arrow keys or spinner arrows should change the values in
        // the least significant figure.
        double step = std::pow( double(10), double(-1 * precision) );
        mDoubleManager->setSingleStep( item, step );
    }

    // Unblock value changed signals from managers. Failure to do this will
    // cause the value set below in _setItemValue to never show up in the browser.
    //_blockManagerSignals( false );
    m_ignoreValueChanges = false;

    // Update value
    _setItemValue( item, property );

    // Update enabled state
    item->setEnabled( property->GetEnabled() );
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::_extractMinMaxValues( PropertyPtr property,
                                            double* min, double* max,
                                            bool* hasMin, bool* hasMax )
{
    if( property->AttributeExists( "minimumValue" ) )
    {
        ( *hasMin ) = true;
        Persistence::DatumPtr minVal = property->GetAttribute( "minimumValue" );
        if( minVal->IsInt() )
        {
            ( *min ) = static_cast < double > (  minVal->extract<int>() );
        }
        else if( minVal->IsFloat() )
        {
            ( *min ) = static_cast < double > ( minVal->extract<float>() );
        }
        else if( minVal->IsDouble() )
        {
            ( *min ) = minVal->extract<double>();
        }
    }
    if( property->AttributeExists( "maximumValue" ) )
    {
        ( *hasMax ) = true;
        Persistence::DatumPtr maxVal = property->GetAttribute( "maximumValue" );
        if( maxVal->IsInt() )
        {
            ( *max ) = static_cast < double > ( maxVal->extract<int>() );
        }
        else if( maxVal->IsFloat() )
        {
            ( *max ) = static_cast < double > ( maxVal->extract<float>() );
        }
        else if( maxVal->IsDouble() )
        {
            ( *max ) = maxVal->extract<double>();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::_createHierarchy()
{
    LOG_TRACE( "_createHierarchy" );
    PropertyPtr property;
    int index;
    int max = static_cast < int > ( mProperties.size() );
    for( index = 0; index < max; index++ )
    {
        QtProperty* item = mItems[index];
        bool subItem = false;
        property = mProperties[index];

        // Get the property's name
        std::string propertyName =
                property->GetAttribute( "nameInSet" )->extract<std::string>();
        QString name = QString::fromStdString( propertyName );

        // Put this item into the hierachical arrangement specified by its
        // name. Eg.: if name = "Axes_XLabel", then this item should
        // be a sub-item of the item named "Axes".
        QStringList nameList = name.split( "_" );
        if( nameList.size() > 1 )
        {
            // Rebuild the parent item's name, which may itself be a sub-item
            nameList.removeLast();
            QString parentName = nameList.join( "_" );

            // Do we have an item with the supposed parent's name?
            int parentIndex = -1;
            parentIndex = _getPropertyIndexByName( parentName.toStdString() );
            if( parentIndex > -1 )
            {
                // Got a hit; add this as sub-item
                subItem = true;
                QtProperty* parent = mItems[parentIndex];
                LOG_TRACE( "parentIndex = " << parentIndex <<
                           " Looked for parent " << parentName.toStdString() );
                LOG_TRACE( " and picked up property with label "
                           << parent->propertyName().toStdString() );
                parent->addSubProperty( item );
            }
            else
            {
                LOG_ERROR ( "Error finding parent property named "
                              << parentName.toStdString() << " for property "
                              << propertyName );
            }

        }

        if( !subItem )
        {
            // We only add the item directly to this list if it is NOT a
            // subItem. SubItems will get automatically added to a viewer by
            // simply adding their parent.
            mTreedItems.push_back( item );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::BoolValueChanged( QtProperty* item, bool value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "BoolValueChanged" );
    _setPropertyValue( item, value );
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::IntValueChanged( QtProperty* item, int value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "IntValueChanged" );
    _setPropertyValue( item, value );
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::StringValueChanged( QtProperty* item, const QString & value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "StringValueChanged" );
    std::string castValue = value.toStdString();
    _setPropertyValue( item, castValue );
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::DoubleValueChanged( QtProperty* item, double value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "DoubleValueChanged: " << value );
    int index = _getItemIndex( item );
    if( index > 0 )
    {
        PropertyPtr property = mProperties[index];
        // Must cast to float if the underlying type is really a float
        if( property->IsFloat() )
        {
            _setPropertyValue( item, static_cast < float > ( value ) );
        }
        else
        {
            _setPropertyValue( item, value );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::Refresh()
{
    LOG_TRACE( "Refresh" );
    // Request list of changed properties from set
    PropertySet::PSVectorOfStrings changes = mSet->GetChanges();
    PropertySet::PSVectorOfStrings::iterator iterator;
    PropertySet::PSVectorOfStrings::iterator end = changes.end();

    // Walk through list and update the UI items
    for( iterator = changes.begin(); iterator != end; iterator++ )
    {
        int index = _getPropertyIndexByName( ( *iterator ) );
        if( index > -1 )
        {
            _refreshItem( index );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
int PropertyParser::_getPropertyIndex( PropertyPtr property )
{
    // Get the index of this property in mProperties. The corresponding Item will
    // have the same index in mItems.
    bool found = false;
    int index = -1;
    int max = static_cast < int > ( mProperties.size() );
    max--;
    while( ( !found ) && ( index < max ) )
    {
        index++;
        if( mProperties[ index ] == property )
        {
            found = true;
        }
    }

    if( found )
    {
        return index;
    }
    else
    {
        return -1;
    }
}
////////////////////////////////////////////////////////////////////////////////
int PropertyParser::_getPropertyIndexByName( std::string name )
{
    // Get the index of this name in mPropertyNames. The corresponding Property will
    // have the same index in mProperties.
    bool found = false;
    int index = -1;
    int max = static_cast < int > ( mPropertyNames.size() );
    max--;
    while( ( !found ) && ( index < max ) )
    {
        index++;
        if( mPropertyNames[ index ] == name )
        {
            found = true;
        }
    }
    if( found )
    {
        return index;
    }
    else
    {
        return -1;
    }
}
////////////////////////////////////////////////////////////////////////////////
int PropertyParser::_getItemIndex( QtProperty* item )
{
    // Get the index of this property in mItems. The corresponding Property will
    // have the same index in mProperties.
    bool found = false;
    int index = -1;
    int max = static_cast < int > ( mItems.size() );
    max--;
    while( ( !found ) && ( index < max ) )
    {
        index++;
        if( mItems[ index ] == item )
        {
            found = true;
        }
    }
    if( found )
    {
        return index;
    }
    else
    {
        return -1;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::_setItemValue( QtProperty* item, PropertyPtr property )
{
    //LOG_TRACE( "_setItemValue" );
    //boost::any value = property->GetValue();

    if( property->IsBool() )
    {
        bool castValue = property->extract<bool>();
        LOG_TRACE( "_setItemValue: " << castValue );
        mBooleanManager->setValue( item, castValue );
    }
    else if( property->IsEnum() )
    {
        int castValue = property->GetAttribute( "enumCurrentIndex" )->
                extract<int>();
        LOG_TRACE( "_setItemValue: " << castValue );
        mEnumManager->setValue( item, castValue );
    }
    else if( property->IsInt() )
    {
        int castValue = property->extract<int>();
        LOG_TRACE( "_setItemValue: " << castValue );
        mIntManager->setValue( item, castValue );
    }
    else if( property->IsFloat() )
    {
        double castValue = static_cast < double > ( property->extract<float>() );
        LOG_TRACE( "_setItemValue: " << castValue );
        mDoubleManager->setValue( item, castValue );
    }
    else if( property->IsDouble() )
    {
        double castValue = property->extract<double>();
        LOG_TRACE( "_setItemValue: " << castValue );
        mDoubleManager->setValue( item, castValue );
    }
    else if( property->IsString() )
    {
        std::string castValue = property->extract<std::string>();
        QString qCastValue = QString::fromStdString( castValue );
        bool found = false;
        std::map< std::string, ExternalStringSelectManager* >::iterator itr =
                m_customStringManagers.begin();
        while( itr != m_customStringManagers.end() )
        {
            std::string str = itr->first;
            if( (property->AttributeExists( str )) &&
                    property->GetAttribute( str )->extract<bool>() )
            {
                LOG_TRACE( "_setItemValue: (" << str << ") " << castValue );
                found = true;
                itr->second->setValue( item, qCastValue );
            }
            ++itr;
        }
        if( !found )
        {
            LOG_TRACE( "_setItemValue: " << castValue );
            mStringManager->setValue( item, qCastValue );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyParser::_setPropertyValue( QtProperty* item, boost::any value )
{
    int index = _getItemIndex( item );
    LOG_TRACE( "_setPropertyValue: index " << index << "(" << item->propertyName().toStdString() << ")"  );

    if( index > -1 )
    {
        if( mProperties[ index ]->SetValue( value ) )
        {
            // Setvalue was accepted; check for changes to other properties that
            // may occur due to links between properties in the set
            Refresh();
        }
        else
        {
            // SetValue was denied; re-read this property's value
            _refreshItem( index );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
}

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
#define VES_DEBUG
#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>

#include <gmtl/Math.h>

#include <iostream>

using namespace ves::conductor;
using namespace ves::xplorer::data;
using namespace ves;
////////////////////////////////////////////////////////////////////////////////
PropertyBrowser::PropertyBrowser( QObject* parent ) :
    QObject( parent ),
    m_ignoreValueChanges( false ),
    m_logger( Poco::Logger::get("conductor.PropertyBrowser") ),
    m_logStream( ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) ) )
{
    // Create a standard set of property managers
    mDoubleManager = new QtDoublePropertyManager( this );
    mStringManager = new QtStringPropertyManager( this );
    mBooleanManager = new QtBoolPropertyManager( this );
    mEnumManager = new QtEnumPropertyManager( this );
    mGroupManager = new QtGroupPropertyManager( this );
    mIntManager = new QtIntPropertyManager( this );
    //mFilePathManager = new FilePathManager( this );

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
//    connect( mFilePathManager, SIGNAL(valueChanged(QtProperty*,QString)),
//             this, SLOT(FilePathValueChanged(QtProperty*,QString)));
}
////////////////////////////////////////////////////////////////////////////////
PropertyBrowser::~PropertyBrowser()
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
    //mFilePathManager->clear();

    delete mGroupManager;
    delete mBooleanManager;
    delete mEnumManager;
    delete mIntManager;
    delete mDoubleManager;
    delete mStringManager;
    //delete mFilePathManager;
}
////////////////////////////////////////////////////////////////////////////////
QtDoublePropertyManager* PropertyBrowser::GetDoubleManager()
{
    return mDoubleManager;
}
////////////////////////////////////////////////////////////////////////////////
QtStringPropertyManager* PropertyBrowser::GetStringManager()
{
    return mStringManager;
}
////////////////////////////////////////////////////////////////////////////////
QtBoolPropertyManager* PropertyBrowser::GetBoolManager()
{
    return mBooleanManager;
}
////////////////////////////////////////////////////////////////////////////////
QtEnumPropertyManager* PropertyBrowser::GetEnumManager()
{
    return mEnumManager;
}
////////////////////////////////////////////////////////////////////////////////
QtGroupPropertyManager* PropertyBrowser::GetGroupManager()
{
    return mGroupManager;
}
////////////////////////////////////////////////////////////////////////////////
QtIntPropertyManager* PropertyBrowser::GetIntManager()
{
    return mIntManager;
}
////////////////////////////////////////////////////////////////////////////////
//FilePathManager* PropertyBrowser::GetFilePathManager()
//{
//    return mFilePathManager;
//}
////////////////////////////////////////////////////////////////////////////////
PropertyBrowser::ItemVector* PropertyBrowser::GetItems()
{
    return &mTreedItems;
}
////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::ParsePropertySet( xplorer::data::PropertySetPtr set )
{
    LOG_TRACE( "ParsePropertySet" );
    using xplorer::data::PropertySet;
    using xplorer::data::Property;

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
    //mFilePathManager->clear();

    mSet = set;

    // If we were passed a null set our slate has been cleaned above and we 
    // stop here.
    if( !mSet )
    {
        return;
    }
    // Ask the set for a list of its properties
    mPropertyNames = set->GetPropertyList();

    // Walk through properties list and store a pointer to each underlying property
    { // Bracket used to scope iterator and end
        PropertySet::PSVectorOfStrings::iterator iterator;
        //PropertySet::PSVectorOfStrings::iterator end = mPropertyNames.end();
        for( iterator = mPropertyNames.begin(); iterator != mPropertyNames.end();  )
        {
            // If the userVisible attribute is false, do not add this property
            // to any of our lists.
            bool show = true;
            boost::any visible = set->GetPropertyAttribute( (*iterator),
                                                            "userVisible" );
            if( !visible.empty() )
            {
                show = boost::any_cast<bool>( visible );
            }
            if( show )
            {
                mProperties.push_back( set->GetProperty( ( *iterator ) ) );
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

        std::string propertyLabel =
                boost::any_cast<std::string > (
                property->GetAttribute( "uiLabel" ) );

        // Convert label to type needed by Qt functions
        QString label = QString::fromStdString( propertyLabel );

        boost::any value = property->GetValue();

        // Check whether this property should be visible to users
        bool show = true;
        if( property->AttributeExists( "userVisible" ) )
        {
            show = boost::any_cast<bool>( property->GetAttribute( "userVisible" ) );
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
                // We must be careful to ask about Enum status *BEFORE* Int status,
                // since enums *ARE* Ints. But not all Ints are Enums....
            else if( property->IsEnum() )
            {
                item = mEnumManager->addProperty( label );
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
            else if( property->IsString() )
            {
                LOG_DEBUG( "Checking for FilePath" );
                if( property->AttributeExists( "isFilePath" ) )
                {
                    bool flag = boost::any_cast<bool>( property->GetAttribute("isFilePath") );
                    if( flag )
                    {
                        LOG_DEBUG( "Adding a FilePath item" );
                        //item = mFilePathManager->addProperty( label );
                        item = mStringManager->addProperty( label );
                    }
                    else
                    {
                        item = mStringManager->addProperty( label );
                    }
                }
                else
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
void PropertyBrowser::RefreshAll()
{
    LOG_TRACE( "RefreshAll" );
    int max = static_cast < int > ( mProperties.size() );
    for( int index = 0; index < max; index++ )
    {
        _refreshItem( index );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::_refreshItem( int index )
{
    // This log message is usually too verbose to be useful
    //LOG_TRACE( "_refreshItem " << index );
    if( index >= static_cast < int > ( mItems.size() ) )
    {
        // We haven't set this item up yet!
        return;
    }

    using xplorer::data::Property;
    PropertyPtr property = mProperties[index];
    QtProperty* item = mItems[index];

    boost::any value = property->GetValue();

    bool hasMin = false;
    bool hasMax = false;
    double min = 0.0;
    double max = 0.0;
    _extractMinMaxValues( property, &min, &max, &hasMin, &hasMax );

    LOG_TRACE( "Refreshing " << boost::any_cast<std::string>(property->GetAttribute("nameInSet")) );

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
                boost::any_cast< Property::PSVectorOfStrings > (
                property->GetAttribute( std::string( "enumValues" ) ) );
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
            precision = boost::any_cast<int>( property->GetAttribute( "DisplayPrecision" ) );
        }
        mDoubleManager->setDecimals( item, precision );

        // Pressing arrow keys or spinner arrows should change the values in
        // the least significant figure.
        double step = gmtl::Math::pow( double(10), double(-1 * precision) );
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
            precision = boost::any_cast<int>( property->GetAttribute( "DisplayPrecision" ) );
        }
        mDoubleManager->setDecimals( item, precision );

        // Pressing arrow keys or spinner arrows should change the values in
        // the least significant figure.
        double step = gmtl::Math::pow( double(10), double(-1 * precision) );
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
void PropertyBrowser::_extractMinMaxValues( xplorer::data::PropertyPtr property,
                                            double* min, double* max,
                                            bool* hasMin, bool* hasMax )
{
    if( property->AttributeExists( "minimumValue" ) )
    {
        ( *hasMin ) = true;
        boost::any minVal = property->GetAttribute( "minimumValue" );
        if( property->IsInt( minVal ) )
        {
            ( *min ) = static_cast < double > ( boost::any_cast<int>( minVal ) );
        }
        else if( property->IsFloat( minVal ) )
        {
            ( *min ) = static_cast < double > ( boost::any_cast<float>( minVal ) );
        }
        else if( property->IsDouble( minVal ) )
        {
            ( *min ) = boost::any_cast<double>( minVal );
        }
    }
    if( property->AttributeExists( "maximumValue" ) )
    {
        ( *hasMax ) = true;
        boost::any maxVal = property->GetAttribute( "maximumValue" );
        if( property->IsInt( maxVal ) )
        {
            ( *max ) = static_cast < double > ( boost::any_cast<int>( maxVal ) );
        }
        else if( property->IsFloat( maxVal ) )
        {
            ( *max ) = static_cast < double > ( boost::any_cast<float>( maxVal ) );
        }
        else if( property->IsDouble( maxVal ) )
        {
            ( *max ) = boost::any_cast<double>( maxVal );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::_createHierarchy()
{
    LOG_TRACE( "_createHierarchy" );
    xplorer::data::PropertyPtr property;
    int index;
    int max = static_cast < int > ( mProperties.size() );
    for( index = 0; index < max; index++ )
    {
        QtProperty* item = mItems[index];
        bool subItem = false;
        property = mProperties[index];

        // Get the property's name
        std::string propertyName =
                boost::any_cast<std::string > (
                property->GetAttribute( "nameInSet" ) );
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
void PropertyBrowser::BoolValueChanged( QtProperty* item, bool value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "BoolValueChanged" );
    _setPropertyValue( item, value );
}
////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::IntValueChanged( QtProperty* item, int value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "IntValueChanged" );
    _setPropertyValue( item, value );
}
////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::StringValueChanged( QtProperty* item, const QString & value )
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
//void PropertyBrowser::FilePathValueChanged( QtProperty* item, const QString& value )
//{
//    if( m_ignoreValueChanges )
//    {
//        return;
//    }

//    LOG_TRACE( "FilePathValueChanged" );
//    std::string castValue = value.toStdString();
//    _setPropertyValue( item, castValue );
//}

////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::DoubleValueChanged( QtProperty* item, double value )
{
    if( m_ignoreValueChanges )
    {
        return;
    }

    LOG_TRACE( "DoubleValueChanged: " << value );
    int index = _getItemIndex( item );
    if( index > 0 )
    {
        xplorer::data::PropertyPtr property = mProperties[index];
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
void PropertyBrowser::Refresh()
{
    LOG_TRACE( "Refresh" );
    // Request list of changed properties from set
    xplorer::data::PropertySet::PSVectorOfStrings changes = mSet->GetChanges();
    xplorer::data::PropertySet::PSVectorOfStrings::iterator iterator;
    xplorer::data::PropertySet::PSVectorOfStrings::iterator end = changes.end();

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
int PropertyBrowser::_getPropertyIndex( xplorer::data::PropertyPtr property )
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
int PropertyBrowser::_getPropertyIndexByName( std::string name )
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
int PropertyBrowser::_getItemIndex( QtProperty* item )
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
void PropertyBrowser::_setItemValue( QtProperty* item, xplorer::data::PropertyPtr property )
{
    //LOG_TRACE( "_setItemValue" );
    boost::any value = property->GetValue();

    if( property->IsBool() )
    {
        bool castValue = boost::any_cast<bool>( value );
        LOG_TRACE( "_setItemValue: " << castValue );
        mBooleanManager->setValue( item, castValue );
    }
    else if( property->IsEnum() )
    {
        int castValue = boost::any_cast<int>( value );
        LOG_TRACE( "_setItemValue: " << castValue );
        mEnumManager->setValue( item, castValue );
    }
    else if( property->IsInt() )
    {
        int castValue = boost::any_cast<int>( value );
        LOG_TRACE( "_setItemValue: " << castValue );
        mIntManager->setValue( item, castValue );
    }
    else if( property->IsFloat() )
    {
        double castValue = static_cast < double > ( boost::any_cast<float>( value ) );
        LOG_TRACE( "_setItemValue: " << castValue );
        mDoubleManager->setValue( item, castValue );
    }
    else if( property->IsDouble() )
    {
        double castValue = boost::any_cast<double>( value );
        LOG_TRACE( "_setItemValue: " << castValue );
        mDoubleManager->setValue( item, castValue );
    }
    else if( property->IsString() )
    {
        std::string castValue = boost::any_cast<std::string > ( value );
        QString qCastValue = QString::fromStdString( castValue );
        if( property->AttributeExists( "isFilePath" ) )
        {
            bool flag = boost::any_cast<bool>( property->GetAttribute("isFilePath") );
            if( flag )
            {
                LOG_TRACE( "_setItemValue: (filePath) " << castValue );
                //mFilePathManager->setValue( item, qCastValue );
            }
            else
            {
                LOG_TRACE( "_setItemValue: " << castValue );
                mStringManager->setValue( item, qCastValue );
            }
        }
        else
        {
            LOG_TRACE( "_setItemValue: " << castValue );
            mStringManager->setValue( item, qCastValue );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void PropertyBrowser::_setPropertyValue( QtProperty* item, boost::any value )
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

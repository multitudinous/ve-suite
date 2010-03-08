#include <ves/conductor/qt/propertyBrowser/PropertyBrowser.h>


#include <iostream>

using namespace ves::conductor;

PropertyBrowser::PropertyBrowser(QObject *parent) : QObject(parent)
{
    // Create a standard set of property managers
    mDoubleManager = new QtDoublePropertyManager(this);
    mStringManager = new QtStringPropertyManager(this);
    mBooleanManager = new QtBoolPropertyManager(this);
    mEnumManager = new QtEnumPropertyManager(this);
    mGroupManager = new QtGroupPropertyManager(this);
    mIntManager = new QtIntPropertyManager(this);

    // Connect managers' valueChanged signals to our slots
    connect(mBooleanManager, SIGNAL(valueChanged(QtProperty*,bool)),
            this, SLOT(BoolValueChanged(QtProperty*,bool)));
    connect(mIntManager, SIGNAL(valueChanged(QtProperty*,int)),
            this, SLOT(IntValueChanged(QtProperty*,int)));
    connect(mDoubleManager, SIGNAL(valueChanged(QtProperty*,double)),
            this, SLOT(DoubleValueChanged(QtProperty*,double)));
    connect(mEnumManager, SIGNAL(valueChanged(QtProperty*,int)),
            this, SLOT(IntValueChanged(QtProperty*,int)));
    connect(mStringManager, SIGNAL(valueChanged(QtProperty*,QString)),
            this, SLOT(StringValueChanged(QtProperty*,QString)));
}

QtDoublePropertyManager* PropertyBrowser::GetDoubleManager()
{
    return mDoubleManager;
}

QtStringPropertyManager* PropertyBrowser::GetStringManager()
{
    return mStringManager;
}

QtBoolPropertyManager* PropertyBrowser::GetBoolManager()
{
    return mBooleanManager;
}

QtEnumPropertyManager* PropertyBrowser::GetEnumManager()
{
    return mEnumManager;
}

QtGroupPropertyManager* PropertyBrowser::GetGroupManager()
{
    return mGroupManager;
}

QtIntPropertyManager* PropertyBrowser::GetIntManager()
{
    return mIntManager;
}

PropertyBrowser::ItemVector* PropertyBrowser::GetItems()
{
    return &mTreedItems;
}

void PropertyBrowser::ParsePropertySet( xplorer::data::PropertySet* set )
{
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

    mSet = set;

    // If we were passed a null set our slate has been cleaned above and we 
    // stop here.
    if( mSet == NULL )
    {
        return;
    }
    // Ask the set for a list of its properties
    mPropertyNames = set->GetPropertyList();

    // Walk through properties list and store a pointer to each underlying property
    { // Bracket used to scope iterator and end
        PropertySet::VectorOfStrings::iterator iterator;
        PropertySet::VectorOfStrings::iterator end = mPropertyNames.end();
        for(iterator = mPropertyNames.begin(); iterator != end; iterator++)
        {
            mProperties.push_back( set->GetProperty( (*iterator) ) );
        }
    }

    // Look at each property in turn and create an appropriate browser item
    // for it
    PropertyVector::iterator iterator;
    PropertyVector::iterator end = mProperties.end();
    Property* property;
    for(iterator = mProperties.begin(); iterator != end; iterator++)
    {
        property = (*iterator);
        QtProperty *item = NULL;

        // Get the property's name, label, value, and enabled status
        std::string propertyName =
                boost::any_cast<std::string>(
                        property->GetAttribute("nameInSet"));

        std::string propertyLabel =
                boost::any_cast<std::string>(
                        property->GetAttribute("uiLabel"));

        boost::any value = property->GetValue();
        bool enabled = property->GetEnabled();


        // Check for min and max value attributes
        bool hasMin = false;
        bool hasMax = false;
        double min = 0.0;
        double max = 0.0;
        _extractMinMaxValues( property, &min, &max, &hasMin, &hasMax );


        // Convert name and label to types needed by Qt functions
        QString name = QString::fromStdString(propertyName);
        QString label = QString::fromStdString(propertyLabel);

        // Do type-specific item creation operations
        if( value.empty() )
        {
            // Empty value...is this property intended to be a group?
            if(property->AttributeExists("isUIGroupOnly"))
            {
                item = mGroupManager->addProperty(label);
            }
        }
        else if( property->IsBool() )
        {
            bool castValue = boost::any_cast<bool>(value);
            item = mBooleanManager->addProperty(label);
            mBooleanManager->setValue(item, castValue);
        }
        // We must be careful to ask about Enum status *BEFORE* Int status,
        // since enums *ARE* Ints. But not all Ints are Enums....
        else if( property->IsEnum() )
        {
            int castValue = boost::any_cast<int>(value);
            item = mEnumManager->addProperty(label);

            // Get the list of valid choices
            QStringList qEnumNames;
            Property::VectorOfStrings enumNames =
                    boost::any_cast< Property::VectorOfStrings >(
                            property->GetAttribute("enumValues") );
            Property::VectorOfStrings::iterator iterator;
            Property::VectorOfStrings::iterator end = enumNames.end();
            for( iterator = enumNames.begin(); iterator != end; iterator++ )
            {
                qEnumNames << QString::fromStdString( (*iterator) );
            }
            mEnumManager->setEnumNames(item, qEnumNames);
            mEnumManager->setValue( item, castValue );
        }
        else if( property->IsInt() )
        {
            int castValue = boost::any_cast<int>(value);
            item = mIntManager->addProperty(label);
            mIntManager->setValue(item, castValue);
            if(hasMin)
            {
                mIntManager->setMinimum(item, static_cast<int>(min));
            }
            if(hasMax)
            {
                mIntManager->setMaximum(item, static_cast<int>(max));
            }
        }
        else if( property->IsFloat() )
        {
            // Qt's property browser doesn't handle floats directly,
            // so all floats much be cast to double. We must cast back and forth
            // in gets and sets to make sure that the underlying data type
            // remains float.
            float castValue = boost::any_cast<float>(value);
            item = mDoubleManager->addProperty(label);
            mDoubleManager->setValue(item, static_cast<double>(castValue));
            if(hasMin)
            {
                mDoubleManager->setMinimum(item, min);
            }
            if(hasMax)
            {
                mDoubleManager->setMaximum(item, max);
            }
        }
        else if( property->IsDouble() )
        {
            double castValue = boost::any_cast<double>(value);
            item = mDoubleManager->addProperty(label);
            mDoubleManager->setValue(item, castValue);
            if(hasMin)
            {
                mDoubleManager->setMinimum(item, min);
            }
            if(hasMax)
            {
                mDoubleManager->setMaximum(item, max);
            }
        }
        else if( property->IsString() )
        {
            std::string castValue = boost::any_cast<std::string>(value);
            item = mStringManager->addProperty(label);
            QString qvalue = QString::fromStdString(castValue);
            mStringManager->setValue(item, qvalue);
        }

        // These are done for all items
        item->setEnabled(enabled);
        mItems.push_back(item);
    }

    // Set up specified hierarchy of elements
    _createHierarchy();
}

void PropertyBrowser::RefreshAll()
{
    size_t max = mProperties.size();
    for( int index = 0; index < max; index++)
    {
        _refreshItem( index );
    }
}

void PropertyBrowser::_extractMinMaxValues( xplorer::data::Property* property, double* min,
                                            double* max, bool* hasMin,
                                            bool* hasMax )
{
    if(property->AttributeExists("minimumValue"))
    {
        (*hasMin) = true;
        boost::any minVal = property->GetAttribute("minimumValue");
        if(property->IsInt(minVal))
        {
            (*min) = static_cast<double>(boost::any_cast<int>(minVal));
        }
        else if(property->IsFloat(minVal))
        {
            (*min) = static_cast<double>(boost::any_cast<float>(minVal));
        }
        else if(property->IsDouble(minVal))
        {
            (*min) = boost::any_cast<double>(minVal);
        }
    }
    if(property->AttributeExists("maximumValue"))
    {
        (*hasMax) = true;
        boost::any maxVal = property->GetAttribute("maximumValue");
        if(property->IsInt(maxVal))
        {
            (*max) = static_cast<double>(boost::any_cast<int>(maxVal));
        }
        else if(property->IsFloat(maxVal))
        {
            (*max) = static_cast<double>(boost::any_cast<float>(maxVal));
        }
        else if(property->IsDouble(maxVal))
        {
            (*max) = boost::any_cast<double>(maxVal);
        }
    }
}

void PropertyBrowser::_createHierarchy()
{
    xplorer::data::Property* property;
    int index;
    int max = static_cast< int >( mProperties.size( ) );
    for(index = 0; index < max; index++)
    {
        QtProperty* item = mItems[index];
        bool subItem = false;
        property = mProperties[index];

        // Get the property's name
        std::string propertyName =
                boost::any_cast<std::string>(
                        property->GetAttribute("nameInSet"));
        QString name = QString::fromStdString(propertyName);

        // Put this item into the hierachical arrangement specified by its
        // name. Eg.: if name = "Axes_XLabel", then this item should
        // be a sub-item of the item named "Axes".
        QStringList nameList = name.split("_");
        if( nameList.size() > 1 )
        {
            // Rebuild the parent item's name, which may itself be a sub-item
            nameList.removeLast();
            QString parentName = nameList.join("_");

            // Do we have an item with the supposed parent's name?
            int parentIndex = -1;
            parentIndex = _getPropertyIndexByName( parentName.toStdString() );
            if( parentIndex > -1 )
            {
                // Got a hit; add this as sub-item
                subItem = true;
                QtProperty* parent = mItems[parentIndex];
                parent->addSubProperty( item );
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

void PropertyBrowser::BoolValueChanged(QtProperty* item, bool value)
{
    _setPropertyValue( item, value );
}

void PropertyBrowser::IntValueChanged(QtProperty* item, int value)
{
    _setPropertyValue( item, value );
}

void PropertyBrowser::StringValueChanged(QtProperty* item, const QString & value)
{
    std::string castValue = value.toStdString();
    _setPropertyValue( item, castValue );
}

void PropertyBrowser::DoubleValueChanged(QtProperty* item, double value)
{
    int index = _getItemIndex(item);
    if( index > 0 )
    {
        xplorer::data::Property* property = mProperties[index];
        // Must cast to float if the underlying type is really a float
        if(property->IsFloat())
        {
            _setPropertyValue( item, static_cast< float >( value ) );
        }
        else
        {
            _setPropertyValue( item, value );
        }
    }
}

void PropertyBrowser::Refresh()
{
    // Request list of changed properties from set
    xplorer::data::PropertySet::VectorOfStrings changes = mSet->GetChanges();
    xplorer::data::PropertySet::VectorOfStrings::iterator iterator;
    xplorer::data::PropertySet::VectorOfStrings::iterator end = changes.end();

    // Walk through list and update the UI items
    for(iterator = changes.begin(); iterator != end; iterator++)
    {
        int index = _getPropertyIndexByName( (*iterator) );
        if( index > -1 )
        {
            _refreshItem( index );
        }
    }
}

int PropertyBrowser::_getPropertyIndex(xplorer::data::Property* property)
{
    // Get the index of this property in mProperties. The corresponding Item will
    // have the same index in mItems.
    bool found = false;
    int index = -1;
    int max = static_cast<int>( mProperties.size( ) );
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

int PropertyBrowser::_getPropertyIndexByName( std::string name )
{
    // Get the index of this name in mPropertyNames. The corresponding Property will
    // have the same index in mProperties.
    bool found = false;
    int index = -1;
    int max = static_cast<int>( mPropertyNames.size( ) );
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

int PropertyBrowser::_getItemIndex(QtProperty* item)
{
    // Get the index of this property in mItems. The corresponding Property will
    // have the same index in mProperties.
    bool found = false;
    int index = -1;
    int max = static_cast<int>( mItems.size( ) );
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

void PropertyBrowser::_setItemValue( QtProperty* item, xplorer::data::Property* property )
{
    boost::any value = property->GetValue();

    if( property->IsBool() )
    {
        bool castValue = boost::any_cast<bool>( value );
        mBooleanManager->setValue( item, castValue );
    }
    else if( property->IsEnum() )
    {
        int castValue = boost::any_cast<int>( value );
        mEnumManager->setValue( item, castValue );
    }
    else if( property->IsInt() )
    {
        int castValue = boost::any_cast<int>( value );
        mIntManager->setValue( item, castValue );
    }
    else if( property->IsFloat() )
    {
        double castValue = static_cast<double>( boost::any_cast<float>( value ) );
        mDoubleManager->setValue( item, castValue );
    }
    else if( property->IsDouble() )
    {
        double castValue = boost::any_cast<double>( value );
        mDoubleManager->setValue( item, castValue );
    }
    else if( property->IsString() )
    {
        std::string castValue = boost::any_cast<std::string>( value );
        QString qCastValue = QString::fromStdString( castValue );
        mStringManager->setValue( item, qCastValue );
    }
}

void PropertyBrowser::_setPropertyValue( QtProperty* item, boost::any value )
{
    int index = _getItemIndex( item );

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

void PropertyBrowser::_refreshItem( int index )
{
    xplorer::data::Property* property = mProperties[index];
    QtProperty* item = mItems[index];

    // Update enabled state
    item->setEnabled( property->GetEnabled() );

    // Update value
    _setItemValue( item, property );

    // TODO: Update min, max...

    // TODO: Update anything else we need to know about...
}

/*
void PropertyBrowser::_printSet()
{
    std::cout << "_printSet()" << std::endl;
    PropertySet::VectorOfStrings list = mSet->GetPropertyList();

    PropertySet::VectorOfStrings::iterator iterator;
    PropertySet::VectorOfStrings::iterator end = list.end();
    for( iterator = list.begin(); iterator != end; iterator++ )
    {
        Property* p = mSet->GetProperty( (*iterator) );
        std::string name = boost::any_cast<std::string>(p->GetAttribute("nameInSet"));
        std::cout << "\t" << name << ": ";
        boost::any val = p->GetValue();
        _printValue( val );
        std::cout << std::endl;
    }
}

void PropertyBrowser::_printValue( boost::any value )
{
    if( value.type( ) == typeid ( int ) )
    {
        int v = boost::any_cast<int>( value );
        std::cout << v;
    }
    else if( value.type( ) == typeid ( float ) )
    {
        float v = boost::any_cast<float>( value );
        std::cout << v;
    }
    else if( value.type( ) == typeid ( double ) )
    {
        double v = boost::any_cast<double>( value );
        std::cout << v;
    }
    else if( value.type( ) == typeid ( bool ) )
    {
        bool v = boost::any_cast<bool>( value );
        std::cout << v;
    }
    else if( boost::any_cast<std::string > ( &value ) )
    {
        std::string v = boost::any_cast<std::string > ( value );
        std::cout << v;
    }
    else if( boost::any_cast< std::vector<std::string> >( &value ) )
    {
        std::cout << "Vector of strings. The strings are:" << std::endl;
        std::vector<std::string> *vec = boost::any_cast< std::vector<std::string> >( &value );
        for ( size_t count = 0; count < vec->size( ); count++ )
        {
            std::string v = ( *vec )[count];
            std::cout << "     " << count << ", " << v << std::endl;
        }
    }
    else
    {
        std::cout << "Unsupported type";
    }
}
*/


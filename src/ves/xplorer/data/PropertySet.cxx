#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/Property.h>
#include <ves/xplorer/data/BindableAnyWrapper.h>

#include <boost/bind.hpp>
#include <sstream>
#include <iostream>
#include <cstdio>

#include <Poco/Data/RecordSet.h>
#include <Poco/Data/Common.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/DataException.h>

using namespace ves::xplorer::data;

PropertySet::PropertySet( )
{
    mPropertyMap.clear( );
    mAccumulatedChanges.clear( );
    mID = 0;
}

PropertySet::PropertySet( const PropertySet& orig )
{
}

PropertySet::~PropertySet( )
{
    PropertyMap::iterator iterator = mPropertyMap.begin( );
    PropertyMap::iterator end = mPropertyMap.end( );
    while ( iterator != end )
    {
        delete (*iterator ).second;
        iterator++;
    }

    mPropertyMap.clear( );
}

void PropertySet::AddProperty( std::string propertyName,
                               boost::any value,
                               std::string uiLabel )
{
    // Check that a property with this name doesn't already exist
    if( PropertyExists( propertyName ) )
    {
        // Do nothing and get out
        return;
    }

    // Create a new property and add it to the map
    Property *property = new Property( value );
    property->SetAttribute( "nameInSet", propertyName );
    property->SetAttribute( "uiLabel", uiLabel );
    mPropertyMap[ propertyName ] = property;

    // Add the name to our list of properties that maintains insertion order
    mPropertyList.push_back( propertyName );

    // Connect change signals to the change accumulator
    _connectChanges( property );
}

bool PropertySet::PropertyExists( std::string propertyName ) const
{
    bool result = false;

    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        result = true;
    }
    return result;
}

PropertySet::VectorOfStrings PropertySet::GetPropertyList( )
{
    return mPropertyList;
}

Property* PropertySet::GetProperty( std::string propertyName )
{
    PropertyMap::iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        return (*iterator ).second;
    }
    else
    {
        return NULL;
    }
}

boost::any PropertySet::GetPropertyValue( std::string propertyName ) const
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        return (*iterator ).second->GetValue( );
    }
    else
    {
        return boost::any( );
    }
}

const PropertySet::VectorOfStrings PropertySet::GetPropertyAttributeList( std::string
                                                                          propertyName
                                                                          ) const
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        return (*iterator ).second->GetAttributeList( );
    }
    else
    {
        // return an empty vector of strings
        return VectorOfStrings( );
    }
}

boost::any PropertySet::GetPropertyAttribute( std::string propertyName,
                                              std::string attributeName )
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        return (*iterator ).second->GetAttribute( attributeName );
    }
    else
    {
        return boost::any( );
    }
}

bool PropertySet::GetPropertyEnabled( std::string propertyName ) const
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        return (*iterator ).second->GetEnabled( );
    }
    else
    {
        return false;
    }
}

bool PropertySet::SetPropertyValue( std::string propertyName,
                                    boost::any value )
{
    bool result = false;
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        result = ( *iterator ).second->SetValue( value );
    }

    return result;
}

void PropertySet::SetPropertyAttribute( std::string propertyName,
                                        std::string attributeName,
                                        boost::any value )
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        ( *iterator ).second->SetAttribute( attributeName, value );
    }
}

void PropertySet::SetPropertyEnabled( std::string propertyName,
                                      bool enabled )
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        if( enabled )
        {
            ( *iterator ).second->SetEnabled( );
        }
        else
        {
            ( *iterator ).second->SetDisabled( );
        }
    }
}

PropertySet::VectorOfStrings PropertySet::GetChanges( )
{
    VectorOfStrings changes = mAccumulatedChanges;
    ClearAccumulatedChanges( );
    return changes;
}

void PropertySet::ClearAccumulatedChanges( )
{
    mAccumulatedChanges.clear( );
}

void PropertySet::SetTableName( std::string TableName )
{
    mTableName = TableName;
}

std::string PropertySet::GetTableName( )
{
    return mTableName;
}

void PropertySet::SetRecordID( long unsigned int id )
{
    mID = id;
}

long unsigned int PropertySet::GetRecordID( )
{
    return mID;
}

bool PropertySet::DeleteFromDatabase( std::string DatabaseName )
{
    return DeleteFromDatabase( DatabaseName, mTableName );
}

bool PropertySet::DeleteFromDatabase( std::string DatabaseName, std::string TableName )
{
    // Open db connection and session
    // TODO: wrap in exception handler in case db is inaccessible
    // Also may want to use a connection pool to speed up access
    Poco::Data::SQLite::Connector::registerConnector( );
    Poco::Data::Session session( "SQLite", DatabaseName );

    bool returnValue = DeleteFromDatabase( &session, TableName );

    // Close db connection
    Poco::Data::SQLite::Connector::unregisterConnector( );

    return returnValue;
}

bool PropertySet::DeleteFromDatabase( Poco::Data::Session *session )
{
    return DeleteFromDatabase( session, mTableName );
}

bool PropertySet::DeleteFromDatabase( Poco::Data::Session *session, std::string TableName )
{
    ( *session ) << "DELETE FROM " << TableName << " WHERE id=:mID", Poco::Data::use( mID ), Poco::Data::now;
}

bool PropertySet::LoadFromDatabase( std::string DatabaseName )
{
    return LoadFromDatabase( DatabaseName, mTableName, mID );
}

bool PropertySet::LoadFromDatabase( std::string DatabaseName,
                                    std::string TableName )
{
    return LoadFromDatabase( DatabaseName, TableName, mID );
}

bool PropertySet::LoadFromDatabase( std::string DatabaseName,
                                    std::string TableName,
                                    long unsigned int ID )
{
    // Open db connection and session
    // TODO: wrap in exception handler in case db is inaccessible
    // Also may want to use a connection pool to speed up access
    Poco::Data::SQLite::Connector::registerConnector( );
    Poco::Data::Session session( "SQLite", DatabaseName );

    bool returnValue = LoadFromDatabase( &session, TableName, ID );

    // Close db connection
    Poco::Data::SQLite::Connector::unregisterConnector( );

    return returnValue;
}

bool PropertySet::LoadFromDatabase( Poco::Data::Session *session )
{
    return LoadFromDatabase( session, mTableName, mID );
}

bool PropertySet::LoadFromDatabase( Poco::Data::Session *session,
                                    std::string TableName )
{
    return LoadFromDatabase( session, TableName, mID );
}

bool PropertySet::LoadFromDatabase( Poco::Data::Session *session,
                                    std::string TableName,
                                    long unsigned int ID )
{
    if( !session )
    {
        std::cout << "Invalid database session" << std::endl << std::flush;
        return false;
    }
    // Get the entire record we need with one query
    mID = ID;
    Poco::Data::Statement statement( ( *session ) );
    statement << "SELECT * FROM " << TableName << " WHERE id=:0", Poco::Data::use( mID );
    statement.execute( );

    Poco::Data::RecordSet recordset( statement );

    // Parse out the record column by column and put the data into the
    // appropriate places
    for ( size_t index = 0; index < recordset.columnCount( ); index++ )
    {
        // Get the name of the column
        std::string name = recordset.columnName( index );

        // If column name corresponds to a property name in this set, extract
        // the value from the column and set the property value
        if( PropertyExists( name ) )
        {
            boost::any bValue;
            Poco::DynamicAny value = recordset[index];
            Poco::Data::MetaColumn::ColumnDataType dataType = recordset.columnType( index );
            std::string columnName = recordset.columnName( index );

            switch( dataType )
            {
            case Poco::Data::MetaColumn::FDT_BOOL: // Never gets used by SQLite
                bValue = value.convert<bool>( );
                break;
            case Poco::Data::MetaColumn::FDT_INT8:
                bValue = value.convert<int>( );
                break;
            case Poco::Data::MetaColumn::FDT_INT16:
                bValue = value.convert<int>( );
                break;
            case Poco::Data::MetaColumn::FDT_INT32: // Bools appear to also be int32
                if( mPropertyMap[columnName]->IsBool( ) )
                {
                    //std::cout << "Converting as Bool\n";
                    bValue = value.convert<bool>( );
                }
                else
                {
                    //std::cout << "Converting as Int\n";
                    bValue = value.convert<int>( );
                }
                break;
            case Poco::Data::MetaColumn::FDT_INT64:
                bValue = value.convert<int>( );
                break;
            case Poco::Data::MetaColumn::FDT_FLOAT:
                bValue = value.convert<float>( );
                break;
            case Poco::Data::MetaColumn::FDT_DOUBLE:
                //std::cout << "Double\n";
                bValue = value.convert<double>( );
                break;
            case Poco::Data::MetaColumn::FDT_STRING:
                bValue = value.convert<std::string > ( );
                break;
            default:
                std::cout << "Didn't find conversion type" << std::endl << std::flush;
            }

            if( !bValue.empty( ) )
            {
                mPropertyMap[name]->SetValue( bValue );
            }
        }
    }

    return true;
}

bool PropertySet::WriteToDatabase( std::string DatabaseName )
{
    bool returnValue = WriteToDatabase( DatabaseName, mTableName );
    return returnValue;
}

bool PropertySet::WriteToDatabase( std::string DatabaseName,
                                   std::string TableName )
{
    // Open db connection and session
    // TODO: wrap in exception handler in case db is inaccessible
    // Also may want to use a connection pool to speed up access
    Poco::Data::SQLite::Connector::registerConnector( );
    Poco::Data::Session session( "SQLite", DatabaseName );

    bool returnValue = WriteToDatabase( &session, TableName );

    // Close db connection
    Poco::Data::SQLite::Connector::unregisterConnector( );

    return returnValue;
}

bool PropertySet::WriteToDatabase( Poco::Data::Session *session )
{
    bool returnValue = WriteToDatabase( session, mTableName );
    return returnValue;
}

bool PropertySet::WriteToDatabase( Poco::Data::Session *session,
                                   std::string TableName )
{
    // We can write from the base class because table column names correspond
    // to property names.

    // Need to have explicitly named variables if we want to be able to bind
    // with Poco::Data
    std::string m_TableName = TableName;

    bool returnVal = false;

    // Check that we have non-empty table name
    if( m_TableName.empty( ) )
    {
        return false;
    }

    // See if a table for this type already exists; if not create the table
    //std::cout << "Checking for table..." << std::flush;
    std::string tableExists;
    try
    {
        ( *session ) << "SELECT name FROM sqlite_master WHERE name=:name",
                Poco::Data::into( tableExists ),
                Poco::Data::use( m_TableName ),
                Poco::Data::now;
    }
    catch ( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
    catch ( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }
    if( tableExists.empty( ) ) // Table doesn't exist
    {
        //std::cout << "creating table..." << std::flush;
        std::string columnHeaderString = _buildColumnHeaderString( );

        Poco::Data::Statement sm( ( *session ) );
        sm << "CREATE TABLE " << m_TableName << " (" << columnHeaderString << ")";
        try
        {
            sm.execute( );
        }
        catch ( Poco::Data::DataException &e )
        {
            std::cout << e.displayText( ) << std::endl;
        }
        catch ( ... )
        {
            std::cout << "Error writing to database." << std::endl;
        }
    }
    //std::cout << "done." << std::endl << std::flush;

    // Determine whether a record already exists for this PropertySet.
    // This query will return a non-zero, positive id iff the record exists
    std::cout << "Checking for existing record with this ID..." << std::flush;
    int idTest = 0;
    try
    {
        ( *session ) << "SELECT id FROM " << m_TableName << " WHERE id=:id",
                Poco::Data::into( idTest ),
                Poco::Data::use( mID ),
                Poco::Data::now;
    }
    catch ( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
    catch ( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    // Since the data binding part will be the same for INSERT and UPDATE
    // operations on this PropertySet, we only need to build the string part
    // of the query separately.

    // Will hold the string part of any query we do.
    std::string query;
    // Will hold the list of fields in the order added to query.
    std::vector< std::string > fieldNames;

    if( idTest == 0 ) //  Record does not exist; perform an INSERT
    {
        //std::cout << "record does not exist." << std::endl << std::flush;

        // Build a query that looks like this:
        // "INSERT INTO tablename (field1name_1,fieldname_2,...) VALUES (:1,:2,...)"
        query = "INSERT INTO ";
        query.append( m_TableName );
        query.append( " (" );

        Property *property;
        PropertyMap::const_iterator iterator = mPropertyMap.begin( );
        while ( iterator != mPropertyMap.end( ) )
        {
            property = iterator->second;
            // Check for a known type
            if( ( property->IsBool( ) ) || ( property->IsDouble( ) ) ||
                    ( property->IsFloat( ) ) || ( property->IsInt( ) ) ||
                    ( property->IsString( ) ) )
            {
                query.append( iterator->first );
                query.append( "," );
                fieldNames.push_back( iterator->first );
                iterator++;
            }
            else // Didn't put in field name because we had an unknown type
            {
                iterator++;
            }
        }

        // There should be an extra comma at the end of the query that must be
        // removed. Test for it and remove if it is there.
        if( query.substr( query.size( ) - 1, query.size( ) ) == "," )
        {
            query.erase( --query.end( ) );
        }

        query.append( ") VALUES (" );

        // Put in the binding labels (:0,:1,...) for Poco::Data
        size_t max = fieldNames.size( );
        for ( size_t count = 0; count < max; count++ )
        {
            query.append( ":" );
            query.append( _toString( count ) );
            query.append( "," );
        }
        // There should be an extra comma at the end of the query that must be
        // removed. Test for it and remove if it is there.
        if( query.substr( query.size( ) - 1, query.size( ) ) == "," )
        {
            query.erase( --query.end( ) );
        }

        query.append( ")" );
    }
    else // Record exists; perform an UPDATE
    {
        //std::cout << "record exists... doing update on id=" << _toString( mID ) << std::endl << std::flush;
        // Build a query that looks like:
        // "UPDATE tablename SET field_0=:0, field_1=:1 WHERE id=mID"
        query = "UPDATE ";
        query.append( m_TableName );
        query.append( " SET " );

        Property *property;
        PropertyMap::const_iterator iterator = mPropertyMap.begin( );
        while ( iterator != mPropertyMap.end( ) )
        {
            property = iterator->second;
            // Check for a known type
            if( ( property->IsBool( ) ) || ( property->IsDouble( ) ) ||
                    ( property->IsFloat( ) ) || ( property->IsInt( ) ) ||
                    ( property->IsString( ) ) )
            {
                query.append( iterator->first );
                query.append( "=:" );
                query.append( _toString( fieldNames.size( ) ) );
                query.append( "," );

                fieldNames.push_back( iterator->first );
                iterator++;
            }
            else // Didn't put in field name because we had an unknown type
            {
                iterator++;
            }
        }
        // There should be an extra comma at the end of the query that must be
        // removed. Test for it and remove if it is there.
        if( query.substr( query.size( ) - 1, query.size( ) ) == "," )
        {
            query.erase( --query.end( ) );
        }

        query.append( " WHERE id=" );
        query.append( _toString( mID ) );
    }

    // Turn the query into a statement that can accept bound values
    Poco::Data::Statement statement( ( *session ) );
    statement << query;

    // The data binding looks the same for either query type (INSERT or UPDATE)

    // Bindable wrapper for property data
    BindableAnyWrapper *bindable;
    // Stores wrappers for later deletion
    std::vector< BindableAnyWrapper* > bindableVector;

    Property *property;
    std::vector<std::string>::iterator iterator = fieldNames.begin( );
    while ( iterator != fieldNames.end( ) )
    {
        property = mPropertyMap[ ( *iterator ) ];

        bindable = new BindableAnyWrapper;
        bindableVector.push_back( bindable );
        bindable->BindValue( &statement, property->GetValue( ) );

        iterator++;
    }

    //std::cout << "Executing statement..." << std::endl << std::flush;
    //std::cout << statement.toString() << std::endl << std::flush;
    try
    {
        statement.execute( );
    }
    catch ( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
    catch ( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    // If we've made it here, then we successfully wrote to database
    returnVal = true;

    // If we just did an INSERT (mID == 0), we need to get the id of the
    // record we just INSERTed and store it as mID.
    if( mID == 0 )
    {
        try
        {
            ( *session ) << "SELECT MAX(id) FROM " << TableName,
                    Poco::Data::into( mID ),
                    Poco::Data::now;
        }
        catch ( Poco::Data::DataException &e )
        {
            std::cout << e.displayText( ) << std::endl;
        }
        catch ( ... )
        {
            std::cout << "Error writing to database." << std::endl;
        }
    }

    //std::cout << "Wrote to record with id = " << _toString( mID ) << std::endl;

    // Delete the BindableAnyWrapperS that were created in the binding loop
    std::vector< BindableAnyWrapper* >::iterator biterator = bindableVector.begin( );
    while ( biterator != bindableVector.end( ) )
    {
        delete (*biterator );
        biterator++;
    }

    return returnVal;
}

std::string PropertySet::_buildColumnHeaderString( )
{
    std::string result;

    // Forcing the primary key to autoincrement ensures that we can always
    // find the most recently inserted entry simply by issuing
    // SELECT MAX(id) from table_name
    result.append( "id INTEGER PRIMARY KEY AUTOINCREMENT," );

    Property *property;
    PropertyMap::const_iterator iterator = mPropertyMap.begin( );
    while ( iterator != mPropertyMap.end( ) )
    {
        property = iterator->second;
        std::string dataType;

        // Figure out what to put in for the colum data type
        if( property->IsBool( ) )
        {
            dataType = "INTEGER";
        }
        else if( property->IsInt( ) )
        {
            dataType = "INTEGER";
        }
        else if( property->IsFloat( ) )
        {
            dataType = "FLOAT";
        }
        else if( property->IsDouble( ) )
        {
            dataType = "DOUBLE";
        }
        else if( property->IsString( ) )
        {
            dataType = "TEXT";
        }
        else
        {
            dataType = "UNKNOWN";
        }

        // Put the property's name string in as the column name
        // FIXME: If the property is of an unknown type, we skip it for now
        // and there will be no db column for it.
        if( dataType != "UNKNOWN" )
        {
            result.append( iterator->first );
            result.append( " " );
            result.append( dataType );

            // Don't want to add a comma at the end if this is the last entry
            iterator++;
            if( iterator != mPropertyMap.end( ) )
            {
                result.append( "," );
            }
        }
        else
        {
            iterator++;
        }
    }

    return result;
}

void PropertySet::_connectChanges( Property* property )
{
    property->SignalAttributeChanged.connect( boost::bind( &PropertySet::
                                                           _changeAccumulator,
                                                           this, _1 ) );

    property->SignalDisabled.connect( boost::bind( &PropertySet::
                                                   _changeAccumulator,
                                                   this, _1 ) );

    property->SignalEnabled.connect( boost::bind( &PropertySet::
                                                  _changeAccumulator,
                                                  this, _1 ) );

    property->SignalValueChanged.connect( boost::bind( &PropertySet::
                                                       _changeAccumulator,
                                                       this, _1 ) );
}

void PropertySet::_changeAccumulator( Property* property )
{
    // Ask the property for its name
    std::string name;
    name = boost::any_cast< std::string > ( property->GetAttribute( "nameInSet" ) );

    // See if we already have changes recorded for this property
    bool found = false;
    VectorOfStrings::const_iterator iterator = mAccumulatedChanges.begin( );
    VectorOfStrings::const_iterator end = mAccumulatedChanges.end( );
    while ( ( !found ) && ( iterator != end ) )
    {
        if( ( *iterator ) == name )
        {
            found = true;
        }
        iterator++;
    }

    // Add the property's name to our list if it isn't already there, but also
    // restrict the size of the vector to 1000 elements.
    if( ( !found ) && ( mAccumulatedChanges.size( ) < 1000 ) )
    {
        mAccumulatedChanges.push_back( name );
    }
}

std::string PropertySet::_toString( int value )
{
    std::stringstream ss;
    std::string result;
    ss << value;
    ss >> result;
    return result;
}

std::string PropertySet::_toString( long unsigned int value )
{
    std::stringstream ss;
    std::string result;
    ss << value;
    ss >> result;
    return result;
}

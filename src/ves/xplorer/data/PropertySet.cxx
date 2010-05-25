/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/data/PropertySet.h>
#include <ves/xplorer/data/Property.h>
#include <ves/xplorer/data/BindableAnyWrapper.h>
#include <ves/xplorer/data/DatabaseManager.h>

#include <boost/bind.hpp>
#include <boost/lexical_cast.hpp>

#include <iostream>

#include <Poco/Data/RecordSet.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/SessionPool.h>
#include <Poco/Data/SQLite/Connector.h>
#include <Poco/Data/DataException.h>

#include "BindableAnyWrapper.h"

namespace ves
{
namespace xplorer
{
namespace data
{

PropertySet::PropertySet( )
{
    mPropertyMap.clear( );
    mAccumulatedChanges.clear( );
    mID = 0;
}
////////////////////////////////////////////////////////////////////////////////

PropertySet::PropertySet( const PropertySet& orig )
{
}
////////////////////////////////////////////////////////////////////////////////

PropertySet::~PropertySet( )
{
    PropertyMap::iterator iterator = mPropertyMap.begin( );
    PropertyMap::iterator end = mPropertyMap.end( );
    while( iterator != end )
    {
        delete (*iterator ).second;
        iterator++;
    }

    mPropertyMap.clear( );
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::AddProperty( const std::string& propertyName,
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
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::PropertyExists( const std::string& propertyName ) const
{
    bool result = false;

    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        result = true;
    }
    return result;
}
////////////////////////////////////////////////////////////////////////////////

const PropertySet::PSVectorOfStrings& PropertySet::GetPropertyList( )
{
    return mPropertyList;
}
////////////////////////////////////////////////////////////////////////////////

Property* PropertySet::GetProperty( const std::string& propertyName )
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
////////////////////////////////////////////////////////////////////////////////

boost::any PropertySet::GetPropertyValue( const std::string& propertyName ) const
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
////////////////////////////////////////////////////////////////////////////////

const PropertySet::PSVectorOfStrings& PropertySet::GetPropertyAttributeList( const std::string&
                                                                             propertyName
                                                                             )
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        return (*iterator ).second->GetAttributeList( );
    }
    else
    {
        // return an empty vector of strings
        return emptyPSVectorOfStrings;
    }
}
////////////////////////////////////////////////////////////////////////////////

boost::any PropertySet::GetPropertyAttribute( const std::string& propertyName,
                                              const std::string& attributeName )
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
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::GetPropertyEnabled( const std::string& propertyName ) const
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
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::SetPropertyValue( const std::string& propertyName,
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
////////////////////////////////////////////////////////////////////////////////

void PropertySet::SetPropertyAttribute( const std::string& propertyName,
                                        const std::string& attributeName,
                                        boost::any value )
{
    PropertyMap::const_iterator iterator = mPropertyMap.find( propertyName );
    if( iterator != mPropertyMap.end( ) )
    {
        ( *iterator ).second->SetAttribute( attributeName, value );
    }
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::SetPropertyEnabled( const std::string& propertyName,
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
////////////////////////////////////////////////////////////////////////////////

const PropertySet::PSVectorOfStrings& PropertySet::GetChanges( )
{
    mAccumulatedChangesReturnable = mAccumulatedChanges;
    ClearAccumulatedChanges( );
    return mAccumulatedChangesReturnable;
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::ClearAccumulatedChanges( )
{
    mAccumulatedChanges.clear( );
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::SetTableName( const std::string& TableName )
{
    mTableName = TableName;
}
////////////////////////////////////////////////////////////////////////////////

const std::string& PropertySet::GetTableName( ) const
{
    return mTableName;
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::SetRecordID( long unsigned int id )
{
    mID = id;
}
////////////////////////////////////////////////////////////////////////////////

unsigned int PropertySet::GetRecordID( ) const
{
    return mID;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::DeleteFromDatabase( )
{
    Poco::Data::Session session( ves::xplorer::data::DatabaseManager::
                                 instance( )->GetPool( )->get( ) );
    bool retval = DeleteFromDatabase( &session );
    return retval;
}

bool PropertySet::DeleteFromDatabase( const std::string& DatabaseName )
{
    return DeleteFromDatabase( DatabaseName, mTableName );
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::DeleteFromDatabase( const std::string& DatabaseName, const std::string& TableName )
{
    bool returnValue = false;

    // Open db connection and session
    try
    {
        Poco::Data::SQLite::Connector::registerConnector( );
        Poco::Data::Session session( "SQLite", DatabaseName );

        returnValue = DeleteFromDatabase( &session, TableName );

        // Close db connection
        Poco::Data::SQLite::Connector::unregisterConnector( );
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::DeleteFromDatabase( Poco::Data::Session* session )
{
    return DeleteFromDatabase( session, mTableName );
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::DeleteFromDatabase( Poco::Data::Session* session, const std::string& TableName )
{
    bool returnValue = false;

    // Check for existence of TableName in db and fail if false.
    if( !_tableExists(session, TableName) )
    {
        return false;
    }

    try
    {
        ( *session ) << "DELETE FROM " << TableName << " WHERE id=:mID", Poco::Data::use( mID ), Poco::Data::now;
        returnValue = true;
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( )
{
    Poco::Data::Session session( ves::xplorer::data::DatabaseManager::
                                 instance( )->GetPool( )->get( ) );
    bool retval = LoadFromDatabase( &session );
    return retval;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( const std::string& DatabaseName )
{
    return LoadFromDatabase( DatabaseName, mTableName, mID );
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( const std::string& DatabaseName,
                                    const std::string& TableName )
{
    return LoadFromDatabase( DatabaseName, TableName, mID );
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( const std::string& DatabaseName,
                                    const std::string& TableName,
                                    unsigned int ID )
{
    bool returnValue = false;

    // Open db connection and session
    try
    {
        Poco::Data::SQLite::Connector::registerConnector( );
        Poco::Data::Session session( "SQLite", DatabaseName );

        returnValue = LoadFromDatabase( &session, TableName, ID );

        // Close db connection
        Poco::Data::SQLite::Connector::unregisterConnector( );
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( Poco::Data::Session* session )
{
    return LoadFromDatabase( session, mTableName, mID );
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( Poco::Data::Session* session,
                                    const std::string& TableName )
{
    return LoadFromDatabase( session, TableName, mID );
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadFromDatabase( Poco::Data::Session* session,
                                    const std::string& TableName,
                                    Poco::UInt32 ID )
{
    if( !session )
    {
        std::cout << "Invalid database session" << std::endl << std::flush;
        return false;
    }

    // Check for existence of TableName in db and fail if false.
    if( !_tableExists(session, TableName) )
    {
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
    for( size_t index = 0; index < recordset.columnCount( ); index++ )
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
                    bValue = value.convert<bool>( );
                }
                else
                {
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

    // Look through PropertySet for vectorized data types. These will not have been
    // stored in the main table and must be looked for elsewhere in the database.
    Property* property;
    PropertyMap::const_iterator iterator = mPropertyMap.begin( );
    while( iterator != mPropertyMap.end( ) )
    {
        property = iterator->second;
        if( property->IsVectorized( ) )
        {
            std::string fieldName = boost::any_cast<std::string > ( property->GetAttribute( "nameInSet" ) );
            Poco::Data::Statement statement( ( *session ) );
            statement << "SELECT " << fieldName << " FROM " << mTableName
                    << "_" << iterator->first << " WHERE PropertySetParentID=:0"
                    , Poco::Data::use( mID );
            statement.execute( );
            Poco::Data::RecordSet recordset( statement );

            if( property->IsIntVector( ) )
            {
                std::vector< int > vec;
                Poco::DynamicAny value;
                for( int rowIndex = 0; rowIndex < recordset.rowCount( ); rowIndex++ )
                {
                    value = recordset.value( 0, rowIndex );
                    vec.push_back( value.convert< int >( ) );
                }
                property->SetValue( vec );
            }
            else if( property->IsFloatVector( ) )
            {
                std::vector< float > vec;
                Poco::DynamicAny value;
                for( int rowIndex = 0; rowIndex < recordset.rowCount( ); rowIndex++ )
                {
                    value = recordset.value( 0, rowIndex );
                    vec.push_back( value.convert< float >( ) );
                }
                property->SetValue( vec );
            }
            else if( property->IsDoubleVector( ) )
            {
                std::vector< double > vec;
                Poco::DynamicAny value;
                for( int rowIndex = 0; rowIndex < recordset.rowCount( ); rowIndex++ )
                {
                    value = recordset.value( 0, rowIndex );
                    vec.push_back( value.convert< double >( ) );
                }
                property->SetValue( vec );
            }
            else if( property->IsStringVector( ) )
            {
                std::vector< std::string > vec;
                Poco::DynamicAny value;
                for( int rowIndex = 0; rowIndex < recordset.rowCount( ); rowIndex++ )
                {
                    value = recordset.value( 0, rowIndex );
                    vec.push_back( value.convert< std::string > ( ) );
                }
                property->SetValue( vec );
            }
        }
        iterator++;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadByKey( const std::string& KeyName, boost::any KeyValue )
{
    Poco::Data::Session session( ves::xplorer::data::DatabaseManager::
                                 instance( )->GetPool( )->get( ) );
    bool retval = LoadByKey( &session, KeyName, KeyValue );
    return retval;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::LoadByKey( Poco::Data::Session* session, const std::string& KeyName, boost::any KeyValue )
{
    bool returnVal = false;

    if( !session )
    {
        std::cout << "Invalid database session" << std::endl << std::flush;
        return false;
    }

    // Check for existence of mTableName in db and fail if false.
    if( !_tableExists(session, mTableName) )
    {
        return false;
    }

    Poco::Data::Statement statement( ( *session ) );
    // If more than one record was returned, we simply load the first one.
    // It's the caller's problem if this isn't the record they wanted, since
    // they are implicitly requesting a unique result to a possibly
    // multi-result query.
    statement << "SELECT id FROM " << mTableName << " WHERE " << KeyName << "=:KeyValue LIMIT 1";
    BindableAnyWrapper bindable;
    bindable.BindValue( &statement, KeyValue );
    Poco::UInt32 result = 0;
    statement, Poco::Data::into( result );
    try
    {
        statement.execute( );
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << "PropertySet::LoadByKey: " << e.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "PropertySet::LoadByKey: Unknown error accessing database." << std::endl;
    }

    if( result != 0 )
    {
        mID = result;
        returnVal = LoadFromDatabase( session, mTableName, mID );
    }

    return returnVal;
}

bool PropertySet::LoadByKey( const std::string& DatabaseName, const std::string& KeyName, boost::any KeyValue )
{
    bool returnValue = false;
    try
    {
        Poco::Data::SQLite::Connector::registerConnector( );
        Poco::Data::Session session( "SQLite", DatabaseName );

        returnValue = LoadByKey( &session, KeyName, KeyValue );

        // Close db connection
        Poco::Data::SQLite::Connector::unregisterConnector( );
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "Unspecified error while reading from database." << std::endl;
    }
    return returnValue;
}

bool PropertySet::WriteToDatabase( )
{
    Poco::Data::Session session( ves::xplorer::data::DatabaseManager::
                                 instance( )->GetPool( )->get( ) );
    bool retval = WriteToDatabase( &session );
    return retval;
}

bool PropertySet::WriteToDatabase( const std::string& DatabaseName )
{
    bool returnValue = WriteToDatabase( DatabaseName, mTableName );
    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::WriteToDatabase( const std::string& DatabaseName,
                                   const std::string& TableName )
{
    bool returnValue = false;

    // Open db connection and session
    try
    {
        Poco::Data::SQLite::Connector::registerConnector( );
        Poco::Data::Session session( "SQLite", DatabaseName );

        returnValue = WriteToDatabase( &session, TableName );

        // Close db connection
        Poco::Data::SQLite::Connector::unregisterConnector( );
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::WriteToDatabase( Poco::Data::Session* session )
{
    bool returnValue = WriteToDatabase( session, mTableName );
    return returnValue;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::WriteToDatabase( Poco::Data::Session* session,
                                   const std::string& TableName )
{
    Poco::Data::Statement statement( ( *session ) );
    return WriteToDatabase( session, TableName, statement );
}

bool PropertySet::WriteToDatabase( Poco::Data::Session* session, const std::string& TableName, Poco::Data::Statement& statement )
{
    // We can write from the base class because table column names correspond
    // to property names.

    // Need to have explicitly named variables if we want to be able to bind
    // with Poco::Data
    std::string m_TableName = TableName;

    bool returnVal = false;

    if( m_TableName.empty( ) )
    {
        return false;
    }

    // These two don't get used until about 150 lines down, but need to be
    // declared outside the try{} block so memory can be properly cleaned
    // up in case of an exception during writing to database
    // Stores bindable wrappers for later deletion
    std::vector< BindableAnyWrapper* > bindableVector;

    try
    {
        // See if a table for this type already exists; if not create the table
        if( !_tableExists( session, m_TableName ) ) // Table doesn't exist
        {
            std::string columnHeaderString = _buildColumnHeaderString( );
            Poco::Data::Statement sm( ( *session ) );
            sm << "CREATE TABLE " << m_TableName << " (" << columnHeaderString << ")";
            sm.execute( );
        }

        // Determine whether a record already exists for this PropertySet.
        // This query will return a non-zero, positive id iff the record exists
        int idTest = 0;

        ( *session ) << "SELECT id FROM " << m_TableName << " WHERE id=:id",
                Poco::Data::into( idTest ),
                Poco::Data::use( mID ),
                Poco::Data::now;

        // Since the data binding part will be the same for INSERT and UPDATE
        // operations on this PropertySet, we only need to build the string part
        // of the query separately.

        // Will hold the string part of any query we do.
        std::string query;
        // Will hold the list of fields in the order added to query.
        std::vector< std::string > fieldNames;

        if( idTest == 0 ) //  Record does not exist; perform an INSERT
        {
            // Build a query that looks like this:
            // "INSERT INTO tablename (field1name_1,fieldname_2,...) VALUES (:1,:2,...)"
            query = "INSERT INTO ";
            query.append( m_TableName );
            query.append( " (" );

            Property* property;
            PropertyMap::const_iterator iterator = mPropertyMap.begin( );
            while( iterator != mPropertyMap.end( ) )
            {
                property = iterator->second;
                // Check for a known type
                if( ( property->IsBool( ) ) || ( property->IsDouble( ) ) ||
                        ( property->IsFloat( ) ) || ( property->IsInt( ) ) ||
                        ( property->IsString( ) ) )
                {
                    // Skip the property if its name contains illegal characters
                    if( !_containsIllegalCharacter( iterator->first ) )
                    {
                        query.append( iterator->first );
                        query.append( "," );
                        fieldNames.push_back( iterator->first );
                    }
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
            for( size_t count = 0; count < max; count++ )
            {
                query.append( ":" );
                query.append( boost::lexical_cast<std::string > ( count ) );
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
            // Build a query that looks like:
            // "UPDATE tablename SET field_0=:0, field_1=:1 WHERE id=mID"
            query = "UPDATE ";
            query.append( m_TableName );
            query.append( " SET " );

            Property* property;
            PropertyMap::const_iterator iterator = mPropertyMap.begin( );
            while( iterator != mPropertyMap.end( ) )
            {
                property = iterator->second;
                // Check for a known type
                if( ( property->IsBool( ) ) || ( property->IsDouble( ) ) ||
                        ( property->IsFloat( ) ) || ( property->IsInt( ) ) ||
                        ( property->IsString( ) ) )
                {
                    // Skip the property if its name contains illegal characters
                    if( !_containsIllegalCharacter( iterator->first ) )
                    {
                        query.append( iterator->first );
                        query.append( "=:" );
                        query.append( boost::lexical_cast<std::string > ( fieldNames.size( ) ) );
                        query.append( "," );

                        fieldNames.push_back( iterator->first );
                    }
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
            query.append( boost::lexical_cast<std::string > ( mID ) );
        }

        // Turn the query into a statement that can accept bound values
        //Poco::Data::Statement statement( ( *session ) );
        statement << query;

        // The data binding looks the same for either query type (INSERT or UPDATE)
        BindableAnyWrapper* bindable; // Bindable wrapper for property data
        Property* property;
        std::vector<std::string>::iterator iterator = fieldNames.begin( );
        while( iterator != fieldNames.end( ) )
        {
            std::string currentFieldName = ( *iterator );
            property = mPropertyMap[ currentFieldName ];

            bindable = new BindableAnyWrapper;
            bindableVector.push_back( bindable );
            // Force enums to save their associated string value
            if( ( property->IsEnum( ) ) )
            {
                bindable->BindValue( &statement, property->GetAttribute( "enumCurrentString" ) );
            }
            else
            {
                bindable->BindValue( &statement, property->GetValue( ) );
            }

            iterator++;
        }

        //std::cout << statement.toString( ) << std::endl;

        statement.execute( );
        // If we've made it here, we successfully wrote to database
        returnVal = true;

        // If we just did an INSERT we need to get the id of the
        // record we just INSERTed and store it as mID.
        if( statement.toString( ).substr( 0, 6 ) == "INSERT" )
        {
            ( *session ) << "SELECT MAX(id) FROM " << TableName,
                    Poco::Data::into( mID ),
                    Poco::Data::now;
        }
    }
    catch( Poco::Data::DataException &e )
    {
        std::cout << e.displayText( ) << std::endl;
    }
    catch( ... )
    {
        std::cout << "Error writing to database." << std::endl;
    }

    // Delete the BindableAnyWrapperS that were created in the binding loop
    std::vector< BindableAnyWrapper* >::iterator biterator =
            bindableVector.begin( );
    while( biterator != bindableVector.end( ) )
    {
        delete ( *biterator );
        biterator++;
    }


    // After the top-level entries have been written, we write out any vectorized
    // quantities which must go in their own sub-table(s)
    std::vector< BindableAnyWrapper* > bindVector;

    // Open a db transaction. This allows multiple INSERTs and UPDATEs to
    // happen very quickly. Failure to use a transaction in this instance
    // will cause lists to take roughly .25 seconds *per item*. With a transaction,
    // 10,000 items can be inserted or updated in ~1 second.
    session->begin( );
    Property* property;
    PropertyMap::const_iterator iterator = mPropertyMap.begin( );
    while( iterator != mPropertyMap.end( ) )
    {
        property = iterator->second;
        if( property->IsVectorized( ) )
        {
            // Vectors (lists) are written into separate tables named
            // ParentTable_[ThisPropertyName]. Prefixing with the parent table
            // name ensures that property names need not be unique across all property
            // sets. For example, PropertySetA might have a property called
            // 'Directions' that is a list of strings, and PropertySetB might have a property
            // set called 'Directions' that is a list of integers. If the child table is
            // simply called "Directions", there could be foreign key overlap in the child table,
            // as well as problems with type mis-match. Prefixing with the parent table's
            // name prevents such issues.

            enum LISTTYPE
            {
                UNKNOWN, INTEGER, FLOAT, DOUBLE, STRING
            };
            LISTTYPE listType = UNKNOWN;
            std::string columnType( "" );

            // Determine the list type and set the column type in case the table
            // must be created.
            if( property->IsIntVector( ) )
            {
                listType = INTEGER;
                columnType = "INTEGER";
            }
            else if( property->IsFloatVector( ) )
            {
                listType = FLOAT;
                columnType = "FLOAT";
            }
            else if( property->IsDoubleVector( ) )
            {
                listType = DOUBLE;
                columnType = "DOUBLE";
            }
            else if( property->IsStringVector( ) )
            {
                listType = STRING;
                columnType = "TEXT";
            }

            if( listType != UNKNOWN )
            {
                // New (sub)table gets the name
                // [ParentTableName]_[currentFieldName]
                std::string newTableName( m_TableName );
                newTableName += "_";
                std::string fieldName( boost::any_cast<std::string > (
                                       property->GetAttribute( "nameInSet" ) ) );
                newTableName += fieldName;

                // Check for existing table; if table doesn't exist, create it.
                if( !_tableExists( session, newTableName ) )
                {
                    ( *session ) << "CREATE TABLE " << newTableName <<
                            " (id INTEGER PRIMARY KEY,PropertySetParentID INTEGER,"
                            << fieldName << " " << columnType << ")", Poco::Data::now;
                }

                // First part of query will delete everything from the sub-table
                // that this propertyset owns. We do this because it is
                // much easier than checking whether the data exists
                // and attempting to do an update. In the case of a list like this,
                // we'd not only have to see if the list already exists in the table,
                // but whether it's the same length. Then we'd have to either delete
                // rows from the table or insert rows to make the lengths match
                // before doing an update. Far easier to wipe out what's there
                // and start fresh. It's probably a little slower for large
                // lists to do it this way. If lists become a performance
                // problem, this is one place to look for possible speedups.
                // "DELETE FROM [newtablename] WHERE PropertySetParentID=[id]"
                std::string listQuery;
                listQuery += "DELETE FROM ";
                listQuery += newTableName;
                listQuery += " WHERE ";
                listQuery += "PropertySetParentID=";
                listQuery += boost::lexical_cast<std::string > ( mID );

                ( *session ) << listQuery, Poco::Data::now;
                listQuery.clear( );

                BindableAnyWrapper* bindable;
                unsigned int max = GetBoostAnyVectorSize( property->GetValue( ) );
                for( unsigned int index = 0; index < max; index++ )
                {
                    // Build up query:
                    // INSERT INTO [newTableName]
                    // ([fieldName],PropertySetParentID) VALUES (:num,[mID])
                    listQuery += "INSERT INTO ";
                    listQuery += newTableName;
                    listQuery += " (";
                    listQuery += fieldName;
                    listQuery += ",PropertySetParentID) VALUES (:";
                    listQuery += boost::lexical_cast<std::string > ( index );
                    listQuery += ",";
                    listQuery += boost::lexical_cast<std::string > ( mID );
                    listQuery += ")";

                    // Turn into a prepared statement that can accept bindings
                    Poco::Data::Statement listStatement( ( *session ) );
                    listStatement << listQuery;
                    listQuery.clear( );

                    // Extract data from vector for binding into query
                    boost::any currentValue;
                    switch( listType )
                    {
                    case INTEGER:
                    {
                        std::vector<int> vec = boost::any_cast< std::vector<int> >( property->GetValue( ) );
                        currentValue = vec.at( index );
                        break;
                    }
                    case FLOAT:
                    {
                        std::vector<float> vec = boost::any_cast< std::vector<float> >( property->GetValue( ) );
                        currentValue = vec.at( index );
                        break;
                    }
                    case DOUBLE:
                    {
                        std::vector<double> vec = boost::any_cast< std::vector<double> >( property->GetValue( ) );
                        currentValue = vec.at( index );
                        break;
                    }
                    case STRING:
                    {
                        std::vector<std::string> vec = boost::any_cast< std::vector<std::string> >( property->GetValue( ) );
                        currentValue = vec.at( index );
                        break;
                    }
                    }

                    // Bind the data and execute the statement if no binding errors
                    bindable = new BindableAnyWrapper;
                    bindVector.push_back( bindable );
                    if( !bindable->BindValue( &listStatement, currentValue ) )
                    {
                        std::cout << "Error in binding data" << std::endl;
                    }
                    else
                    {
                        listStatement.execute( );
                    }
                }
            }
        }
        iterator++;
    }
    // Close the db transaction
    session->commit( );

    { // Braces protect scoping of biterator
        std::vector< BindableAnyWrapper* >::iterator biterator =
                bindVector.begin( );
        while( biterator != bindVector.end( ) )
        {
            delete ( *biterator );
            biterator++;
        }
    }

    return returnVal;
}

////////////////////////////////////////////////////////////////////////////////

bool PropertySet::_tableExists( Poco::Data::Session* session, const std::string& TableName )
{
    bool tableExists = false;

    // "SELECT 1 ... will put a 1 (true) into the boolean value if the tablename
    // is found in the database.
    ( *session ) << "SELECT 1 FROM sqlite_master WHERE name=:name",
            Poco::Data::into( tableExists ),
            Poco::Data::use( TableName ),
            Poco::Data::now;

    return tableExists;
}
////////////////////////////////////////////////////////////////////////////////

std::string PropertySet::_buildColumnHeaderString( )
{
    std::string result;

    // Forcing the primary key to autoincrement ensures that we can always
    // find the most recently inserted entry simply by issuing
    // SELECT MAX(id) from table_name
    result.append( "id INTEGER PRIMARY KEY AUTOINCREMENT," );

    Property *property;
    PropertyMap::const_iterator iterator = mPropertyMap.begin( );
    while( iterator != mPropertyMap.end( ) )
    {
        property = iterator->second;
        std::string dataType;

        // Figure out what to put in for the colum data type
        if( property->IsBool( ) )
        {
            dataType = "INTEGER";
        }
        else if( property->IsEnum( ) )
        {
            // We treat enum persistence as a string rather than an int
            dataType = "TEXT";
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

        if( _containsIllegalCharacter( iterator->first ) )
        {
            // This will cause the property to be skipped in db writes:
            dataType = "UNKNOWN";

            std::cout << "Error: Property " << iterator->first << " contains a"
                    << " disallowed character. Allowed characters are digits"
                    << " 0-9, letters, and underscore. This property will not"
                    << " be written to the database." << std::endl;
        }
        // Put the property's name string in as the column name
        // If the property is of an unknown type, we skip it for now
        // and there will be no db column for it.
        if( dataType != "UNKNOWN" )
        {
            result.append( iterator->first );
            result.append( " " );
            result.append( dataType );
            result.append( "," );

            // Don't want to add a comma at the end if this is the last entry
            iterator++;
            //            if( iterator != mPropertyMap.end( ) )
            //            {
            //                result.append( "," );
            //            }
        }
        else
        {
            iterator++;
        }
    }

    // There May be an extra comma at the end of the result that must be
    // removed. Test for it and remove if it is there.
    if( result.substr( result.size( ) - 1, result.size( ) ) == "," )
    {
        result.erase( --result.end( ) );
    }

    return result;
}
////////////////////////////////////////////////////////////////////////////////

bool PropertySet::_containsIllegalCharacter( const std::string& value )
{
    size_t position = value.find_first_not_of(
                                               "1234567890_aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ" );
    if( position != value.npos )
    {
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::_connectChanges( Property* property )
{
    property->SignalAttributeChanged.connect( boost::bind( &PropertySet::
                                                           ChangeAccumulator,
                                                           this, ::_1 ) );

    property->SignalDisabled.connect( boost::bind( &PropertySet::
                                                   ChangeAccumulator,
                                                   this, ::_1 ) );

    property->SignalEnabled.connect( boost::bind( &PropertySet::
                                                  ChangeAccumulator,
                                                  this, ::_1 ) );

    property->SignalValueChanged.connect( boost::bind( &PropertySet::
                                                       ChangeAccumulator,
                                                       this, ::_1 ) );
}
////////////////////////////////////////////////////////////////////////////////

void PropertySet::ChangeAccumulator( Property* property )
{
    // Ask the property for its name
    std::string nameInSet;
    nameInSet = boost::any_cast< std::string > ( property->GetAttribute( "nameInSet" ) );

    // See if we already have changes recorded for this property
    bool found = false;
    PSVectorOfStrings::const_iterator iterator = mAccumulatedChanges.begin( );
    PSVectorOfStrings::const_iterator end = mAccumulatedChanges.end( );
    while( ( !found ) && ( iterator != end ) )
    {
        if( ( *iterator ) == nameInSet )
        {
            found = true;
        }
        iterator++;
    }

    // Add the property's name to our list if it isn't already there, but also
    // restrict the size of the vector to 1000 elements.
    if( ( !found ) && ( mAccumulatedChanges.size( ) < 1000 ) )
    {
        mAccumulatedChanges.push_back( nameInSet );
    }
}

unsigned int PropertySet::GetBoostAnyVectorSize( const boost::any& value )
{
    unsigned int size = 0;
    Property temp( 0 );
    if( temp.IsIntVector( value ) )
    {
        size = boost::any_cast< std::vector<int> >( value ).size( );
    }
    else if( temp.IsFloatVector( value ) )
    {
        size = boost::any_cast< std::vector<float> >( value ).size( );
    }
    else if( temp.IsDoubleVector( value ) )
    {
        size = boost::any_cast< std::vector<double> >( value ).size( );
    }
    else if( temp.IsStringVector( value ) )
    {
        size = boost::any_cast< std::vector<std::string> >( value ).size( );
    }

    return size;
}
////////////////////////////////////////////////////////////////////////////////

}
}
}

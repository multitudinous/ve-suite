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
#include <Persistence/MongoStore.h>

#include <Persistence/Datum.h>
#include <Persistence/Persistable.h>
#include <Persistence/BindableAnyWrapper.h>

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/algorithm/string.hpp>


namespace Persistence
{

MongoStore::MongoStore():
    m_connection(0)
{
}
////////////////////////////////////////////////////////////////////////////////
MongoStore::~MongoStore()
{
    Detach();
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::SetStorePath( const std::string& path )
{
    //std::cout << "MongoStore::SetStorePath: path = " << path << std::endl << std::flush;
    m_path = path;
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::Attach()
{
    //std::cout << "MongoStore::Attach" << std::endl << std::flush;
    if( !m_connection )
    {
        m_connection = new mongo::DBClientConnection;
    }

    m_connection->connect( m_path );
}
////////////////////////////////////////////////////////////////////////////////
bool MongoStore::HasTypename( const std::string& typeName )
{
    bool exists = false;
    return exists;
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::Detach()
{
    //std::cout << "MongoStore::Detach" << std::endl << std::flush;
    if( m_connection )
    {
        delete m_connection;
        m_connection = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::SaveImpl( const Persistable& persistable,
                   Role role )
{
    //std::cout << "MongoStore::SaveImpl" << std::endl << std::flush;
    mongo::BSONObjBuilder builder;
    builder.append( "_id", persistable.GetUUIDAsString() );

    std::vector< std::string > dataList = persistable.GetDataList();
    std::vector< std::string >::iterator it = dataList.begin();
    while( it != dataList.end() )
    {
        std::string name = *it;
        DatumPtr datum = persistable.GetDatum( name );

        if( datum->IsBool() )
        {
            builder.append( name, datum->extract< bool >() );
        }
        else if( datum->IsInt() )
        {
            builder.append( name, datum->extract< int >() );
        }
        else if( datum->IsFloat() )
        {
            builder.append( name, datum->extract< float >() );
        }
        else if( datum->IsDouble() )
        {
            builder.append( name, datum->extract< double >() );
        }
        else if( datum->IsString() )
        {
            builder.append( name, datum->extract< std::string >() );
        }
        else if( datum->IsBLOB() )
        {
            // This is way more efficient than storing a BSON array of char.
            std::vector< char > vec = datum->extract< std::vector< char > >();
            if( !vec.empty() )
            {
                void* data = &(vec[0]);
                builder.appendBinData( name, vec.size(), mongo::BinDataGeneral, data );
            }
        }
        else if( datum->IsIntVector() )
        {
            builder.append( name, datum->extract< std::vector< int > >() );
        }
        else if( datum->IsFloatVector() )
        {
            builder.append( name, datum->extract< std::vector< float > >() );
        }
        else if( datum->IsDoubleVector() )
        {
            builder.append( name, datum->extract< std::vector< double > >() );
        }
        else if( datum->IsStringVector() )
        {
            builder.append( name, datum->extract< std::vector< std::string > >() );
        }

        ++it;
    }

    std::string dbNamespace = "ves.";
    dbNamespace += persistable.GetTypeName();

    // Do this as an update operation with the "upsert" flag set true: if the
    // document exists, it is replaced; if it doesn't exist, it is inserted.
    m_connection->update( dbNamespace,
                          QUERY("_id" << persistable.GetUUIDAsString()),
                          builder.obj(),
                          true );
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::LoadImpl( Persistable& persistable, Role role )
{
    //std::cout << "MongoStore::LoadImpl" << std::endl << std::flush;
    std::string dbNamespace = "ves.";
    dbNamespace += persistable.GetTypeName();

    std::auto_ptr<mongo::DBClientCursor> cursor =
        m_connection->query( dbNamespace,
                             QUERY( "_id" << persistable.GetUUIDAsString() ) );

    if( cursor->more() )
    {
        mongo::BSONObj p = cursor->next();
        //std::cout << "MongoStore::LoadImpl: found obj; loading." << std::endl << std::flush;
        std::vector< std::string > dataList = persistable.GetDataList();
        std::vector< std::string >::iterator it = dataList.begin();
        while( it != dataList.end() )
        {
            mongo::BSONElement value = p.getField( *it );
            if( value.ok() )
            {
                DatumPtr datum = persistable.GetDatum( *it );
                if( datum->IsBool() )
                {
                    bool val = value.Bool();
                    datum->SetValue( val );
                }
                else if( datum->IsInt() )
                {
                    int val = value.Int();
                    datum->SetValue( val );
                }
                else if( datum->IsFloat() )
                {
                    float val = value.Double();
                    datum->SetValue( val );
                }
                else if( datum->IsDouble() )
                {
                    double val = value.Double();
                    datum->SetValue( val );
                }
                else if( datum->IsString() )
                {
                    std::string val = value.String();
                    datum->SetValue( val );
                }
                else if( datum->IsBLOB() )
                {
                    int size = value.size();
                    const char* data = value.binData( size );
                    std::vector< char > val( data, data + size );
                }
                else if( datum->IsIntVector() )
                {
                    std::vector< int > val;
                    mongo::BSONObj subObj = value.Obj();
                    subObj.Vals( val );
                    datum->SetValue( val );
                }
                else if( datum->IsFloatVector() )
                {
                    std::vector< double > val;
                    mongo::BSONObj subObj = value.Obj();
                    subObj.Vals( val );
                    std::vector< float > castVals;
                    for( size_t index = 0; index < val.size(); ++index )
                    {
                        castVals.push_back( val.at( index ) );
                    }
                    datum->SetValue( castVals );
                }
                else if( datum->IsDoubleVector() )
                {
                    std::vector< double > val;
                    mongo::BSONObj subObj = value.Obj();
                    subObj.Vals( val );
                    datum->SetValue( val );
                }
                else if( datum->IsStringVector() )
                {
                    std::vector< std::string > val;
                    mongo::BSONObj subObj = value.Obj();
                    subObj.Vals( val );
                    datum->SetValue( val );
                }
            }
            ++it;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::Remove( Persistable& persistable )
{
    std::string dbNamespace = "ves.";
    dbNamespace += persistable.GetTypeName();

    m_connection->remove(dbNamespace,
                         QUERY( "_id" << persistable.GetUUIDAsString() ) );
}
////////////////////////////////////////////////////////////////////////////////
bool MongoStore::HasIDForTypename( const boost::uuids::uuid& id,
                                   const std::string& typeName )
{
    //std::cout << "MongoStore::HasIDForTypename" << std::endl << std::flush;
    std::string dbNamespace = "ves.";
    dbNamespace += typeName;

    std::string idString = boost::lexical_cast< std::string >( id );

    std::auto_ptr<mongo::DBClientCursor> cursor =
            m_connection->query( dbNamespace, QUERY( "_id" << idString ) );
    if( cursor->objsLeftInBatch() )
    {
        return true;
    }
    else
    {
        return false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::GetIDsForTypename( const std::string& typeName,
                                std::vector< std::string >& resultIDs )
{
    std::string dbNamespace = "ves.";
    dbNamespace += typeName;

    std::auto_ptr<mongo::DBClientCursor> cursor =
            m_connection->query( dbNamespace, mongo::BSONObj() );

    while( cursor->more() )
    {
        mongo::BSONObj rec = cursor->next();
        resultIDs.push_back( rec.getStringField("_id") );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::Search( const std::string& typeName,
                         std::vector< SearchCriterion >& criteria,
                         const std::string& returnField,
                         std::vector< std::string >& results )
{
    std::string dbNamespace = "ves.";
    dbNamespace += typeName;

    // Very limited -- assumes a single criterion of form key, comparison, value
    SearchCriterion sc( criteria.at( 0 ) );
    std::string key = sc.m_key;
    std::string comparison = sc.m_comparison;
    boost::any value = sc.m_value;

    mongo::Labeler::Label cmp( mongo::GT );
    if( comparison == ">")
    {
        cmp = mongo::GT;
    }
    else if( comparison == "<" )
    {
        cmp = mongo::LT;
    }
    else if( comparison == ">=" )
    {
        cmp = mongo::GTE;
    }
    else if( comparison == "<=" )
    {
        cmp = mongo::LTE;
    }
    else if( comparison == "!=" )
    {
        cmp = mongo::NE;
    }

    Datum tester(0);
    std::auto_ptr<mongo::DBClientCursor> cursor;

    if( tester.IsBool( value ) )
    {
        bool v = boost::any_cast< bool >( value );
        if( comparison == "=")
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << v  ) );
        }
        else
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << cmp << v  ) );
        }
    }
    else if( tester.IsDouble( value ) )
    {
        double v = boost::any_cast< double >( value );
        if( comparison == "=")
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << v  ) );
        }
        else
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << cmp << v  ) );
        }
    }
    else if( tester.IsFloat( value ) )
    {
        float v = boost::any_cast< float >( value );
        if( comparison == "=")
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << v  ) );
        }
        else
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << cmp << v  ) );
        }
    }
    else if( tester.IsInt( value ) )
    {
        int v = boost::any_cast< int >( value );
        if( comparison == "=")
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << v  ) );
        }
        else
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << cmp << v  ) );
        }
    }
    else if( tester.IsString( value ) )
    {
        std::string v = boost::any_cast< std::string >( value );
        if( comparison == "=")
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << v  ) );
        }
        else
        {
            cursor = m_connection->query( dbNamespace, QUERY( key << cmp << v  ) );
        }
    }

    std::string field = returnField;
    // We store the uuid field in the special _id field in keeping with mongo's
    // suggested schema
    if( returnField == "uuid" )
    {
        field = "_id";
    }

    while( cursor->more() )
    {
        mongo::BSONObj rec = cursor->next();
        mongo::BSONElement elem = rec.getField( field.c_str() );
        if( elem.type() == mongo::String )
        {
            results.push_back( elem.String() );
        }
        else
        {
            results.push_back( elem.toString(false) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::ProcessBackgroundTasks()
{
    // No bg tasks for Mongo yet.
    // This will be the place to do indexing
}
////////////////////////////////////////////////////////////////////////////////
unsigned int MongoStore::GetBoostAnyVectorSize( const boost::any& value )
{
    unsigned int size = 0;
    Datum temp( 0 );
    if( temp.IsIntVector( value ) )
    {
        size = boost::any_cast< std::vector<int> >( value ).size();
    }
    else if( temp.IsFloatVector( value ) )
    {
        size = boost::any_cast< std::vector<float> >( value ).size();
    }
    else if( temp.IsDoubleVector( value ) )
    {
        size = boost::any_cast< std::vector<double> >( value ).size();
    }
    else if( temp.IsStringVector( value ) )
    {
        size = boost::any_cast< std::vector<std::string> >( value ).size();
    }

    return size;
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::MapReduce( const std::string& typeName,
                const std::string& jsMapFunction,
                const std::string& jsReduceFunction,
                mongo::BSONObj queryObj,
                const std::string& outputUUID,
                const std::string& outputcollection )
{
    std::string ns = "ves.";
    ns += typeName;
    m_connection->mapreduce( ns, jsMapFunction, jsReduceFunction, queryObj,
                             outputcollection );

    // mongo screws up our ORM schema by placing the reduce results as a
    // subobject of the field "value". We need to unpack this subobject
    // into the main document.
    ns = "ves.";
    ns += outputcollection;
    std::auto_ptr<mongo::DBClientCursor> cursor =
        m_connection->query( ns, QUERY( "_id" << outputUUID ) );

    if( !cursor->more() )
    {
        // This means MapReduce produced no output
        return;
    }

    mongo::BSONObj rec = cursor->next();
    mongo::BSONElement elem = rec.getField( "value" );
    rec = elem.Obj();

    // Build a new object with same UUID and "update" the collection with this
    // new object
    mongo::BSONObjBuilder builder;
    builder.append( "_id", outputUUID );

    std::vector< mongo::BSONElement > elements;
    rec.elems( elements );
    for( size_t index = 0; index < elements.size(); ++index )
    {
        mongo::BSONElement subElem = elements[ index ];
        builder.append( subElem );
    }

    m_connection->update( ns, QUERY( "_id" << outputUUID ), builder.obj(), true );
}
////////////////////////////////////////////////////////////////////////////////
void MongoStore::Drop( const std::string& typeName, Role role )
{
    std::string dbNamespace = "ves.";
    dbNamespace += typeName;
    m_connection->dropCollection( dbNamespace );
}

////////////////////////////////////////////////////////////////////////////////
} // namespace Persistence

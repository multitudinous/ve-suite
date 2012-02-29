#include "MongoStore.h"

#include "Datum.h"
#include "Persistable.h"
#include "BindableAnyWrapper.h"

#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>


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
        else if( datum->IsIntVector() )
        {
            mongo::BSONObjBuilder array;
            std::vector< int > vec = datum->extract< std::vector< int > >();
            for( size_t index = 0; index < vec.size(); ++index )
            {
                std::stringstream label;
                label << index;
                array.append( label.str(), vec.at( index ) );
            }
            builder.append( name, array.obj() );
        }
        else if( datum->IsFloatVector() )
        {
            mongo::BSONObjBuilder array;
            std::vector< float > vec = datum->extract< std::vector< float > >();
            for( size_t index = 0; index < vec.size(); ++index )
            {
                std::stringstream label;
                label << index;
                array.append( label.str(), vec.at( index ) );
            }
            builder.append( name, array.obj() );
        }
        else if( datum->IsDoubleVector() )
        {
            mongo::BSONObjBuilder array;
            std::vector< double > vec =
                    datum->extract< std::vector< double > >();
            for( size_t index = 0; index < vec.size(); ++index )
            {
                std::stringstream label;
                label << index;
                array.append( label.str(), vec.at( index ) );
            }
            builder.append( name, array.obj() );
        }
        else if( datum->IsStringVector() )
        {
            mongo::BSONObjBuilder array;
            std::vector< std::string > vec =
                    datum->extract< std::vector< std::string > >();
            for( size_t index = 0; index < vec.size(); ++index )
            {
                std::stringstream label;
                label << index;
                array.append( label.str(), vec.at( index ) );
            }
            builder.append( name, array.obj() );
        }

        ++it;
    }

    std::string dbNamespace = "ves.";
    dbNamespace += persistable.GetTypeName();
    m_connection->insert( dbNamespace, builder.obj() );
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
                     /*criteria,*/
                     std::vector< std::string >& resultIDs )
{
    std::string dbNamespace = "ves.";
    dbNamespace += typeName;

//    std::auto_ptr<mongo::DBClientCursor> cursor =
//            m_connection->query( dbNamespace, QUERY( /*criteria*/ ) );

//    while( cursor->more() )
//    {
//        mongo::BSONObj rec = cursor->next();
//        resultIDs.push_back( rec.getStringField("_id") );
//    }
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
                const std::string& outputcollection )
{
    std::string ns = "ves.";
    ns += typeName;
    m_connection->mapreduce( ns, jsMapFunction, jsReduceFunction, queryObj,
                             outputcollection );
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

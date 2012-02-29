#include <stdlib.h>
#include <iostream>

#include "Persistable.h"
#include "Datum.h"
#include "DataManager.h"
#include "NullBuffer.h"
#include "NullCache.h"
#include "DataAbstractionLayer.h"

#include "SQLiteStore.h"
#include "MongoStore.h"

//#include "PropertySet.h"
//#include "Property.h"
#include <boost/any.hpp>

int main(int argc, char *argv[])
{

    using namespace std;
    using namespace Persistence;

    Persistable p;
    p.AddDatum( "Test", 1.2 );

    // We have two ways of getting at the value, neither of which requires the
    // explicit use of boost::any_cast in the application code -- it now happens
    // behind the scenes:
    double d;
    d = p.GetDatum( "Test" )->extract< double >( );
    d = p.GetDatumValue< double >("Test");

    cout << d << endl;

    cout << "Attempt to cast incorrectly should throw exception..." << endl << "\t";
    // The templated GetDatumValue<>() and Datum->extract<>() will throw
    // an exception if the cast can't be done. The exception tells us what
    // the two incompatible types are.
    try
    {
        cout << p.GetDatumValue< int >("Test") << endl;
        //cout << p.GetDatum( "Test" )->extract< int >( ) << endl;
    }
    catch( char const* e )
    {
        cout << e << endl;
    }

    // Testing the copy constructor
    cout << "Copy constructor...";
    Persistable o;
    o.AddDatum( "Num", 1234.98735 );
    Persistable c( o );
    if( o.GetDatumValue< double >("Num") != c.GetDatumValue< double >("Num") )
    {
        cout << "fail! Reason: Copied value not equal to original" << endl;
    }
    else
    {
        c.SetDatumValue( "Num", 2.0 );
        if( o.GetDatumValue< double >("Num") != c.GetDatumValue< double >("Num") )
        {
            cout << "pass." << endl;
        }
        else
        {
            cout << "fail! Reason: \"Changed\" copy still equal to original" << endl;
        }
    }

//    PropertySet ps;
//    ps.AddProperty( "Test", 99.9, "This" );
//    // We can access the value the old way...
//    cout << boost::any_cast< double >( ps.GetPropertyValue("Test") ) << endl;
//    // Or the new way through the base class's interface
//    cout << ps.GetDatumValue< double >("Test") << endl << flush;

//    cout << boost::any_cast< std::string >( ps.GetPropertyAttribute( "Test", "uiLabel" ) ) << endl;
//    cout << boost::any_cast< std::string >( ps.GetPropertyAttribute( "Test", "nameInSet" ) ) << endl;

    // Set up a datamanager to test persistence
    DataManager manager;
    DataAbstractionLayerPtr cache( new NullCache );
    DataAbstractionLayerPtr buffer( new NullBuffer );
    manager.SetCache( cache );
    manager.SetBuffer( buffer );

    // Add an SQLite store
    DataAbstractionLayerPtr sqstore( new SQLiteStore );
    static_cast<SQLiteStore*>(sqstore.get())->SetStorePath( "/tmp/DALTest.db" );
    //manager.AttachStore( sqstore, Store::WORKINGSTORE_ROLE );

    // Add a mongoDB store
    //cout << "Creating MongoStore" << endl << flush;
    DataAbstractionLayerPtr mongostore( new MongoStore );
    //cout << "Setting MongoStore Path" << endl << flush;
    static_cast<MongoStore*>(mongostore.get())->SetStorePath("localhost");
    //cout << "Attaching MongoStore" << endl << flush;
    manager.AttachStore( mongostore, Store::BACKINGSTORE_ROLE );

    // Build up a persistable with some useful test types
    Persistable q;
    q.SetTypeName( "TestType" );
    q.AddDatum( "Num", 1234.98735 );
    q.AddDatum( "ABool", true );
    q.AddDatum( "AString", std::string("This is a test") );
    q.AddDatum( "AnInt", 19 );

    std::vector<std::string> strs;
    strs.push_back( "Test One" );
    strs.push_back( "Test Two" );
    strs.push_back( "Test Three" );
    strs.push_back( "Test Four" );
    q.AddDatum( "TestVec", strs );

    std::vector<double> dubs;
    dubs.push_back(1.1);
    dubs.push_back(2.2);
    dubs.push_back(3.141592653587);
    q.AddDatum( "DubVec", dubs );

    static_cast<MongoStore*>(mongostore.get())->Drop( "TestType" );

    // Ensure that persistable is saved to backing as well as working dbs.
    // Normally, persistables will just be saved to the working db, but there
    // are cases when we may wish to explicitly place something in a backing
    // store, so we test that here.
    //cout << "Saving Persistable" << endl << flush;
    manager.Save( q );
    manager.Save( q, Store::BACKING_ROLE );

    //cout << "Loading..." << endl << flush;

    // Set the value of "Num" field to some other value. When we load below, this
    // value will be reset to 1234.98735 if the load was successful. We then
    // print out this value as a test.
    q.SetDatumValue( "Num", 7.77 );
    manager.Load( q );
    cout << "This value should be 1234.98735: " << q.GetDatumValue<double>("Num") << endl;

    // Print out the strings that should be stored in TestVec
    cout << "Strings in TestVec field:" << endl;
    std::vector<std::string> strout = q.GetDatumValue<std::vector< std::string > >("TestVec");
    for( size_t i=0; i < strout.size(); ++i )
    {
        cout << "\t" << strout.at(i) << endl;
    }

    // Print out the doubles that should be stored in DubVec
    cout << "Doubles in DubVec field:" << endl;
    std::vector<double> dout = q.GetDatumValue<std::vector< double > >("DubVec");
    for( size_t i=0; i < dout.size(); ++i )
    {
        cout << "\t" << dout.at(i) << endl;
    }

    Persistable one;
    one.SetTypeName( "MRIN" );
    one.AddDatum( "Num", 1 );

    Persistable two;
    two.SetTypeName( "MRIN" );
    two.AddDatum( "Num", 2 );

    Persistable three;
    three.SetTypeName( "MRIN" );
    three.AddDatum( "Num", 2.5 );

    // Drop all records with typename MRIN and MROUT
    manager.Drop( "MRIN" );
    manager.Drop( "MROUT" );

    manager.Save( one );
    manager.Save( two );
    manager.Save( three );

    // Placeholder persistable for output of MapReduce. Creating this first
    // gives us a UUID that can be passed into the Map function.
    Persistable mrout;
    mrout.SetTypeName( "MROUT" );
    mrout.AddDatum( "count", 0 );
    mrout.AddDatum( "sum", 0 );

    std::string mapF = "function() {emit( \"";
    mapF += mrout.GetUUIDAsString();
    mapF += "\", {count: 1, num: this.Num} );}";
    std::string reduceF = "function(key, values) {var result = {count: 0, sum: 0};values.forEach(function(value) {result.count += value.count;result.sum += value.num;});return result;}";
    static_cast<MongoStore*>(mongostore.get())->MapReduce( "MRIN", mapF, reduceF, mongo::BSONObj(), "MROUT" );

    manager.Load( mrout );
    cout << "MapReduce output: count = " << mrout.GetDatumValue<int>("count")
         << ", sum = " << mrout.GetDatumValue<double>("sum") << endl;

    return 0;
}

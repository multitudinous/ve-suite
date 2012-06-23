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
#include <stdlib.h>
#include <iostream>

#include <Persistence/Persistable.h>
#include <Persistence/Datum.h>
#include <Persistence/DataManager.h>
#include <Persistence/NullBuffer.h>
#include <Persistence/NullCache.h>
#include <Persistence/DataAbstractionLayer.h>

#include <Persistence/SQLiteStore.h>
#include <Persistence/MongoStore.h>

//#define USE_PROPERTYSETS
#ifdef USE_PROPERTYSETS
#include "PropertySet.h"
#include "Property.h"
#endif

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

    cout << "Recovering double value: " << d << endl;

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

#ifdef USE_PROPERTYSETS
    ves::xplorer::data::PropertySet ps;
    cout << "Testing a propertySet..." << endl;
    ps.SetTypeName( "TestPropertySet" );
    ps.AddProperty( "Test", 99.9, "This is a ui label" );
    // We can access the value the old way...
    cout << "\tTwo ways to access value should yield same value..." << endl;
    cout << "\t\tOld way: " << boost::any_cast< double >( ps.GetPropertyValue("Test") ) << endl;
    // Or the new way through the base class's interface
    cout << "\t\tNew way: " << ps.GetDatumValue< double >("Test") << endl << flush;

    std::vector< std::string > enums;
    enums.push_back( "Zero" );
    enums.push_back( "One" );
    enums.push_back( "Two" );
    enums.push_back( "Three" );
    ps.AddProperty( "ENUM", std::string("This value won't stay after the enum vector is set. It will revert to zero then."), "An Enumerated Value");
    ps.SetPropertyAttribute( "ENUM", "enumValues", enums );

    cout << "\tuiLabel: "
         << ps.GetPropertyAttributeValue< std::string >( "Test", "uiLabel" ) << endl;
    cout << "\tProperty name in set: "
         << ps.GetPropertyAttributeValue< std::string >( "Test", "nameInSet" ) << endl;
    cout << "\tENUM value: "
         << ps.GetDatumValue< std::string >( "ENUM" ) << endl;
    cout << "\tENUM index: "
         << ps.GetPropertyAttributeValue< int >( "ENUM", "enumCurrentIndex" ) << endl;
#endif

    // Set up a datamanager to test persistence
    DataManager manager;
    DataAbstractionLayerPtr cache( new NullCache );
    DataAbstractionLayerPtr buffer( new NullBuffer );
    manager.SetCache( cache );
    manager.SetBuffer( buffer );

    // Add an SQLite store
//    DataAbstractionLayerPtr sqstore( new SQLiteStore );
//    static_cast<SQLiteStore*>(sqstore.get())->SetStorePath( "/tmp/DALTest.db" );
//    manager.AttachStore( sqstore, Store::WORKINGSTORE_ROLE );

    // Add a mongoDB store
    DataAbstractionLayerPtr mongostore( new MongoStore );
    static_cast<MongoStore*>(mongostore.get())->SetStorePath("localhost");
    //manager.AttachStore( mongostore, Store::BACKINGSTORE_ROLE );
    manager.AttachStore( mongostore, Store::WORKINGSTORE_ROLE );

 #ifdef USE_PROPERTYSETS
    // This works for any type of store connected as WORKING_ROLE
    //manager.Drop( "TestPropertySet" );

    manager.Save( ps );
    ps.SetDatumValue( "ENUM", std::string( "Three" ) );
    manager.Load( ps );
    cout << "\tAfter Load: " << ps.GetDatumValue< std::string >( "ENUM" ) << endl;

    cout << "\tTesting search..." << endl;
    std::string psUUID = ps.GetUUIDAsString();
    std::vector< std::string > results;
    SearchCriterion kvc( "Test", "=", 99.9 );
    std::vector< SearchCriterion > criteria;
    criteria.push_back( kvc );
    manager.Search( "TestPropertySet", criteria, "uuid", results );
    if( !results.empty() )
    {
        bool idFound = false;
        for( size_t index = 0; index < results.size(); ++index )
        {
            cout << "\t\t" << results.at(index) << endl;
            if( results.at(index) == psUUID )
                idFound = true;
        }
        if( idFound )
            cout << "\t...Search successful" << endl;
        else
            cout << "\t...Search returned wrong result" << endl;
    }
    else
    {
        cout << "\tSearch failed, or problem with search" << endl;
    }

    // If there are more than 5 entries in the TestPropertySet table, get rid
    // of the table. This both tests the Drop functionality and makes search
    // result sizes reasonable for these tests.
    if( results.size() > 5 )
    {
        manager.Drop( "TestPropertySet" );
    }
#endif

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

    std::string bdata( "0123456789 10111213141516171819 20212223242526272829" );
    std::vector<char> blob( bdata.begin(), bdata.end() );
    q.AddDatum( "ABlob", blob );

    manager.Drop( "TestType" );

    // Ensure that persistable is saved to backing as well as working dbs.
    // Normally, persistables will just be saved to the working db, but there
    // are cases when we may wish to explicitly place something in a backing
    // store, so we test that here.
    //cout << "Saving Persistable" << endl << flush;
    manager.Save( q );
    //manager.Save( q, Store::BACKING_ROLE );

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

    // Re-form a string from the chars that should be stored in ABlob
    cout << "Chars in ABlob field, re-string-ized:" << endl;
    std::vector<char> bout = q.GetDatumValue<std::vector< char > >("ABlob");
    char* pb = &(bout[0]);
    string bstr( pb, bout.size() );
    cout << "\t" << bstr << endl;

    // Try out MapReduce functionality
#if 1
    // Drop all records with typename MRIN and MROUT
    manager.Drop( "MRIN" );
    manager.Drop( "MROUT" );

    Persistable one;
    one.SetTypeName( "MRIN" );
    one.AddDatum( "Num", 1 );

    Persistable two;
    two.SetTypeName( "MRIN" );
    two.AddDatum( "Num", 2 );

    Persistable three;
    three.SetTypeName( "MRIN" );
    three.AddDatum( "Num", 5 );

    manager.Save( one );
    manager.Save( two );
    manager.Save( three );

    // Placeholder persistable for output of MapReduce. Creating this first
    // gives us a UUID that can be passed into the Map function.
    Persistable mrout;
    mrout.SetTypeName( "MROUT" );
    mrout.AddDatum( "count", 0.0 );
    mrout.AddDatum( "sum", 0.0 );

    std::string mapF = "function() {emit( \"";
    mapF += mrout.GetUUIDAsString();
    mapF += "\", {count: 1, num: this.Num} );}";
    std::string reduceF = "function(key, values) {var result = {count: 0, sum: 0};values.forEach(function(value) {result.count += value.count;result.sum += value.num;});return result;}";
    static_cast<MongoStore*>(mongostore.get())->MapReduce( "MRIN", mapF,
        reduceF, mongo::BSONObj(), mrout.GetUUIDAsString(), "MROUT" );

    manager.Load( mrout );
    cout << "MapReduce output: count = " << mrout.GetDatumValue<double>("count")
         << ", sum = " << mrout.GetDatumValue<double>("sum") << endl;
#endif

    return 0;
}

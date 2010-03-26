#include <ves/xplorer/data/BindableAnyWrapper.h>
#include <Poco/Data/Binding.h>

//#include <Poco/Data/Common.h>
#include <Poco/Data/Statement.h>
//#include <Poco/Data/SQLite/Connector.h>

#include <iostream>

using namespace ves::xplorer::data;

BindableAnyWrapper::BindableAnyWrapper( )
{
}

BindableAnyWrapper::BindableAnyWrapper( const BindableAnyWrapper& orig )
{
}

BindableAnyWrapper::~BindableAnyWrapper( )
{
}

bool BindableAnyWrapper::BindValue( Poco::Data::Statement* statement,
                                   boost::any value )
{
    bool returnValue = false;

    if( value.type( ) == typeid ( bool ) )
    {
        mBool = boost::any_cast< bool >( value );
        (*statement), Poco::Data::use( mBool );
        returnValue = true;
    }
    else if( value.type( ) == typeid (int ) )
    {
        mInt = boost::any_cast< int >( value );
        (*statement), Poco::Data::use( mInt );
        returnValue = true;
    }
    else if( value.type( ) == typeid (float ) )
    {
        mFloat = boost::any_cast< float >( value );
        (*statement), Poco::Data::use( mFloat );
        returnValue = true;
    }
    else if( value.type( ) == typeid (double ) )
    {
        mDouble = boost::any_cast< double >( value );
        (*statement), Poco::Data::use( mDouble );
        returnValue = true;
    }
    else if( boost::any_cast< std::string > ( &value ) )
    {
        mString = boost::any_cast< std::string > ( value );
        (*statement), Poco::Data::use( mString );
        returnValue = true;
    }

    return returnValue;
}


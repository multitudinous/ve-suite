
#include "src/apps/voice/SpeechNavigator.h"

#include <ace/SString.h>
#include <ace/SStringfwd.h>
#include <tao/TAO_Internal.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>

#include <ves/open/moduleC.h>
#include <ves/open/VjObsC.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <sstream>

SpeechNavigator::SpeechNavigator()
   : mClient(new JuliusNetworkClient()), mParser(new JuliusXMLParser()),
     mArgc(0), mArgv(NULL)
{
}

SpeechNavigator::~SpeechNavigator()
{
    delete mClient;
    delete mParser;
}

bool
SpeechNavigator::connectToJulius(const std::string& host, 
                                const unsigned short port)
{
    return mClient->connect(host, port);
}

bool 
SpeechNavigator::isConnectedToNamingService()
{
    if ( CORBA::is_nil( mNamingContext.in() ) )
    {
        return connectToNamingService();
    }
    return true;
}

bool 
SpeechNavigator::connectToNamingService()
{
    try
    {
        // First initialize the ORB,
        // the ORB name, it can be anything!
        mOrb = CORBA::ORB_init( mArgc, mArgv, "" );

        // Here is the part to contact the naming service and get the reference
        // for the executive
        CORBA::Object_var naming_context_object =
            mOrb->resolve_initial_references( "NameService" );
        mNamingContext = CosNaming::NamingContext::_narrow( 
                naming_context_object.in() );
        return true;
    }
    catch ( CORBA::Exception& ex )
    {
        mOrb->destroy();
        std::string tempMessage =
    "Cannot init ORB or can't connect to the Naming Service: CORBA Exception " +
            std::string( ex._info().c_str() ) + "\n";
        return false;
    }
}

bool
SpeechNavigator::connectToXplorer()
{
    // Copied from CORBAServiceList per instructions.

    if ( !isConnectedToNamingService() )
    {
        return false;
    }

    ///This is the old way of communication
    try
    {
        CosNaming::Name name( 1 );
        name.length( 1 );
        //Now get the reference of the VE server
        name[0].id   = CORBA::string_dup( "Master" );
        name[0].kind = CORBA::string_dup( "VE_Xplorer" );
        CORBA::Object_var naming_context_object =
            mOrb->resolve_initial_references( "NameService" );
        CosNaming::NamingContext_var naming_context1 =
            CosNaming::NamingContext::_narrow( naming_context_object.in() );
        CORBA::Object_var ve_object = naming_context1->resolve( name );
        vjobs = VjObs::_narrow( ve_object.in() );

    }
    catch ( CORBA::Exception& ex )
    {
        std::string tempMessage = 
            "Cannot find VE-Xplorer: CORBA Exception " + 
            std::string( ex._info().c_str() ) + "\n";
        return false;
    }
}

void 
SpeechNavigator::setArgcArgv( int argc, char** argv )
{
    //Copy the command line args because tao deletes them after processing them
    mArgc = argc;
    mArgv = new char*[ argc ];
    for (int i = 0; i < mArgc; ++i)
    {
        int stringLength = strlen( argv[ i ] );
        mArgv[ i ] = new char[ stringLength + 1 ];
        strncpy( mArgv[ i ], argv[ i ], stringLength );
    }
}

// vim:ts=4:sw=4:et:tw=0

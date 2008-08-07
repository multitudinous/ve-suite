
#include "src/apps/voice/SpeechNavigator.h"

#include <ace/Thread_Manager.h>
#include <ace/SString.h>
#include <ace/SStringfwd.h>
#include <tao/TAO_Internal.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>

#include <ves/open/moduleC.h>
#include <ves/open/VjObsC.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <sstream>
#include <map>
#include <vector>

#include <iostream>

namespace 
{

    /**
     * Function invoked by the Parser thread.
     *
     * @param   arg         the network client to spin in this thread.
     */
    void* parserThreadFunction(void* arg)
    {
        if (!arg)
        {
            // FIXME:  Indicate a problem in a different manner.
            std::cout << "[DBG] The parser thread did not receive an "
                      << "argument!" << std::endl;
            return NULL;
        }
        JuliusNetworkClient* client = 
            reinterpret_cast<JuliusNetworkClient*>(arg);
        client->startDataLoop();
    }
}

SpeechNavigator::SpeechNavigator()
   : mClient(new JuliusNetworkClient()), mParser(new JuliusXMLParser()),
     mThreadGroupId(-1), mParserThread(0), mStop(false),
     mArgc(0), mArgv(NULL)
{
    mParserObserver = new SpeechRecognitionObserver(
        SpeechRecognitionObserver::PhraseRecognitionHandler(this, 
            &SpeechNavigator::onPhraseRecognition));
    mClient->setParser(mParser);
}

SpeechNavigator::~SpeechNavigator()
{
    if (mThreadGroupId != -1)
    {
        // Politely tell the parser loop to stop.
        if (mParserThread)
        {
            mClient->stopDataLoop();
        }
        if (isParserThreadRunning())
        {
            ACE_Thread_Manager::instance()->join(mParserThread);
        }
    }
    mParser->detach(mParserObserver);
    for (int i = 0; i < mArgc; ++i)
    {
        delete[] mArgv[i];
    }
    delete mClient;
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
    }
    catch ( CORBA::Exception& ex )
    {
        mOrb->destroy();
        std::string tempMessage =
    "Cannot init ORB or can't connect to the Naming Service: CORBA Exception " +
            std::string( ex._info().c_str() ) + "\n";
        std::cerr << "[ERR] Unable to connect to CORBA Naming Service: "
                  << tempMessage << std::endl;
        return false;
    }
    return true;
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
        std::cerr << "[ERR] Unable to connect to VE Xplorer: "
                  << tempMessage << std::endl;
        return false;
    }
    return true;
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
        strncpy( mArgv[ i ], argv[ i ], stringLength + 1 );
    }
}

bool
SpeechNavigator::startParserThread()
{
    mParser->attach(mParserObserver);
    mThreadGroupId = ACE_Thread_Manager::instance()->spawn(
                                          reinterpret_cast<ACE_THR_FUNC>(&parserThreadFunction), 
                                          reinterpret_cast<void*>(mClient),
                                THR_NEW_LWP | THR_JOINABLE | THR_INHERIT_SCHED,
                                          &mParserThread);
    if (mThreadGroupId == -1)
    {
        std::cerr << "[ERR] Unable to spawn parser thread!." << std::endl;
        return false;
    }
    return true;
}

void
SpeechNavigator::stopParserThread()
{
    if (isParserThreadRunning())
    {
        mClient->stopDataLoop();         
        ACE_Thread_Manager::instance()->join(mParserThread);
        mParserThread = 0;
        mThreadGroupId = -1;
    }
}

bool
SpeechNavigator::runDataIteration()
{
    using namespace ves::open::xml;

    std::string data;
    int nav_value = 0;
    if (mSpeechQueue.remove(data))
    {
        if ("MOVE UP" == data)
        {
            nav_value = NAV_UP;
        }
        else if ("MOVE DOWN" == data)
        {
            nav_value = NAV_DOWN;
        }
        else if ("MOVE LEFT" == data)
        {
            nav_value = NAV_LEFT;
        }
        else if ("MOVE RIGHT" == data)
        {
            nav_value = NAV_RIGHT;
        }
        else if ("MOVE FORWARD" == data)
        {
            nav_value = NAV_FWD;
        }
        else if ("MOVE BACKWARD" == data)
        {
            nav_value = NAV_BKWD;
        }
        else if ("PITCH DOWN" == data)
        {
            nav_value = PITCH_DOWN;
        }
        else if ("PITCH UP" == data)
        {
            nav_value = PITCH_UP;
        }
        else if ("ROLL CLOCKWISE" == data)
        {
            nav_value = ROLL_CW;
        }
        else if ("ROLL COUNTERCLOCKWISE" == data)
        {
            nav_value = ROLL_CCW;
        }
        else if ("YAW CLOCKWISE" == data)
        {
            nav_value = YAW_CW;
        }
        else if ("YAW COUNTERCLOCKWISE" == data)
        {
            nav_value = YAW_CCW;
        }
        else
        {
            // Return here since we don't know what type of data to send to
            // Xplorer.
            std::cerr << "[ERR] Unrecognized speech data '" << data 
                      << "'" << std::endl;
            return false;
        }
        DataValuePairPtr data_value_pair(new DataValuePair("FLOAT"));
        data_value_pair->SetDataName(std::string("GUI_NAV"));
        data_value_pair->SetDataValue( static_cast<double>(nav_value) );
        CommandPtr ve_command( new Command() );
        ve_command->SetCommandName( std::string("Navigation_Data") );
        ve_command->AddDataValuePair(data_value_pair);
        // Copied from CORBAServiceList::SendCommandStringToXplorer().
        //Now send the data to xplorer
        ves::open::xml::XMLReaderWriter netowrkWriter;
        netowrkWriter.UseStandaloneDOMDocumentManager();

        // New need to destroy document and send it
        std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
        nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( ve_command, "vecommand" ) );
        std::string xmlDocument( "returnString" );
        netowrkWriter.WriteXMLDocument( nodes, xmlDocument, "Command" );

        if( CORBA::is_nil( vjobs.in() ) || xmlDocument.empty() )
        {
            std::cerr << "[ERR] No XML Data to send to Xplorer." << std::endl;
            return false;
        }

        try
        {
            // CORBA releases the allocated memory so we do not have to
            vjobs->SetCommandString( xmlDocument.c_str() );
        }
        catch ( ... )
        {
            std::cerr << "[ERR] Exception thrown while sending command string."
                      << std::endl;
            return false;
        }
    }
    return true;
}

bool
SpeechNavigator::startDataLoop()
{
    mStop = false;
    while (!mStop)
    {
        if (!runDataIteration())
        {
            return false;
        }
    }
    return true;
}

void
SpeechNavigator::onPhraseRecognition(const std::string& phrase)
{
    mSpeechQueue.add(phrase);
}

bool
SpeechNavigator::isParserThreadRunning() const
{
    ACE_UINT32 state = 0;
    ACE_Thread_Manager::instance()->thr_state(mParserThread, state);
    return (ACE_Thread_Manager::ACE_THR_SPAWNED == state || 
            ACE_Thread_Manager::ACE_THR_RUNNING == state);
}

// vim:ts=4:sw=4:et:tw=0

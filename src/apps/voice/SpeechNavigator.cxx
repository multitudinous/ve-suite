
#include "src/apps/voice/SpeechNavigator.h"

#include <ace/Thread_Manager.h>
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
                                          &parserThreadFunction, 
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
    std::string data;
    if (mSpeechQueue.remove(data))
    {
         
    }
}

bool
SpeechNavigator::startDataLoop()
{
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

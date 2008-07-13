
#ifndef VE_VOICE_SPEECH_NAVIGATOR_H_
#define VE_VOICE_SPEECH_NAVIGATOR_H_

#include "apps/voice/CircularQueue.h"
#include "apps/voice/SpeechRecognitionObserver.h"
#include "apps/voice/JuliusXMLParser.h"
#include "apps/voice/JuliusNetworkClient.h"

#include <ace/Thread.h>
#include <tao/TAO_Internal.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>

#include <orbsvcs/CosNamingC.h>
#include <ves/open/moduleC.h>
#include <ves/open/VjObsC.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>

#include <string>

/**
 * This class mediates between the Speech Recognition network client and the
 * results parser.  It will then translate the results into navigation commands
 * and send the commands to VE-Xplorer.
 */

class SpeechNavigator
{
public:

    /**
     * Default Ctor
     */
    SpeechNavigator();

    ~SpeechNavigator();

    /**
     * Connects the network client to the Julius Daemon.
     *
     * @param   host        the host to connect to.
     * @param   port        the port to connect to.
     *
     * @return     true if successful, false otherwise.
     *
     * @post    the Julius Network Client is connected to the remote host.
     */
    bool connectToJulius(const std::string& host, 
                        const unsigned short port=10500);

    /**
     * Connects the speech navigator to VE-Xplorer.
     *
     * @return     true if successful, false otherwise.
     */
    bool connectToXplorer();

    /**
     * Checks to see if the ORB is connected to the naming service.
     * If it is not connected, this function will attempt to connect.
     *
     * @return      true if successful, false otherwise.
     */
    bool isConnectedToNamingService();

    /**
     * Connects to the CORBA Naming Service.
     *
     * @return      true if successful, false otherwise.
     */
    bool connectToNamingService();

    /**
     * This function will spawn a new thread for the speech parsing
     * (if necessary) and have it run.
     *
     * @pre     connectToJulius() has been successfully called.
     *
     * @return      true if successful, false otherwise.
     */
    bool startParserThread();

    /**
     * Returns true if the parser thread is running, false if it is
     * suspended (or hasn't been created).
     */
    bool isParserThreadRunning() const;

    /**
     * Stops the execution of the parser thread if it is running
     *
     * @note        the parser thread will have to be started again with
     *              start parser thread after this, which will create a new
     *              thread.  This function is NOT a thread suspend operation.
     */
    void stopParserThread();

    /**
     * Runs one iteration of the data loop and returns.
     *
     * @pre     connectToJulius() and connectToXplorer() have been called.
     *
     * @return      true if successful, false if an error occurred.
     */
    bool runDataIteration();

    /**
     * Starts the data loop.  This call will block indefinitely until an
     * error occurs or stopDataLoop() is called.
     *
     * @pre     connectToJulius() and connectToXplorer() have been called.
     *
     * @return      false if an error occurred.
     */
    bool startDataLoop();

    /**
     * Stops the data loop.
     */
    void stopDataLoop()
    {
        mStop = true;
    }


    /**
     * Sets the local copies of argc and argv
     */
    void setArgcArgv(int argc, char** argv);

    /**
     * Responds to changes in the Phrase Parsing subject; the changes will
     * be notification of recognized parsing events.
     */
    void onPhraseRecognition(const std::string& phrase);

private:

    /// The Julius Network Client that this navigator is using.
    JuliusNetworkClient*                                  mClient;

    /// The Julius XML parser this navigator is using.
    JuliusXMLParserPtr                                    mParser;

    /// The Observer that responds to changes in the Julius XML Parser subject.
    SpeechRecognitionObserverPtr                          mParserObserver;

    /// The Circular Queue used by the Parser and the Xplorer threads for
    /// storing shared input and output.
    CircularQueue<std::string>                            mSpeechQueue;

    /// The ACE Group ID for the Parser and Xplorer Threads
    int                                                   mThreadGroupId;

    /// The Speech Client Parser Thread (the Producer).
    ACE_thread_t                                          mParserThread;

    /// Is it time to stop the data loop?
    bool                                                  mStop;

    /* Stuff used to connect to VE Xplorer */

    /// ???
    CosNaming::NamingContext_var                          mNamingContext;

    /// The ORB?
    CORBA::ORB_var                                        mOrb;
    
    /// A copy of the original argc passed to the main function.
    int                                                   mArgc;

    /// A copy of the original argv passed to the main function.
    char**                                                mArgv;

    /* These are unknowns. */
    //PortableServer::POA_var poa;
    //PortableServer::POA_var poa_root;
    VjObs_var vjobs;
};

#endif
// vim:ts=4:sw=4:et:tw=0

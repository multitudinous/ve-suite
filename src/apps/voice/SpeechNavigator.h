
#ifndef VE_VOICE_SPEECH_NAVIGATOR_H_
#define VE_VOICE_SPEECH_NAVIGATOR_H_

#include "apps/voice/SpeechRecognitionObserver.h"
#include "apps/voice/JuliusXMLParser.h"
#include "apps/voice/JuliusNetworkClient.h"

#include <tao/TAO_Internal.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>

#include <orbsvcs/CosNamingC.h>
#include <ves/open/moduleC.h>
#include <ves/open/VjObsC.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>

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
     * Sets the local copies of argc and argv
     */
    void setArgcArgv(int argc, char** argv);

    /**
     * Creates the CORBA Module?
     */
    void createCORBAModule();

private:

    /// The Julius Network Client that this navigator is using.
    JuliusNetworkClient*                                  mClient;

    /// The Julius XML parser this navigator is using.
    JuliusXMLParser*                                      mParser;

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

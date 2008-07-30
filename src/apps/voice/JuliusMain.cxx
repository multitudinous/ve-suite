
#include "SpeechNavigator.h"
#include "AceFunctorHandler.h"

#include <ace/Signal.h>
#include <ace/Version.h>
#if ACE_MAJOR_VERSION >= 5 && ACE_MINOR_VERSION >= 6 
#include <ace/Sig_Handler.h>
#endif

#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>

/**
 * This file contains the main() function for the Julius Speech Navigation
 * plugin for Xplorer.  It is necessary to run this application to make 
 * the CORBA connections to Xplorer and establish the socket communication with
 * the Julius Speech Engine.
 */

// Use a global variables for communication via the signal handlers and the
// main() function.
static SpeechNavigator* gSpeechNavigator = 0;

/**
 * Prints the usage to std::err and exits.
 */
void usage()
{
    std::cerr << "JuliusSpeechNavigator julius_host julius_port CORBA_ARGS"
              << std::endl;
}

/**
 * Cleans up after the global speech navigator.
 */
void cleanup()
{
    if (gSpeechNavigator)
    {
        delete gSpeechNavigator;
    }
}

/**
 * Handles a SIGINT; it stops the parser thread and the data loop.
 */
int on_sigint(int signum, siginfo_t* si, ucontext_t* ut)
{
    std::cout << "[DBG] Entering SIGINT handler." << std::endl;
    gSpeechNavigator->stopParserThread();
    gSpeechNavigator->stopDataLoop();
    return 0;
}

/**
 * Handles a SIGUSR1; it will stop the parser thread if it is running.
 * This allows an external application to send a SIGUSR1 to stop this
 * app from sending speech recognition results.
 */
int on_sigusr1(int signum, siginfo_t* si, ucontext_t* ut)
{
    std::cout << "[DBG] Entering SIGUSR1 handler." << std::endl;
    if (gSpeechNavigator && gSpeechNavigator->isParserThreadRunning())
    {
        gSpeechNavigator->stopParserThread();
    }
    return 0;
}

/**
 * Handles a SIGUSR2; it will start the parser thread if it is not running.
 * This allows an external application that previously sent a SIGUSR1 to stop
 * the speech recognition to resume speech recogntion.
 */
int on_sigusr2(int signum, siginfo_t* si, ucontext_t* ut)
{
    std::cout << "[DBG] Entering SIGUSR2 handler." << std::endl;
    if (gSpeechNavigator && !gSpeechNavigator->isParserThreadRunning())
    {
        gSpeechNavigator->startParserThread();
    }
    return 0;
}

int main(int argc, char* argv[])
{
    if (argc < 4)
    {
        usage();
        return 1;
    }
    atexit(cleanup);
    std::string julius_host = argv[1];
    unsigned short port = atoi(argv[2]);
    SpeechNavigator* gSpeechNavigator = new SpeechNavigator();
    // TODO:  Create signal handlers.
    ACE_Sig_Handler signal_handler;
    // Create the SIGINT handlers.
    AceFunctorHandler sigint_handler;
    AceFunctorHandler::SignalHandler sih(on_sigint);
    sigint_handler.setSignalHandler(sih);
    // Create the SIGUSR1 handlers.
    AceFunctorHandler sigusr1_handler;
    AceFunctorHandler::SignalHandler su1(on_sigusr1);
    sigusr1_handler.setSignalHandler(su1);
    // Create the SIGUSR2 handlers.
    AceFunctorHandler sigusr2_handler;
    AceFunctorHandler::SignalHandler su2(on_sigusr2);
    sigusr2_handler.setSignalHandler(su2);
    // Register all handlers with ACE.
    signal_handler.register_handler(SIGINT, &sigint_handler); 
    signal_handler.register_handler(SIGUSR1, &sigusr1_handler);
    signal_handler.register_handler(SIGUSR2, &sigusr2_handler);
     
    gSpeechNavigator->setArgcArgv(argc, argv);
    std::cout << "Connecting to Julius on " << julius_host << ":"
              << port << "..." << std::endl;
    if (!gSpeechNavigator->connectToJulius(std::string("localhost")))
    {
        std::cerr << "[ERR] Unable to connect to julius." << std::endl;
        return 2;
    }
    std::cout << "Connected." << std::endl;
    std::cout << "Connecting to Xplorer..." << std::endl;
    if (!gSpeechNavigator->connectToXplorer())
    {
        std::cerr << "[ERR] Unable to connect to Xplorer." << std::endl;
        return 3;
    }
    std::cout << "Connected." << std::endl;
    std::cout << "Starting the data loop..." << std::endl;
    // Start the data loop.
    if (!gSpeechNavigator->startDataLoop())
    {
        std::cerr << "[ERR] Unable to start the data loop." << std::endl;
        return 4;
    }

    delete gSpeechNavigator;
    return 0;
}

// vim:ts=4:sw=4:et:tw=0

// This block of includes is only required if you intend to use EventManager's
// logging facilities.
#include <Poco/Logger.h>
#include <Poco/ConsoleChannel.h>
#include <Poco/PatternFormatter.h>
#include <Poco/FormattingChannel.h>

// The minimal set of headers required to register a signal and connect a slot
// to it.
#include <eventmanager/ConnectSignals.h>

// If you're just registering a signal but not connecting any slots in that
// compilation unit, you only need <eventmanager/EventManager.h>


#include "slotinclass.h"

void prt( int num )
{
    std::cout << "prt: argument = " << num << std::endl;
}

int main()
{
    // Set up a logger so we can see log messages from the Eventmanager library.
    // This is not required in a program using this library, but it can be very
    // useful for debugging event/slot connections during development. Here we
    // set the log level to "information". For more thorough debugging,
    // recompile eventmanager library with EVENTMANAGER_DEBUG defined, and set
    // the log level to either "debug" or "trace".
    Poco::Logger::root().setLevel("information");
    Poco::ConsoleChannel* console = new Poco::ConsoleChannel;
    Poco::PatternFormatter* formatter = new Poco::PatternFormatter;
    formatter->setProperty("pattern", "%s: %t");
    Poco::FormattingChannel* formattingChannel =
            new Poco::FormattingChannel( formatter, console );
    Poco::Logger::root().setChannel( formattingChannel );




    // Declare an event (usually done as a class member variable)
    eventmanager::Event< void (int) > intSignal;
    
    // Register the event with EventManager
    eventmanager::EventManager::instance()->
      RegisterSignal( &intSignal , "ATestEvent" );
         
    // Create a ScopedConnectionList, which is required for connecting to an
    // event. This is frequently a class member variable also.
    eventmanager::ScopedConnectionList connections;
         
    // Request a connection between "ATestEvent" and function prt. This shows
    // how to connect to static functions or other functions which are not
    // members of a class. Notice the word "Static" in the function name.
    eventmanager::ConnectSignalsStatic<void (int)>(
                    "ATestEvent",
                    &prt,
                    connections,
                    eventmanager::EventManager::any_SignalType,
                    eventmanager::EventManager::normal_Priority );

    // Instantiate a class with a valid slot that connects to "ATestEvent". Look
    // at the class's code to see how this works. Most applications will connect
    // to events from inside classes far more often than in global or static
    // functions like above.
    SlotInClass mySlotClass;

    // We could even instantiate a few of these, and each one will be called
    // in turn. Uncomment to see the behavior.
    //SlotInClass msc2;
    //SlotInClass msc3;
                            
    // Fire the signal several times
    intSignal.signal( 1 );
    intSignal.signal( 0 );
    intSignal.signal( 5 );
    intSignal.signal( 4 );
    intSignal.signal( 5 );

#if 0
    // Expired event test
    {
        eventmanager::Event< void (int) > intSignal2;
        eventmanager::EventManager::instance()->
                RegisterSignal( &intSignal2 , "ExpiredSignal" );
    }
    // intSignal2 has now gone out of scope, so we should fail to connect
    // and the event should be scrubbed from EventManager's lists. Info
    // will show up in the log. This also demonstrates the optional macro
    // version of connecting to an event, which is useful if you really dislike
    // templates. Use of the macro is not critical to the example of expired
    // signals, however.
#include <eventmanager/OptionalMacros.h>
    CONNECTSIGNAL_STATIC( "ExpiredSignal",
                            void( int ),
                            &prt,
                            connections,
                            normal_Priority );
#endif


    delete formatter;

    return 0;
}

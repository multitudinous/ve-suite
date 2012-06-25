
#include <eventmanager/EventManager.h>
#include <eventmanager/ScopedConnectionList.h>
#include <eventmanager/Event.h>
#include <Poco/Logger.h>
#include <Poco/ConsoleChannel.h>
#include <Poco/PatternFormatter.h>
#include <Poco/FormattingChannel.h>

void prt( int num )
{
    std::cout << "Signal argument = " << num << std::endl;
}

int main()
{
    Poco::Logger::root().setLevel("information");
    Poco::ConsoleChannel* console = new Poco::ConsoleChannel;
    Poco::PatternFormatter* formatter = new Poco::PatternFormatter;
    formatter->setProperty("pattern", "%s: %t");
    Poco::FormattingChannel* formattingChannel =
            new Poco::FormattingChannel( formatter, console );
    Poco::Logger::root().setChannel( formattingChannel );


    // Declare a signal (usually done as a class member variable)
    eventmanager::Event< void (int) > intSignal;
    
    // Register the signal with EventManager
    eventmanager::EventManager::instance()->
      RegisterSignal(
      new eventmanager::SignalWrapper< void (int) >
        ( &intSignal ),
         "AnIntSignalThatSendsAnInt" );
         
    // Create ScopedConnectionList, which is required for connecting to a
    // signal
    eventmanager::ScopedConnectionList connections;
         
    // Request a connection between "AnIntSignalThatSendsAnInt" and function
    // prt
    CONNECTSIGNAL_STATIC( "AnIntSignalThatSendsAnInt", 
                            void( int ), 
                            &prt,
                            connections,
                            normal_Priority );
                            
    // Fire the signal several times
    intSignal.signal( 1 );
    intSignal.signal( 0 );
    intSignal.signal( 5 );
    intSignal.signal( 4 );
    intSignal.signal( 5 );

#if 1
    // Expired event test
    {        eventmanager::Event< void (int) > intSignal2;

        // Register the signal with EventManager
        eventmanager::EventManager::instance()->
          RegisterSignal(
          new eventmanager::SignalWrapper< void (int) >
            ( &intSignal2 ),
             "ExpiredSignal" );
    }
    // intSignal2 has now gone out of scope, so we should fail to connect
    // and the signal should be scrubbed from EventManager's lists. Info
    // will show up in the log.
    CONNECTSIGNAL_STATIC( "ExpiredSignal",
                            void( int ),
                            &prt,
                            connections,
                            normal_Priority );
#endif

    return 0;
}
    


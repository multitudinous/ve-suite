
#include <eventmanager/EventManager.h>
#include <eventmanager/ScopedConnectionList.h>
#include <boost/signals2/signal.hpp>

void prt( int num )
{
    std::cout << "Signal argument = " << num << std::endl;
}

int main()
{
    // Declare a signal (usually done as a class member variable)
    boost::signals2::signal<void (int)> intSignal;
    
    // Register the signal with EventManager
    eventmanager::EventManager::instance()->
      RegisterSignal( 
      new eventmanager::SignalWrapper<boost::signals2::signal< void (int) > >
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
    intSignal( 1 );
    intSignal( 0 );
    intSignal( 5 );
    intSignal( 4 );
    intSignal( 5 );

    return 0;
}
    


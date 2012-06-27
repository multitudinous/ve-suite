#pragma once
#include <iostream>
#include <eventmanager/ConnectSignals.h>
#include <eventmanager/ScopedConnectionList.h>

class SlotInClass
{
public:
    SlotInClass()
    {
        // Connect mySlot to the event with signature void(int) and name
        // "ATestEvent". Notice that we do not need any information about the
        // object that owns "ATestEvent". All we need to know is the name of the
        // event and its signature.

        eventmanager::ConnectSignals_1< void (int), SlotInClass >(
                    "ATestEvent",
                    &SlotInClass::mySlot,
                    this,
                    m_connections,
                    eventmanager::EventManager::any_SignalType,
                    eventmanager::EventManager::normal_Priority );

        // Key points to notice in the above call:
        // * The template function name includes the number of arguments the
        //   event has; in this case, one.
        // * The template parameters specify the signature of the event you want
        //   and the type of the class which owns the slot
        // * The second function argument, &SlotInClass::mySlot, does not have
        //   parentheses after it! We want a pointer to the method, not a
        //   method call.
    }

    void mySlot( int arg )
    {
        std::cout << "SlotInClass::mySlot: argument = " << arg << std::endl;
    }

    eventmanager::ScopedConnectionList m_connections;
};


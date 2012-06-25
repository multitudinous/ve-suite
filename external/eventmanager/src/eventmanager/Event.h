#pragma once

#include<boost/signals2/signal.hpp>
#include<boost/shared_ptr.hpp>
#include<boost/weak_ptr.hpp>
#include <eventmanager/EventBase.h>

namespace eventmanager
{

template <typename T>
class Event : public EventBase
{
public:
    Event( )
    {
    }

    virtual ~Event()
    {
    }

    boost::signals2::signal<T> signal;
};

}


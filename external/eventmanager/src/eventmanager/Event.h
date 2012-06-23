#include<boost/signals2/signal.hpp>
#include<boost/shared_ptr.hpp>
#include<boost/weak_ptr.hpp>
#include <eventmanager/Exports.h>



namespace eventmanager
{


class EVENTMANAGER_EXPORT EventBase
{
public:
    EventBase(){}
    virtual ~EventBase(){}

    /// Returns a weak pointer to this object without the need to create a
    /// shared ptr first.
    boost::weak_ptr< EventBase > GetWeakPtr() const { return this_; }

private:
    boost::shared_ptr< EventBase > this_;

};

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

    void operator()()
    {
        signal();
    }

    boost::signals2::signal<T> signal;
};

}


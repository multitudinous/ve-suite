#pragma once

#include<boost/signals2/signal.hpp>
#include <eventmanager/Exports.h>
#include <eventmanager/SlotWrapperBase.h>
#include <eventmanager/ScopedConnectionList.h>

namespace eventmanager
{
struct null_deleter
{
    void operator()( void* ) {}
};
/// @file EventBase.h
/// @namespace eventmanager
/** @class EventBase
  * Base class for templated Event<> class. Mainly for internal use in the
  * eventmanager library. Applications generally should not need to use this
  * class explicitly.
  **/
class EVENTMANAGER_EXPORT EventBase
{
public:
    EventBase(): this_(this, null_deleter()){}
    EventBase( const EventBase& ): this_(this, null_deleter()){}
    EventBase & operator=(EventBase const & rhs){}
    virtual ~EventBase(){}

    /// Returns a weak pointer to this object without the need to create a
    /// shared ptr first.
    boost::weak_ptr< EventBase > GetWeakPtr() { return this_; }

    /// Connects slot held in SlotWrapper to the signal owned by derived Event<>
    /// instance.
    /// @param slot Pointer to SlotWrapper<> that holds the slot
    /// @param connections Reference to a ScopedConnectionList object that is
    /// responsible for lifetime management of the resulting connnection
    /// @param priority The priority with which slot should be connected to this
    /// signal. This priority is a raw integer rather than one of the enumerated
    /// priorities from EventManager so that this class need not depend on
    /// EventManager.h
    virtual bool ConnectSlot( SlotWrapperBase* slot,
                              ScopedConnectionList& connections,
                              int priority ) = 0;

    /// Returns the list of scoped_connection objects referring to all slots
    /// connected to this signal. Note that the list holds boost::weak_ptrS to
    /// the scoped_connectionS. This list functions in the primary role of
    /// observer of the connections rather than the owner. The class holding the
    /// connected slot typically owns the connection.
    std::list< boost::weak_ptr< boost::signals2::scoped_connection > >& GetConnections()
    {
        return mConnections;
    }

    /// Returns a pointer cast as an int to the underlying signal. This is
    /// intended for debugging purposes to allow the signal's address to be printed
    /// out.
    virtual long unsigned int GetSignalAddress() = 0;


protected:
    std::list< boost::weak_ptr< boost::signals2::scoped_connection > > mConnections;

private:
    boost::shared_ptr< EventBase > this_;

};

} // namespace eventmanager

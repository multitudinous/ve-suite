#pragma once

// Macros to make connecting signals an easier process. Unless you really
// dislike templated code, you should use the template functions in
// ConnectSignals.h
// The first part below contains sub-macros that are used to build up the ones
// that are actually of interest. Those are the ones that are named
// CONNECTSIGNAL_0, CONNECTSIGNALS_0, etc.
//
// To add more macros for making connections that require N arguments,
// add a new CONNECT____N macro, adding the appropriate number of parameters
// to the boost::bind call and then add both a CONNECTSIGNAL_N and a
// CONNECTSIGNALS_N macro that use the CONNECT____N macro.

#define CONNECTSIGNALPRE( signature ) do{\
        typedef boost::signals2::signal< signature > sig_type; \
        sig_type::slot_type* slotFunctor = new sig_type::slot_type(

#define CONNECTSIGNALPOST  ); \
                eventmanager::SlotWrapper< sig_type >* slotWrapper = new eventmanager::SlotWrapper< sig_type >( slotFunctor );

#define CONNECTSIGNALCALL( name, connections, priority ) \
        eventmanager::EventManager::instance()->ConnectSignal( name, slotWrapper, \
            connections, eventmanager::EventManager::priority ); \
        }while(0)

#define CONNECTSIGNALSCALL( name, connections, type, priority ) \
        eventmanager::EventManager::instance()->ConnectSignals( name, slotWrapper, \
            connections, eventmanager::EventManager::type, eventmanager::EventManager::priority ); \
        }while(0)

#define CONNECT____0( signature, slot ) CONNECTSIGNALPRE( signature ) boost::bind( slot, boost::ref( *this ) ) CONNECTSIGNALPOST
#define CONNECT____1( signature, slot ) CONNECTSIGNALPRE( signature ) boost::bind( slot, boost::ref( *this ), _1 ) CONNECTSIGNALPOST
#define CONNECT____2( signature, slot ) CONNECTSIGNALPRE( signature ) boost::bind( slot, boost::ref( *this ), _1, _2 ) CONNECTSIGNALPOST
#define CONNECT____3( signature, slot ) CONNECTSIGNALPRE( signature ) boost::bind( slot, boost::ref( *this ), _1, _2, _3 ) CONNECTSIGNALPOST
#define CONNECT____4( signature, slot ) CONNECTSIGNALPRE( signature ) boost::bind( slot, boost::ref( *this ), _1, _2, _3, _4 ) CONNECTSIGNALPOST
#define CONNECT____5( signature, slot ) CONNECTSIGNALPRE( signature ) boost::bind( slot, boost::ref( *this ), _1, _2, _3, _4, _5 ) CONNECTSIGNALPOST

// Connects to a single, exactly-named signal (no patterns allowed). Does not
// require an EventManager::SignalType to be passed since no pattern matching
// is done.
#define CONNECTSIGNAL_0( name, signature, slot, connections, priority ) \
            CONNECT____0( signature, slot ) CONNECTSIGNALCALL( name, connections, priority )

#define CONNECTSIGNAL_1( name, signature, slot, connections, priority ) \
            CONNECT____1( signature, slot ) CONNECTSIGNALCALL( name, connections, priority )

#define CONNECTSIGNAL_2( name, signature, slot, connections, priority ) \
            CONNECT____2( signature, slot ) CONNECTSIGNALCALL( name, connections, priority )

#define CONNECTSIGNAL_3( name, signature, slot, connections, priority ) \
            CONNECT____3( signature, slot ) CONNECTSIGNALCALL( name, connections, priority )

#define CONNECTSIGNAL_4( name, signature, slot, connections, priority ) \
            CONNECT____4( signature, slot ) CONNECTSIGNALCALL( name, connections, priority )

#define CONNECTSIGNAL_5( name, signature, slot, connections, priority ) \
            CONNECT____5( signature, slot ) CONNECTSIGNALCALL( name, connections, priority )


// Uses pattern matching to connect to one (or more) signals with names matching
// the name argument. In contrast to the CONNECTSIGNAL_X (No "S" before "_"),
// these macros require an EventManager::SignalType to be passed as well.
#define CONNECTSIGNALS_0( name, signature, slot, connections, type, priority ) \
            CONNECT____0( signature, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_1( name, signature, slot, connections, type, priority ) \
            CONNECT____1( signature, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_2( name, signature, slot, connections, type, priority ) \
            CONNECT____2( signature, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_3( name, signature, slot, connections, type, priority ) \
            CONNECT____3( signature, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_4( name, signature, slot, connections, type, priority ) \
            CONNECT____4( signature, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_5( name, signature, slot, connections, type, priority ) \
            CONNECT____5( signature, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )


// This block takes an extra parameter: a custom combiner. This version must
// be used if the signal to which you are trying to connect uses a custom
// combiner.
#define CONNECTSIGNALPRE_COMBINER( signature, combiner ) do{\
        typedef boost::signals2::signal< signature, combiner > sig_type; \
        sig_type::slot_type* slotFunctor = new sig_type::slot_type(

#define CONNECT____0_COMBINER( signature, combiner, slot ) \
                CONNECTSIGNALPRE_COMBINER( signature, combiner ) \
                boost::bind( slot, boost::ref( *this ) ) CONNECTSIGNALPOST

#define CONNECT____1_COMBINER( signature, combiner, slot ) \
                CONNECTSIGNALPRE_COMBINER( signature, combiner ) \
                boost::bind( slot, boost::ref( *this ), _1 ) CONNECTSIGNALPOST

#define CONNECT____2_COMBINER( signature, combiner, slot ) \
                CONNECTSIGNALPRE_COMBINER( signature, combiner ) \
                boost::bind( slot, boost::ref( *this ), _1, _2 ) CONNECTSIGNALPOST

#define CONNECT____3_COMBINER( signature, combiner, slot ) \
                CONNECTSIGNALPRE_COMBINER( signature, combiner ) \
                boost::bind( slot, boost::ref( *this ), _1, _2, _3 ) CONNECTSIGNALPOST

#define CONNECT____4_COMBINER( signature, combiner, slot ) \
                CONNECTSIGNALPRE_COMBINER( signature, combiner ) \
                boost::bind( slot, boost::ref( *this ), _1, _2, _3, _4 ) CONNECTSIGNALPOST

#define CONNECT____5_COMBINER( signature, combiner, slot ) \
                CONNECTSIGNALPRE_COMBINER( signature, combiner ) \
                boost::bind( slot, boost::ref( *this ), _1, _2, _3, _4, _5 ) CONNECTSIGNALPOST

#define CONNECTSIGNALS_0_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECT____0_COMBINER( signature, combiner, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_1_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECT____1_COMBINER( signature, combiner, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_2_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECT____2_COMBINER( signature, combiner, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_3_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECT____3_COMBINER( signature, combiner, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_4_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECT____4_COMBINER( signature, combiner, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNALS_5_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECT____5_COMBINER( signature, combiner, slot ) CONNECTSIGNALSCALL( name, connections, type, priority )

/// The CONNECTSIGNAL_STATIC and CONNECTSIGNALS_STATIC macros are to be used
/// to connect signals to slots that do not reside in a class. In most cases,
/// such slots will be static functions declared in a header. Internally, the
/// difference between these two macros and the CONNECTSIGNAL(S)_N macros
/// is that the _STATIC versions do not use boost::bind to make a functor from
/// a class method. The _COMBINER version accepts a custom combiner, which is
/// required if the signal to which you are trying to connect uses a custom
/// combiner.
#define CONNECTSIGNAL_STATIC( name, signature, slot, connections, priority ) \
            CONNECTSIGNALPRE( signature ) slot CONNECTSIGNALPOST CONNECTSIGNALCALL( name, connections, priority )

#define CONNECTSIGNALS_STATIC( name, signature, slot, connections, type, priority ) \
            CONNECTSIGNALPRE( signature ) slot CONNECTSIGNALPOST CONNECTSIGNALSCALL( name, connections, type, priority )

#define CONNECTSIGNAL_STATIC_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECTSIGNALPRE_COMBINER( signature, combiner ) slot CONNECTSIGNALPOST CONNECTSIGNALCALL( name, connections, type, priority )

#define CONNECTSIGNALS_STATIC_COMBINER( name, signature, combiner, slot, connections, type, priority ) \
            CONNECTSIGNALPRE_COMBINER( signature, combiner ) slot CONNECTSIGNALPOST CONNECTSIGNALSCALL( name, connections, type, priority )

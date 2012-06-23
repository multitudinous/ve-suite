/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <boost/signals2/signal.hpp>
#include <boost/mem_fn.hpp>

/*
 * Notes:
 *
 * C++ doesn't (yet) allow defaults for template parameters, so there are two
 * overloaded versions of each of these: one that requires a combiner as a
 * template parameter, and one that doesn't. If you add more versions to deal
 * with extra function arguments, be sure to add two versions: one that takes
 * a combiner and one that doesn't.
 *
 * Q: Why are there N versions of this functor, instead of a
 * (combiner,non-combiner) pair that take a variable indicating the number
 * of slot arguments? Then you could do a switch statement and build the correct
 * slotFunctor.
 * A: You can't do that because each case in the switch statement has to be
 * compilable at compile-time. Since there will be a mis-match in all but one
 * of the cases, all the other cases won't compile correctly.
 */

namespace eventmanager
{
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class R,
          class T>
void ConnectSignals_0( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class R,
          class T>
void ConnectSignals_0( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class R,
          class T>
void ConnectSignals_1( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class R,
          class T>
void ConnectSignals_1( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class R,
          class T>
void ConnectSignals_2( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class R,
          class T>
void ConnectSignals_2( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class R,
          class T>
void ConnectSignals_3( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2, _3 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class R,
          class T>
void ConnectSignals_3( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2, _3 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class R,
          class T>
void ConnectSignals_4( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2, _3, _4 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class R,
          class T>
void ConnectSignals_4( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2, _3, _4 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class R,
          class T>
void ConnectSignals_5( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2, _3, _4, _5 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class R,
          class T>
void ConnectSignals_5( const std::string& name,
                       R (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr, _1, _2, _3, _4, _5 ) );

    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );

    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
}


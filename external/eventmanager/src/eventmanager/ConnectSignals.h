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
#pragma once
#include <boost/signals2/signal.hpp>
#include <boost/mem_fn.hpp>
#include <eventmanager/ScopedConnectionList.h>
#include <eventmanager/EventManager.h>

/*
 * Notes:
 *
 * C++ doesn't (yet) allow defaults for template parameters, so there are two
 * versions of each of these: one that requires a combiner as a template
 * parameter, and one that doesn't. If you add more versions to deal
 * with extra function arguments, be sure to add two versions: one that takes
 * a combiner and one that doesn't. C++0X should remove this limiation.
 *
 * Q: Why are there N versions of this functor, instead of a
 * (combiner,non-combiner) pair that take a variable indicating the number
 * of slot arguments? Then you could do a switch statement and build the correct
 * slotFunctor.
 * A: You can't do that because each case in the switch statement has to be
 * compilable at compile-time. Since there will be a mis-match for number
 * of arguments to boost::bind in all but one of the cases, none of the other
 * cases will compile. Variadic templates should solve this issue in C++0X and
 * allow much of this unnecessary code duplication to go away.
 *
 * When C++0X support is standard, this header should reduce to two function
 * templates: one for static slots and one for class member slots.
 */

/// @file ConnectSignals.h
/// @namespace eventmanager
namespace eventmanager
{
////////////////////////////////////////////////////////////////////////////////
/**
  * Hides all the messy parts of requesting a connection to a signal. This
  * version is for slots that are static or global functions, not class member
  * functions.
  * @tparam Signature The signature of the signal(s) to which you want to
  *                   connect. Example: @code void (int) @endcode
  *                   This should also match the signature of the connecting
  *                   slot.
  * @param name Name (or name pattern) of the signal(s) to which you want to
  *                  connect.
  * @param slotfunc Pointer to slot function. Example: @code &slot @endcode.
  *                 Note the absense of () after the function name.
  * @param connections A ScopedConnectionList object that will manage the
  *                    lifetime of the connection. Once this object goes out of
  *                    scope, the connections it holds are broken.
  * @param signalType The type of signal to which you want to connect. If in
  *                   doubt, use \c eventmanager::EventManager::any_SignalType.
  * @param priority The priority with which to connect to the signal. Use
  *                 \c eventmanager::EventManager::normal_Priority unless you
  *                 specifically need something else.
  */
template <typename Signature>
void ConnectSignalsStatic( const std::string& name,
                       Signature* slotfunc,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type( slotfunc );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
/**
  * Just like ConnectSignalsStatic<typename Signature>(...) but with the
  * addition of a combiner on the signal. Notice the combiner is a template
  * parameter, not a function argument. See BooleanPropagationCombiner.h
  * for an example combiner. See the boost::signals2 documentation for a
  * thorough discussion of combiners and their use.
  * @see ConnectSignalsStatic
  */
template <typename Signature,
          typename Combiner>
void ConnectSignalsCombinerStatic( const std::string& name,
                       Signature* slotfunc,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type( slotfunc );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
/**
  * Just like ConnectSignals_0<typename Signature, class T>(...) but with the
  * addition of a combiner on the signal. Notice the combiner is a template
  * parameter, not a function argument. See BooleanPropagationCombiner.h
  * for an example combiner. See the boost::signals2 documentation for a
  * thorough discussion of combiners and their use.
  * @see ConnectSignals_0
  */
template <typename Signature,
          typename Combiner,
          class T>
void ConnectSignalsCombiner_0( const std::string& name,
                       Signature (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ), parentPtr ) );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
/**
  * Hides all the messy parts of requesting a connection to a signal. This
  * version is for slots that class member functions. ConnectSignals_N will
  * allow you to connect slots that take N arguments to signals that provide
  * N arguments. ConnectSignals_0 through ConnectSignals_5 are defined here
  * for you.
  * @tparam Signature The signature of the signal(s) to which you want to
  *                   connect. Example: @code void (int) @endcode
  *                   This should also match the signature of the connecting
  *                   slot.
  * @tparam T The class type of which slotfunc is a method. Example: @c MyClass
  * @param name Name (or name pattern) of the signal(s) to which you want to
  *                  connect.
  * @param slotfunc Pointer to slot function. Example: @code &slot @endcode.
  *                 Note the absense of () after the function name.
  * @param parentPtr Raw pointer to class instance of type T which contains the
  *                  slot you want to connect to the signal.
  *                  Example: If @c mc is an instance of @c MyClass, you would
  *                  pass @c &mc for this argument.
  * @param connections A ScopedConnectionList object that will manage the
  *                    lifetime of the connection. Once this object goes out of
  *                    scope, the connections it holds are broken.
  * @param signalType The type of signal to which you want to connect. If in
  *                   doubt, use \c eventmanager::EventManager::any_SignalType.
  * @param priority The priority with which to connect to the signal. Use
  *                 \c eventmanager::EventManager::normal_Priority unless you
  *                 specifically need something else.
  *
  * @note If you need to deal with signals/slots with more than five parameters,
  *       just copy/paste and alter the boost::bind command in the function
  *       appropriately. If you go above seven arguments, you will likely need
  *       to look at the documentation of boost::signals2::signal to see how
  *       to make it support more arguments. If your application never uses
  *       signal/slots with N parameters, you can marginally speed up
  *       compilation time by commenting out ConnectSignals_N and
  *       ConnectSignalsCombiner_N.
  */
template <typename Signature,
          class T>
void ConnectSignals_0( const std::string& name,
                     Signature (T::*slotfunc),
                     T* parentPtr,
                     ScopedConnectionList& connections,
                     EventManager::SignalType signalType,
                     EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
                boost::bind( boost::mem_fn( slotfunc ), parentPtr ) );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class T>
void ConnectSignalsCombiner_1( const std::string& name,
                       Signature (T::*slotfunc),
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
          class T>
void ConnectSignals_1( const std::string& name,
                     Signature (T::*slotfunc),
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
          class T>
void ConnectSignalsCombiner_2( const std::string& name,
                       Signature (T::*slotfunc),
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
          class T>
void ConnectSignals_2( const std::string& name,
                     Signature (T::*slotfunc),
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
          class T>
void ConnectSignalsCombiner_3( const std::string& name,
                       Signature (T::*slotfunc),
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
          class T>
void ConnectSignals_3( const std::string& name,
                     Signature (T::*slotfunc),
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
          class T>
void ConnectSignalsCombiner_4( const std::string& name,
                       Signature (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ),
                         parentPtr, _1, _2, _3, _4 ) );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class T>
void ConnectSignals_4( const std::string& name,
                     Signature (T::*slotfunc),
                     T* parentPtr,
                     ScopedConnectionList& connections,
                     EventManager::SignalType signalType,
                     EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ),
                         parentPtr, _1, _2, _3, _4 ) );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          typename Combiner,
          class T>
void ConnectSignalsCombiner_5( const std::string& name,
                       Signature (T::*slotfunc),
                       T* parentPtr,
                       ScopedConnectionList& connections,
                       EventManager::SignalType signalType,
                       EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature, Combiner > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ),
                         parentPtr, _1, _2, _3, _4, _5 ) );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
template <typename Signature,
          class T>
void ConnectSignals_5( const std::string& name,
                     Signature (T::*slotfunc),
                     T* parentPtr,
                     ScopedConnectionList& connections,
                     EventManager::SignalType signalType,
                     EventManager::Priority priority )
{
    typedef typename boost::signals2::signal< Signature > signalT;
    typename signalT::slot_type* slotFunctor;
    slotFunctor = new typename signalT::slot_type(
            boost::bind( boost::mem_fn( slotfunc ),
                         parentPtr, _1, _2, _3, _4, _5 ) );
    SlotWrapper< signalT >* slotWrapper = new SlotWrapper< signalT >( slotFunctor );
    EventManager::instance()->ConnectSignals( name, slotWrapper,
            connections, signalType, priority );
}
////////////////////////////////////////////////////////////////////////////////
}


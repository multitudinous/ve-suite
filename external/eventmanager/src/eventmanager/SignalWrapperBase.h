/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include<boost/signals2/signal.hpp>

#include <eventmanager/SlotWrapperBase.h>


namespace eventmanager
{

/// Base class for templated SignalWrapper.
/// Existence of a base class allows disparate instances of SignalWrapper<> to be
/// passed into the same function without requiring multiple overloaded variations,
/// and also allows disparate instances of SignalWrapper<> to be held in a single
/// container.
class SignalWrapperBase
{
public:
    SignalWrapperBase(){}
    virtual ~SignalWrapperBase(){}

    /// Connects slot held in SlotWrapper to the signal owned by derived SignalWrapper<>
    /// instance.
    /// @param slot Pointer to SlotWrapper<> that holds the slot
    /// @param connections Reference to a ScopedConnectionList object that is
    /// responsible for lifetime management of the resulting connnection
    /// @param priority The priority with which slot should be connected to this
    /// signal. This priority is a raw integer rather than one of the enumerated
    /// priorities from EventManager so that this class need not depend on EventManager.h
    virtual bool ConnectSlot( SlotWrapperBase* slot, 
                              ScopedConnectionList& connections, 
                              int priority ) = 0;

    /// Returns the list of scoped_connection objects referring to all slots
    /// connected to this signal. Note that the list holds boost::weak_ptrS to
    /// the scoped_connectionS. This list functions in the primary role of
    /// observer of the connections rather than the owner. The class holding the connected
    /// slot typically owns the connection.
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

};

}


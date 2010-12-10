/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include <ves/xplorer/eventmanager/SlotWrapper.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>
#include <ves/xplorer/eventmanager/SignalWrapperBase.h>


namespace ves
{
namespace xplorer
{
namespace eventmanager
{

/**
 * SignalWrapper provides a wrapper around a boost::signals2::signal<> to allow
 * it to be passed into other functions without requiring multiple overloads to support
 * all instantiated template types: this is accomplished via its parent class, SignalWrapperBase.
 *
 * SignalWrapper is the intended entry point for connecting managed slots to signals via
 * the ConnectSlot method.
 **/
template <typename T>
class SignalWrapper : public SignalWrapperBase
{
public:

    /**
    * Constructs a SignalWrapper of type T.
    * @param signal Pointer to a boost::signals2::signal<T> object.
     * 
     * Example usage:
     * @code
     * // Create a signal that takes a double and an int and returns nothing.
     * boost::signals2::signal< void (double, int) > mySignal;
     * 
     * // Wrap it in an appropriate SignalWrapper. Notice the template parameter
     * // matches the type of mySignal exactly.
     * SignalWrapper< boost::signals2::signal< void (double, int) > > myWrapper( &mySignal );
     * @endcode
    **/
    SignalWrapper( T* signal ) :
    mSignal( signal )
    {
    }

    virtual ~SignalWrapper()
    {
    }

    virtual bool ConnectSlot( SlotWrapperBase* slot,
                              ScopedConnectionList& connections,
                              int priority )
    {
        SlotWrapper<T>* castSlot = dynamic_cast < SlotWrapper<T>* > ( slot );
        if ( castSlot )
        {
            boost::shared_ptr< boost::signals2::scoped_connection > connection( new boost::signals2::scoped_connection );
            *( connection.get() ) = mSignal->connect( priority, castSlot->GetSlot() );

            // If the connection was successful, we store the details of it
            if ( connection->connected() )
            {
                connections.AddConnection( connection );
                boost::weak_ptr< boost::signals2::scoped_connection > weakConnection( connection );
                mConnections.push_back( weakConnection );
                return true;
            }
            else
            {
                return false;
            }
        }
        else
        {
            return false;
        }
    }

    virtual long unsigned int GetSignalAddress()
    {
        return reinterpret_cast<long unsigned int>(mSignal);
    }


private:
    T* mSignal;

};

}
}
}

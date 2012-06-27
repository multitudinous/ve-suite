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

#include <eventmanager/SlotWrapperBase.h>
#include <stdio.h>


namespace eventmanager
{
/// @file SlotWrapper.h
/// @namespace eventmanager
/** @class SlotWrapper
 * SlotWrapper provides a wrapper around a slot, allowing slots with different
 * types (signatures) to be passed into the same function and to be held in
 * containers (via SignalWrapperBase). If you use the functions in
 * ConnectSignals.h to make connections between signals and slots (and we
 * highly reccommend this practice) you will never need to explicitly use
 * this class.
 *
 * SlotWrapper holds a reference to a boost::signals2::signal<T>::slot_type.
 **/
template <typename T>
class SlotWrapper : public SlotWrapperBase
{
public:

    /**
     * Constructs a SlotWrapper.
     * Notice that the template parameter and the type
     * passed in to the constructor differ slightly: the template parameter should
     * be a boost::signals2::signal<> type, whereas the argument to the constructor
     * is a boost::signals2::signal<>::slot_type&.
     *
     * For a usage example, please
     * see EventManager::ConnectSignal, and pay attention to the template
     * parameter when instantiating the wrapper.
     * @param slot A reference to the slot.
     * 
     * @see EventManager::ConnectSignal
     **/
    //SlotWrapper( const typename T::slot_type& slot ) :
    SlotWrapper( const typename T::slot_type* slot ) :
    mSlot( slot )
    {
        ;
    }

    SlotWrapper( const SlotWrapper& orig )
    {
        mSlot = orig.mSlot;
    }

    SlotWrapper& operator= (const SlotWrapper& orig)
    {
        mSlot = orig.mSlot;
        return *this;
    }

    virtual ~SlotWrapper( )
    {
        delete mSlot;
    }

    //const typename T::slot_type& GetSlot( )
    const typename T::slot_type& GetSlot( )
    {
        return (*mSlot);
    }
private:
    //const typename T::slot_type& mSlot;
    const typename T::slot_type* mSlot;

};

}


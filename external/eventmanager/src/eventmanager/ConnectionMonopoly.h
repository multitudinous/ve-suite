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

#include<boost/signals2/shared_connection_block.hpp>
#include<boost/shared_ptr.hpp>

#include <eventmanager/Exports.h>


namespace eventmanager
{
/// @file ConnectionMonopoly.h
/// @namespace eventmanager
/** @class ConnectionMonopoly
 * A shared_ptr to a ConnectionMonopoly is returned by calls to either of the variants of
 *  EventManager::MonopolizeConnection[] and holds a list of shared_connection_block objects.
 * These shared_connection_blocks are scoped objects that prevent other slots
 * from receiving a signal. As long as the ConnectionMonopoly stays in
 * scope, the monopoly stays in effect. When the ConnectionMonopoly goes out of scope,
 * the monopoly is automatically ended, and associated memory cleaned up.
 *
 * @warning It is essential that the ConnectionMonoply held by an object be 
 * destroyed when the class is destroyed, otherwise the monopoly will never end.
 * The reccommended practice is to hold a boost::shared_ptr to a
 * ConnectionMonopoly as a class member, as this will ensure automatic destruction
 * of the monopoly when the owning class is destroyed. EventManager attempts to
 * enforce this behavior by returning a shared_ptr to a ConnectionMonopoly rather
 * than a raw ConnectionMonopoly.
 */
class EVENTMANAGER_EXPORT ConnectionMonopoly
{
public:
    ///
    /// Constructor
    ConnectionMonopoly( );

    ///
    /// Destructor
    virtual ~ConnectionMonopoly( );

    /**
     * Add a new shared_connection_block to the monopoly.
     * @param blocker Shared pointer to the shared_connection_block to add. Shared_ptr is
     * used so that observing weak_ptrS may be held elsewhere.
     */
    void AddBlocker( boost::shared_ptr< boost::signals2::shared_connection_block > blocker );

private:
    std::vector< boost::shared_ptr< boost::signals2::shared_connection_block > > mBlockers;

};

}


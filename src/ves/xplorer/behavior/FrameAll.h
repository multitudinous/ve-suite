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

#include <ves/VEConfig.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <gadget/Type/PositionInterface.h>

namespace ves
{
namespace xplorer
{
namespace behavior
{

/**
  * Moves the view so that the entire bounding volume of the scene is visible.
  * This class works by listening for the FrameAll signal, which is guaranteed
  * to be emitted during LatePreFrameUpdate, when it is safe to touch the
  * scenegraph.
**/
class VE_XPLORER_EXPORTS FrameAll
{
public:
    /// ctor
    FrameAll();

    /// dtor
    ~FrameAll();


private:
    /// Execute the frame all functionality
    void DoFrameAll();

    /// VRJuggler's head positional interface
    gadget::PositionInterface mHead;

    /// Required connections list for connecting to events via EventManager
    ves::xplorer::eventmanager::ScopedConnectionList mConnections;
};

}
}
}

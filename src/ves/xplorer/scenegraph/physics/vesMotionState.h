/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef VES_MOTION_STATE_H
#define VES_MOTION_STATE_H

/*!\file vesMotionState.h
 *
 */

/*!\class ves::xplorer::scenegraph::vesMotionState
 *  vesMotionState provides a common implementation to synchronize world transforms with offsets
 */

/*!\namespace ves::xplorer::scenegraph
 *
 */

// --- VE-Suite Stuff --- //
#include <ves/VEConfig.h>

// --- Bullet Includes --- //
#include <LinearMath/btTransform.h>
#include <LinearMath/btMotionState.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
struct vesMotionState : public btMotionState
{
    btTransform m_graphicsWorldTrans;
    btTransform m_centerOfMassOffset;
    btTransform m_startWorldTrans;
    void* m_userPointer;

    vesMotionState( const btTransform& startTrans = btTransform::getIdentity(),
                    const btTransform& centerOfMassOffset = btTransform::getIdentity() )
            :
            m_graphicsWorldTrans( startTrans ),
            m_centerOfMassOffset( centerOfMassOffset ),
            m_startWorldTrans( startTrans ),
            m_userPointer( 0 )
    {
        ;
    }

    //Synchronizes world transform from user to physics
    virtual void getWorldTransform( btTransform& worldTrans ) const
    {
        worldTrans = m_centerOfMassOffset.inverse() * m_graphicsWorldTrans;
    }

    //Synchronizes world transform from physics to user
    //Bullet only calls the update of worldtransform for active objects
    virtual void setWorldTransform( const btTransform& worldTrans )
    {
        m_graphicsWorldTrans = worldTrans * m_centerOfMassOffset;
    }

};
}
}
}

#endif //VES_MOTION_STATE_H

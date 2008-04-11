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

#ifndef BLOCK_ENTITY_H
#define BLOCK_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- Bullet Includes --- //
class btGeneric6DofConstraint;

namespace bots
{
// --- My Includes --- //
class Block;

class BlockEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    BlockEntity(
        osg::ref_ptr< bots::Block > block,
        ves::xplorer::scenegraph::DCS* pluginDCS,
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~BlockEntity();

    bots::Block* GetGeometry();

    //Get the block's occupancy matrix
    std::map< std::pair< int, int >, bool > GetOccMatrix();

    void SetNameAndDescriptions( int number );

    void SetConstraints( int gridSize );

    //Set the block's occupancy matrix
    void SetOccMatrix( std::map< std::pair< int, int >, bool > occMatrix );

    void UpdateSideStates();

private:
    osg::ref_ptr< bots::Block > mBlock;

    btGeneric6DofConstraint* mConstraint;

    //Blocks have a copy of the occupancy matrix
    std::map< std::pair< int, int >, bool > mOccMatrix;
    //Blocks store location info for shared coordinate system
    std::pair< int, int > mLocation;
    //Are blocks attatched to sides or not
            //[0]-F
    //[1]-L         //[3]-R
            //[2]-N
    bool mSideState[ 4 ];

};
} //end bots

#endif //BLOCK_ENTITY_H

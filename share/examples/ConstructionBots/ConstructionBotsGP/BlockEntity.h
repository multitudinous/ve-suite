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

#include <osgUtil/LineSegmentIntersector>

namespace osg
{
class Drawable;
}

// --- Bullet Includes --- //
class btGeneric6DofConstraint;

// --- C/C++ Libraries --- //
#include <map>
#include <string>

namespace bots
{
// --- My Includes --- //
class Block;

class BlockEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    BlockEntity(
        bots::Block* block,
        ves::xplorer::scenegraph::DCS* pluginDCS,
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~BlockEntity();

    //
    void AttachUpdate();
    //
    void UpdateSideStates();
    //
    bool PermissionToAttach( osg::Drawable* drawable );

    //
    bots::Block* GetBlockGeometry();
    //
    const std::pair< int, int >& GetLocation();
    //Get the block's occupancy matrix
    const std::map< std::pair< int, int >, bool >& GetOccupancyMatrix();

    //
    void SetBlockConnection(
        unsigned int side, bots::BlockEntity* blockEntity );
    //
    void SetBlockEntityMap(
        const std::map< std::string, bots::BlockEntity* >& blockEntityMap );
    //
    void SetConstraints( int gridSize );
    //
    void SetNameAndDescriptions( int number );
    //Set the block's occupancy matrix
    void SetOccupancyMatrix(
        const std::map< std::pair< int, int >, bool >& occMatrix );

protected:

private:
    void Initialize();
    void CalculateLocalPositions();
    void ConnectionDetection();

    //A pointer to the plugin DCS
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

    //This in only here to get connections when first attached to structure
    std::map< std::string, bots::BlockEntity* > mBlockEntityMap;

    //The geometry of this block
    osg::ref_ptr< bots::Block > mBlockGeometry;

    //The physics constraints of this block
    btGeneric6DofConstraint* mConstraint;

    //
    std::map< osg::Drawable*, bool > mSideStates;

    //Blocks have a copy of the occupancy matrix
    //The occupancy matrix stores the desired structure to be built
    std::map< std::pair< int, int >, bool > mOccupancyMatrix;

    //Are blocks attached to sides or not
    //This map stores the physical connections to this block
    //This forms the basis for a data line in the structure
    /*    1
        2 B 0
          3    */
    std::map< unsigned int, bots::BlockEntity* > mConnectedBlocks;

    //The location of this block in the shared coordinate system
    std::pair< int, int > mLocation;

    //
    osg::ref_ptr< osg::Vec3Array > mLocalPositions;
    //
    osg::ref_ptr< osgUtil::LineSegmentIntersector > mLineSegmentIntersector;

};
} //end bots

#endif //BLOCK_ENTITY_H

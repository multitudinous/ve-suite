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
    ///Constructor
    BlockEntity(
        bots::Block* block,
        ves::xplorer::scenegraph::DCS* pluginDCS,
        ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    ///Destructor
    virtual ~BlockEntity();

    ///Update with the structure on attachment
    const bool AttachUpdate();

    ///Get this block's geometry
    bots::Block* const GetBlockGeometry() const;

    ///Get this block's location
    const std::pair< int, int >& GetLocation() const;

    ///Get this block's occupancy matrix
    std::map< std::pair< int, int >,
              std::pair< bool, bool > >& GetOccupancyMatrix() const;

    ///Special initialize function for the start block
    void InitializeStartBlock();

    ///Return if this block is attached to the structure
    const bool IsAttached() const;

    ///Return if a block can be attached to a side of this block
    const bool PermissionToAttach( osg::Drawable* drawable ) const;

    ///Set a connection to this block
    void SetBlockConnection(
        unsigned int side, bots::BlockEntity* blockEntity );

    ///Set the block entity map
    void SetBlockEntityMap(
        std::map< std::string, bots::BlockEntity* >& blockEntityMap );

    ///Set the physics constraints
    void SetConstraints( int gridSize );

    ///Set the name and descriptions for this CADEntity
    void SetNameAndDescriptions( int number );

    ///Set the block's occupancy matrix
    void SetOccupancyMatrix(
        std::map< std::pair< int, int >,
                  std::pair< bool, bool > >& occupancyMatrix );

    ///Update the side states when new block is attached
    void UpdateSideStates();

protected:

private:
    ///Initialize this block entity
    void Initialize();

    ///Calculate the local intersection positions of this block entity
    void CalculateLocalPositions();

    ///Detect connections with this block entity
    void ConnectionDetection();

    ///Is this block attached to the structure?
    bool mAttached;

    ///Store the neighbors occupation
    /*
            __
         __|__|__
        |__|__|__|
           |__|
    */
    bool mNeighborOccupancy[ 4 ];

    ///The color of the site
    osg::Vec4 mSiteColor;

    ///The color that shows valid attachments
    osg::Vec4 mAttachColor;

    ///The color that shows invalid attachments
    osg::Vec4 mNoAttachColor;

    ///A pointer to the plugin DCS
    const osg::ref_ptr< ves::xplorer::scenegraph::DCS > mPluginDCS;

    ///This in only here to get connections when first attached to structure
    ///A pointer to the block entity map
    const std::map< std::string, bots::BlockEntity* >* mBlockEntityMap;

    ///The geometry of this block entity
    osg::ref_ptr< bots::Block > mBlockGeometry;

    ///The physics constraints of this block
    btGeneric6DofConstraint* mConstraint;

    ///Blocks have a copy of the occupancy matrix
    ///The occupancy matrix stores the desired structure to be built
    ///A pointer to the occupancy matrix
    std::map< std::pair< int, int >,
              std::pair< bool, bool > >* mOccupancyMatrix;

    ///A map of the side states
    std::map< osg::Drawable*, bool > mSideStates;

    ///Are blocks attached to sides or not
    ///This map stores the physical connections to this block
    ///This forms the basis for a data line in the structure
    /*    1
        2 B 0
          3    */
    std::map< unsigned int, bots::BlockEntity* > mConnectedBlocks;

    ///The location of this block in the shared coordinate system
    std::pair< int, int > mLocation;

    ///A vertex array which contains the local intersection positions
    osg::ref_ptr< osg::Vec3Array > mLocalPositions;

    ///A line segment intersector used to perform intersection tests
    osg::ref_ptr< osgUtil::LineSegmentIntersector > mLineSegmentIntersector;

};
} //end bots

#endif //BLOCK_ENTITY_H

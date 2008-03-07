#ifndef BLOCK_ENTITY_H
#define BLOCK_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- Bullet Includes --- //
class btGeneric6DofConstraint;

// --- My Includes --- //
namespace Construction
{
    class Block;
}

namespace Construction
{
class BlockEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    BlockEntity( osg::ref_ptr< Construction::Block > block,
                 ves::xplorer::scenegraph::DCS* pluginDCS,
                 ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~BlockEntity();

    Construction::Block* GetGeometry();

    //Get the block's occupancy matrix
    std::map< std::pair< int, int >, bool > GetOccMatrix();

    void SetNameAndDescriptions( int number );

    void SetConstraints( int gridSize );

    //Set the block's occupancy matrix
    void SetOccMatrix( std::map< std::pair< int, int >, bool > occMatrix );

    void UpdateSideStates();

private:
    osg::ref_ptr< Construction::Block > m_block;

    btGeneric6DofConstraint* m_constraint;

    //Blocks have a copy of the occupancy matrix
    std::map< std::pair< int, int >, bool > m_occMatrix;
    //Blocks store location info for shared coordinate system
    std::pair< int, int > m_location;
    //Are blocks attatched to sides or not
            //[0]-F
    //[1]-L         //[3]-R
            //[2]-N
    bool sideState[ 4 ];
};
}

#endif //BLOCK_ENTITY_H

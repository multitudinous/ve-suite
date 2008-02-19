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
    BlockEntity( Construction::Block* block,
                 ves::xplorer::scenegraph::DCS* pluginDCS,
                 ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~BlockEntity();

    Construction::Block* GetGeometry();

    void SetNameAndDescriptions( int number );

    void SetConstraints( int gridSize );

    /*
    //Set the block's occupancy matrix
    void SetOccMatrix( std::vector< bool > temp );
    //Set the block's location
    void SetLocation( osg::Vec3 temp );
    //Set the block's side states
    void SetSideStates( bool* temp );
    //Set the starting block
    void SetStartBlock( bool temp );

    //Get the block's CADEntity
    ves::xplorer::scenegraph::CADEntity* GetEntity();
    //Get the block's occupancy matrix
    std::vector< bool > GetOccMatrix();
    //Get the block's location
    osg::Vec3 GetLocation();
    //Get the block's side states
    bool* GetSideStates();
    */

private:
    osg::ref_ptr< Construction::Block > m_geometry;

    btGeneric6DofConstraint* m_constraint;
/*
    ves::xplorer::scenegraph::CADEntity* entity;

    //Blocks have a copy of the occupancy matrix
    std::vector< bool > occMatrix;
    //Blocks store location info for shared coordinate system
    osg::Vec3 location;
    //Is block starting block?
    bool start;
    //Are blocks attatched to sides or not
            //[0]-F
    //[1]-L         //[3]-R
            //[2]-N
    bool sideState[4];
    */
};
}

#endif //BLOCK_ENTITY_H

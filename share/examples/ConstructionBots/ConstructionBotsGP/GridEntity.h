#ifndef GRID_ENTITY_H
#define GRID_ENTITY_H

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/CADEntity.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

// --- My Includes --- //
namespace Construction
{
    class Grid;
}

namespace Construction
{
class GridEntity : public ves::xplorer::scenegraph::CADEntity
{
public:
    GridEntity( osg::ref_ptr< Construction::Grid > grid,
                ves::xplorer::scenegraph::DCS* pluginDCS,
                ves::xplorer::scenegraph::PhysicsSimulator* physicsSimulator );

    virtual ~GridEntity();

    void SetNameAndDescriptions();

private:
    osg::ref_ptr< Construction::Grid > geometry;

};
}

#endif //GRID_ENTITY_H

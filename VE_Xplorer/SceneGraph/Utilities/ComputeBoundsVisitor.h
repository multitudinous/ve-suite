#ifndef COMPUTE_BOUNDS_VISITOR_H
#define COMPUTE_BOUNDS_VISITOR_H

#include "VE_Installer/include/VEConfig.h"

//#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>
#include <osg/Polytope>

namespace VE_SceneGraph
{
namespace Utilities
{
class VE_SCENEGRAPH_UTILS_EXPORTS ComputeBoundsVisitor : public osg::NodeVisitor
{
public:
    ComputeBoundsVisitor( TraversalMode traversalMode = TRAVERSE_ALL_CHILDREN );

    virtual void reset();
    
    osg::BoundingBox& getBoundingBox() { return _bb; }

    void getPolytope( osg::Polytope& polytope, float margin = 0.1 ) const;
        
    void getBase( osg::Polytope& polytope, float margin = 0.1 ) const;
    
    void apply( osg::Node& node );
    
    void apply( osg::Transform& transform );
    
    void apply( osg::Geode& geode );
    
    inline void pushMatrix( osg::Matrix& matrix ) { _matrixStack.push_back( matrix ); }
    
    inline void popMatrix() { _matrixStack.pop_back(); }

    void applyDrawable( osg::Drawable* drawable );
    
protected:
    typedef std::vector< osg::Matrix > MatrixStack;

    MatrixStack _matrixStack;
    osg::BoundingBox _bb;
};
}
}

#endif //COMPUTE_BOUNDS_VISITOR_H

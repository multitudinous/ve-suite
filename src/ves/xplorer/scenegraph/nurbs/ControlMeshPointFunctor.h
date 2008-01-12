#ifndef CONTROL_MESH_ATTRIBUTE_FUNCTOR_H
#define CONTROL_MESH_ATTRIBUTE_FUNCTOR_H
#include <osg/Vec3>
#include <osg/LineSegment>
#include <osg/Drawable>

#include <map>

#include <ves/VEConfig.h>

/*!\file ControlMeshPointFunctor.h
  Select a control point of a NURBSObject 
  */
/*!\file ControlMeshPointFunctor.cxx
  Select a control point of a NURBSObject 
  */
/*!\class ves::xplorer::scenegraph::nurbs::ControlMeshPointFunctor
 * Class implementing selection of a control point of a NURBSObject 
  */

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
struct VE_NURBS_EXPORTS PointIntersection
{
   PointIntersection(unsigned int index, float r1, const osg::Vec3* v1):
        _index(index),
        _r1(r1),
        _v1(v1){}

    unsigned int        _index;
    float               _r1;
    const osg::Vec3*    _v1;
};

typedef std::multimap<float,PointIntersection> PointIntersections;
class VE_NURBS_EXPORTS ControlMeshPointFunctor: public osg::Drawable::AttributeFunctor
{
public:
    ///Constructor
    ControlMeshPointFunctor();

    ///Destructor
    virtual ~ControlMeshPointFunctor();
    
    //META_Node( ves::xplorer::scenegraph::nurbs, ControlMeshPointFunctor );

    ///The current "selecting" LineSegment 
    void SetLineSegment( osg::LineSegment* line );

    ///Iterate over the ControlMeshDrawable to check for selection
    virtual void apply( osg::Drawable::AttributeType type,
                        unsigned int count,
                        osg::Vec3* begin );

    ///Clear the stored selection information
    void Reset();

    ///Determine if we selected a control point
    bool HitControlPoint();

    ///Get the selected ControlPoint index
    //unsigned int GetSelectedControlPointIndex();

    ///Equal operator
    ///\param rhs Right hand side of the operator
    ControlMeshPointFunctor& operator=(const ControlMeshPointFunctor& rhs);
    
    ///Get the PointIntersection
    PointIntersections& GetPointIntersections(); 
protected:
    PointIntersections m_intersections;
    osg::ref_ptr<osg::LineSegment> m_lineSegment;///<The selecting LineSegment 
};
}
}
}
}

#endif// CONTROL_MESH_ATTRIBUTE_FUNCTOR_H

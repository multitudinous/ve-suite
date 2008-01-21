/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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

#ifndef INTERSECT_POINT_VISITOR_H
#define INTERSECT_POINT_VISITOR_H

#include <osgUtil/IntersectionVisitor>
#include <osg/Drawable>
#include <ves/VEConfig.h>

/*!\file PointLineSegmentIntersector.h
  Select a control point of a NURBSObject
  */
/*!\file PointLineSegmentIntersector.cxx
  Select a control point of a NURBSObject
  */
/*!\class ves::xplorer::scenegraph::nurbs::PointLineSegmentIntersector
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
class VE_NURBS_EXPORTS PointLineSegmentIntersector : public osgUtil::Intersector
{
public:

    /** Construct a PointLineSegmentIntersector the runs between the specified start and end points in MODEL coordinates. */
    PointLineSegmentIntersector(const osg::Vec3d& start, const osg::Vec3d& end);
        
    /** Construct a PointLineSegmentIntersector the runs between the specified start and end points in the specified coordinate frame. */
    PointLineSegmentIntersector(CoordinateFrame cf, const osg::Vec3d& start, const osg::Vec3d& end);
        
    /** Convenience constructor for supporting picking in WINDOW, or PROJECTION coordinates
      * In WINDOW coordinates creates a start value of (x,y,0) and end value of (x,y,1).
      * In PROJECTION coordinates (clip space cube) creates a start value of (x,y,-1) and end value of (x,y,1).
      * In VIEW and MODEL coordinates creates a start value of (x,y,0) and end value of (x,y,1).*/
    PointLineSegmentIntersector(CoordinateFrame cf, double x, double y);
        
    struct Intersection
    {
        Intersection():
            ratio(-1.0),
            primitiveIndex(0) {}
   
        bool operator < (const Intersection& rhs) const { return ratio < rhs.ratio; }

        typedef std::vector<unsigned int>   IndexList;
        typedef std::vector<double>         RatioList;

        double                          ratio;
        osg::NodePath                   nodePath;
        osg::ref_ptr<osg::Drawable>     drawable;
        osg::ref_ptr<osg::RefMatrix>    matrix;
        osg::Vec3d                      localIntersectionPoint;
        osg::Vec3                       localIntersectionNormal;
        IndexList                       indexList;
        RatioList                       ratioList;
        unsigned int                    primitiveIndex;
            
        const osg::Vec3d& getLocalIntersectPoint() const { return localIntersectionPoint; }
        osg::Vec3d getWorldIntersectPoint() const { return matrix.valid() ? localIntersectionPoint * (*matrix) : localIntersectionPoint; }
            
        const osg::Vec3& getLocalIntersectNormal() const { return localIntersectionNormal; }
        osg::Vec3 getWorldIntersectNormal() const { return matrix.valid() ? osg::Matrix::transform3x3(osg::Matrix::inverse(*matrix),localIntersectionNormal) : localIntersectionNormal; }
    };
        
    typedef std::multiset<Intersection> Intersections;
        
    inline void insertIntersection(const Intersection& intersection) { getIntersections().insert(intersection); }

    inline Intersections& getIntersections() { return _parent ? _parent->_intersections : _intersections; }

    inline Intersection getFirstIntersection() { Intersections& intersections = getIntersections(); return intersections.empty() ? Intersection() : *(intersections.begin()); }
        
    inline void setStart(const osg::Vec3d& start) { _start = start; }
    inline const osg::Vec3d& getStart() const { return _start; }

    inline void setEnd(const osg::Vec3d& end) { _end = end; }
    inline const osg::Vec3d& setEnd() const { return _end; }

public:

    virtual Intersector* clone(osgUtil::IntersectionVisitor& iv);
        
    virtual bool enter(const osg::Node& node);
        
    virtual void leave();
        
    virtual void intersect(osgUtil::IntersectionVisitor& iv, osg::Drawable* drawable);
        
    virtual void reset();

    virtual bool containsIntersections() { return !_intersections.empty(); }

protected:
    
    bool intersects(const osg::BoundingSphere& bs);
    bool intersectAndClip(osg::Vec3d& s, osg::Vec3d& e,const osg::BoundingBox& bb);

    PointLineSegmentIntersector* _parent;

    osg::Vec3d  _start;
    osg::Vec3d  _end;
        
    Intersections _intersections;
    
};
}
}
}
}

#endif


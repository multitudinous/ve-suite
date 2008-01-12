/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/


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

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


#include <ves/xplorer/scenegraph/nurbs/PointLineSegmentIntersector.h>
#include <ves/xplorer/scenegraph/nurbs/ControlMeshPointFunctor.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>

#include <osg/Geometry>
#include <osg/LineSegment>
#include <osg/Notify>
#include <osg/io_utils>
#include <iostream>

using namespace osgUtil;
using namespace ves::xplorer::scenegraph::nurbs;

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//  PointLineSegmentIntersector
//

PointLineSegmentIntersector::PointLineSegmentIntersector(const osg::Vec3d& start, const osg::Vec3d& end):
    _parent(0),
    _start(start),
    _end(end)
{
}

PointLineSegmentIntersector::PointLineSegmentIntersector(CoordinateFrame cf, const osg::Vec3d& start, const osg::Vec3d& end):
    Intersector(cf),
    _parent(0),
    _start(start),
    _end(end)
{
}

PointLineSegmentIntersector::PointLineSegmentIntersector(CoordinateFrame cf, double x, double y):
    Intersector(cf),
    _parent(0)
{
    switch(cf)
    {
        case WINDOW : _start.set(x,y,0.0); _end.set(x,y,1.0); break;
        case PROJECTION : _start.set(x,y,-1.0); _end.set(x,y,1.0); break;
        case VIEW : _start.set(x,y,0.0); _end.set(x,y,1.0); break;
        case MODEL : _start.set(x,y,0.0); _end.set(x,y,1.0); break;
    }
}

Intersector* PointLineSegmentIntersector::clone(osgUtil::IntersectionVisitor& iv)
{
    if (_coordinateFrame==MODEL && iv.getModelMatrix()==0)
    {
        osg::ref_ptr<PointLineSegmentIntersector> lsi = new PointLineSegmentIntersector(_start, _end);
        lsi->_parent = this;
        return lsi.release();
    }

    // compute the matrix that takes this Intersector from its CoordinateFrame into the local MODEL coordinate frame
    // that geometry in the scene graph will always be in.
    osg::Matrix matrix;
    switch (_coordinateFrame)
    {
        case(WINDOW): 
            if (iv.getWindowMatrix()) matrix.preMult( *iv.getWindowMatrix() );
            if (iv.getProjectionMatrix()) matrix.preMult( *iv.getProjectionMatrix() );
            if (iv.getViewMatrix()) matrix.preMult( *iv.getViewMatrix() );
            if (iv.getModelMatrix()) matrix.preMult( *iv.getModelMatrix() );
            break;
        case(PROJECTION): 
            if (iv.getProjectionMatrix()) matrix.preMult( *iv.getProjectionMatrix() );
            if (iv.getViewMatrix()) matrix.preMult( *iv.getViewMatrix() );
            if (iv.getModelMatrix()) matrix.preMult( *iv.getModelMatrix() );
            break;
        case(VIEW): 
            if (iv.getViewMatrix()) matrix.preMult( *iv.getViewMatrix() );
            if (iv.getModelMatrix()) matrix.preMult( *iv.getModelMatrix() );
            break;
        case(MODEL):
            if (iv.getModelMatrix()) matrix = *iv.getModelMatrix();
            break;
    }

    osg::Matrix inverse;
    inverse.invert(matrix);

    osg::ref_ptr<PointLineSegmentIntersector> lsi = new PointLineSegmentIntersector(_start * inverse, _end * inverse);
    lsi->_parent = this;
    return lsi.release();
}

bool PointLineSegmentIntersector::enter(const osg::Node& node)
{
    return !node.isCullingActive() || intersects( node.getBound() );
}

void PointLineSegmentIntersector::leave()
{
    // do nothing
}
void PointLineSegmentIntersector::intersect(osgUtil::IntersectionVisitor& iv,
                                            osg::Drawable* drawable)
{

    //std::cout<<"=====NURBS intersection code====="<<std::endl;
    osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBSControlMesh> ctMesh =
        dynamic_cast<ves::xplorer::scenegraph::nurbs::NURBSControlMesh*>(drawable);

    if( !ctMesh.valid() )
    {
     //  std::cout<<drawable->className()<<std::endl;
       return;
    }
    //std::cout<<"=====found NURBS drawable====="<<std::endl;
    osg::Vec3d s(_start), e(_end);
    if ( !intersectAndClip( s, e, drawable->getBound() ) ) return;

    // reset the clipped range as it can be too close in on the BB, and cause missing due precission issues.
    s = _start;
    e = _end;

    ves::xplorer::scenegraph::nurbs::ControlMeshPointFunctor ctMeshPtFunctor;
    ctMeshPtFunctor.SetLineSegment( new osg::LineSegment(s,e) );
    ctMesh->accept(ctMeshPtFunctor);

    if ( ctMeshPtFunctor.HitControlPoint() )
    {
        ves::xplorer::scenegraph::nurbs::PointIntersections cti = ctMeshPtFunctor.GetPointIntersections();
        ves::xplorer::scenegraph::nurbs::PointIntersections::iterator ctiIter;
        for(ctiIter = cti.begin();
            ctiIter != cti.end();
            ++ctiIter)
        {

            // get ratio in s,e range
            float ratio = ctiIter->first;

            // remap ratio into _start, _end range
            ratio = ((s-_start).length() + ratio * (e-s).length() )/(_end-_start).length();

            ves::xplorer::scenegraph::nurbs::PointIntersection& ptHit = ctiIter->second;

            Intersection hit;
            hit.ratio = ratio;
            hit.matrix = iv.getModelMatrix();
            hit.nodePath = iv.getNodePath();

            hit.drawable = ctMesh.get();
            hit.primitiveIndex = ptHit._index;

            hit.localIntersectionPoint = s*(1.0f-ratio) + e*ratio;

            if (ctMesh.valid())
            {

                osg::Vec3Array* vertices = dynamic_cast<osg::Vec3Array*>(ctMesh->getVertexArray());
                if (vertices)
                {
                    osg::Vec3* first = &(vertices->front());
                    if (ptHit._v1)
                    {
                        hit.indexList.push_back(ptHit._v1-first);
                        hit.ratioList.push_back(ptHit._r1);
                    }
                }
            }

            insertIntersection(hit);

        }
    }

}

void PointLineSegmentIntersector::reset()
{
    Intersector::reset();
    
    _intersections.clear();
}

bool PointLineSegmentIntersector::intersects(const osg::BoundingSphere& bs)
{
    // if bs not valid then return true based on the assumption that an invalid sphere is yet to be defined.
    if (!bs.valid()) return true; 

    osg::Vec3d sm = _start - bs._center;
    double c = sm.length2()-bs._radius*bs._radius;
    if (c<0.0) return true;

    osg::Vec3d se = _end-_start;
    double a = se.length2();
    double b = (sm*se)*2.0;
    double d = b*b-4.0*a*c;

    if (d<0.0) return false;

    d = sqrt(d);

    double div = 1.0/(2.0*a);

    double r1 = (-b-d)*div;
    double r2 = (-b+d)*div;

    if (r1<=0.0 && r2<=0.0) return false;

    if (r1>=1.0 && r2>=1.0) return false;

    // passed all the rejection tests so line must intersect bounding sphere, return true.
    return true;
}

bool PointLineSegmentIntersector::intersectAndClip(osg::Vec3d& s, osg::Vec3d& e,const osg::BoundingBox& bb)
{
    // compate s and e against the xMin to xMax range of bb.
    if (s.x()<=e.x())
    {

        // trivial reject of segment wholely outside.
        if (e.x()<bb.xMin()) return false;
        if (s.x()>bb.xMax()) return false;

        if (s.x()<bb.xMin())
        {
            // clip s to xMin.
            s = s+(e-s)*(bb.xMin()-s.x())/(e.x()-s.x());
        }

        if (e.x()>bb.xMax())
        {
            // clip e to xMax.
            e = s+(e-s)*(bb.xMax()-s.x())/(e.x()-s.x());
        }
    }
    else
    {
        if (s.x()<bb.xMin()) return false;
        if (e.x()>bb.xMax()) return false;

        if (e.x()<bb.xMin())
        {
            // clip s to xMin.
            e = s+(e-s)*(bb.xMin()-s.x())/(e.x()-s.x());
        }

        if (s.x()>bb.xMax())
        {
            // clip e to xMax.
            s = s+(e-s)*(bb.xMax()-s.x())/(e.x()-s.x());
        }
    }

    // compate s and e against the yMin to yMax range of bb.
    if (s.y()<=e.y())
    {

        // trivial reject of segment wholely outside.
        if (e.y()<bb.yMin()) return false;
        if (s.y()>bb.yMax()) return false;

        if (s.y()<bb.yMin())
        {
            // clip s to yMin.
            s = s+(e-s)*(bb.yMin()-s.y())/(e.y()-s.y());
        }

        if (e.y()>bb.yMax())
        {
            // clip e to yMax.
            e = s+(e-s)*(bb.yMax()-s.y())/(e.y()-s.y());
        }
    }
    else
    {
        if (s.y()<bb.yMin()) return false;
        if (e.y()>bb.yMax()) return false;

        if (e.y()<bb.yMin())
        {
            // clip s to yMin.
            e = s+(e-s)*(bb.yMin()-s.y())/(e.y()-s.y());
        }

        if (s.y()>bb.yMax())
        {
            // clip e to yMax.
            s = s+(e-s)*(bb.yMax()-s.y())/(e.y()-s.y());
        }
    }

    // compate s and e against the zMin to zMax range of bb.
    if (s.z()<=e.z())
    {

        // trivial reject of segment wholely outside.
        if (e.z()<bb.zMin()) return false;
        if (s.z()>bb.zMax()) return false;

        if (s.z()<bb.zMin())
        {
            // clip s to zMin.
            s = s+(e-s)*(bb.zMin()-s.z())/(e.z()-s.z());
        }

        if (e.z()>bb.zMax())
        {
            // clip e to zMax.
            e = s+(e-s)*(bb.zMax()-s.z())/(e.z()-s.z());
        }
    }
    else
    {
        if (s.z()<bb.zMin()) return false;
        if (e.z()>bb.zMax()) return false;

        if (e.z()<bb.zMin())
        {
            // clip s to zMin.
            e = s+(e-s)*(bb.zMin()-s.z())/(e.z()-s.z());
        }

        if (s.z()>bb.zMax())
        {
            // clip e to zMax.
            s = s+(e-s)*(bb.zMax()-s.z())/(e.z()-s.z());
        }
    }
    
    // osg::notify(osg::NOTICE)<<"clampped segment "<<s<<" "<<e<<std::endl;
    
    // if (s==e) return false;

    return true;
}

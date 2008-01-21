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


#include <ves/xplorer/scenegraph/nurbs/ControlMeshPointFunctor.h>
#include <iostream>
#include <cmath>

using namespace ves::xplorer::scenegraph::nurbs;
//////////////////////////////////////////////////
ControlMeshPointFunctor::ControlMeshPointFunctor()
{
}
///////////////////////////////////////////////////////////
ControlMeshPointFunctor::~ControlMeshPointFunctor()
{
}
//////////////////////////////////////////////////////////////////////////
void ControlMeshPointFunctor::SetLineSegment( osg::LineSegment* line )
{
    m_lineSegment = line;
}
//////////////////////////////////////////////////////////////////////////
void ControlMeshPointFunctor::apply( osg::Drawable::AttributeType type,
                                         unsigned int count,
                                         osg::Vec3* begin )
{
    if (type == osg::Drawable::VERTICES)
    {
        osg::Vec3* end = begin + count;
        osg::Vec3 beam = m_lineSegment->start() - m_lineSegment->end();
        unsigned int index = 0;
        for ( osg::Vec3* itr = begin;
              itr < end;
              ++itr )
        {
            float distance = 100.0;
            osg::Vec3 r =  m_lineSegment->start() - (*itr);
            osg::Vec3 crossProduct = beam^r;
            
            distance = crossProduct.length()/beam.length(); 
            ///This could be adjusted probably
            if( fabs(distance) < 1.0 )  
            {
                osg::Vec3 vert = (*itr);
    //            std::cout<<"distance("<<index<<": "<<distance<<std::endl;
                float alpha = ((*itr) - m_lineSegment->start()).length()/beam.length();
     //           std::cout<<"alpha:"<<alpha<<std::endl;
                //m_intersectionMap[distance] = index;
                m_intersections.insert( std::pair<const float,PointIntersection>(distance,PointIntersection( index,distance,&vert)));
            }
            index++;
        }
    }
}
///////////////////////////////////////////////////////////////////
PointIntersections& ControlMeshPointFunctor::GetPointIntersections()
{
    return m_intersections;
}
///////////////////////////////////////////////
bool ControlMeshPointFunctor::HitControlPoint()
{
    return !m_intersections.empty();
}
////////////////////////////////////////////////////////////////////
/*unsigned int ControlMeshPointFunctor::GetSelectedControlPointIndex()
{
    return ( m_intersectionMap.begin()->second );
}*/
/////////////////////////////////////
void ControlMeshPointFunctor::Reset()
{
    m_intersections.clear();
}
///////////////////////////////////////////////////////////////////////
ControlMeshPointFunctor&
ControlMeshPointFunctor::operator=( const ControlMeshPointFunctor& rhs)
{
    if( this != & rhs )
    {
        m_lineSegment = rhs.m_lineSegment;
        m_intersections = rhs.m_intersections;
    }
    return *this;
}
 


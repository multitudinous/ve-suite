/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/Definitions.h>

// --- OSG Includes --- //
#include <osg/Drawable>
#include <osg/Plane>

// --- STL Includes --- //
#include <cmath>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
class ForceCullCallback : public osg::Drawable::CullCallback
{
public:
    ///
    ForceCullCallback()
        :
        osg::Drawable::CullCallback()
    {
        ;
    }

    ///
    ForceCullCallback(
        const ForceCullCallback& forceCullCallback,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY )
        :
        osg::Drawable::CullCallback( forceCullCallback, copyop )
    {
        ;
    }

    ///
    META_Object(
        ves::xplorer::scenegraph::manipulator::Dragger, ForceCullCallback );

    ///
    virtual bool cull(
        osg::NodeVisitor* nv,
        osg::Drawable* drawable,
        osg::RenderInfo* renderInfo ) const
    {
        return true;
    }

protected:

private:

};
////////////////////////////////////////////////////////////////////////////////
struct RotateAxisBBCallback : public osg::Drawable::ComputeBoundingBoxCallback
{
    RotateAxisBBCallback()
        :
        osg::Drawable::ComputeBoundingBoxCallback()
    {
        ;
    }

    RotateAxisBBCallback(
        const RotateAxisBBCallback& rotateAxisBBCallback,
        const osg::CopyOp& copyOp )
        :
        osg::Drawable::ComputeBoundingBoxCallback(
            rotateAxisBBCallback, copyOp )
    {
        ;
    }

    META_Object( ves::xplorer::scenegraph::manipulator, RotateAxisBBCallback );

    virtual osg::BoundingBox computeBound( const osg::Drawable& drawable ) const
    {
        osg::BoundingBox bb = drawable.computeBound();

        //bb.yMax() = 0.0;

        return bb;
    }
};
////////////////////////////////////////////////////////////////////////////////
void ves::xplorer::scenegraph::manipulator::SetDrawableToAlwaysCull(
    osg::Drawable& drawable )
{
    osg::ref_ptr< ForceCullCallback > fcc = new ForceCullCallback();
    drawable.setCullCallback( fcc.get() );
}
////////////////////////////////////////////////////////////////////////////////
void ves::xplorer::scenegraph::manipulator::SetComputeBBCallback(
    osg::Drawable& drawable )
{
    osg::ref_ptr< RotateAxisBBCallback > rabbc = new RotateAxisBBCallback();
    drawable.setComputeBoundingBoxCallback( rabbc.get() );
}
////////////////////////////////////////////////////////////////////////////////
const bool ves::xplorer::scenegraph::manipulator::GetLinePlaneIntersection(
    const osg::Vec3d& lineStart,
    const osg::Vec3d& lineEnd,
    const osg::Plane& plane,
    osg::Vec3d& intersection )
{
    const double error = -1E-05;

    osg::Vec3d direction = lineEnd - lineStart;
    direction.normalize();

    const double num2 = plane.dotProductNormal( direction );
    if( fabs( num2 ) < error )
    {
        return false;
    }

    const double num3 = plane.dotProductNormal( lineStart );
    double intersectDistance = ( -plane[ 3 ] - num3 ) / num2;
    if( intersectDistance < 0.0 )
    {
        if( intersectDistance < error )
        {
            return false;
        }

        intersectDistance = 0.0;
    }

    //Calculate the intersection position of the ray on the plane
    intersection = lineStart + ( direction * intersectDistance );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
const bool ves::xplorer::scenegraph::manipulator::IsFiniteNumber(
    const double& number )
{
    return ( number <= DBL_MAX && number >= -DBL_MAX );
}
////////////////////////////////////////////////////////////////////////////////
const double ves::xplorer::scenegraph::manipulator::SignedAngle(
    const osg::Vec3d& v1,
    const osg::Vec3d& v2,
    const osg::Vec3d& reference )
{
    osg::Vec3d c = v1 ^ v2;
    double angle = std::atan2( c.length(), v1 * v2 );

    if( ( c * reference ) < 0.0 )
    {
        angle *= -1.0;
    }

    return angle;
}
////////////////////////////////////////////////////////////////////////////////

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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_DEFINITIONS_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_DEFINITIONS_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- VR Juggler Includes --- //
#include <gmtl/Math.h>

// --- OSG Includes --- //
namespace osg
{
class Drawable;
class Plane;
class Vec3d;
}

//Define manipulator constants
const unsigned int NUM_CIRCLE_SEGMENTS = 64;
const unsigned int NUM_GHOST_DISK_SEGMENTS = 16;
const unsigned int NUM_CIRCLE_SIDES = 8;
const double DELTA_SEGMENT_ANGLE = gmtl::Math::TWO_PI / NUM_CIRCLE_SEGMENTS;
const double DELTA_SIDE_ANGLE = gmtl::Math::TWO_PI / NUM_CIRCLE_SIDES;

const double BOX_WIDTH = 0.1;
const double CONE_HEIGHT = 0.2;
const double CONE_RADIUS = 0.05;
const double PICK_RADIUS = 0.05;
const double LINE_WIDTH = 1.0;
const double ROTATE_AXIS_RADIUS = 1.0;
const double HELP_CIRCLE_RADIUS = ROTATE_AXIS_RADIUS;
const double ROTATE_TWIST_RADIUS = 1.2;
const double TRANSLATE_PAN_RADIUS = 0.2;

//Define manipulator enums w/ namespaces
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{

///Defines transformation type associations with the draggers
namespace TransformationType
{
    enum Enum
    {
        NONE = 0x000,

        TRANSLATE_AXIS = 0x001,
        TRANSLATE_PLANE = 0x002,
        TRANSLATE_PAN = 0x004,
        TRANSLATE_COMPOUND = TRANSLATE_AXIS | TRANSLATE_PLANE | TRANSLATE_PAN,
        ROTATE_AXIS = 0x010,
        ROTATE_TWIST = 0x020,
        HELP_CIRCLE = 0x040,
        ROTATE_COMPOUND = ROTATE_AXIS | ROTATE_TWIST | HELP_CIRCLE,
        SCALE_AXIS = 0x100,
        SCALE_UNIFORM = 0x200,
        SCALE_COMPOUND = SCALE_AXIS | SCALE_UNIFORM,

        ALL = TRANSLATE_COMPOUND | ROTATE_COMPOUND | SCALE_COMPOUND
    };
}

///Defines what space the manipulators operate in
namespace VectorSpace
{
    enum Enum
    {
        GLOBAL = 0x1,
        LOCAL = 0x2,
        VIEW = 0x4
    };
}

///
namespace AxisDirection
{
    enum Enum
    {
        POSITIVE = 0x1,
        NEGATIVE = 0x2,

        ALL = POSITIVE | NEGATIVE
    };
}

///
namespace Color
{
    enum Enum
    {
        DEFAULT = 0x01,
        FOCUS = 0x02,
        ACTIVE = 0x04,
        DISABLED = 0x08,
        OTHER = 0x10
    };
}

///
namespace Event
{
    enum Enum
    {
        NONE = 0x0,
        FOCUS = 0x1,
        PUSH = 0x2,
        DRAG = 0x4,
        RELEASE = 0x8
    };
}

///
///\param drawable
void VE_SCENEGRAPH_EXPORTS SetDrawableToAlwaysCull( osg::Drawable& drawable );

///
///\param drawable
void VE_SCENEGRAPH_EXPORTS SetComputeBBCallback( osg::Drawable& drawable );

///
///\param lineStart
///\param lineEnd
///\param plane
///\param intersection
///\return const bool
const bool VE_SCENEGRAPH_EXPORTS GetLinePlaneIntersection(
    const osg::Vec3d& lineStart,
    const osg::Vec3d& lineEnd,
    const osg::Plane& plane,
    osg::Vec3d& intersection );

///
const bool VE_SCENEGRAPH_EXPORTS IsFiniteNumber( const double& number );

///
///\param point The point to project onto the plane
///\param planePoint Any point that lies on the plane
///\param planeNormal The normal of the plane; must be normalized
osg::Vec3d VE_SCENEGRAPH_EXPORTS ProjectPointOntoPlane(
    const osg::Vec3d& point,
    const osg::Vec3d& planePoint,
    const osg::Vec3d& planeNormal );

///
///\param source
///\param destination
///\param reference
const double VE_SCENEGRAPH_EXPORTS SignedAngle(
    const osg::Vec3d& source,
    const osg::Vec3d& destination,
    const osg::Vec3d& reference );

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_DEFINITIONS_H

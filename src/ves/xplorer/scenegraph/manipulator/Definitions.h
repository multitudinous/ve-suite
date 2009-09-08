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
const double DELTA_SEGMENT_ANGLE = gmtl::Math::TWO_PI / NUM_CIRCLE_SEGMENTS;

const double BOX_WIDTH = 0.1;
const double CONE_HEIGHT = 0.2;
const double CONE_RADIUS = 0.05;
const double CYLINDER_RADIUS = 0.025;
const double LINE_WIDTH = 2.0;
const double ROTATE_AXIS_RADIUS = 1.0;
const double CLIPPING_CIRCLE_RADIUS = ROTATE_AXIS_RADIUS;
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

/*
///
namespace AxesFlag
{
    enum Enum
    {
        NONE = 0x0,

        X = 0x1,
        Y = 0x2,
        Z = 0x4,

        XY = X | Y,
        XZ = X | Z,
        YZ = Y | Z,

        XYZ = X | Y | Z,

        BILLBOARD = 0x8,

        ALL = XYZ | BILLBOARD
    };
}
*/

///Defines transformation type associations with the draggers
namespace TransformationType
{
    enum Enum
    {
        NONE = 0x00,

        TRANSLATE_AXIS = 0x01,
        TRANSLATE_PAN = 0x02,
        TRANSLATE_COMPOUND = TRANSLATE_AXIS | TRANSLATE_PAN,
        ROTATE_AXIS = 0x04,
        ROTATE_TWIST = 0x08,
        ROTATE_COMPOUND = ROTATE_AXIS | ROTATE_TWIST,
        SCALE_AXIS = 0x10,
        SCALE_UNIFORM = 0x20,
        SCALE_COMPOUND = SCALE_AXIS | SCALE_UNIFORM,

        /*
        NONE = 0x0,
        TRANSLATE = 0x1,
        ROTATE = 0x2,
        SCALE = 0x3,
        */

        ALL = TRANSLATE_COMPOUND | ROTATE_COMPOUND | SCALE_COMPOUND
    };
}

///Defines what space the manipulators operate in
namespace VectorSpace
{
    enum Enum
    {
        GLOBAL,
        LOCAL,
        VIEW
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
        DEFAULT,
        FOCUS,
        ACTIVE,
        OTHER
    };
}

///
namespace Event
{
    enum Enum
    {
        FOCUS,
        PUSH,
        DRAG,
        RELEASE
    };
}

///
///\param drawable
void VE_SCENEGRAPH_EXPORTS SetDrawableToAlwaysCull( osg::Drawable& drawable );

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

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_DEFINITIONS_H

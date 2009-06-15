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
 * Date modified: $Date: 2009-05-13 15:17:12 -0600 (Wed, 13 May 2009) $
 * Version:       $Rev: 12684 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: Enums.h 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef ENUMS_H
#define ENUMS_H

// --- VE-Suite Includes --- //
//#include <ves/VEConfig.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{

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
        YX = Y | X,
        XZ = X | Z,
        ZX = Z | X,
        YZ = Y | Z,
        ZY = Z | Y,

        XYZ = X | Y | Z,

        ALL = XYZ
    };
}

///
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

        ALL = TRANSLATE_COMPOUND | ROTATE_COMPOUND | SCALE_COMPOUND
    };
}

///
namespace VectorSpace
{
    enum Enum
    {
        WORLD,
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
namespace ColorTag
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

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //ENUMS_H

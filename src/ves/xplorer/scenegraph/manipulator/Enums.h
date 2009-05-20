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

///
namespace AxesFlag
{
    enum Enum
    {
        None = 0x0,

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

        All = XYZ
    };
}

///
namespace TransformationMode
{
    enum Enum
    {
        None = 0x00,

        TranslateAxis = 0x01,
        TranslatePlane = 0x02,
        RotateAxis = 0x04,
        ScaleAxis = 0x08,
        ScalePlane = 0x10,
        ScaleUniform = 0x20,

        All = TranslateAxis | TranslatePlane |
              RotateAxis |
              ScaleAxis | ScalePlane | ScaleUniform
    };
}

///
namespace VectorSpace
{
    enum Enum
    {
        World,
        Local
    };
}

///
namespace AxisDirection
{
    enum Enum
    {
        Positive = 0x1,
        Negative = 0x2,

        All = Positive | Negative
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

} //end scenegraph
} //end xplorer
} //end ves

#endif //ENUMS_H

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

#ifndef MANIPULATOR_H
#define MANIPULATOR_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorPtr.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class PositionAttitudeTransform;
}

// --- C/C++ Includes --- //
#include <map>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{

namespace AxisFlags
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

namespace VectorSpace
{
    enum Enum
    {
        World,
        Local
    };
}

namespace AxisDirections
{
    enum Enum
    {
        Positive = 0x1,
        Negative = 0x2,

        All = Positive | Negative
    };
}

/*!\file Manipulator.h
 * Manipulator API
 */

/*!\class ves::xplorer::scenegraph::manipulator::Manipulator
 *
 */
class VE_SCENEGRAPH_EXPORTS Manipulator
{
public:
    ///Constructor
    Manipulator();

    ///Destructor
    virtual ~Manipulator();

    ///Gets the manipulator's active transformation mode
    const TransformationMode::Enum& GetActiveMode() const;

    ///Gets the transformation modes enabled on the manipulator
    const TransformationMode::Enum& GetEnabledModes() const;

    ///Gets the axes currently being operated on by the manipulator
    const AxisFlags::Enum& GetSelectedAxes() const;

    ///Gets the vector space in which the manipulator will operate
    const VectorSpace::Enum& GetVectorSpace() const;

    ///
    osg::PositionAttitudeTransform* const GetPAT() const;

    ///Sets the transformation modes enabled on the manipulator
    void SetEnabledModes( TransformationMode::Enum& value );

    ///Sets the vector space in which the manipulator will operate
    void SetVectorSpace( VectorSpace::Enum& value ); 

protected:
    ///
    class ManipFunction
    {
    public:
        ///
        virtual void call() = 0;

    protected:

    private:

    };

    ///
    TransformationMode::Enum m_activeMode;
    
    ///
    TransformationMode::Enum m_enabledModes;
    
    ///
    AxisFlags::Enum m_selectedAxes;
    
    ///
    VectorSpace::Enum m_vectorSpace;

    ///
    bool m_manipulating;

    ///
    std::map< TransformationMode::Enum, std::map< AxisFlags::Enum, ManipFunction* > > m_manipFunctions;
    //std::map< TransformationMode::Enum, std::map< AxisFlags::Enum, Dragger* > > m_draggers;

    ///
    osg::ref_ptr< osg::PositionAttitudeTransform > m_pat;

private:

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //MANIPULATOR_H

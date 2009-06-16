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

#ifndef SCALE_AXIS_H
#define SCALE_AXIS_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Box;
class Geometry;
class ShapeDrawable;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
/*!\file ScaleAxis.h
 * ScaleAxis API
 */

/*!\class ves::xplorer::scenegraph::ScaleAxis
 *
 */
class VE_SCENEGRAPH_EXPORTS ScaleAxis : public Dragger
{
public:
    ///
    ScaleAxis();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    ScaleAxis(
        const ScaleAxis& scaleAxis,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, ScaleAxis );

    ///
    virtual void DirtyGeometry();

    ///
    osg::Box* const GetBox() const;

    ///
    osg::Vec3Array* const GetLineVertices() const;

    ///
    //virtual void UseColor( ColorTag::Enum colorTag );

protected:
    ///
    virtual ~ScaleAxis();

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    osg::Vec4 m_defaultAxisColor;

    ///
    osg::ref_ptr< osg::Uniform > m_axisColor;

    ///
    osg::ref_ptr< osg::Vec3Array > m_lineVertices;

    ///
    osg::ref_ptr< osg::Geometry > m_lineGeometry;

    ///
    osg::ref_ptr< osg::Box > m_box;

    ///
    osg::ref_ptr< osg::ShapeDrawable > m_shapeDrawable;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //SCALE_AXIS_H

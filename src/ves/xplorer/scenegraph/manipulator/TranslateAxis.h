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

#ifndef TRANSLATE_AXIS_H
#define TRANSLATE_AXIS_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Geode;
class Geometry;
class Cone;
class LineSegment;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
/*!\file TranslateAxis.h
 * TranslateAxis API
 */

/*!\class ves::xplorer::scenegraph::TranslateAxis
 *
 */
class VE_SCENEGRAPH_EXPORTS TranslateAxis : public Dragger
{
public:
    ///
    TranslateAxis();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    TranslateAxis(
        const TranslateAxis& translateAxis,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, TranslateAxis );

    ///
    virtual void ComboForm();

    ///
    virtual void DefaultForm();

    ///
    osg::Geode* const GetLineAndCylinderGeode() const;

    ///
    osg::Cone* const GetCone() const;

protected:
    ///
    virtual void ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint );

    ///
    virtual ~TranslateAxis();

    ///
    virtual void ManipFunction( const osgUtil::LineSegmentIntersector& deviceInput );

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    osg::Vec3 m_lineExplodeVector;

    ///
    osg::ref_ptr< osg::LineSegment > m_unitAxis;

    ///
    osg::ref_ptr< osg::Vec3Array > m_lineVertices;

    ///
    osg::ref_ptr< osg::Geometry > m_lineGeometry;

    ///
    osg::ref_ptr< osg::Geode > m_lineAndCylinderGeode;

    ///
    osg::ref_ptr< osg::Cone > m_cone;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //TRANSLATE_AXIS_H

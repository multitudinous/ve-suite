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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_AXIS_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_AXIS_H

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
class Cylinder;
class LineSegment;
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
class Manipulator;

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
    TranslateAxis(
        const AxesFlag::Enum& axesFlag,
        Manipulator* const parentManipulator );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    TranslateAxis(
        const TranslateAxis& translateAxis,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\param nv
    virtual void accept( osg::NodeVisitor& nv );

    ///
    ///\return
    virtual const char* className() const;

    ///
    ///\param copyop
    ///\return
    virtual osg::Object* clone( const osg::CopyOp& copyop ) const;

    ///
    ///\return
    virtual osg::Object* cloneType() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    virtual void ComboForm();

    ///
    virtual void DefaultForm();

    ///
    void DirtyCone();

    ///
    osg::Geode* const GetLineAndCylinderGeode() const;

    ///
    osg::Cone* const GetCone() const;

protected:
    ///
    virtual const bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint );

    ///
    virtual ~TranslateAxis();

    ///
    virtual void ManipFunction(
        const osgUtil::LineSegmentIntersector& deviceInput );

    ///
    virtual void SetupDefaultGeometry();

private:
    ///
    osg::Vec3d m_lineExplodeVector;

    ///
    osg::ref_ptr< osg::Vec3dArray > m_lineVertices;

    ///
    osg::ref_ptr< osg::Geometry > m_lineGeometry;

    ///
    osg::ref_ptr< osg::Geode > m_lineAndCylinderGeode;

    ///
    osg::ref_ptr< osg::Cone > m_cone;

    ///
    osg::ref_ptr< osg::Cylinder > m_cylinder;

    ///
    osg::ref_ptr< osg::ShapeDrawable > m_coneDrawable;

    ///
    osg::ref_ptr< osg::ShapeDrawable > m_cylinderDrawable;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_TRANSLATE_AXIS_H

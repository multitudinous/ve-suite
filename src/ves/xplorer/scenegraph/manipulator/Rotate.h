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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_ROTATE_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_ROTATE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Geode;
class Geometry;
}

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace manipulator
{
class HelpCircle;

/*!\file Rotate.h
 * Rotate API
 */

/*!\class ves::xplorer::scenegraph::Rotate
 * Abstract class
 */
class VE_SCENEGRAPH_EXPORTS Rotate : public Dragger
{
public:
    ///
    Rotate( const TransformationType::Enum& transformationType );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Rotate(
        const Rotate& rotateAxis,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    virtual Rotate* AsRotate();

    ///
    ///\return
    virtual const char* className() const;

    ///
    void CreateGhostDisk();

    ///
    const HelpCircle* const GetHelpCircle() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    virtual void SetHelpCircle( HelpCircle* const helpCircle );

protected:
    ///
    virtual ~Rotate();

    ///
    virtual void ComputeDeltaTransform();

    ///
    virtual const bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint );

    ///
    virtual void CustomPushAction();

    ///
    virtual void CustomDragAction();

    ///
    virtual void CustomReleaseAction();

    ///
    virtual void SetupDefaultGeometry();

    ///
    osg::ref_ptr< HelpCircle > m_helpCircle;

    ///
    osg::ref_ptr< osg::Geode > m_rotateGeode;

private:
    ///
    virtual const double& GetRadius() const = NULL;

    ///
    void SetLineEndPoint( const osg::Vec3& endPoint );

    ///
    osg::Vec3d m_localStartPoint;

    ///
    double m_startAngle;

    ///
    double m_angle;

    ///
    osg::ref_ptr< osg::Vec3Array > m_lineVertices;

    ///
    osg::ref_ptr< osg::Vec3Array > m_ghostDiskVertices;

    ///
    osg::ref_ptr< osg::Geometry > m_lineGeometry;

    ///
    osg::ref_ptr< osg::Geometry > m_ghostDiskGeometry;

    ///
    osg::ref_ptr< osg::Geode > m_lineGeode;

    ///
    osg::ref_ptr< osg::Geode > m_ghostDiskGeode;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_ROTATE_H

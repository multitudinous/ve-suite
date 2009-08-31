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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_DRAGGER_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_DRAGGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Definitions.h>

// --- OSG Includes --- //
#include <osg/PositionAttitudeTransform>
#include <osg/Drawable>
#include <osg/Plane>

#include <osgUtil/LineSegmentIntersector>

namespace osgUtil
{
class LineSegmentIntersector;
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
/*!\file Dragger.h
 * Dragger API
 */

/*!\class ves::xplorer::scenegraph::Dragger
 * Abstract Class
 */
class VE_SCENEGRAPH_EXPORTS Dragger : public osg::PositionAttitudeTransform
{
public:
    ///
    Dragger( const TransformationType::Enum& transformationType );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Dragger(
        const Dragger& dragger,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\return
    virtual const char* className() const;

    ///
    virtual void ComboForm();

    ///
    virtual void DefaultForm();

    ///
    void Enable( const bool& enable = true );

    ///
    virtual Dragger* Drag( const osgUtil::LineSegmentIntersector& deviceInput );

    ///
    virtual Dragger* Focus( osg::NodePath::iterator& npItr );

    ///
    const osg::Plane GetPlane( const bool& transform = true ) const;

    ///
    ///\return
    const TransformationType::Enum GetTransformationType() const;

    ///
    const osg::Vec3d GetUnitAxis(
        const bool& zero = false, const bool& transform = false ) const;

    ///
    const VectorSpace::Enum& GetVectorSpace() const;

    ///
    virtual void Hide();

    ///
    const bool IsCompound() const;

    ///
    const bool& IsEnabled() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    virtual Dragger* Push(
        const osgUtil::LineSegmentIntersector& deviceInput,
        const osg::NodePath& np,
        osg::NodePath::iterator& npItr );

    ///
    virtual Dragger* Release( osg::NodePath::iterator& npItr );

    ///
    virtual void SetColor(
        Color::Enum colorTag, osg::Vec4 newColor, bool use = false );

    ///
    virtual void SetRootDragger( Dragger* rootDragger );

    ///
    virtual void SetVectorSpace( const VectorSpace::Enum& vectorSpace );

    ///
    virtual void Show();

    ///
    virtual void UseColor( Color::Enum colorTag );

protected:
    ///
    virtual ~Dragger();

    ///Will be pure virtual eventually
    ///
    virtual const bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint ){return false;}// = 0;

    ///
    osg::Vec4& GetColor( Color::Enum colorTag );

    ///
    const bool IntersectsPlane(
        const osg::Vec3d& startPosition,
        const osg::Vec3d& direction,
        double& intersectDistance );

    ///Pure virtual
    ///
    virtual void SetupDefaultGeometry() = 0;

    ///
    const TransformationType::Enum m_transformationType;

    ///
    VectorSpace::Enum m_vectorSpace;

    ///
    bool m_enabled;

    ///
    bool m_comboForm;

    ///
    osg::Vec3d m_startProjectedPoint;

    ///
    osg::Matrixd m_localToWorld;

    ///
    osg::Matrixd m_worldToLocal;

    ///
    Dragger* m_rootDragger;

private:
    ///
    typedef std::map< Color::Enum, osg::Vec4 > ColorMap;

    ///
    void CreateDefaultShader();

    ///
    ColorMap m_colorMap;

    ///
    osg::ref_ptr< osg::Uniform > m_color;

};

///
///\param drawable
void VE_SCENEGRAPH_EXPORTS SetDrawableToAlwaysCull( osg::Drawable& drawable );

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_DRAGGER_H

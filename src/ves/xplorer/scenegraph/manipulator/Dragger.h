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
#include <osg/MatrixTransform>
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
class Manipulator;

/*!\file Dragger.h
 * Dragger API
 */

/*!\class ves::xplorer::scenegraph::Dragger
 * Abstract Class
 */
class VE_SCENEGRAPH_EXPORTS Dragger : public osg::MatrixTransform
{
public:
    ///
    Dragger(
        const AxesFlag::Enum& axesFlag,
        const TransformationType::Enum& transformationType,
        Manipulator* const parentManipulator );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Dragger(
        const Dragger& dragger,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* className() const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    virtual void ComboForm();

    ///
    virtual void DefaultForm();

    ///
    Dragger* Drag( const osgUtil::LineSegmentIntersector& deviceInput );

    ///
    virtual Dragger* Focus( osg::NodePath::iterator& npItr );

    ///
    ///\return
    const AxesFlag::Enum GetAxesFlag() const;

    ///
    const osg::Plane GetPlane() const;

    ///
    ///\return
    const TransformationType::Enum GetTransformationType() const;

    ///
    const osg::Vec3d GetUnitAxis( const bool& transformToWorld = false ) const;

    ///
    virtual Dragger* Push(
        const osgUtil::LineSegmentIntersector& deviceInput,
        const osg::NodePath& np,
        osg::NodePath::iterator& npItr );

    ///
    virtual Dragger* Release( osg::NodePath::iterator& npItr );

    ///
    virtual void SetColor(
        ColorTag::Enum colorTag, osg::Vec4 newColor, bool use = false );

    ///
    virtual void UseColor( ColorTag::Enum colorTag );

    ///Deactivate this dragger
    void TurnOff();

    ///Activate this dragger
    void TurnOn();

protected:
    ///
    virtual ~Dragger();

    ///Will be pure virtual eventually
    ///
    virtual const bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint ){return false;}// = 0;

    ///
    osg::Vec4& GetColor( ColorTag::Enum colorTag );

    ///
    const bool IntersectsPlane(
        const osg::Vec3d& startPosition,
        const osg::Vec3d& direction,
        double& intersectDistance );

    ///Will be pure virtual eventually
    ///
    virtual void ManipFunction(
        const osgUtil::LineSegmentIntersector& deviceInput ){;}// = 0;

    ///Pure virtual
    ///
    virtual void SetupDefaultGeometry() = 0;

    ///
    const AxesFlag::Enum m_axesFlag;

    ///
    const TransformationType::Enum m_transformationType;

    ///
    bool m_comboForm;

    ///
    osg::Vec3d m_startProjectedPoint;

    ///
    osg::Vec3d m_startPosition;

    ///
    osg::Matrixd m_localToWorld;

    ///
    osg::Matrixd m_worldToLocal;

    ///
    std::map< osg::Transform*, std::pair< osg::Matrixd, osg::Matrixd > > m_associatedMatrices;

    ///
    osg::ref_ptr< osg::Uniform > m_color;

    ///
    Manipulator* const m_parentManipulator;

    // --- *** --- //
    ///
    class ForceCullCallback : public osg::Drawable::CullCallback
    {
    public:
        ///
        ForceCullCallback();

        ///
        ForceCullCallback(
            const ForceCullCallback& forceCullCallback,
            const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

        ///
        META_Object(
            ves::xplorer::scenegraph::manipulator::Dragger, ForceCullCallback );

        ///
        virtual bool cull(
            osg::NodeVisitor* nv,
            osg::Drawable* drawable,
            osg::RenderInfo* renderInfo ) const;

    protected:

    private:

    };

    ///
    ///\param drawable
    void SetDrawableToAlwaysCull( osg::Drawable& drawable );
    // --- *** --- //

private:
    ///
    typedef std::map< ColorTag::Enum, osg::Vec4 > ColorMap;

    ///
    void CreateDefaultShader();

    ///
    ColorMap m_colorMap;

};

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_DRAGGER_H

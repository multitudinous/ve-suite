/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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

#include <ves/xplorer/scenegraph/AutoTransform.h>

#include <ves/xplorer/scenegraph/manipulator/Definitions.h>

// --- OSG Includes --- //
#include <osg/Plane>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
class btRigidBody;

// --- STL Includes --- //
#include <set>
#include <map>

#include <boost/concept_check.hpp>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsSimulator;
class SceneManager;
class DCS;

namespace manipulator
{
class TranslateAxis;
class TranslatePlane;
class TranslatePan;
class Rotate;
class RotateAxis;
class RotateTwist;
class ScaleAxis;
class ScaleUniform;
class CompoundDragger;

/*!\file Dragger.h
 * Dragger API
 * \class ves::xplorer::scenegraph::Dragger
 * Abstract Class
 */
class VE_SCENEGRAPH_EXPORTS Dragger : public AutoTransform
{
public:
    ///
    ///\param transformationType
    Dragger( const TransformationType::Enum& transformationType );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    ///\param dragger
    ///\param copyop
    Dragger(
        const Dragger& dragger,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    ///\param nv
    virtual void accept( osg::NodeVisitor& nv );

    ///
    ///\return
    virtual TranslateAxis* AsTranslateAxis();

    ///
    ///\return
    virtual TranslatePlane* AsTranslatePlane();

    ///
    ///\return
    virtual TranslatePan* AsTranslatePan();

    ///
    ///\return
    virtual Rotate* AsRotate();

    ///
    ///\return
    virtual RotateAxis* AsRotateAxis();

    ///
    ///\return
    virtual RotateTwist* AsRotateTwist();

    ///
    ///\return
    virtual ScaleAxis* AsScaleAxis();

    ///
    ///\return
    virtual ScaleUniform* AsScaleUniform();

    ///
    ///\return
    virtual CompoundDragger* AsCompoundDragger();

    ///
    ///\return
    virtual const char* className() const;

    ///
    virtual void ComboForm();

    ///
    ///\param activeAssociation
    ///\return
    virtual bool Connect( osg::Transform* activeAssociation );

    ///
    virtual void DefaultForm();

    ///
    virtual void Disconnect();

    ///
    ///\param deviceInput
    ///\return
    Dragger* Drag( const osgUtil::LineSegmentIntersector& deviceInput );

    ///
    ///\param enable
    void Enable( const bool& enable = true );

    ///
    ///\param npItr
    ///\return
    virtual Dragger* Focus( osg::NodePath::iterator& npItr );

    ///
    ///\param parallel
    ///\return
    const osg::Plane GetPlane( const bool& parallel = false ) const;

    ///
    ///\return
    TransformationType::Enum GetTransformationType() const;

    ///
    ///\param premultiply
    ///\return
    const osg::Vec3d GetAxis( const bool& premultiply = false ) const;

    ///
    ///\return
    const osg::Vec3d GetUnitAxis() const;

    ///
    ///\return
    const osg::Plane GetUnitPlane() const;

    ///
    ///\return
    const VectorSpace::Enum& GetVectorSpace() const;

    ///
    virtual void Hide();

    ///
    ///\return
    const bool& IsEnabled() const;

    ///
    ///\param obj
    ///\return
    virtual bool isSameKindAs( const osg::Object* obj ) const;

    ///
    ///\return
    virtual const char* libraryName() const;

    ///
    ///\param deviceInput
    ///\param np
    ///\param npItr
    ///\return
    virtual Dragger* Push(
        const osgUtil::LineSegmentIntersector& deviceInput,
        const osg::NodePath& np,
        osg::NodePath::iterator& npItr );

    ///
    ///\param npItr
    ///\return
    virtual Dragger* Release( osg::NodePath::iterator& npItr );

    ///
    ///\param axisDirction
    virtual void SetAxisDirection( const AxisDirection::Enum& axisDirection );

    ///
    ///\param colorTag
    ///\param newColor
    ///\param use
    virtual void SetColor(
        Color::Enum colorTag, osg::Vec4 newColor, bool use = false );

    ///
    ///\param rootDragger
    virtual void SetRootDragger( Dragger* rootDragger );

    ///
    ///\param vectorSpace
    virtual void SetVectorSpace( const VectorSpace::Enum& vectorSpace );

    ///
    virtual void Show();

    ///
    ///\param colorTag
    virtual void UseColor( Color::Enum colorTag );

protected:
    ///
    virtual ~Dragger();

    ///
    virtual void ComputeDeltaTransform(){;}

    ///Will be pure virtual eventually
    ///
    ///\param deviceInput
    ///\param projectedPoint
    ///\return
    virtual bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint )
    {
        boost::ignore_unused_variable_warning( deviceInput );
        boost::ignore_unused_variable_warning( projectedPoint );
        return false;
    }

    ///
    virtual void CustomFocusAction(){;}

    ///
    virtual void CustomPushAction(){;}

    ///
    virtual void CustomDragAction(){;}

    ///
    virtual void CustomReleaseAction(){;}

    ///
    ///\param colorTag
    ///\return
    osg::Vec4& GetColor( Color::Enum colorTag );

    ///
    virtual void SetupDefaultGeometry() = 0;

    ///
    ///\return
    const TransformationType::Enum m_transformationType;

    ///
    VectorSpace::Enum m_vectorSpace;

    ///
    AxisDirection::Enum m_axisDirection;

    ///
    bool m_enabled;

    ///
    bool m_comboForm;

    ///
    osg::Vec3d m_startProjectedPoint;

    ///
    osg::Vec3d m_endProjectedPoint;

    ///
    Dragger* m_rootDragger;

    ///
    osg::Quat m_deltaRotation;

    ///
    osg::Vec3d m_deltaTranslation;

    ///
    osg::Vec3d m_deltaScale;

    ///
    osg::Matrixd m_localToWorld;

    ///
    osg::Matrixd m_worldToLocal;

    ///
    PhysicsSimulator& m_physicsSimulator;

    ///
    SceneManager& m_sceneManager;

private:
    ///
    void ComputeAssociationMatrices();

    ///
    void CreateDefaultShader();

    ///
    void UpdateAssociations();

    ///
    ///\param dcs
    void UpdateConductorData( ves::xplorer::scenegraph::DCS* dcs );

    ///
    bool m_isRootDragger;

    ///
    osg::Transform* m_activeAssociation;

    ///
    typedef std::set< osg::Transform* > AssociationSet;
    AssociationSet m_associationSet;

    ///
    typedef std::map< osg::Transform*,
            std::pair< osg::Matrixd, osg::Matrixd > > AssociationMatricesMap;
    AssociationMatricesMap m_associationMatricesMap;

    ///
    typedef std::map< Color::Enum, osg::Vec4 > ColorMap;

    ///
    ColorMap m_colorMap;

    ///
    osg::ref_ptr< osg::Uniform > m_color;

};

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_DRAGGER_H

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
#include <osg/AutoTransform>
#include <osg/Plane>

#include <osgUtil/LineSegmentIntersector>

// --- C/C++ Includes --- //
#include <set>
#include <map>

class btRigidBody;
class btTypedConstraint;

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsSimulator;
class SceneManager;

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
 */

/*!\class ves::xplorer::scenegraph::Dragger
 * Abstract Class
 */
class VE_SCENEGRAPH_EXPORTS Dragger : public osg::AutoTransform
{
public:
    ///
    Dragger( const TransformationType::Enum& transformationType );

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    Dragger(
        const Dragger& dragger,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    virtual void accept( osg::NodeVisitor& nv );

    ///
    virtual TranslateAxis* AsTranslateAxis();

    ///
    virtual TranslatePlane* AsTranslatePlane();

    ///
    virtual TranslatePan* AsTranslatePan();

    ///
    virtual Rotate* AsRotate();

    ///
    virtual RotateAxis* AsRotateAxis();

    ///
    virtual RotateTwist* AsRotateTwist();

    ///
    virtual ScaleAxis* AsScaleAxis();

    ///
    virtual ScaleUniform* AsScaleUniform();

    ///
    virtual CompoundDragger* AsCompoundDragger();

    ///
    ///\return
    virtual const char* className() const;

    ///
    virtual void ComboForm();

    ///
    virtual bool Connect( osg::Transform* activeAssociation );

    ///
    virtual void DefaultForm();

    ///
    virtual void Disconnect();

    ///
    void Enable( const bool& enable = true );

    ///
    Dragger* Drag( const osgUtil::LineSegmentIntersector& deviceInput );

    ///
    virtual Dragger* Focus( osg::NodePath::iterator& npItr );

    ///
    const osg::Plane GetPlane( const bool& parallel = false ) const;

    ///
    ///\return
    const TransformationType::Enum GetTransformationType() const;

    ///
    const osg::Vec3d GetAxis(
        const bool& zero = false, const bool& premultiply = false ) const;

    ///
    const osg::Vec3d GetPreviousEyePoint() const;

    ///
    const osg::Vec3d GetPreviousLocalUp() const;

    ///
    const osg::Vec3d GetUnitAxis() const;

    ///
    const osg::Plane GetUnitPlane() const;

    ///
    const VectorSpace::Enum& GetVectorSpace() const;

    ///
    virtual void Hide();

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
    virtual void SetAxisDirection( const AxisDirection::Enum& axisDirection );

    ///
    virtual void SetColor(
        Color::Enum colorTag, osg::Vec4 newColor, bool use = false );

    ///
    virtual void SetRootDragger( Dragger* rootDragger );

    ///
    void SetScale( const double scale );

    ///
    void SetScale( const osg::Vec3d& scale );

    ///
    virtual void SetVectorSpace( const VectorSpace::Enum& vectorSpace );

    ///
    virtual void Show();

    ///
    virtual void UseColor( Color::Enum colorTag );

    ///
    void ResetPhysics();
    
protected:
    ///
    virtual ~Dragger();

    ///
    virtual void ComputeDeltaTransform(){;}

    ///Will be pure virtual eventually
    ///
    virtual const bool ComputeProjectedPoint(
        const osgUtil::LineSegmentIntersector& deviceInput,
        osg::Vec3d& projectedPoint ){return false;}

    ///
    virtual void CustomFocusAction(){;}

    ///
    virtual void CustomPushAction(){;}

    ///
    virtual void CustomDragAction(){;}

    ///
    virtual void CustomReleaseAction(){;}

    ///
    osg::Vec4& GetColor( Color::Enum colorTag );

    ///
    virtual void SetupDefaultGeometry() = 0;

    ///Create physics point constraint
    bool CreatePointConstraint();
    ///Clear point constraint
    void ClearPointConstraint();
    ///Update point constraint
    void UpdatePointConstraint();
    
    ///
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
    osg::Vec3d m_scale;

    ///
    osg::Matrixd m_localToWorld;

    ///
    osg::Matrixd m_worldToLocal;

    ///
    ves::xplorer::scenegraph::PhysicsSimulator& m_physicsSimulator;
    
    ///
    ves::xplorer::scenegraph::SceneManager& m_sceneManager;
    
private:
    ///
    typedef std::map< Color::Enum, osg::Vec4 > ColorMap;

    ///
    void ComputeAssociationMatrices();

    ///
    void CreateDefaultShader();

    ///
    void UpdateAssociations();

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
    ColorMap m_colorMap;

    ///
    osg::ref_ptr< osg::Uniform > m_color;
    
    ///The rigid body that has been selected during physics mouse picking
    btRigidBody* m_pickedBody;
    
    ///Bullet constraint used for physics mouse picking
    btTypedConstraint* m_pickConstraint; 
    
    ///The distance from the head position to the picked btRigidBody point
    ///Used to calculate point to point constraints for physics picking
    double m_prevPhysicsRayPos;
};

} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_DRAGGER_H

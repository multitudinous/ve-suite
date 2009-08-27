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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/TranslateAxis.h>
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //
#include <osg/Hint>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/ShapeDrawable>
#include <osg/LineWidth>
#include <osg/LineSegment>
#include <osg/AutoTransform>
#include <osg/PositionAttitudeTransform>

#include <osgUtil/LineSegmentIntersector>

// --- osgBullet Includes --- //
#include <osgBullet/AbsoluteModelTransform.h>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis(
    const AxesFlag::Enum& axesFlag,
    Manipulator* const parentManipulator )
    :
    Dragger(
        axesFlag,
        TransformationType::TRANSLATE_AXIS,
        parentManipulator ),
    m_lineExplodeVector( GetUnitAxis() * TRANSLATE_PAN_RADIUS ),
    m_lineVertices( NULL ),
    m_lineGeometry( NULL ),
    m_lineAndCylinderGeode( NULL ),
    m_cone( NULL ),
    m_cylinder( NULL ),
    m_coneDrawable( NULL ),
    m_cylinderDrawable( NULL )
{
    SetupDefaultGeometry();
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::TranslateAxis(
    const TranslateAxis& translateAxis, const osg::CopyOp& copyop )
    :
    Dragger( translateAxis, copyop ),
    m_lineVertices( translateAxis.m_lineVertices.get() ),
    m_lineGeometry( translateAxis.m_lineGeometry.get() ),
    m_lineAndCylinderGeode( translateAxis.m_lineAndCylinderGeode.get() ),
    m_cone( translateAxis.m_cone.get() ),
    m_cylinder( translateAxis.m_cylinder.get() ),
    m_coneDrawable( translateAxis.m_coneDrawable.get() ),
    m_cylinderDrawable( translateAxis.m_cylinderDrawable.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis::~TranslateAxis()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::accept( osg::NodeVisitor& nv )
{
    if( nv.validNodeMask( *this ) )
    {
        nv.pushOntoNodePath( this );
        nv.apply( *this );
        nv.popFromNodePath();
    }
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslateAxis::className() const
{
    return "TranslateAxis";
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslateAxis::clone( const osg::CopyOp& copyop ) const
{
    return new TranslateAxis( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* TranslateAxis::cloneType() const
{
    return new TranslateAxis( m_axesFlag, m_parentManipulator );
}
////////////////////////////////////////////////////////////////////////////////
bool TranslateAxis::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const TranslateAxis* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* TranslateAxis::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::ComboForm();

    //Move the line in from the origin and unit axis
    (*m_lineVertices)[ 0 ] += m_lineExplodeVector;

    m_lineGeometry->dirtyDisplayList();
    m_lineGeometry->dirtyBound();

    //Move the invisible cylinder to match the new line position

    m_cylinder->setCenter(
        m_cylinder->getCenter() + ( m_lineExplodeVector * 0.5 ) );
    m_cylinder->setHeight( m_cylinder->getHeight() - TRANSLATE_PAN_RADIUS );

    m_cylinderDrawable->dirtyDisplayList();
    m_cylinderDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    //Call base method
    Dragger::DefaultForm();

    //Move the line back to the origin and unit axis
    (*m_lineVertices)[ 0 ] -= m_lineExplodeVector;

    m_lineGeometry->dirtyDisplayList();
    m_lineGeometry->dirtyBound();

    //Move the invisible cylinder to match the new line position
    m_cylinder->setCenter(
        m_cylinder->getCenter() - ( m_lineExplodeVector * 0.5 ) );
    m_cylinder->setHeight( m_cylinder->getHeight() + TRANSLATE_PAN_RADIUS );

    m_cylinderDrawable->dirtyDisplayList();
    m_cylinderDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::DirtyCone()
{
    m_coneDrawable->dirtyDisplayList();
    m_coneDrawable->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
//See http://softsurfer.com/Archive/algorithm_0106/algorithm_0106.htm
const bool TranslateAxis::ComputeProjectedPoint(
    const osgUtil::LineSegmentIntersector& deviceInput,
    osg::Vec3d& projectedPoint )
{
    //Get the start and end points for the dragger axis in world space
    const osg::Vec3d startDraggerAxis =
        m_localToWorld * osg::Vec3d( 0.0, 0.0, 0.0 );
    const osg::Vec3d endDraggerAxis = m_localToWorld * GetUnitAxis();

    //Get the near and far points for the active device
    const osg::Vec3d& startDeviceInput = deviceInput.getStart();
    const osg::Vec3d& endDeviceInput = deviceInput.getEnd();

    osg::Vec3d u = endDraggerAxis - startDraggerAxis;
    osg::Vec3d v = endDeviceInput - startDeviceInput;
    osg::Vec3d w = startDraggerAxis - startDeviceInput;

    double a = u * u;
    double b = u * v;
    double c = v * v;
    double d = u * w;
    double e = v * w;
    
    //Compute the line parameters of the two closest points
    double sc( 0.0 );
    double D = ( a * c ) - ( b * b );
    //If the lines are not parallel
    if( D > 0.00000001 )
    {
        sc = ( b * e - c * d ) / D;
    }

    projectedPoint = startDraggerAxis + ( u * sc );
    //projectedPoint = projectedPoint * m_worldToLocal;

    return true;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* const TranslateAxis::GetLineAndCylinderGeode() const
{
    return m_lineAndCylinderGeode.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Cone* const TranslateAxis::GetCone() const
{
    return m_cone.get();
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::ManipFunction( const osgUtil::LineSegmentIntersector& deviceInput )
{
    osg::AutoTransform* autoTransform =
        static_cast< osg::AutoTransform* >(
            m_parentManipulator->getParent( 0 ) );

    //Get the end projected point
    osg::Vec3d endProjectedPoint;
    if( !ComputeProjectedPoint( deviceInput, endProjectedPoint ) )
    {
        return;
    }

    //Calculate the delta transform
    osg::Vec3d deltaTranslation = endProjectedPoint - m_startProjectedPoint;
    
    //Set the m_parentManipulator's transform
    osg::Vec3d newTranslation = autoTransform->getPosition() + deltaTranslation;
    autoTransform->setPosition( newTranslation );

    //Set all associated node's transforms
    const std::vector< osg::Transform* >& associatedTransforms =
        m_parentManipulator->GetAssociatedTransforms();
    std::vector< osg::Transform* >::const_iterator itr =
        associatedTransforms.begin();
    for( itr; itr != associatedTransforms.end(); ++itr )
    {
        osg::Transform* transform = *itr;
        std::map< osg::Transform*, std::pair< osg::Matrixd, osg::Matrixd > >::const_iterator transformMatrices =
            m_associatedMatrices.find( transform );
        if( transformMatrices == m_associatedMatrices.end() )
        {
            //Error output
            break;
        }

        const osg::Matrixd& localToWorld = transformMatrices->second.first;
        const osg::Matrixd& worldToLocal = transformMatrices->second.second;

        osg::MatrixTransform* mt( NULL );
        osg::PositionAttitudeTransform* pat( NULL );
        osgBullet::AbsoluteModelTransform* amt( NULL );
        if( mt = transform->asMatrixTransform() )
        {
            const osg::Matrix& currentMatrix = mt->getMatrix();
            mt->setMatrix(
                localToWorld *
                osg::Matrix::translate( deltaTranslation ) * currentMatrix *
                worldToLocal );
        }
        else if( pat = transform->asPositionAttitudeTransform() )
        {
            const osg::Vec3d& currentPosition = pat->getPosition();
            osg::Vec3d newTranslation = currentPosition;
            newTranslation = newTranslation * localToWorld; 
            newTranslation += deltaTranslation;
            newTranslation = newTranslation * worldToLocal;
            pat->setPosition( newTranslation );
        }
        else if( amt = dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform ) )
        {
            ;
        }
    }

    //Reset
    m_startProjectedPoint = endProjectedPoint;
}
////////////////////////////////////////////////////////////////////////////////
void TranslateAxis::SetupDefaultGeometry()
{
    //The geode to add the line and cylinder geometry to
    m_lineAndCylinderGeode = new osg::Geode();

    //The geode to add the cone geometry to
    osg::ref_ptr< osg::Geode > coneGeode = new osg::Geode();

    //The unit axis
    const osg::Vec3d unitAxis = GetUnitAxis();
    m_lineVertices = new osg::Vec3dArray();
    m_lineVertices->resize( 2 );
    (*m_lineVertices)[ 0 ] = osg::Vec3d( 0.0, 0.0, 0.0 );
    (*m_lineVertices)[ 1 ] = unitAxis;

    //Rotation for cones and cylinders
    osg::Quat rotation;
    rotation.makeRotate( osg::Vec3d( 0.0, 0.0, 1.0 ), unitAxis );

    //Create a positive line
    {
        m_lineGeometry = new osg::Geometry();

        m_lineGeometry->setVertexArray( m_lineVertices.get() );
        m_lineGeometry->addPrimitiveSet(
            new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, 2 ) );

        m_lineAndCylinderGeode->addDrawable( m_lineGeometry.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_lineGeometry->getOrCreateStateSet();

        //Set line width
        osg::ref_ptr< osg::LineWidth > lineWidth = new osg::LineWidth();
        lineWidth->setWidth( LINE_WIDTH );
        stateSet->setAttributeAndModes(
            lineWidth.get(), osg::StateAttribute::ON );

        //Set line hints
        stateSet->setMode( GL_LINE_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_LINE_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create a positive cone
    {
        osg::Vec3d CONE_CENTER = unitAxis * CONE_HEIGHT;
        (*m_lineVertices)[ 1 ] -= CONE_CENTER;
        CONE_CENTER *= 0.25;
        m_cone = new osg::Cone(
            (*m_lineVertices)[ 1 ] + CONE_CENTER, CONE_RADIUS, CONE_HEIGHT );
        m_cone->setRotation( rotation );

        m_coneDrawable = new osg::ShapeDrawable( m_cone.get() );
        coneGeode->addDrawable( m_coneDrawable.get() );

        //Set StateSet
        osg::ref_ptr< osg::StateSet > stateSet =
            m_coneDrawable->getOrCreateStateSet();

        //Set polygon hints
        stateSet->setMode( GL_POLYGON_SMOOTH,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        osg::ref_ptr< osg::Hint > hint =
            new osg::Hint( GL_POLYGON_SMOOTH_HINT, GL_NICEST );
        stateSet->setAttributeAndModes( hint.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }

    //Create an invisible cylinder for picking the positive line
    {
        m_cylinder = new osg::Cylinder(
            (*m_lineVertices)[ 1 ] * 0.5,
            CYLINDER_RADIUS,
            (*m_lineVertices)[ 1 ].length() );
        m_cylinder->setRotation( rotation );
        m_cylinderDrawable = new osg::ShapeDrawable( m_cylinder.get() );

        SetDrawableToAlwaysCull( *m_cylinderDrawable.get() );
        m_lineAndCylinderGeode->addDrawable( m_cylinderDrawable.get() );
    }
    
    //Add line and invisible cylinder to this
    addChild( m_lineAndCylinderGeode.get() );

    //Add cone to this
    addChild( coneGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////

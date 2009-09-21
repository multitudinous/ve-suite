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
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

//#include <ves/xplorer/CommandHandler.h>

// --- OSG Includes --- //
#include <osg/io_utils>
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/AutoTransform>
#include <osgUtil/CullVisitor>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

// --- osgBullet Includes --- //
#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/Utils.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const TransformationType::Enum& transformationType )
    :
    osg::AutoTransform(),
    m_transformationType( transformationType ),
    m_vectorSpace( VectorSpace::GLOBAL ),
    m_axisDirection( AxisDirection::POSITIVE ),
    m_enabled( false ),
    m_comboForm( false ),
    m_startProjectedPoint( 0.0, 0.0, 0.0 ),
    m_endProjectedPoint( 0.0, 0.0, 0.0 ),
    m_rootDragger( NULL ),
    m_deltaRotation( 0.0, 0.0, 0.0, 0.0 ),
    m_deltaTranslation( 0.0, 0.0, 0.0 ),
    m_deltaScale( 0.0, 0.0, 0.0 ),
    m_isRootDragger( true ),
    m_color( NULL ),
    m_physicsSimulator( *vxs::PhysicsSimulator::instance() ),
    m_sceneManager( *vxs::SceneManager::instance() )
{
    m_rootDragger = this;

    m_colorMap[ Color::DEFAULT ] = osg::Vec4f( 1.0, 0.0, 0.0, 1.0 );
    m_colorMap[ Color::FOCUS ] = osg::Vec4f( 1.0, 1.0, 0.0, 1.0 );
    m_colorMap[ Color::ACTIVE ] = osg::Vec4f( 1.0, 0.0, 1.0, 1.0 );
    m_colorMap[ Color::DISABLED ] = osg::Vec4f( 0.6, 0.6, 0.6, 1.0 );
    m_colorMap[ Color::OTHER ] = osg::Vec4f( 0.0, 0.0, 0.0, 1.0 );

    m_color = new osg::Uniform( "color", GetColor( Color::DEFAULT ) );

    CreateDefaultShader();
}
////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const Dragger& dragger, const osg::CopyOp& copyop )
    :
    osg::AutoTransform( dragger, copyop ),
    m_transformationType( dragger.m_transformationType ),
    m_vectorSpace( dragger.m_vectorSpace ),
    m_axisDirection( dragger.m_axisDirection ),
    m_enabled( dragger.m_enabled ),
    m_comboForm( dragger.m_comboForm ),
    m_startProjectedPoint( dragger.m_startProjectedPoint ),
    m_endProjectedPoint( dragger.m_endProjectedPoint ),
    m_rootDragger( dragger.m_rootDragger ),
    m_deltaRotation( dragger.m_deltaRotation ),
    m_deltaTranslation( dragger.m_deltaTranslation ),
    m_deltaScale( dragger.m_deltaScale ),
    m_isRootDragger( dragger.m_isRootDragger ),
    m_colorMap( dragger.m_colorMap ),
    m_color( dragger.m_color ),
    m_physicsSimulator( *vxs::PhysicsSimulator::instance() ),
    m_sceneManager( *vxs::SceneManager::instance() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Dragger::~Dragger()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::accept( osg::NodeVisitor& nv )
{
    if( !nv.validNodeMask( *this ) )
    {
        return;
    }

    if( !m_isRootDragger && ( getAutoRotateMode() == NO_ROTATION ) )
    {
        //Now do the proper accept
        osg::Transform::accept( nv );

        return;
    }

    if( nv.getVisitorType() == osg::NodeVisitor::CULL_VISITOR )
    {
        osg::CullStack* cs = dynamic_cast< osg::CullStack* >( &nv );
        if( cs )
        {
            /*
            osg::Viewport::value_type width = _previousWidth;
            osg::Viewport::value_type height = _previousHeight;

            osg::Viewport* viewport = cs->getViewport();
            if( viewport )
            {
                width = viewport->width();
                height = viewport->height();
            }
            */

            osg::Vec3d eyePoint = cs->getEyeLocal();
            osg::Vec3d localUp = cs->getUpLocal();
            osg::Vec3d position = getPosition();

            const osg::Matrix& projection = *(cs->getProjectionMatrix());

            bool doUpdate = _firstTimeToInitEyePoint;
            if( !_firstTimeToInitEyePoint )
            {
                osg::Vec3d dv = _previousEyePoint - eyePoint;
                if( dv.length2() >
                    getAutoUpdateEyeMovementTolerance() *
                    ( eyePoint - position ).length2() )
                {
                    doUpdate = true;
                }
                osg::Vec3d dupv = _previousLocalUp - localUp;
                //Rotating the camera only affects ROTATE_TO_*
                if( _autoRotateMode &&
                    dupv.length2() > getAutoUpdateEyeMovementTolerance() )
                {
                    doUpdate = true;
                }
                /*
                else if( width != _previousWidth || height != _previousHeight )
                {
                    doUpdate = true;
                }
                */
                else if( projection != _previousProjection )
                {
                    doUpdate = true;
                }
                else if( position != _previousPosition )
                {
                    doUpdate = true;
                }
            }
            _firstTimeToInitEyePoint = false;

            if( doUpdate )
            {
                if( getAutoScaleToScreen() )
                {
                    //Need code to set pixel size based off distance from screen
                    //Flat screen mode
                    /*

                    */
                    //This is code to set pixel size based off distance from eye
                    //Cave mode
                    osg::Matrixd modelViewMatrix = *(cs->getModelViewMatrix());
                    modelViewMatrix.invert( modelViewMatrix );
                    osg::Vec3d eye = modelViewMatrix.getTrans() - getPosition();
                    eye = osg::Vec3d( 0.0, 1.0, 0.0 ) * eye.length();
                    osg::Vec3d center( 0.0, 0.0, 0.0 );
                    osg::Matrixd scaleView = osg::Matrixd::lookAt(
                        eye, center, osg::Vec3d( 0.0, 0.0, 1.0 ) );
                    osg::Matrixd mvpwMatrix = scaleView *
                        *(cs->getProjectionMatrix()) * cs->getWindowMatrix();
                    osg::Vec3d ps = center * mvpwMatrix;
                    osg::Vec3d pe = osg::Vec3d( 1.0, 0.0, 0.0 ) * mvpwMatrix;
                    double size = 1.0 / ( pe - ps ).length();
                    /*
                    if( _autoScaleTransitionWidthRatio > 0.0 )
                    {
                        if( _minimumScale > 0.0 )
                        {
                            double j = _minimumScale;
                            double i = ( _maximumScale < DBL_MAX ) ? 
                                _minimumScale + ( _maximumScale - _minimumScale ) *
                                _autoScaleTransitionWidthRatio :
                                _minimumScale * ( 1.0 + _autoScaleTransitionWidthRatio );
                            double c = 1.0 / ( 4.0 * ( i - j ) );
                            double b = 1.0 - 2.0 * c * i;
                            double a = j + b * b / ( 4.0 * c );
                            double k = -b / ( 2.0 * c );

                            if( size < k ) size = _minimumScale;
                            else if( size < i ) size = a + b * size + c * ( size * size );
                        }

                        if( _maximumScale < DBL_MAX )
                        {
                            double n = _maximumScale;
                            double m = ( _minimumScale > 0.0 ) ?
                                _maximumScale + ( _minimumScale-_maximumScale ) *
                                _autoScaleTransitionWidthRatio :
                                _maximumScale * ( 1.0 - _autoScaleTransitionWidthRatio );
                            double c = 1.0 / ( 4.0 * ( m - n ) );
                            double b = 1.0 - 2.0 * c * m;
                            double a = n + b * b / ( 4.0 * c );
                            double p = -b / ( 2.0 * c );

                            if( size > p ) size = _maximumScale;
                            else if( size > m ) size = a + b * size + c * ( size * size );
                        }
                    }
                    */

                    setScale( m_scale * size );
                }

                if( _autoRotateMode == ROTATE_TO_SCREEN )
                {
                    osg::Vec3d translation;
                    osg::Quat rotation;
                    osg::Vec3d scale;
                    osg::Quat so;
                    cs->getModelViewMatrix()->decompose(
                        translation, rotation, scale, so );

                    setRotation( rotation.inverse() );
                }
                else if( _autoRotateMode == ROTATE_TO_CAMERA )
                {
                    osg::Vec3d PosToEye = getPosition() - eyePoint;
                    osg::Matrix lookto = osg::Matrix::lookAt(
                        //osg::Vec3d( 0.0, 0.0, 0.0 ), -eyePoint, localUp );
                        osg::Vec3d( 0.0, 0.0, 0.0 ), PosToEye, localUp );
                    osg::Quat q;
                    q.set( osg::Matrix::inverse( lookto ) );
                    setRotation( q );
                }

                _previousEyePoint = eyePoint;
                _previousLocalUp = localUp;
                //_previousWidth = width;
                //_previousHeight = height;
                _previousProjection = projection;
                _previousPosition = position;

                _matrixDirty = true;
            }
        }
    }

    //Now do the proper accept
    osg::Transform::accept( nv );
}
////////////////////////////////////////////////////////////////////////////////
TranslateAxis* Dragger::AsTranslateAxis()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
TranslatePlane* Dragger::AsTranslatePlane()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
TranslatePan* Dragger::AsTranslatePan()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Rotate* Dragger::AsRotate()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
RotateAxis* Dragger::AsRotateAxis()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
RotateTwist* Dragger::AsRotateTwist()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
ScaleAxis* Dragger::AsScaleAxis()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
ScaleUniform* Dragger::AsScaleUniform()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
CompoundDragger* Dragger::AsCompoundDragger()
{
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::isSameKindAs( const osg::Object* obj ) const
{
    return dynamic_cast< const Dragger* >( obj ) != NULL;
}
////////////////////////////////////////////////////////////////////////////////
const char* Dragger::className() const
{
    return "Dragger";
}
////////////////////////////////////////////////////////////////////////////////
const char* Dragger::libraryName() const
{
    return "ves::xplorer::scenegraph::manipulator";
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::ComboForm()
{
    if( m_comboForm )
    {
        return;
    }

    m_comboForm = true;
}
////////////////////////////////////////////////////////////////////////////////
bool Dragger::Connect( osg::Transform* activeAssociation )
{
    //Store the active association
    m_activeAssociation = activeAssociation;

    //If we have a physics enabled transform then we need to add a 
    //constraint for non static objects
    osgBullet::AbsoluteModelTransform* amt =
        dynamic_cast< osgBullet::AbsoluteModelTransform* >( activeAssociation );
    if( !amt )
    {
        //Associate transform with this dragger
        m_associationSet.insert( activeAssociation );

        return true;
    }

    //If this is the root dragger, create point constraint
    if( m_isRootDragger )
    {
        osgBullet::RigidBody* rb =
            static_cast< osgBullet::RigidBody* >( amt->getUserData() );
        btRigidBody* btRB = rb->getRigidBody();
        if( !btRB )
        {
            //Error output
            return false;
        }

        CreatePointConstraint( *btRB );
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::DefaultForm()
{
    if( !m_comboForm )
    {
        return;
    }

    m_comboForm = false;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Disconnect()
{
    if( m_isRootDragger )
    {
        ClearPointConstraint();
    }

    m_associationSet.clear();
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Enable( const bool& enable )
{
    m_enabled = enable;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Drag( const osgUtil::LineSegmentIntersector& deviceInput )
{
    //Get the end projected point
    ComputeProjectedPoint( deviceInput, m_endProjectedPoint );

    //Compute the delta transform for the drag
    ComputeDeltaTransform();

    //Update all associated matrices with the delta transform
    UpdateAssociations();

    //Custom Event::Drag functionality
    CustomDragAction();

    //Reset
    m_startProjectedPoint = m_endProjectedPoint;

    return this;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Focus( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        //Use focused color if this is focused
        UseColor( Color::FOCUS );

        //Custom Event::Focus functionality
        CustomFocusAction();

        //This is in the node path
        return this;
    }

    //Use default color if this is not focused
    UseColor( Color::DEFAULT );

    //This is not in the node path
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
const TransformationType::Enum Dragger::GetTransformationType() const
{
    return m_transformationType;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Push(
    const osgUtil::LineSegmentIntersector& deviceInput,
    const osg::NodePath& np,
    osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        //Tell root dragger to stop auto scaling
        m_rootDragger->setAutoScaleToScreen( false );

        //Use active color if this is active
        UseColor( Color::ACTIVE );

        //Compute local to world and world to local matrices for this
        m_localToWorld = osg::computeLocalToWorld( np );
        m_worldToLocal = osg::Matrix::inverse( m_localToWorld );

        //Compute the associated ltw and wtl matrices
        ComputeAssociationMatrices();

        //Get the start projected point
        ComputeProjectedPoint( deviceInput, m_startProjectedPoint );

        //Custom Event::Push functionality
        CustomPushAction();

        //This is in the node path
        return this;
    }

    //Hide if this is not active
    Hide();

    //This is not in the node path
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        //Tell root dragger to auto scale
        m_rootDragger->setAutoScaleToScreen( true );
        //Force update now on release event for this frame
        m_rootDragger->setAutoRotateMode( m_rootDragger->getAutoRotateMode() );

        //Use default color if this is active
        UseColor( Color::DEFAULT );

        //Custom Event::Release functionality
        CustomReleaseAction();

        //Clear the associated matrices
        m_associationMatricesMap.clear();

        //Reset the start and end points
        m_startProjectedPoint.set( 0.0, 0.0, 0.0 );
        m_endProjectedPoint.set( 0.0, 0.0, 0.0 );

        //This is in the node path
        return this;
    }

    //Show if this is not active
    Show();

    //This is not in the node path
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::ComputeAssociationMatrices()
{
    //Compute local to world and world to local matrices for associated transforms
    AssociationSet::const_iterator itr = m_associationSet.begin();
    for( itr; itr != m_associationSet.end(); ++itr )
    {
        LocalToWorldNodePath nodePath(
            *itr, SceneManager::instance()->GetModelRoot() );
        LocalToWorldNodePath::NodeAndPathList npl =
            nodePath.GetLocalToWorldNodePath();
        LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );

        osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
        osg::Matrixd worldToLocal = osg::Matrixd::inverse( localToWorld );
        m_associationMatricesMap[ *itr ] =
            std::make_pair( localToWorld, worldToLocal );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::CreateDefaultShader()
{
    //Create the shader used to render the dragger
    std::string fragmentSource =
    "uniform vec4 color; \n"

    "void main() \n"
    "{ \n"
        "gl_FragData[ 0 ] = color; \n"
        "gl_FragData[ 1 ] = vec4( 0.0, 0.0, 0.0, 1.0 ); \n"
    "} \n";

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragmentShader.get() );

    osg::ref_ptr< osg::StateSet > stateSet = getOrCreateStateSet();
    stateSet->setAttributeAndModes( program.get(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    stateSet->addUniform( m_color.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::UpdateAssociations()
{
    //Update physics first
    ConstraintMap::const_iterator cmItr = (*m_constraintMap).begin();
    for( cmItr; cmItr != (*m_constraintMap).end(); ++cmItr )
    {
        btRigidBody* btRB = cmItr->first;
        if( btRB->isStaticObject() || btRB->isKinematicObject() )
        {
            continue;
        }

        const bool physicsOn = !m_physicsSimulator.GetIdle();

        osgBullet::MotionState* ms = static_cast< osgBullet::MotionState* >(
            btRB->getMotionState() );
        btTransform currentMatrix;
        ms->getWorldTransform( currentMatrix );
        if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
        {
            btVector3 deltaTranslation(
                m_deltaTranslation.x(),
                m_deltaTranslation.y(),
                m_deltaTranslation.z() );

            currentMatrix.setOrigin(
                deltaTranslation + currentMatrix.getOrigin() );

            btPoint2PointConstraint* p2p = cmItr->second;
            if( p2p )
            {
                //Move the constraint pivot
                const btVector3& currentLocation = p2p->getPivotInB();
                btVector3 newPos = currentLocation + deltaTranslation;
                p2p->setPivotB( newPos );
            }
        }
        else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
        {
            btQuaternion deltaRotation(
                m_deltaRotation.x(), m_deltaRotation.y(),
                m_deltaRotation.z(), m_deltaRotation.w() );
            currentMatrix.setRotation(
                deltaRotation * currentMatrix.getRotation() );
        }

        if( physicsOn )
        {
            btRB->setWorldTransform( currentMatrix );
            btRB->setInterpolationWorldTransform( currentMatrix );
        }

        ms->setWorldTransform( currentMatrix );
    }

    //Set all associated transforms
    AssociationSet::const_iterator asItr = m_associationSet.begin();
    for( asItr; asItr != m_associationSet.end(); ++asItr )
    {
        osg::Transform* transform = *asItr;
        AssociationMatricesMap::const_iterator ammItr =
            m_associationMatricesMap.find( transform );
        if( ammItr == m_associationMatricesMap.end() )
        {
            //Error output
            continue;
        }
        const std::pair< osg::Matrixd, osg::Matrixd >& matrices = ammItr->second;

        //Test for PATs 1st
        osg::PositionAttitudeTransform* pat =
            transform->asPositionAttitudeTransform();
        if( pat )
        {
            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                /*
                osg::Vec3d translation =
                    pat->getPosition() +
                    matrices.first.getTrans() +
                    m_deltaTranslation +
                    matrices.second.getTrans();
                pat->setPosition( translation );
                */
                osg::Vec3d position = pat->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                pat->setPosition( position );
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                osg::Quat rotation =
                    pat->getAttitude() *
                    matrices.first.getRotate() *
                    m_deltaRotation *
                    matrices.second.getRotate();
                pat->setAttitude( rotation );
            }
            else if( m_transformationType & TransformationType::SCALE_COMPOUND )
            {
                osg::Vec3d scale = pat->getScale();
                scale = scale * matrices.first;
                scale += m_deltaScale;
                scale = scale * matrices.second;
                pat->setScale( scale );
            }

            if( m_rootDragger )
            {
                vxs::DCS* tempDCS = dynamic_cast< vxs::DCS* >( pat );
                if( tempDCS )
                {
                    UpdateConductorData();
                }
            }
            
            continue;
        }

        //Test for MTs 2nd
        osg::MatrixTransform* mt = transform->asMatrixTransform();
        if( mt )
        {
            const osg::Matrix& currentMatrix = mt->getMatrix();
            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                ;
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                ;
            }
            else if( m_transformationType & TransformationType::SCALE_COMPOUND )
            {
                ;
            }

            continue;
        }

        //Test for ATs 3rd
        osg::AutoTransform* at =
            dynamic_cast< osg::AutoTransform* >( transform );
        if( at )
        {
            if( m_transformationType & TransformationType::TRANSLATE_COMPOUND )
            {
                osg::Vec3d position = at->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                at->setPosition( position );
            }
            else if( m_transformationType & TransformationType::ROTATE_COMPOUND )
            {
                osg::Quat rotation = at->getRotation();
                rotation *= matrices.first.getRotate();
                rotation *= m_deltaRotation;
                rotation *= matrices.second.getRotate();
                at->setRotation( rotation );
            }
            else if( m_transformationType & TransformationType::SCALE_COMPOUND )
            {
                ;
            }

            continue;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Vec4& Dragger::GetColor( Color::Enum colorTag )
{
    std::map< Color::Enum, osg::Vec4 >::iterator itr =
        m_colorMap.find( colorTag );
    /*
    if( itr == m_colorMap.end() )
    {
        //error handling, but shouldn't need it since we know colors
    }
    */

    return itr->second;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Plane Dragger::GetPlane( const bool& parallel ) const
{
    //The unit plane
    //| i   j   k  |
    //| Ax  Ay  Az |
    //| Bx  By  Bz |
    //A x B = ( AyBz - ByAz )i + ( BxAz - AxBz )j + ( AxBy - BxAy )k
    //P1 = ( 0, 0, 0 ) : P2 = ( 1, 0, 0 ) : P3 = ( 0, 0, 1 )
    //N = P1P2 x P1P3 = ( 1, 0, 0 ) x ( 0, 0, 1 ) = ( 0, -1, 0 )
    //-y + d = 0 : d = 0

    osg::Plane plane;
    if( parallel )
    {
        plane.set(
            m_rootDragger->GetPreviousEyePoint() - m_rootDragger->getPosition(),
            m_rootDragger->getPosition() );
    }
    else
    {
        plane.set( GetUnitAxis(), 0.0 );
        plane.transformProvidingInverse( m_worldToLocal );
    }

    return plane;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetAxis(
    const bool& zero, const bool& premultiply ) const
{
    osg::Vec3d axis( 0.0, 0.0, 0.0 );
    if( !zero )
    {
        axis = GetUnitAxis();
    }

    if( premultiply )
    {
        return axis * m_localToWorld;
    }

    return m_localToWorld * axis;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetPreviousEyePoint() const
{
    return _previousEyePoint;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetPreviousLocalUp() const
{
    return _previousLocalUp;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d Dragger::GetUnitAxis() const
{
    osg::Vec3d unitAxis( 0.0, 0.0, 1.0 );

    return unitAxis;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Plane Dragger::GetUnitPlane() const
{
    osg::Plane plane( GetUnitAxis(), 0.0 );

    return plane;
}
////////////////////////////////////////////////////////////////////////////////
const VectorSpace::Enum& Dragger::GetVectorSpace() const
{
    return m_vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
const bool& Dragger::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetAxisDirection( const AxisDirection::Enum& axisDirection )
{
    m_axisDirection = axisDirection;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetColor( Color::Enum colorTag, osg::Vec4 newColor, bool use )
{
    osg::Vec4& color = GetColor( colorTag );
    if( color == newColor )
    {
        return;
    }

    color = newColor;

    if( use )
    {
        UseColor( colorTag );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetConstraintMap( ConstraintMap& constraintMap )
{
    m_constraintMap = &constraintMap;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetRootDragger( Dragger* rootDragger )
{
    if( this != rootDragger )
    {
        m_rootDragger = rootDragger;
        m_isRootDragger = false;
    }
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetScale( const double scale )
{
    osg::Vec3d temp( scale, scale, scale );
    SetScale( temp );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetScale( const osg::Vec3d& scale )
{
    m_scale = scale;

    //Force scale update
    setAutoRotateMode( getAutoRotateMode() );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::SetVectorSpace( const VectorSpace::Enum& vectorSpace )
{
    m_vectorSpace = vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Hide()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::Show()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::UseColor( Color::Enum colorTag )
{
    m_color->set( GetColor( colorTag ) );
}
////////////////////////////////////////////////////////////////////////////////
/*
void Dragger::ResetPhysics()
{
    AssociationSet::const_iterator itr = m_associationSet.begin();
    for( itr; itr != m_associationSet.end(); ++itr )
    {
        osg::Transform* transform = *itr;
        
        //Test for PATs 1st
        osg::PositionAttitudeTransform* pat =
            transform->asPositionAttitudeTransform();
        if( pat )
        {
            continue;
        }
        
        //Test for AMTs 2nd
        osgBullet::AbsoluteModelTransform* amt =
            dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform );
        if( amt )
        {
            osgBullet::RigidBody* rb = 
            static_cast< osgBullet::RigidBody* >( amt->getUserData() );
            btRigidBody* btRB = rb->getRigidBody();
            if( btRB->isStaticObject() )
            {
                continue;
            }
            
            btRB->setGravity( btVector3( 0, 0, -32.174 ) );
            //btRB->setGravity( btVector3( 0, 0, 0 ) );
            continue;
        }
        
        //Test for MTs 3rd
        osg::MatrixTransform* mt = transform->asMatrixTransform();
        if( mt )
        {
            continue;
        }
        
        //Test for ATs 4th
        osg::AutoTransform* at =
            dynamic_cast< osg::AutoTransform* >( transform );
        if( at )
        {
            continue;
        }
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
const bool Dragger::CreatePointConstraint( btRigidBody& btRB )
{
    btRB.setActivationState( DISABLE_DEACTIVATION );

    btPoint2PointConstraint* p2p =
        new btPoint2PointConstraint( btRB, btVector3( 0.0, 0.0, 0.0 ) );
    //Very weak constraint for picking
    p2p->m_setting.m_tau = 0.1;

    m_physicsSimulator.GetDynamicsWorld()->addConstraint( p2p );
    (*m_constraintMap)[ &btRB ] = p2p;

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::ClearPointConstraint()
{
    ConstraintMap::const_iterator itr = (*m_constraintMap).begin();
    for( itr; itr != (*m_constraintMap).end(); ++itr )
    {
        btPoint2PointConstraint* p2p = itr->second;
        if( p2p )
        {
            m_physicsSimulator.GetDynamicsWorld()->removeConstraint( p2p );
            delete p2p;
            p2p = NULL;

            btRigidBody* btRB = itr->first;
            btRB->forceActivationState( ACTIVE_TAG );
            btRB->setDeactivationTime( 0.0 );
            btRB = NULL;
        }
    }

    (*m_constraintMap).clear();
}
////////////////////////////////////////////////////////////////////////////////
void Dragger::UpdateConductorData()
{
    ///Get command
    ves::open::xml::CommandPtr modelUpdateData( new ves::open::xml::Command() );
    modelUpdateData->SetCommandName( "MODEL_DATA_UPDATE" );

    //Create the uuid for the model of the transform
    ves::open::xml::DataValuePairPtr parentSystemId( new ves::open::xml::DataValuePair() );
    parentSystemId->SetData( "PARENT_SYSTEM_ID", "1234" );
    modelUpdateData->AddDataValuePair( parentSystemId );
    
    //Create the uuid for the plugin of the transform
    ves::open::xml::DataValuePairPtr pluginId( new ves::open::xml::DataValuePair() );
    pluginId->SetData( "PLUGIN_ID", "1234" );
    modelUpdateData->AddDataValuePair( pluginId );

    ves::open::xml::CommandPtr transformData( new ves::open::xml::Command() );
    transformData->SetCommandName( "MODEL_DATA" );

    ves::open::xml::DataValuePairPtr cadTransformDVP( new ves::open::xml::DataValuePair() );
    cadTransformDVP->SetData( "CAD_TRANSFORM", "1234" );//transformPtr );
    transformData->AddDataValuePair( cadTransformDVP );

    ves::open::xml::DataValuePairPtr cadIdDVP( new ves::open::xml::DataValuePair() );
    cadIdDVP->SetData( "CAD_ID", "1234" );
    transformData->AddDataValuePair( cadIdDVP );

    ves::open::xml::DataValuePairPtr pluginDataDVP( new ves::open::xml::DataValuePair() );
    pluginDataDVP->SetData( "PLUGIN_DATA", transformData );

    modelUpdateData->AddDataValuePair( pluginDataDVP );

    //ves::xplorer::CommandHandler::instance()->SetXMLCommand( modelUpdateData );
}
////////////////////////////////////////////////////////////////////////////////

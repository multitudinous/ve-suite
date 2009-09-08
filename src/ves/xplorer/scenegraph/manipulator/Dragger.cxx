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

// --- OSG Includes --- //
#include <osg/PositionAttitudeTransform>
#include <osg/MatrixTransform>
#include <osg/AutoTransform>
#include <osg/io_utils>
#include <osgUtil/CullVisitor>

// --- osgBullet Includes --- //
#include <osgBullet/AbsoluteModelTransform.h>
#include <osgBullet/MotionState.h>
#include <osgBullet/RigidBody.h>
#include <osgBullet/Utils.h>

using namespace ves::xplorer::scenegraph::manipulator;
namespace vxs = ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Dragger::Dragger( const TransformationType::Enum& transformationType )
    :
    osg::AutoTransform(),
    m_transformationType( transformationType ),
    m_vectorSpace( VectorSpace::GLOBAL ),
    m_enabled( false ),
    m_comboForm( false ),
    m_startProjectedPoint( 0.0, 0.0, 0.0 ),
    m_endProjectedPoint( 0.0, 0.0, 0.0 ),
    m_rootDragger( NULL ),
    m_color( NULL )
{
    m_rootDragger = this;

    m_colorMap[ Color::DEFAULT ] = osg::Vec4f( 1.0, 0.0, 0.0, 1.0 );
    m_colorMap[ Color::FOCUS ] = osg::Vec4f( 1.0, 1.0, 0.0, 1.0 );
    m_colorMap[ Color::ACTIVE ] = osg::Vec4f( 0.7, 0.7, 0.7, 1.0 );
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
    m_enabled( dragger.m_enabled ),
    m_comboForm( dragger.m_comboForm ),
    m_startProjectedPoint( dragger.m_startProjectedPoint ),
    m_endProjectedPoint( dragger.m_endProjectedPoint ),
    m_rootDragger( dragger.m_rootDragger ),
    m_colorMap( dragger.m_colorMap ),
    m_color( dragger.m_color )
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

    if( ( this != m_rootDragger ) && ( getAutoRotateMode() == NO_ROTATION ) )
    {
        //Now do the proper accept
        osg::Transform::accept( nv );

        return;
    }

    //If app traversal update the frame count
    if( nv.getVisitorType() == osg::NodeVisitor::UPDATE_VISITOR )
    {
        ;
    }
    else if( nv.getVisitorType() == osg::NodeVisitor::CULL_VISITOR )
    {
        osg::CullStack* cs = dynamic_cast< osg::CullStack* >( &nv );
        if( cs )
        {
            osg::Viewport::value_type width = _previousWidth;
            osg::Viewport::value_type height = _previousHeight;

            osg::Viewport* viewport = cs->getViewport();
            if( viewport )
            {
                width = viewport->width();
                height = viewport->height();
            }

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
                else if( width != _previousWidth || height != _previousHeight )
                {
                    doUpdate = true;
                }
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
                        osg::Vec3d( 0.0, 0.0, 0.0 ), PosToEye, localUp );
                    osg::Quat q;
                    q.set( osg::Matrix::inverse( lookto ) );
                    setRotation( q );
                }

                _previousEyePoint = eyePoint;
                _previousLocalUp = localUp;
                _previousWidth = width;
                _previousHeight = height;
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
osg::Object* Dragger::clone( const osg::CopyOp& copyop ) const
{
    return new Dragger( *this, copyop );
}
////////////////////////////////////////////////////////////////////////////////
osg::Object* Dragger::cloneType() const
{
    return new Dragger( m_transformationType );
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

    //Associate transform with this dragger
    m_associationSet.insert( activeAssociation );

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
    if( !ComputeProjectedPoint( deviceInput, m_endProjectedPoint ) )
    {
        return this;
    }

    ComputeDeltaTransform();

    UpdateAssociations();

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
        UseColor( Color::FOCUS );

        return this;
    }

    UseColor( Color::DEFAULT );

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
        UseColor( Color::ACTIVE );

        //Compute local to world and world to local matrices for dragger
        m_localToWorld = osg::computeLocalToWorld( np );
        m_worldToLocal = osg::Matrix::inverse( m_localToWorld );

        //Get the start projected point
        if( !ComputeProjectedPoint( deviceInput, m_startProjectedPoint ) )
        {
            return this;
        }

        //Compute the association matrices
        ComputeAssociationMatrices();

        m_rootDragger->setAutoScaleToScreen( false );

        return this;
    }

    Hide();

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
Dragger* Dragger::Release( osg::NodePath::iterator& npItr )
{
    //Get the active dragger
    osg::Node* node = *npItr;
    if( this == node )
    {
        UseColor( Color::DEFAULT );

        //Clear the associated matrices
        m_associationMatricesMap.clear();

        m_rootDragger->setAutoScaleToScreen( true );
        //Force update now on release event for this frame
        m_rootDragger->setAutoRotateMode( m_rootDragger->getAutoRotateMode() );

        return this;
    }

    Show();

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
    //If no associations, return
    if( m_associationSet.empty() )
    {
        //Error output
        return;
    }

    //Set all associated transforms
    AssociationSet::const_iterator itr = m_associationSet.begin();
    for( itr; itr != m_associationSet.end(); ++itr )
    {
        osg::Transform* transform = *itr;
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
            switch( m_transformationType )
            {
            case TransformationType::TRANSLATE_AXIS:
            case TransformationType::TRANSLATE_PAN:
            {
                osg::Vec3d position = pat->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                pat->setPosition( position );

                break;
            }
            case TransformationType::ROTATE_AXIS:
            case TransformationType::ROTATE_TWIST:
            {
                osg::Quat rotation =
                    pat->getAttitude() *
                    matrices.first.getRotate() *
                    m_deltaRotation *
                    matrices.second.getRotate();
                pat->setAttitude( rotation );

                break;
            }
            default:
            {
                break;
            }
            } //end switch( m_transformationType )

            continue;
        }

        //Test for AMTs 2nd
        osgBullet::AbsoluteModelTransform* amt =
            dynamic_cast< osgBullet::AbsoluteModelTransform* >( transform );
        if( amt )
        {
            osgBullet::RigidBody* rb = static_cast< osgBullet::RigidBody* >(
                amt->getUserData() );
            btRigidBody* btRB = rb->getRigidBody();
            if( btRB->isStaticObject() )
            {
                continue;
            }

            osgBullet::MotionState* ms = static_cast< osgBullet::MotionState* >(
                btRB->getMotionState() );
            btTransform currentMatrix;
            ms->getWorldTransform( currentMatrix );
            switch( m_transformationType )
            {
            case TransformationType::TRANSLATE_AXIS:
            case TransformationType::TRANSLATE_PAN:
            {
                btVector3 deltaTranslation(
                    m_deltaTranslation.x(),
                    m_deltaTranslation.y(),
                    m_deltaTranslation.z() );
                currentMatrix.setOrigin(
                    deltaTranslation + currentMatrix.getOrigin() );

                break;
            }
            case TransformationType::ROTATE_AXIS:
            case TransformationType::ROTATE_TWIST:
            {
                btQuaternion deltaRotation(
                    m_deltaRotation.x(), m_deltaRotation.y(),
                    m_deltaRotation.z(), m_deltaRotation.w() );
                currentMatrix.setRotation(
                    deltaRotation * currentMatrix.getRotation() );

                break;
            }
            default:
            {
                break;
            }
            } //end switch( m_transformationType )

            btRB->setWorldTransform( currentMatrix );
            btRB->setInterpolationWorldTransform( currentMatrix );
            btRB->activate();
            btRB->setLinearVelocity( btVector3( 0.0, 0.0, 0.0 ) );
            btRB->setAngularVelocity( btVector3( 0.0, 0.0, 0.0 ) );

            continue;
        }

        //Test for MTs 3rd
        osg::MatrixTransform* mt = transform->asMatrixTransform();
        if( mt )
        {
            const osg::Matrix& currentMatrix = mt->getMatrix();
            switch( m_transformationType )
            {
            case TransformationType::TRANSLATE_AXIS:
            case TransformationType::TRANSLATE_PAN:
            {
                mt->setMatrix(
                    matrices.first *
                    osg::Matrix::translate( m_deltaTranslation ) *
                    currentMatrix *
                    matrices.second );

                break;
            }
            default:
            {
                break;
            }
            } //end switch( m_transformationType )

            continue;
        }

        //Test for ATs 4th
        osg::AutoTransform* at =
            dynamic_cast< osg::AutoTransform* >( transform );
        if( at )
        {
            switch( m_transformationType )
            {
            case TransformationType::TRANSLATE_AXIS:
            case TransformationType::TRANSLATE_PAN:
            {
                osg::Vec3d position = at->getPosition();
                position = position * matrices.first;
                position += m_deltaTranslation;
                position = position * matrices.second;
                at->setPosition( position );

                break;
            }
            case TransformationType::ROTATE_AXIS:
            case TransformationType::ROTATE_TWIST:
            {
                osg::Quat rotation = at->getRotation();
                rotation *= matrices.first.getRotate();
                rotation *= m_deltaRotation;
                rotation *= matrices.second.getRotate();
                at->setRotation( rotation );

                break;
            }
            default:
            {
                break;
            }
            } //end switch( m_transformationType )

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
const osg::Plane Dragger::GetPlane( const bool& transform ) const
{
    //The unit plane
    //| i   j   k  |
    //| Ax  Ay  Az |
    //| Bx  By  Bz |
    //A x B = ( AyBz - ByAz )i + ( BxAz - AxBz )j + ( AxBy - BxAy )k
    //P1 = ( 0, 0, 0 ) : P2 = ( 1, 0, 0 ) : P3 = ( 0, 0, 1 )
    //N = P1P2 x P1P3 = ( 1, 0, 0 ) x ( 0, 0, 1 ) = ( 0, -1, 0 )
    //-y + d = 0 : d = 0

    osg::Plane plane( GetUnitAxis(), 0.0 );
    if( transform )
    {
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
const osg::Vec3d Dragger::GetUnitAxis() const
{
    osg::Vec3d unitAxis( 0.0, 0.0, 1.0 );

    return unitAxis;
}
////////////////////////////////////////////////////////////////////////////////
const VectorSpace::Enum& Dragger::GetVectorSpace() const
{
    return m_vectorSpace;
}
////////////////////////////////////////////////////////////////////////////////
const bool Dragger::IsCompound() const
{
    return false;
}
////////////////////////////////////////////////////////////////////////////////
const bool& Dragger::IsEnabled() const
{
    return m_enabled;
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
void Dragger::SetRootDragger( Dragger* rootDragger )
{
    m_rootDragger = rootDragger;
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

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
#include <ves/xplorer/scenegraph/AutoTransform.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <gmtl/Vec.h>
#include <gmtl/Point.h>
#include <gmtl/Matrix.h>
#include <gmtl/Misc/MatrixConvert.h>
#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>

// --- OSG Includes --- //
#include <osg/CullStack>
#include <osg/Notify>
#include <osg/io_utils>
#include <osg/Matrix>

#include <osgUtil/CullVisitor>
#include <osgUtil/IntersectionVisitor>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;
using namespace gmtl;
////////////////////////////////////////////////////////////////////////////////
AutoTransform::AutoTransform()
    :
    osg::Transform(),
    m_autoRotateMode( NO_ROTATION ),
    m_autoScaleToScreen( false ),
    m_maximumScale( DBL_MAX ),
    m_minimumScale( 0.0 ),
    m_pivotPoint( 0.0, 0.0, 0.0 ),
    m_position( 0.0, 0.0, 0.0 ),
    _scale( 1.0, 1.0, 1.0 ),
    m_scale( 1.0, 1.0, 1.0 ),
    m_rotation( 0.0, 0.0, 0.0, 1.0 ),
    m_currentGLTransformInfo( GLTransformInfoPtr() )
{
    m_headPosition.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
AutoTransform::AutoTransform(
    const AutoTransform& autoTransform,
    const osg::CopyOp& copyop )
    :
    osg::Transform( autoTransform, copyop ),
    m_autoRotateMode( autoTransform.m_autoRotateMode ),
    m_autoScaleToScreen( autoTransform.m_autoScaleToScreen ),
    m_maximumScale( autoTransform.m_maximumScale ),
    m_minimumScale( autoTransform.m_minimumScale ),
    m_pivotPoint( autoTransform.m_pivotPoint ),
    m_position( autoTransform.m_position ),
    _scale( autoTransform._scale ),
    m_scale( autoTransform.m_scale ),
    m_rotation( autoTransform.m_rotation ),
    m_currentGLTransformInfo( GLTransformInfoPtr() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
AutoTransform::~AutoTransform()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingSphere AutoTransform::computeBound() const
{
    osg::BoundingSphere bsphere; //= osg::Transform::computeBound();

    return bsphere;
}
////////////////////////////////////////////////////////////////////////////////
bool AutoTransform::computeLocalToWorldMatrix(
    osg::Matrix& matrix, osg::NodeVisitor* nv ) const
{
    //If !nv, usually this function has been called by Transform::computeBound
    if( !nv )
    {
        //Do compute Bound stuff if desired in future
        //This may not work with multiple contexts
        std::cout << "AutoTransform::computeLocalToWorldMatrix: NULL nv"
                  << std::endl;

        return true;
    }

    //Grab transform variables as they exist
    osg::Vec3d position( m_position );
    osg::Quat rotation( m_rotation );
    osg::Vec3d scale( _scale );
    osg::Vec3d pivotPoint( m_pivotPoint );

    //Declare matrix state variables
    const osg::Matrixd* modelView( NULL );
    const osg::Matrixd* projection( NULL );
    osg::Matrixd window;

    //Set matrix state variables
    switch( nv->getVisitorType() )
    {
    case osg::NodeVisitor::NODE_VISITOR:
    {
        //osgUtil::IntersectionVisitor* iv =
            //dynamic_cast< osgUtil::IntersectionVisitor* >( nv );
        if( m_currentGLTransformInfo != GLTransformInfoPtr() )
        {
            modelView = &(m_currentGLTransformInfo->GetOSGModelViewMatrix());
            projection = &(m_currentGLTransformInfo->GetOSGProjectionMatrix());
            window = m_currentGLTransformInfo->GetOSGWindowMatrix();
        }

        break;
    }
    case osg::NodeVisitor::CULL_VISITOR:
    {
        osgUtil::CullVisitor* cv = dynamic_cast< osgUtil::CullVisitor* >( nv );
        osg::Camera* camera = cv->getCurrentCamera();
        if( camera )
        {
            modelView = &(camera->getViewMatrix());
            projection = &(camera->getProjectionMatrix());
            window = camera->getViewport()->computeWindowMatrix();
        }

        break;
    }
    default:
    {
        //Error output
        std::cout << "AutoTransform::computeLocalToWorldMatrix: "
                  << nv->getVisitorType() << std::endl;

        return true;
    }
    } //end switch( nv->getVisitorType() )

    osg::Vec3d eye, center, up;
    if( !modelView )
    {
        up.set( 0.0, 0.0, 1.0 );

        Matrix44d vjMat = gmtl::convertTo< double >( m_headPosition->getData() );
        gmtl::Point3d jugglerHeadPoint =
            gmtl::makeTrans< gmtl::Point3d >( vjMat );
        //Make it z up
        gmtl::Point3d jugglerHeadPointTrans( jugglerHeadPoint[ 0 ], -jugglerHeadPoint[ 2 ], jugglerHeadPoint[ 1 ] );
        
        ///Transform from juggler space to world space
        gmtl::Point3d worldWandMat =
            ves::xplorer::scenegraph::SceneManager::instance()->GetInvertedWorldDCS() * jugglerHeadPointTrans;
        eye.set( worldWandMat[ 0 ], worldWandMat[ 1 ], worldWandMat[ 2 ] );
    }
    else
    {
        modelView->getLookAt( eye, center, up );
    }

    if( m_autoScaleToScreen && modelView )
    {
        osg::Vec3d eyeVector;
        if( 1 )
        {
            double fLeft, fRight, fBottom, fTop, fNear, fFar;
            projection->getFrustum( fLeft, fRight, fBottom, fTop, fNear, fFar );
            osg::Matrixd ortho =
                osg::Matrixd::ortho2D(  fLeft, fRight, fBottom, fTop );
            osg::Matrixd mvpwMatrix = (*modelView) * ortho * window;

            osg::Vec3d screenPosition = m_position * mvpwMatrix;
            screenPosition.z() = 0.0;
            mvpwMatrix.invert( mvpwMatrix );
            screenPosition = screenPosition * mvpwMatrix;

            eyeVector = screenPosition - m_position;
        }
        else
        {
            eyeVector = eye - m_position;
        }

        eyeVector = osg::Vec3d( 0.0, 1.0, 0.0 ) * eyeVector.length();
        osg::Vec3d center( 0.0, 0.0, 0.0 );
        osg::Matrixd scaleView = osg::Matrixd::lookAt(
            eyeVector, center, osg::Vec3d( 0.0, 0.0, 1.0 ) );
        osg::Matrixd mvpwMatrix = scaleView * (*projection) * window;
        osg::Vec3d ps = center * mvpwMatrix;
        osg::Vec3d pe = osg::Vec3d( 1.0, 0.0, 0.0 ) * mvpwMatrix;
        double size = 1.0 / ( pe - ps ).length();
        scale = m_scale * size;
    }
    else
    {
        scale = m_scale;
    }

    switch( m_autoRotateMode )
    {
    case NO_ROTATION:
    {
        break;
    }
    case ROTATE_TO_SCREEN:
    {
        osg::Vec3d t, s;
        osg::Quat r, so;
        modelView->decompose( t, r, s, so );
        rotation = r.inverse();

        break;
    }
    case ROTATE_TO_CAMERA:
    {
        osg::Vec3d posToEye = m_position - eye;
        osg::Matrix lookto =
            osg::Matrix::lookAt( osg::Vec3d( 0.0, 0.0, 0.0 ), posToEye, up );
        rotation.set( osg::Matrix::inverse( lookto ) );

        break;
    }
    } //end switch( m_autoRotateMode )

    if( _referenceFrame == RELATIVE_RF )
    {
        matrix.preMultTranslate( position );
        matrix.preMultRotate( rotation );
        matrix.preMultScale( scale );
        matrix.preMultTranslate( -pivotPoint );
    }
    else //absolute
    {
        matrix.makeRotate( rotation );
        matrix.postMultTranslate( position );
        matrix.preMultScale( scale );
        matrix.preMultTranslate( -pivotPoint );
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool AutoTransform::computeWorldToLocalMatrix(
    osg::Matrix& matrix, osg::NodeVisitor* nv ) const
{
    if( _scale.x() == 0.0 || _scale.y() == 0.0 || _scale.z() == 0.0 )
    {
        return false;
    }

    if( _referenceFrame == RELATIVE_RF )
    {
        matrix.postMultTranslate( -m_position );
        matrix.postMultRotate( m_rotation.inverse() );
        matrix.postMultScale( osg::Vec3d( 1.0 /_scale.x(), 1.0 / _scale.y(), 1.0 / _scale.z() ) );
        matrix.postMultTranslate( m_pivotPoint );
    }
    else //absolute
    {
        matrix.makeRotate( m_rotation.inverse() );
        matrix.preMultTranslate( -m_position );
        matrix.postMultScale( osg::Vec3d( 1.0 / _scale.x(), 1.0 / _scale.y(), 1.0 / _scale.z() ) );
        matrix.postMultTranslate( m_pivotPoint );
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
AutoTransform::AutoRotateMode AutoTransform::GetAutoRotateMode() const
{
    return m_autoRotateMode;
}
////////////////////////////////////////////////////////////////////////////////
bool AutoTransform::GetAutoScaleToScreen() const
{
    return m_autoScaleToScreen;
}
////////////////////////////////////////////////////////////////////////////////
double AutoTransform::GetMaximumScale() const
{
    return m_maximumScale;
}
////////////////////////////////////////////////////////////////////////////////
double AutoTransform::GetMinimumScale() const
{
    return m_minimumScale;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d& AutoTransform::GetPivotPoint() const
{
    return m_pivotPoint;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d& AutoTransform::GetPosition() const
{
    return m_position;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Quat& AutoTransform::GetRotation() const
{
    return m_rotation;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d& AutoTransform::GetScale() const
{
    return _scale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetAutoRotateMode( AutoRotateMode mode )
{
    m_autoRotateMode = mode;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetAutoScaleToScreen( bool autoScaleToScreen )
{
    m_autoScaleToScreen = autoScaleToScreen;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetCurrentGLTransformInfo(
    GLTransformInfoPtr currentGLTransformInfo )
{
    m_currentGLTransformInfo = currentGLTransformInfo;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetMaximumScale( double maximumScale )
{
    m_maximumScale = maximumScale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetMinimumScale( double minimumScale )
{
    m_minimumScale = minimumScale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetPivotPoint( const osg::Vec3d& pivotPoint )
{
    m_pivotPoint = pivotPoint;

    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetPosition( const osg::Vec3d& position )
{
    m_position = position;

    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetRotation( const osg::Quat& rotation )
{
    m_rotation = rotation;

    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetScale( const double& scale )
{
    osg::Vec3d temp( scale, scale, scale );
    SetScale( temp );
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetScale( const osg::Vec3d& scale )
{
    m_scale = scale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::setScale( const double& scale )
{
    setScale( osg::Vec3d( scale, scale, scale ) );
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::setScale( const osg::Vec3d& scale )
{
    _scale = scale;

    if( _scale.x() < m_minimumScale )
    {
        _scale.x() = m_minimumScale;
    }
    if( _scale.y() < m_minimumScale )
    {
        _scale.y() = m_minimumScale;
    }
    if( _scale.z() < m_minimumScale )
    {
        _scale.z() = m_minimumScale;
    }

    if( _scale.x() > m_maximumScale )
    {
        _scale.x() = m_maximumScale;
    }
    if( _scale.y() > m_maximumScale )
    {
        _scale.y() = m_maximumScale;
    }
    if( _scale.z() > m_maximumScale )
    {
        _scale.z() = m_maximumScale;
    }

    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////

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

// --- OSG Includes --- //
#include <osg/CullStack>
#include <osg/Notify>
#include <osg/io_utils>

#include <osgUtil/CullVisitor>
#include <osgUtil/IntersectionVisitor>

// --- STL Includes --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
AutoTransform::AutoTransform()
    :
    osg::Transform(),
    _autoRotateMode( NO_ROTATION ),
    _autoScaleToScreen( false ),
    _firstTimeToInitEyePoint( true ),
    //_matrixDirty( true ),
    _maximumScale( DBL_MAX ),
    _minimumScale( 0.0 ),
    _previousEyePoint( 0.0, 0.0, 0.0 ),
    _previousLocalUp( 0.0, 0.0, 0.0 ),
    _pivotPoint( 0.0, 0.0, 0.0 ),
    _position( 0.0, 0.0, 0.0 ),
    _scale( 1.0, 1.0, 1.0 ),
    _rotation( 0.0, 0.0, 0.0, 1.0 ),
    m_currentGLTransformInfo( GLTransformInfoPtr() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
AutoTransform::AutoTransform(
    const AutoTransform& autoTransform,
    const osg::CopyOp& copyop )
    :
    osg::Transform( autoTransform, copyop ),
    _autoRotateMode( autoTransform._autoRotateMode ),
    _autoScaleToScreen( autoTransform._autoScaleToScreen ),
    _firstTimeToInitEyePoint( true ),
    //_matrixDirty( true ),
    _maximumScale( autoTransform._maximumScale ),
    _minimumScale( autoTransform._minimumScale ),
    _previousEyePoint( autoTransform._previousEyePoint ),
    _previousLocalUp( autoTransform._previousLocalUp ),
    _pivotPoint( autoTransform._pivotPoint ),
    _position( autoTransform._position ),
    _scale( autoTransform._scale ),
    _rotation( autoTransform._rotation ),
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
    osg::BoundingSphere bsphere;

    /*
    if( _autoScaleToScreen && _firstTimeToInitEyePoint )
    {
        return bsphere;
    }

    bsphere = osg::Transform::computeBound();
    */

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
        std::cout << "AutoTransform::computeLocalToWorldMatrix: NULL nv";
        std::cout << std::endl;

        return true;
    }

    //Grab transform variables as they exist
    osg::Vec3d position( _position );
    osg::Quat rotation( _rotation );
    osg::Vec3d scale( _scale );
    osg::Vec3d pivotPoint( _pivotPoint );

    //Declare matrix state variables
    const osg::Matrixd* modelView( NULL );
    const osg::Matrixd* projection( NULL );
    osg::Matrixd window;

    //Set matrix state variables
    switch( nv->getVisitorType() )
    {
    case osg::NodeVisitor::NODE_VISITOR:
    {
        osgUtil::IntersectionVisitor* iv =
            dynamic_cast< osgUtil::IntersectionVisitor* >( nv );
        if( iv && m_currentGLTransformInfo != GLTransformInfoPtr() )
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
        std::cout << "AutoTransform::computeLocalToWorldMatrix: ";
        std::cout << nv->getVisitorType();
        std::cout << std::endl;

        return true;
    }
    } //end switch( nv->getVisitorType() )

    if( modelView && projection && window != osg::Matrixd::identity() )
    {
        osg::Vec3d eye, center, up;
        modelView->getLookAt( eye, center, up );

        if( _autoScaleToScreen )
        {
            osg::Vec3d eyeVector;
            if( 1 )
            {
                double left, right, bottom, top, near, far;
                projection->getFrustum( left, right, bottom, top, near, far );
                osg::Matrixd ortho =
                    osg::Matrixd::ortho2D( left, right, bottom, top );
                osg::Matrixd mvpwMatrix = (*modelView) * ortho * window;

                osg::Vec3d screenPosition = _position * mvpwMatrix;
                screenPosition.z() = 0.0;
                mvpwMatrix.invert( mvpwMatrix );
                screenPosition = screenPosition * mvpwMatrix;

                eyeVector = screenPosition - _position;
            }
            else
            {
                eyeVector = eye - _position;
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

        switch( _autoRotateMode )
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
            osg::Vec3d posToEye = _position - eye;
            osg::Matrix lookto =
                osg::Matrix::lookAt( osg::Vec3d( 0.0, 0.0, 0.0 ), posToEye, up );
            rotation.set( osg::Matrix::inverse( lookto ) );

            break;
        }
        } //end switch( _autoRotateMode )
    }

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
        matrix.postMultTranslate( -_position );
        matrix.postMultRotate( _rotation.inverse() );
        matrix.postMultScale( osg::Vec3d( 1.0 /_scale.x(), 1.0 / _scale.y(), 1.0 / _scale.z() ) );
        matrix.postMultTranslate( _pivotPoint );
    }
    else //absolute
    {
        matrix.makeRotate( _rotation.inverse() );
        matrix.preMultTranslate( -_position );
        matrix.postMultScale( osg::Vec3d( 1.0 / _scale.x(), 1.0 / _scale.y(), 1.0 / _scale.z() ) );
        matrix.postMultTranslate( _pivotPoint );
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
AutoTransform::AutoRotateMode AutoTransform::GetAutoRotateMode() const
{
    return _autoRotateMode;
}
////////////////////////////////////////////////////////////////////////////////
bool AutoTransform::GetAutoScaleToScreen() const
{
    return _autoScaleToScreen;
}
////////////////////////////////////////////////////////////////////////////////
double AutoTransform::GetMaximumScale() const
{
    return _maximumScale;
}
////////////////////////////////////////////////////////////////////////////////
double AutoTransform::GetMinimumScale() const
{
    return _minimumScale;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d& AutoTransform::GetPivotPoint() const
{
    return _pivotPoint;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d& AutoTransform::GetPosition() const
{
    return _position;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d AutoTransform::GetPreviousEyePoint() const
{
    return _previousEyePoint;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d AutoTransform::GetPreviousLocalUp() const
{
    return _previousLocalUp;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Quat& AutoTransform::GetRotation() const
{
    return _rotation;
}
////////////////////////////////////////////////////////////////////////////////
const osg::Vec3d& AutoTransform::GetScale() const
{
    return _scale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetAutoRotateMode( AutoRotateMode mode )
{
    _autoRotateMode = mode;
    _firstTimeToInitEyePoint = true;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetAutoScaleToScreen( bool autoScaleToScreen )
{
    _autoScaleToScreen = autoScaleToScreen;
    //_matrixDirty = true;
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
    _maximumScale = maximumScale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetMinimumScale( double minimumScale )
{
    _minimumScale = minimumScale;
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetPivotPoint( const osg::Vec3d& pivotPoint )
{
    _pivotPoint = pivotPoint;
    //_matrixDirty = true;
    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetPosition( const osg::Vec3d& position )
{
    _position = position;
    //_matrixDirty = true;
    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void AutoTransform::SetRotation( const osg::Quat& rotation )
{
    _rotation = rotation;
    //_matrixDirty = true;
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

    //Force scale update
    //SetAutoRotateMode( GetAutoRotateMode() );
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

    if( _scale.x() < _minimumScale )
    {
        _scale.x() = _minimumScale;
    }
    if( _scale.y() < _minimumScale )
    {
        _scale.y() = _minimumScale;
    }
    if( _scale.z() < _minimumScale )
    {
        _scale.z() = _minimumScale;
    }

    if( _scale.x() > _maximumScale )
    {
        _scale.x() = _maximumScale;
    }
    if( _scale.y() > _maximumScale )
    {
        _scale.y() = _maximumScale;
    }
    if( _scale.z() > _maximumScale )
    {
        _scale.z() = _maximumScale;
    }

    //_matrixDirty = true;

    dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////

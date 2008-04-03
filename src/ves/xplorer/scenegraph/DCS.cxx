/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/SelectTechnique.h>

#include <ves/xplorer/scenegraph/util/NormalizeVisitor.h>

#include <ves/xplorer/scenegraph/physics/TransferPhysicsDataCallback.h>
#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/vesMotionState.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Vec3d>
#include <osg/MatrixTransform>
#elif _OPENSG
#endif

// --- VR Juggler Includes --- //
#include <gmtl/Coord.h>
#include <gmtl/Generate.h>
#include <gmtl/EulerAngle.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Output.h>

// --- Bullet Includes --- //
#include <LinearMath/btTransform.h>
#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

// --- C/C++ Libraries --- //
#include <iostream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{

////////////////////////////////////////////////////////////////////////////////
DCS::DCS()
        :
        mPhysicsRigidBody( 0 )
{
    double temp[3];
    for( unsigned int i = 0; i < 3; ++i )
    {
        temp[i] = 0.0f;
    }

    SetTranslationArray( temp );

    for( unsigned int i = 0; i < 3; ++i )
    {
        temp[i] = 0.0f;
    }

    SetRotationArray( temp );

    for( unsigned int i = 0; i < 3; ++i )
    {
        temp[i] = 1.0f;
    }

    SetScaleArray( temp );

    //_scale is defined in the inherited class osg::PositionAttitudeTransform
    _scale[0] = 1;

    m_udcb = new TransferPhysicsDataCallback();
    m_udcb->SetPhysicsRigidBody( mPhysicsRigidBody );
    setUpdateCallback( m_udcb.get() );

    AddTechnique( "Select", new ves::xplorer::scenegraph::SelectTechnique
                  ( new osg::StateSet( *getOrCreateStateSet() ) ) );
}
////////////////////////////////////////////////////////////////////////////////
DCS::DCS( double* scale, double* trans, double* rot )
        :
        mPhysicsRigidBody( 0 )
{
    SetTranslationArray( trans );
    SetRotationArray( rot );
    SetScaleArray( scale );

    m_udcb = new TransferPhysicsDataCallback();
    m_udcb->SetPhysicsRigidBody( mPhysicsRigidBody );
    setUpdateCallback( m_udcb.get() );

    AddTechnique( "Select", new ves::xplorer::scenegraph::SelectTechnique
                  ( new osg::StateSet( *getOrCreateStateSet() ) ) );
}
////////////////////////////////////////////////////////////////////////////////
DCS::DCS( const DCS& dcs, const osg::CopyOp& copyop )
        :
        osg::PositionAttitudeTransform( dcs, copyop ),
        mPhysicsRigidBody( 0 )
{
    m_udcb = new TransferPhysicsDataCallback();
    m_udcb->SetPhysicsRigidBody( mPhysicsRigidBody );
    setUpdateCallback( m_udcb.get() );

    AddTechnique( "Select", new ves::xplorer::scenegraph::SelectTechnique
                  ( new osg::StateSet( *getOrCreateStateSet() ) ) );
}
////////////////////////////////////////////////////////////////////////////////
DCS::~DCS()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
double* DCS::GetVETranslationArray()
{
    osg::Vec3d trans = getPosition();
    for( size_t i = 0; i < 3; ++i )
    {
        m_Translation[i] = trans[i];
    }

    return m_Translation;
}
////////////////////////////////////////////////////////////////////////////////
double* DCS::GetRotationArray()
{
    osg::Quat quat = getAttitude();

    gmtl::Quatd tempQuat( quat[0], quat[1], quat[2], quat[3] );
    gmtl::Matrix44d _vjMatrix = gmtl::makeRot< gmtl::Matrix44d >( tempQuat );
    gmtl::EulerAngleZXYd tempZXY = gmtl::makeRot< gmtl::EulerAngleZXYd >( _vjMatrix );

    m_Rotation[0] = gmtl::Math::rad2Deg( tempZXY[0] );
    m_Rotation[1] = gmtl::Math::rad2Deg( tempZXY[1] );
    m_Rotation[2] = gmtl::Math::rad2Deg( tempZXY[2] );

    return m_Rotation;
}
////////////////////////////////////////////////////////////////////////////////
double* DCS::GetScaleArray()
{
    osg::Vec3d tempScale = getScale();
    for( size_t i = 0; i < 3; ++i )
    {
        m_Scale[i] = tempScale[i];
    }

    return m_Scale;
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetTranslationArray( std::vector< double > transArray )
{
#ifdef _OSG
    setPosition( osg::Vec3d( transArray[0], transArray[1], transArray[2] ) );
#elif _OPENSG
#endif

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetQuat( osg::Quat quat )
{
#ifdef _OSG
    setAttitude( quat );
#elif _OPENSG
#endif

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetRotationArray( std::vector< double > rotArray )
{
#ifdef _OSG
    osg::Vec3d pitch( 1, 0, 0 );
    osg::Vec3d roll( 0, 1, 0 );
    osg::Vec3d yaw( 0, 0, 1 );

    osg::Matrixd rotateMat;

    rotateMat.makeRotate( osg::DegreesToRadians( rotArray[2] ), roll,
                          osg::DegreesToRadians( rotArray[1] ), pitch,
                          osg::DegreesToRadians( rotArray[0] ), yaw );

    osg::Quat quat;
    rotateMat.get( quat );
    setAttitude( quat );
    setPivotPoint( osg::Vec3d( 0, 0, 0 ) );
#elif _OPENSG
#endif

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetScaleArray( std::vector< double > scaleArray )
{
    setScale( osg::Vec3d( scaleArray[0], scaleArray[1], scaleArray[2] ) );

    if( scaleArray[0] != 1 )
    {
        ves::xplorer::scenegraph::util::NormalizeVisitor normVis( this, true );
        
    }
    else
    {
        ves::xplorer::scenegraph::util::NormalizeVisitor normVis( this, false );
    }

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetTranslationArray( double* trans )
{
    std::vector< double > temp;
    temp.push_back( trans[0] );
    temp.push_back( trans[1] );
    temp.push_back( trans[2] );

    SetTranslationArray( temp );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetRotationArray( double* rot )
{
    std::vector< double > temp;
    temp.push_back( rot[0] );
    temp.push_back( rot[1] );
    temp.push_back( rot[2] );

    SetRotationArray( temp );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetScaleArray( double* scale )
{
    std::vector< double > temp;
    temp.push_back( scale[0] );
    temp.push_back( scale[1] );
    temp.push_back( scale[2] );

    SetScaleArray( temp );
}
////////////////////////////////////////////////////////////////////////////////
gmtl::Matrix44d DCS::GetMat()
{
#ifdef _OSG
    osg::Matrixd scaleMat = osg::Matrixd::scale( getScale() );
    osg::Matrixd translationMat = osg::Matrixd::translate( getPosition() );
    osg::Matrixd inverseTranslationMat = osg::Matrixd::translate( -getPosition()[0],
                                         -getPosition()[1],
                                         -getPosition()[2] );
    scaleMat = inverseTranslationMat * scaleMat * translationMat;

    osg::Matrixd rotateMat;
    rotateMat.makeRotate( getAttitude() );
    rotateMat = inverseTranslationMat * rotateMat * translationMat;

    osg::Matrixd osgMat = translationMat * scaleMat * rotateMat;
    gmtl::Matrix44d _vjMatrix;
    if( osgMat.valid() )
    {
        _vjMatrix.set( osgMat.ptr() );
    }
    else
    {
        std::cout << "Invalid matrix!" << std::endl;
        std::cout << "DCS::GetMat()" << std::endl;
    }
#elif _OPENSG
    //GetVjMatrix
    cerr << " ERROR: DCS::GetMat is NOT implemented " << endl;
    exit( 1 );
#endif

    return _vjMatrix;
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetMat( gmtl::Matrix44d& input )
{
#ifdef _OSG
    gmtl::Vec3d scaleXVec( input[ 0 ][ 0 ], input[ 1 ][ 0 ], input[ 2 ][ 0 ] );
    gmtl::Vec3d scaleYVec( input[ 0 ][ 1 ], input[ 1 ][ 1 ], input[ 2 ][ 1 ] );
    gmtl::Vec3d scaleZVec( input[ 0 ][ 2 ], input[ 1 ][ 2 ], input[ 2 ][ 2 ] );

    double tempScale = 1.0f / gmtl::length( scaleXVec );
    gmtl::Matrix44d tempScaleMat;
    gmtl::setScale( tempScaleMat, tempScale );
    gmtl::Matrix44d unScaleInput = tempScaleMat * input;

    //Set scale values
    setScale( osg::Vec3d( gmtl::length( scaleXVec ),
                          gmtl::length( scaleYVec ),
                          gmtl::length( scaleZVec ) ) );

    //Set rotation values
    gmtl::Quatd tempQuat = gmtl::make< gmtl::Quatd >( unScaleInput );
    osg::Quat quat( tempQuat[0], tempQuat[1], tempQuat[2], tempQuat[3] );
    setAttitude( quat );

    //Set translation array
    osg::Matrix inMat;
    inMat.set( input.getData() );
    osg::Vec3d trans = inMat.getTrans();
    setPosition( trans );
#elif _OPENSG
    std::cerr << " ERROR: DCS::SetMat is NOT implemented " << std::endl;
    exit( 1 );
#endif

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetRotationMatrix( gmtl::Matrix44d& input )
{
    //There is currently a bug here
    //We need to set the roation array so that if someone requests the rotation array,
    //they will actually get back the current rotation and not and old rotation value

    //Need to set rotation to this matrix
#ifdef _OSG
    //Remove the scale from the rotation
    gmtl::Vec3d scaleVec( input[ 0 ][ 0 ], input[ 1 ][ 0 ], input[ 2 ][ 0 ] );
    double tempScale = 1.0f / gmtl::length( scaleVec );
    gmtl::Matrix44d tempScaleMat;
    gmtl::setScale( tempScaleMat, tempScale );
    gmtl::Matrix44d unScaleInput = tempScaleMat * input;

    //Create the quat for rotataion
    gmtl::Quatd tempQuat = gmtl::make< gmtl::Quatd >( unScaleInput );
    osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
    setAttitude( quat );
#elif _OPENSG
    std::cerr << " ERROR: DCS::SetRotationMatrix is NOT implemented " << std::endl;
    exit( 1 );
#endif

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
int DCS::RemoveChild( SceneNode* child )
{
#ifdef _OSG
    return removeChild( dynamic_cast< osg::Node* >( child ) );
#elif _OPENSG
    cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
    exit( 1 );

    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
int DCS::AddChild( SceneNode* child )
{
#ifdef _OSG
    return addChild( dynamic_cast< Node* >( child ) );
#elif _OPENSG
    cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
    exit( 1 );

    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void DCS::InsertChild( int position, SceneNode* child )
{
#ifdef _OSG
    insertChild( position, dynamic_cast< Node* >( child ) );
#elif _OPENSG
    cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
    exit( 1 );

    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
int DCS::GetNumChildren()
{
#ifdef _OSG
    return getNumChildren();
#elif _OPENSG
    cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
    exit( 1 );

    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string DCS::GetName()
{
#ifdef _OSG
    return getName().data();
#elif _OPENSG
    return 0;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetName( std::string name )
{
#ifdef _OSG
    setName( name );
#elif _OPENSG
    std::cerr << " ERROR: DCS::SetName is NOT implemented " << std::endl;
    exit( 1 );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int DCS::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
#ifdef _OSG
    return replaceChild( dynamic_cast< Node* >( childToBeReplaced ), dynamic_cast< Node* >( newChild ) );
#elif _OPENSG
    cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
    exit( 1 );

    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
bool DCS::SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild )
{
#ifdef _OSG
    return containsNode( dynamic_cast< osg::Node* >( searchChild ) );
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* DCS::GetParent( unsigned int position )
{
#ifdef _OSG
    return getParent( position );
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* DCS::GetChild( unsigned int position )
{
#ifdef _OSG
    return getChild( position );
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
void DCS::ToggleDisplay( bool onOff )
{
    std::string value = ( onOff == true ) ? "ON" : "OFF";

    ToggleDisplay( value );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::ToggleDisplay( std::string onOff )
{
    if( onOff == "ON" )
    {
#ifdef _OSG
        setNodeMask( 1 );
#elif _OPENSG
#endif
    }
    else if( onOff == "OFF" )
    {
#ifdef _OSG
        setNodeMask( 0 );
#elif _OPENSG
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void DCS::UpdatePhysicsTransform()
{
    if( !mPhysicsRigidBody )
    {
        return;
    }

    osg::Quat quat = getAttitude();
    osg::Vec3d trans = getPosition();

    btTransform transform = btTransform::getIdentity();
    btQuaternion btQuat( quat[ 0 ], quat[ 1 ], quat[ 2 ], quat[ 3 ] );
    transform.setOrigin( btVector3( trans.x(), trans.y(), trans.z() ) );
    transform.setRotation( btQuat );

    ves::xplorer::scenegraph::vesMotionState* motionState =
        static_cast< vesMotionState* >( mPhysicsRigidBody->getMotionState() );
    if( motionState )
    {
        motionState->m_startWorldTrans = transform;
        motionState->m_graphicsWorldTrans = transform;
        mPhysicsRigidBody->setWorldTransform( transform );
        mPhysicsRigidBody->setInterpolationWorldTransform( transform );
    }

    //Removed cached contact points
    ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->
    getBroadphase()->getOverlappingPairCache()->cleanProxyFromPairs
    ( mPhysicsRigidBody->getBroadphaseHandle(),
      ves::xplorer::scenegraph::PhysicsSimulator::instance()->GetDynamicsWorld()->getDispatcher() );

    if( mPhysicsRigidBody && !mPhysicsRigidBody->isStaticObject() )
    {
        mPhysicsRigidBody->setLinearVelocity( btVector3( 0, 0, 0 ) );
        mPhysicsRigidBody->setAngularVelocity( btVector3( 0, 0, 0 ) );
    }

}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetPhysicsRigidBody( PhysicsRigidBody* physicsRigidBody )
{
    mPhysicsRigidBody = physicsRigidBody;
    m_udcb->SetPhysicsRigidBody( mPhysicsRigidBody );

    UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::traverse( osg::NodeVisitor& nv )
{
    ves::xplorer::scenegraph::Technique* technique = mTechniques[ mActiveTechnique ];

    technique->Traverse( nv, this );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::InheritedTraverse( osg::NodeVisitor& nv )
{
    typedef osg::PositionAttitudeTransform inherited;
    inherited::traverse( nv );
}
////////////////////////////////////////////////////////////////////////////////

} // end scenegraph
} // end xplorer
} // end ves
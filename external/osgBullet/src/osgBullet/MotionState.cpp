/*
 *
 * Copyright (c) 2008 Blue Newt Software LLC and Skew Matrix Software LLC.
 * All rights reserved.
 *
 */

#include <osgBullet/MotionState.h>
#include <osg/MatrixTransform>
#include <osg/PositionAttitudeTransform>
#include <osgSim/DOFTransform>
#include <osgBullet/Utils.h>

#include <osg/Notify>
#include <osg/io_utils>


using namespace osgBullet;

MotionState::MotionState( const btTransform & startTrans, const btTransform & centerOfMassOffset )
{
    _matrixTransform = NULL;
}

void MotionState::setWorldTransform( const btTransform& trans )
{
    //osg::notify( osg::ALWAYS ) << "incoming trans " <<
    //    osgBullet::asOsgMatrix( trans ) << std::endl;
    btDefaultMotionState::setWorldTransform( trans );

    osg::Matrix dm( osgBullet::asOsgMatrix( m_graphicsWorldTrans ) );
    osg::Matrix m(
        osg::Matrix::translate( -_centerOfMass ) *
        dm *
        _invL2w
        );
    if( _matrixTransform.valid() )
    {
        _matrixTransform->setMatrix( m );
        //osg::notify( osg::ALWAYS ) << "swt:" << m << std::endl;
    }
    if( _debugMT.valid() )
    {
        _debugMT->setMatrix( dm );
        //osg::notify( osg::ALWAYS ) << "dswt:" << dm << std::endl;
    }
}
void
MotionState::setInverseParentWorldTransform( const osg::Matrix& invL2w )
{
    _invL2w = invL2w;
}

DOFMotionState::DOFMotionState( const btTransform & startTrans, const btTransform & centerOfMassOffset )
{
    _dofTransform = NULL;
}

void DOFMotionState::setWorldTransform( const btTransform & centerOfMassWorldTrans )
{
    btDefaultMotionState::setWorldTransform( centerOfMassWorldTrans );

    if( _dofTransform == NULL )
        return;

    btQuaternion const & btRot = m_graphicsWorldTrans.getRotation();
    osg::Quat osgRot( btRot.getX(), btRot.getY(), btRot.getZ(), btRot.getW() );

    btVector3 const & btTrans = m_graphicsWorldTrans.getOrigin();
    osg::Vec3f osgTrans( btTrans.getX(), btTrans.getY(), btTrans.getZ() );

    osg::Matrix mat;
    mat.setTrans( osgTrans );
    mat.setRotate( osgRot );

    _dofTransform->setPutMatrix( mat );
}

PATMotionState::PATMotionState( const btTransform & startTrans, const btTransform & centerOfMassOffset )
{
    _patTransform = NULL;
}

void PATMotionState::setWorldTransform( const btTransform & centerOfMassWorldTrans )
{
    btDefaultMotionState::setWorldTransform( centerOfMassWorldTrans );

    if( _patTransform == NULL )
        return;

    btQuaternion const & btRot = m_graphicsWorldTrans.getRotation();
    osg::Quat osgRot( btRot.getX(), btRot.getY(), btRot.getZ(), btRot.getW() );

    btVector3 const & btTrans = m_graphicsWorldTrans.getOrigin();
    osg::Vec3f osgTrans( btTrans.getX(), btTrans.getY(), btTrans.getZ() );

    osg::Matrix mat;
    mat.setTrans( osgTrans );
    mat.setRotate( osgRot );

    //_patTransform->setPutMatrix( mat );
}


/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/DCS.h"

#include <gmtl/Generate.h>
#include <gmtl/Coord.h>
#include <gmtl/EulerAngle.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Output.h>

#ifdef _PERFORMER
#include <vrj/Draw/Pf/PfUtil.h>
#include <Performer/pf.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pr/pfLinMath.h>
#elif _OSG
#include <osg/MatrixTransform>
#include <osg/Matrix>
#include <osg/Vec3f>
#include <osg/NodeVisitor>
#elif _OPENSG
#endif

#include <LinearMath/btTransform.h>
#include <BulletDynamics/Dynamics/btRigidBody.h>

//C/C++ Libraries
#include <iostream>

using namespace gmtl;
using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
DCS::DCS( void )
{
   mBtBody = 0;

   float temp[3];
   for( unsigned int i = 0; i < 3; i++ )
   {
      temp[i] = 0.0f;
   }
   SetTranslationArray( temp );

   for( unsigned int i = 0; i < 3; i++ )
   {
      temp[i] = 0.0f;
   }
   SetRotationArray( temp );

   for( unsigned int i = 0; i < 3; i++ )
   {
      temp[i] = 1.0f;
   }
   SetScaleArray( temp );
   
   //Why does this compile????
   //Because it is defined in PositionAttitudeTransform
   _scale[ 0 ] = 1;
   
   udcb = new TransferPhysicsDataCallback();
   this->setUpdateCallback( udcb.get() );
   udcb->SetbtRigidBody( mBtBody );
}
////////////////////////////////////////////////////////////////////////////////
DCS::DCS( float* scale, float* trans, float* rot )
{
   mBtBody = 0;

   this->SetTranslationArray( trans );
   this->SetRotationArray( rot );
   this->SetScaleArray( scale );

   udcb = new TransferPhysicsDataCallback();
   this->setUpdateCallback( udcb.get() );
   udcb->SetbtRigidBody( mBtBody );
}
////////////////////////////////////////////////////////////////////////////////
DCS::DCS(const DCS& dcs,const osg::CopyOp& copyop):
osg::PositionAttitudeTransform(dcs,copyop)
{
   mBtBody = 0;
   
   udcb = new TransferPhysicsDataCallback();
   udcb->SetbtRigidBody( mBtBody );
   this->setUpdateCallback( udcb.get() );
}
////////////////////////////////////////////////////////////////////////////////
DCS::~DCS( void )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
/*
float* DCS::GetTranslationArray( void )
{
   return this->_translation;
}
*/
////////////////////////////////////////////////////////////////////////////////
/*
float* DCS::GetVRJTranslationArray( void )
{
   vrjTranslation[0] = _translation[0];
   vrjTranslation[1] = _translation[2];
   vrjTranslation[2] = -_translation[1];

   return vrjTranslation;
}
*/
////////////////////////////////////////////////////////////////////////////////
float* DCS::GetVETranslationArray( void )
{
   osg::Vec3d trans = this->getPosition();
   for( size_t i = 0; i < 3; i++ )
   {
      mTranslation[i] = trans[i];
   }

   return mTranslation;
}
////////////////////////////////////////////////////////////////////////////////
float* DCS::GetRotationArray( void )
{
   osg::Quat quat = this->getAttitude();

   gmtl::Quatf tempQuat( quat[0], quat[1], quat[2], quat[3] );
   gmtl::Matrix44f _vjMatrix = gmtl::makeRot< gmtl::Matrix44f >( tempQuat );
   gmtl::EulerAngleZXYf tempZXY = gmtl::makeRot< gmtl::EulerAngleZXYf >( _vjMatrix );

   mRotation[0] = gmtl::Math::rad2Deg( tempZXY[0] );
   mRotation[1] = gmtl::Math::rad2Deg( tempZXY[1] );
   mRotation[2] = gmtl::Math::rad2Deg( tempZXY[2] );
   
   return mRotation;
}
////////////////////////////////////////////////////////////////////////////////
float* DCS::GetScaleArray( void )
{
   osg::Vec3d tempScale = this->getScale();
   for ( size_t i = 0; i < 3; i++ )
   {
      mScale[i] = tempScale[i];
   }

   return mScale;
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetTranslationArray( std::vector<double> array )
{
#ifdef _PERFORMER
   this->_dcs->setTrans( this->_translation[0], this->_translation[1], this->_translation[2] );
#elif _OSG           
   this->setPosition( osg::Vec3d( array[0], array[1], array[2]) );
#elif _OPENSG
#endif
   UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetRotationArray( std::vector<double> array)
{
#ifdef _PERFORMER
   this->_dcs->setRot(this->_rotation[0], this->_rotation[1], this->_rotation[2]);
#elif _OSG
   osg::Vec3f pitch(1,0,0);
   osg::Vec3f roll(0,1,0);
   osg::Vec3f yaw(0,0,1);
   //rph              
   osg::Matrixd rotateMat;
   
   rotateMat.makeRotate(osg::DegreesToRadians(array[2]),roll,
                        osg::DegreesToRadians(array[1]),pitch,
                        osg::DegreesToRadians(array[0]),yaw);
   osg::Quat quat;
   rotateMat.get( quat );
   this->setAttitude( quat );
   this->setPivotPoint( osg::Vec3d( 0, 0, 0) );
#elif _OPENSG
#endif
   UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetScaleArray( std::vector<double> array )
{
#ifdef _PERFORMER
   this->_dcs->setScale( array[0], array[1], array[2] );
#elif _OSG
   this->setScale( osg::Vec3d( array[0], array[1], array[2]) );
   
   ///Do this so that the normals will not be affected by the scaling applied 
   ///by the user - see osg post one April 19, 2007
   if ( array[0] != 1 )
   {
      this->getOrCreateStateSet()->setMode( GL_NORMALIZE, osg::StateAttribute::ON );
   }
   else
   {
      this->getOrCreateStateSet()->setMode( GL_NORMALIZE, osg::StateAttribute::OFF );
   }
   
#elif _OPENSG
#endif
   UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetTranslationArray( float* trans )
{
   std::vector<double> temp;
   temp.push_back( trans[0] );
   temp.push_back( trans[1] );
   temp.push_back( trans[2] );

   SetTranslationArray( temp );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetRotationArray( float* rot )
{
   std::vector<double> temp;
   temp.push_back( rot[0] );
   temp.push_back( rot[1] );
   temp.push_back( rot[2] );

   SetRotationArray( temp );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetScaleArray( float* scale )
{
   std::vector<double> temp;
   temp.push_back( scale[0] );
   temp.push_back( scale[1] );
   temp.push_back( scale[2] );

   SetScaleArray( temp );
}
////////////////////////////////////////////////////////////////////////////////
Matrix44f DCS::GetMat( void )
{
#ifdef _PERFORMER
   pfMatrix temp;
   this->_dcs->getMat( temp );
   _vjMatrix = vrj::GetVjMatrix( temp );
#elif _OSG
   osg::Matrixd scaleMat = osg::Matrixd::scale(this->getScale());
   osg::Matrixd translationMat = osg::Matrixd::translate(this->getPosition());
   osg::Matrixd inverseTranslationMat = 
                     osg::Matrixd::translate(-this->getPosition()[0],
                                             -this->getPosition()[1],
                                             -this->getPosition()[2]);
   scaleMat = inverseTranslationMat*scaleMat*translationMat;
   
   osg::Matrixd rotateMat;   
   rotateMat.makeRotate( this->getAttitude() );
   rotateMat = inverseTranslationMat*rotateMat*translationMat;
   
   osg::Matrixf osgMat = translationMat*scaleMat*rotateMat;
   gmtl::Matrix44f _vjMatrix;
   if( osgMat.valid() )
   {
      _vjMatrix.set( osgMat.ptr() );
   }
   else
   {
      std::cout<<"Invalid matrix!!"<<std::endl;
      std::cout<<"DCS::GetMat()"<<std::endl;
   }
#elif _OPENSG
   // GetVjMatrix
   cerr << " ERROR: DCS::GetMat is NOT implemented " << endl;
   exit( 1 );
#endif
   return _vjMatrix;
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetMat( Matrix44f& input )
{
#ifdef _PERFORMER
#elif _OSG
   gmtl::Vec3f scaleXVec( input[ 0 ][ 0 ], input[ 1 ][ 0 ], input[ 2 ][ 0 ] );
   gmtl::Vec3f scaleYVec( input[ 0 ][ 1 ], input[ 1 ][ 1 ], input[ 2 ][ 1 ] );
   gmtl::Vec3f scaleZVec( input[ 0 ][ 2 ], input[ 1 ][ 2 ], input[ 2 ][ 2 ] );
   float tempScale = 1.0f/gmtl::length( scaleXVec );
   gmtl::Matrix44f tempScaleMat;
   gmtl::setScale( tempScaleMat, tempScale );
   gmtl::Matrix44f unScaleInput = tempScaleMat * input;

   // Set scale values
   this->setScale( osg::Vec3d( gmtl::length( scaleXVec ), 
                               gmtl::length( scaleYVec ), 
                               gmtl::length( scaleZVec ) ) );

   // set rotation values
   gmtl::Quatf tempQuat = gmtl::make< gmtl::Quatf >( unScaleInput );
   //gmtl::EulerAngleZXYf tempZXY = gmtl::makeRot< gmtl::EulerAngleZXYf >( unScaleInput );
   osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
   this->setAttitude( quat );

   // Set translation array
   osg::Matrix inMat;
   inMat.set( input.getData() );
   osg::Vec3d trans = inMat.getTrans();
   this->setPosition( trans );
#elif _OPENSG
   std::cerr << " ERROR: DCS::SetMat is NOT implemented " << std::endl;
   exit( 1 );
#endif
   UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetRotationMatrix( Matrix44f& input )
{
// There is currently a bug here.
// We need to set the roation array so that 
// if someone requests the rotation array
// they will actually get back the current rotation
// and not and old rotation value

   // Need to set rotation to this matrix
#ifdef _PERFORMER
   pfMatrix temp = vrj::GetPfMatrix( input );
   // The following code is used to double check how juggler is doing
   // some of its matrix manipulation
   /*gmtl::EulerAngleZYXf temp1 = gmtl::make< gmtl::EulerAngleZYXf >( input );
   std::cout << input << std::endl << temp1 << std::endl;
   std::cout << temp[ 0 ][ 0 ] << temp[ 0 ][ 1 ] << temp[ 0 ][ 2 ] << temp[ 0 ][ 3 ] <<std::endl;
   std::cout << temp[ 1 ][ 0 ] << temp[ 1 ][ 1 ] << temp[ 1 ][ 2 ] << temp[ 1 ][ 3 ] <<std::endl;
   std::cout << temp[ 2 ][ 0 ] << temp[ 2 ][ 1 ] << temp[ 2 ][ 2 ] << temp[ 2 ][ 3 ] <<std::endl;
   std::cout << temp[ 3 ][ 0 ] << temp[ 3 ][ 1 ] << temp[ 3 ][ 2 ] << temp[ 3 ][ 3 ] <<std::endl;*/
   pfMatrix ident;
   ident.makeIdent();
   pfVec3 x;
   temp.getCol( 0, x );
   float scale = x.length();
   scale = 1.0f/ scale;
   ident.makeScale( scale, scale, scale );
   pfMatrix unScale = ident * temp;
   pfCoord* coord = new pfCoord();
   unScale.getOrthoCoord( coord );
   _dcs->setRot( coord->hpr[ 0 ], coord->hpr[ 1 ], coord->hpr[ 2 ] );
   _rotation[ 0 ] = coord->hpr[ 0 ];
   _rotation[ 1 ] = coord->hpr[ 1 ];
   _rotation[ 2 ] = coord->hpr[ 2 ];
   
   //std::cout << coord->hpr[ 0 ] << " :" << coord->hpr[ 1 ] << " : " <<  coord->hpr[ 2 ] <<std::endl;
   delete coord;
#elif _OSG
   // Remove the scale from the rotation
   gmtl::Vec3f scaleVec( input[ 0 ][ 0 ], input[ 1 ][ 0 ], input[ 2 ][ 0 ] );
   float tempScale = 1.0f/gmtl::length( scaleVec );
   gmtl::Matrix44f tempScaleMat;
   gmtl::setScale( tempScaleMat, tempScale );
   gmtl::Matrix44f unScaleInput = tempScaleMat * input;

   // Create the quat for rotataion
   gmtl::Quatf tempQuat = gmtl::make< gmtl::Quatf >( unScaleInput );
   osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
   this->setAttitude ( quat );
#elif _OPENSG
   std::cerr << " ERROR: DCS::SetRotationMatrix is NOT implemented " << std::endl;
   exit( 1 );
#endif
   UpdatePhysicsTransform();
}
////////////////////////////////////////////////////////////////////////////////
int DCS::RemoveChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
	return this->removeChild( dynamic_cast< osg::Node* >( child ));
#endif
}
////////////////////////////////////////////////////////////////////////////////
int DCS::AddChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->addChild( dynamic_cast< Node* >( child ));
#endif

}
////////////////////////////////////////////////////////////////////////////////
void DCS::InsertChild( int position, SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   this->insertChild( position, dynamic_cast< Node* >( child ));
#endif
}
////////////////////////////////////////////////////////////////////////////////
int DCS::GetNumChildren( void )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->getNumChildren();
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string DCS::GetName( void )
{
#ifdef _OPENSG
   return 0;
#endif
#ifdef _PERFORMER
   return _dcs->getName();
#elif _OSG
   return this->getName().data();
#endif
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetName( std::string name )
{
#ifdef _OPENSG
   std::cerr << " ERROR: DCS::SetName is NOT implemented " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   _dcs->setName( name.c_str() );
#elif _OSG
  this->setName( name );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int DCS::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSG
   return this->replaceChild( dynamic_cast< Node* >( childToBeReplaced ), dynamic_cast< Node* >( newChild ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
bool DCS::SearchChild( VE_SceneGraph::SceneNode* searchChild )
{
#ifdef _OPENSG
	
#elif _OSG
	return this->containsNode( dynamic_cast< osg::Node* >(searchChild) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* DCS::GetParent( unsigned int position )
{
#ifdef _OPENSG

#elif _OSG
	return this->getParent( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* DCS::GetChild( unsigned int position )
{
#ifdef _OPENSG

#elif _OSG
	return this->getChild( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void DCS::ToggleDisplay(bool onOff)
{
   std::string value = (onOff==true)?"ON":"OFF";
   ToggleDisplay(value);
}
////////////////////////////////////////////////////////////////////////////////
void DCS::ToggleDisplay(std::string onOff)
{      
   if(onOff == "ON")
   {
#ifdef _OSG
      this->setNodeMask(1);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
   else if(onOff == "OFF")
   {
#ifdef _OSG
      this->setNodeMask(0);
#elif _OPENSG
#elif _PERFORMER
#endif
   }
}
////////////////////////////////////////////////////////////////////////////////
void DCS::UpdatePhysicsTransform( void )
{
   if ( !mBtBody )
   {
      return;
   }
   
   osg::Quat quat = this->getAttitude();
   osg::Vec3d trans = this->getPosition();

   btTransform bulletTransform;
   bulletTransform = mBtBody->getWorldTransform();
   
   btQuaternion btQuat( quat[ 0 ], quat[ 1 ], quat[ 2 ], quat[ 3 ] );
   bulletTransform.setOrigin( btVector3( trans.x(), trans.y(), trans.z() ) );
   bulletTransform.setRotation( btQuat );
   
   mBtBody->setWorldTransform( bulletTransform );
}
////////////////////////////////////////////////////////////////////////////////
void DCS::SetbtRigidBody( btRigidBody* rigidBody )
{
   mBtBody = rigidBody;
   udcb->SetbtRigidBody( mBtBody );
   UpdatePhysicsTransform();
}
#ifdef _OSG
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
TransferPhysicsDataCallback::TransferPhysicsDataCallback()
{
   btBody = 0;
}
////////////////////////////////////////////////////////////////////////////////
TransferPhysicsDataCallback::TransferPhysicsDataCallback( const TransferPhysicsDataCallback& input )
:osg::Object( input ), osg::NodeCallback( input )
{
   btBody = 0;
}
////////////////////////////////////////////////////////////////////////////////
void TransferPhysicsDataCallback::SetbtRigidBody( btRigidBody* transform )
{
   btBody = transform;
}
////////////////////////////////////////////////////////////////////////////////
void TransferPhysicsDataCallback::operator()(osg::Node* node, osg::NodeVisitor* nv)
{
   osg::ref_ptr<VE_SceneGraph::DCS> dcs = static_cast<VE_SceneGraph::DCS*>(node);
   
   if ( dcs.valid() && btBody )
   {
      btQuaternion quat = btBody->getWorldTransform().getRotation();
      dcs->setAttitude( osg::Quat( quat[ 0 ], quat[ 1 ], quat[ 2 ], quat[ 3 ] ) );
   
      btVector3 position = btBody->getWorldTransform().getOrigin();
      dcs->setPosition( osg::Vec3d( position[0], position[ 1 ], position[ 2 ] ) );
   }
   
   traverse(node,nv);
}
#endif
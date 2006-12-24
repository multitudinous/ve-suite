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
 * Date modified: $Date: 2006-12-09 16:38:15 -0600 (Sat, 09 Dec 2006) $
 * Version:       $Rev: 6206 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/SceneGraph/DCS.h"

#include <iostream>

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

using namespace gmtl;
using namespace VE_SceneGraph;
//////////////////////
DCS::DCS( void )
{
   float temp[ 3 ];
   for ( unsigned int i = 0; i < 3; i++ )
      temp[ i ] = 0.0f;
   SetTranslationArray( temp );
   for ( unsigned int i = 0; i < 3; i++ )
      temp[ i ] = 0.0f;
   SetRotationArray( temp );
   for ( unsigned int i = 0; i < 3; i++ )
      temp[ i ] = 1.0f;
   SetScaleArray( temp );

   SetCFDNodeType(CFD_DCS);
}
DCS::DCS( float* scale, float* trans, float* rot )
{
#ifdef _PERFORMER
   _dcs = new pfDCS();
#elif _OSG
   _dcs = new osg::MatrixTransform();
   _dcs->setMatrix(osg::Matrix::identity());
   _udcb =new cfdUpdateDCSCallback();
   _dcs->setUpdateCallback(_udcb);
#elif _OPENSG
#endif
   this->SetTranslationArray( trans );
   this->SetRotationArray( rot );
   this->SetScaleArray( scale );
   SetCFDNodeType(CFD_DCS);
}
/////////////////////////////////////
DCS::DCS( const DCS& input )
{
#ifdef _PERFORMER
   _dcs = new pfDCS(*input._dcs);
#elif _OSG 
   this->_dcs = new osg::MatrixTransform(*input._dcs);
  _udcb = new cfdUpdateDCSCallback( (*input._udcb) );
#elif _OPENSG
#endif
  
   SetCFDNodeType(CFD_DCS);
}
///////////////////////////////////////////////
DCS& DCS::operator=( const DCS& input)
{
   if ( this != &input )
   {
      //parents input
      cfdGroup::operator =(input);
      for ( int i = 0; i < 3; i++ )
      {
         this->_translation[ i ] = input._translation[ i ];
         this->vrjTranslation[ i ] = input.vrjTranslation[ i ];
         this->_rotation[ i ] = input._rotation[ i ];
         this->_scale[ i ] = input._scale[ i ];
      }
   
      /*for ( unsigned int i = 0; i < childNodes.size(); i++ )
      {
         delete childNodes.at( i );
      }
      childNodes.clear();
   
      this->childNodes = input.childNodes;*/
      this->_vjMatrix = input._vjMatrix;
#ifdef _PERFORMER
      pfDelete( this->_dcs );
      this->_dcs = input._dcs;
#elif _OSG
      _dcs = input._dcs;
      _udcb = input._udcb;
#elif _OPENSG
#endif
      //_group = _dcs;
      SetCFDNodeType(CFD_DCS);
   }
   return *this;
}
/*
bool DCS::operator== ( const DCS& node1 )
{
   if ( _dcs != node1._dcs )
   {
      return false;
   }
   else
   {
      return true;
   }
}
*/
///////////////////////
DCS::~DCS( void )
{
   // If neccesary
#ifdef _PERFORMER
#elif _OSG
#elif _OPENSG
#endif
}
//////////////////////////////////////////
/*float* DCS::GetTranslationArray( void )
{
   return this->_translation;
}*/
//////////////////////////////////////////
float* DCS::GetVRJTranslationArray( void )
{
   vrjTranslation[ 0 ] = _translation[ 0 ];
   vrjTranslation[ 1 ] = _translation[ 2 ];
   vrjTranslation[ 2 ] = -_translation[ 1 ];

   return vrjTranslation;
}
//////////////////////////////////////////
float* DCS::GetVETranslationArray( void )
{
   return _translation;
}
///////////////////////////////////////
float* DCS::GetRotationArray( void )
{
   return this->_rotation;
}
////////////////////////////////////
float* DCS::GetScaleArray( void )
{
   return this->_scale;
}
/////////////////////////////////////////////////////////////
void DCS::SetTranslationArray( std::vector<double> array )
{
#ifdef _PERFORMER
   this->_dcs->setTrans( this->_translation[ 0 ],
                       this->_translation[ 1 ],
                       this->_translation[ 2 ] );
#elif _OSG           
   this->setPosition( osg::Vec3d( array[0], array[1], array[2]) );
#elif _OPENSG
#endif
}
/////////////////////////////////////////////////////////////
void DCS::SetRotationArray( std::vector<double> array)
{
#ifdef _PERFORMER
   this->_dcs->setRot(this->_rotation[ 0 ],
                    this->_rotation[ 1 ],
                    this->_rotation[ 2 ]);
#elif _OSG
   osg::Vec3f pitch(1,0,0);
   osg::Vec3f roll(0,1,0);
   osg::Vec3f yaw(0,0,1);
   //rph              
   osg::Matrixd rotateMat;
   rotateMat.makeRotate(_rotation[2],roll,
                        _rotation[1],pitch,
                        _rotation[0],yaw);

   rotateMat.get( dcsQuat );
   this->setAttitude( dcsQuat );
#elif _OPENSG
#endif
}
/////////////////////////////////////////////////////////////
void DCS::SetScaleArray( std::vector<double> array )
{
#ifdef _PERFORMER
   this->_dcs->setScale( this->_scale[ 0 ],
                           this->_scale[ 1 ],
                           this->_scale[ 2 ] );
#elif _OSG
   this->setScale( osg::Vec3d( array[0], array[1], array[2]) );
#elif _OPENSG
#endif
}
////////////////////////////////////////////////
void DCS::SetTranslationArray( float* trans )
{
   std::vector< double > temp;
   temp.push_back( trans[ 0 ] );
   temp.push_back( trans[ 1 ] );
   temp.push_back( trans[ 2 ] );
   SetTranslationArray( temp );
}
///////////////////////////////////////////
void DCS::SetRotationArray( float* rot )
{
   std::vector< double > temp;
   temp.push_back( rot[ 0 ] );
   temp.push_back( rot[ 1 ] );
   temp.push_back( rot[ 2 ] );
   SetRotationArray( temp );
}
//////////////////////////////////////////
void DCS::SetScaleArray( float* scale )
{
   std::vector< double > temp;
   temp.push_back( scale[ 0 ] );
   temp.push_back( scale[ 1 ] );
   temp.push_back( scale[ 2 ] );
   SetScaleArray( temp );
}
////////////////////////////////
Matrix44f DCS::GetMat( void )
{
#ifdef _PERFORMER
   pfMatrix temp;
   this->_dcs->getMat( temp );
   _vjMatrix = vrj::GetVjMatrix( temp );
#elif _OSG
   osg::Matrixd scale = osg::Matrixd::scale(this->getScale());
   osg::Matrixd translation = osg::Matrixd::translate(this->getPostion());
   osg::Matrixd inverseTranslation = 
                     osg::Matrixd::translate(-this->getPostion()[0],
                                             -this->getPostion()[1],
                                             -this->getPostion()[2]);
   scale = inverseTranslation*scale*translation;
   
   osg::Matrixd rotateMat;   
   rotateMat.makeRotate( this->getAttitude() );
   rotateMat = inverseTranslation*rotateMat*translation;
   
   osg::Matrixf osgMat = translation*scale*rotateMat;
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
///////////////////////////////////////
void DCS::SetMat( Matrix44f& input )
{
#ifdef _PERFORMER
   pfMatrix temp = vrj::GetPfMatrix( input );
   this->_dcs->setMat( temp );
   
   // Set rotation
   pfCoord* coord = new pfCoord();

   temp.getOrthoCoord( coord );
   _rotation[ 0 ] = coord->hpr[ 0 ];
   _rotation[ 1 ] = coord->hpr[ 1 ];
   _rotation[ 2 ] = coord->hpr[ 2 ];

   _translation[ 0 ] = coord->xyz[ 0 ];
   _translation[ 1 ] = coord->xyz[ 1 ];
   _translation[ 2 ] = coord->xyz[ 2 ];

   // Warning we don't set scale here for pf
   // Add functionality to get scale from pfmatrix
   // by taking the length of each of the column vectors
#elif _OSG
   gmtl::Vec3f scaleXVec( input[ 0 ][ 0 ], input[ 1 ][ 0 ], input[ 2 ][ 0 ] );
   gmtl::Vec3f scaleYVec( input[ 0 ][ 1 ], input[ 1 ][ 1 ], input[ 2 ][ 1 ] );
   gmtl::Vec3f scaleZVec( input[ 0 ][ 2 ], input[ 1 ][ 2 ], input[ 2 ][ 2 ] );
   float tempScale = 1.0f/gmtl::length( scaleXVec );
   gmtl::Matrix44f tempScaleMat;
   gmtl::setScale( tempScaleMat, tempScale );
   gmtl::Matrix44f unScaleInput = tempScaleMat * input;

   // Set scale values
   _scale[ 0 ] = gmtl::length( scaleXVec );
   _scale[ 1 ] = gmtl::length( scaleYVec );
   _scale[ 2 ] = gmtl::length( scaleZVec );
   this->setScale( osg::Vec3d( _scale[ 0 ], _scale[ 1 ], _scale[ 2 ] );

   // set rotation values
   gmtl::Quatf tempQuat = gmtl::make< gmtl::Quatf >( unScaleInput );
   gmtl::EulerAngleZXYf tempZXY = gmtl::makeRot< gmtl::EulerAngleZXYf >( unScaleInput );
   _rotation[ 0 ] = gmtl::Math::rad2Deg( tempZXY[ 0 ] );
   _rotation[ 1 ] = gmtl::Math::rad2Deg( tempZXY[ 1 ] );
   _rotation[ 2 ] = gmtl::Math::rad2Deg( tempZXY[ 2 ] );

   osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
   dcsQuat = quat;
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
}
//////////////////////////////////////////////////
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
   gmtl::EulerAngleZXYf tempZXY = gmtl::makeRot< gmtl::EulerAngleZXYf >( unScaleInput );
   _rotation[ 0 ] = gmtl::Math::rad2Deg( tempZXY[ 0 ] );
   _rotation[ 1 ] = gmtl::Math::rad2Deg( tempZXY[ 1 ] );
   _rotation[ 2 ] = gmtl::Math::rad2Deg( tempZXY[ 2 ] );
   osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
   dcsQuat = quat;
   _udcb->setQuat( quat );
   this->setAttitude ( quat );
#elif _OPENSG
   std::cerr << " ERROR: DCS::SetRotationMatrix is NOT implemented " << std::endl;
   exit( 1 );
#endif
}
////////////////////////////////////////////////
int DCS::RemoveChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSF
   return this->removeChild( dynamic_cast< Node* >( child ))
#endif
}
/////////////////////////////////////////////
int DCS::AddChild( SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSF
   return this->addChild( dynamic_cast< Node* >( child ))
#endif
}
///////////////////////////////////////////////////////////////
void DCS::InsertChild( int position, SceneNode* child )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSF
   this->insertChild( position, dynamic_cast< Node* >( newChild ))
#endif
}

/////////////////////////////////////
int DCS::GetNumChildren( void )
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSF
   return this->getNumChildren();
#endif
}
//////////////////////////////////////
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
////////////////////////////////////
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
////////////////////////////////////////////////////////////
int DCS::ReplaceChild( SceneNode* childToBeReplaced,
                         SceneNode* newChild)
{
#ifdef _OPENSG
   cerr << " ERROR: DCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OSF
   return this->replaceChild( dynamic_cast< Node* >( childToBeReplaced ), 
                       dynamic_cast< Node* >( newChild ) );
#endif
}
//////////////////////////////////////////////////////////////////
void DCS::cfdUpdateDCSCallback::setRotationDegreeAngles(float h,
                                                     float p,
                                                     float r)
{
   _h = osg::DegreesToRadians(h);
   _p = osg::DegreesToRadians(p);
   _r = osg::DegreesToRadians(r);

   //quat make quat from rotations
   osg::Vec3f pitch(1,0,0);
   osg::Vec3f roll(0,1,0);
   osg::Vec3f yaw(0,0,1);
   
   osg::Matrixd rotateMat;

   //rph              
   rotateMat.makeRotate(_r,roll,
                         _p,pitch,
                         _h,yaw);

   rotateMat.get( quat );
}
//////////////////////////////////////////////////////////////////
void DCS::cfdUpdateDCSCallback::setQuat( osg::Quat& input )
{
   quat = input;
}
///////////////////////////////////////////////////////////////
void DCS::cfdUpdateDCSCallback::setTranslation(float* trans)
{
   if ( trans )
   {
      _trans[0] = trans[0];
      _trans[1] = trans[1];
      _trans[2] = trans[2];
   }
}
////////////////////////////////////////////////////////////////
void DCS::cfdUpdateDCSCallback::setScaleValues(float* scale)
{
   if ( scale )
   {
      _scale[0] = scale[0];
      _scale[1] = scale[1];
      _scale[2] = scale[2];
   }
}
//////////////////////////////////////////////////////////////////////////////////////
void DCS::cfdUpdateDCSCallback::operator()(osg::Node* node, osg::NodeVisitor* nv)
{
   osg::ref_ptr<osg::MatrixTransform> dcs = dynamic_cast<osg::MatrixTransform*>(node);

   if ( dcs.valid() )
   {
      osg::Vec3f pitch(1,0,0);
      osg::Vec3f roll(0,1,0);
      osg::Vec3f yaw(0,0,1);
      
      osg::Matrixd scale = osg::Matrixd::scale(_scale[0],_scale[1],_scale[2]);
      osg::Matrixd translation = osg::Matrixd::translate(_trans[0], _trans[1], _trans[2]);
      osg::Matrixd inverseTranslation = osg::Matrixd::translate(-_trans[0], -_trans[1], -_trans[2]);
      scale = inverseTranslation*scale*translation;

      osg::Matrixd rotateMat;

      rotateMat.makeRotate( quat );
      rotateMat = inverseTranslation*rotateMat*translation;

      dcs->setMatrix(translation*scale*rotateMat);
      traverse(node,nv);
   }
}
#endif

/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdDCS.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_SceneGraph/cfdDCS.h"

#include <iostream>
//#include <cstdlib>

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
//////////////////////
cfdDCS::cfdDCS( void )
:cfdGroup()
{
#ifdef _PERFORMER
   _dcs = new pfDCS();
#elif _OSG
   _dcs = new osg::MatrixTransform();
   _dcs->setMatrix(osg::Matrix::identity());
   _dcs->setDataVariance(osg::Object::DYNAMIC);
   _udcb =new cfdUpdateDCSCallback();
   _dcs->setUpdateCallback(_udcb);
#elif _OPENSG
#endif
   _scale[0] = 1;
   _scale[1] = 1;
   _scale[2] = 1;
   _translation[0] = 0;
   _translation[1] = 0;
   _translation[2] = 0;
   vrjTranslation[0] = 0;
   vrjTranslation[1] = 0;
   vrjTranslation[2] = 0;
   _rotation[0] = 0;
   _rotation[1] = 0;
   _rotation[2] = 0;

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
cfdDCS::cfdDCS( float* scale, float* trans, float* rot )
:cfdGroup()
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
cfdDCS::cfdDCS( const cfdDCS& input )
:cfdGroup(input)
{
   for ( int i = 0; i < 3; i++ )
   {
      this->_translation[ i ] = input._translation[ i ];
      this->vrjTranslation[ i ] = input.vrjTranslation[ i ];
      this->_rotation[ i ] = input._rotation[ i ];
      this->_scale[ i ] = input._scale[ i ];
   }
   
   this->_vjMatrix = input._vjMatrix;
   
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
cfdDCS& cfdDCS::operator=( const cfdDCS& input)
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
bool cfdDCS::operator== ( const cfdDCS& node1 )
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
cfdDCS::~cfdDCS( void )
{
   for ( unsigned int i = 0; i < childNodes.size(); i++ )
   {
      if ( childNodes.at( i ) )
         delete childNodes.at( i );
   }
   childNodes.clear();
   // If neccesary
#ifdef _PERFORMER
   //pfDelete ( this->_dcs );
#elif _OSG
   
#elif _OPENSG
#endif
}
//////////////////////////////////////////
/*float* cfdDCS::GetTranslationArray( void )
{
   return this->_translation;
}*/
//////////////////////////////////////////
float* cfdDCS::GetVRJTranslationArray( void )
{
   vrjTranslation[ 0 ] = _translation[ 0 ];
   vrjTranslation[ 1 ] = _translation[ 2 ];
   vrjTranslation[ 2 ] = -_translation[ 1 ];

   return vrjTranslation;
}
//////////////////////////////////////////
float* cfdDCS::GetVETranslationArray( void )
{
   return _translation;
}
///////////////////////////////////////
float* cfdDCS::GetRotationArray( void )
{
   return this->_rotation;
}
////////////////////////////////////
float* cfdDCS::GetScaleArray( void )
{
   return this->_scale;
}
////////////////////////////////////////////////
void cfdDCS::SetTranslationArray( float* trans )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_translation[ i ] = trans[ i ];
   }
#ifdef _PERFORMER

   this->_dcs->setTrans( this->_translation[ 0 ],
                       this->_translation[ 1 ],
                       this->_translation[ 2 ] );
#elif _OSG           
   //update the specified transform
   if(_udcb){
      _udcb->setTranslation(_translation);
   }
   
#elif _OPENSG
#endif
   //std::cout << this->_translation[ 0 ]<< " : " << this->_translation[ 1 ] << " : " <<
//this->_translation[ 2 ]  << std::endl;
}
///////////////////////////////////////////
void cfdDCS::SetRotationArray( float* rot )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_rotation[ i ] = rot[ i ];
   }
#ifdef _PERFORMER
   this->_dcs->setRot(this->_rotation[ 0 ],
                    this->_rotation[ 1 ],
                    this->_rotation[ 2 ]);
#elif _OSG
   if ( _udcb )
   {
      _udcb->setRotationDegreeAngles(_rotation[0],_rotation[1],_rotation[2]);
   }
#elif _OPENSG
#endif
   //std::cout << this->_rotation[ 0 ]<< " : " <<   
//                           this->_rotation[ 1 ] << " : " <<
//                           this->_rotation[ 2 ]  << std::endl;
}
//////////////////////////////////////////
void cfdDCS::SetScaleArray( float* scale )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_scale[ i ] = scale[ i ];
   }
#ifdef _PERFORMER
   this->_dcs->setScale( this->_scale[ 0 ],
                           this->_scale[ 1 ],
                           this->_scale[ 2 ] );
#elif _OSG
   if(_udcb){
      _udcb->setScaleValues(_scale);
   }
#elif _OPENSG
#endif
   //std::cout << this->_scale[ 0 ]<< " : " <<   
//                           this->_scale[ 1 ] << " : " <<
//                           this->_scale[ 2 ]  << std::endl;
}
////////////////////////////////
Matrix44f cfdDCS::GetMat( void )
{
#ifdef _PERFORMER
   pfMatrix temp;
   this->_dcs->getMat( temp );
   _vjMatrix = vrj::GetVjMatrix( temp );
#elif _OSG
   osg::Matrixf osgMat= _dcs->getMatrix();
   if(osgMat.valid()){
   gmtl::Vec3f x_axis( 1.0f, 0.0f, 0.0f );
   _vjMatrix.set( osgMat.ptr() );
   //gmtl::postMult(_vjMatrix, gmtl::makeRot<gmtl::Matrix44f>( gmtl::AxisAnglef( gmtl::Math::deg2Rad(-90.0f), x_axis ) ));
   //gmtl::preMult(_vjMatrix, gmtl::makeRot<gmtl::Matrix44f>( gmtl::AxisAnglef( gmtl::Math::deg2Rad(90.0f), x_axis ) ));
   //_vjMatrix.set(osgMat.ptr());

   }else{
      std::cout<<"Invalid matrix!!"<<std::endl;
      std::cout<<"cfdDCS::GetMat()"<<std::endl;
   }
#elif _OPENSG
   // GetVjMatrix
   cerr << " ERROR: cfdDCS::GetMat is NOT implemented " << endl;
   exit( 1 );
#endif
   return _vjMatrix;
}
///////////////////////////////////////
void cfdDCS::SetMat( Matrix44f& input )
{
#ifdef _PERFORMER
   pfMatrix temp = vrj::GetPfMatrix( input );
   this->_dcs->setMat( temp );
#elif _OSG
   if(_dcs.valid())
   {
      osg::Matrix inMat;
      inMat.set(input.getData());
      _dcs->setMatrix(inMat);
   }else{
      
   }
#elif _OPENSG
   std::cerr << " ERROR: cfdDCS::SetMat is NOT implemented " << std::endl;
   exit( 1 );
#endif
}
//////////////////////////////////////////////////
void cfdDCS::SetRotationMatrix( Matrix44f& input )
{
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
   pfCoord* coord = new pfCoord();
   temp.getOrthoCoord( coord );
   _dcs->setRot( coord->hpr[ 0 ], coord->hpr[ 1 ], coord->hpr[ 2 ] );
   //std::cout << coord->hpr[ 0 ] << " :" << coord->hpr[ 1 ] << " : " <<  coord->hpr[ 2 ] <<std::endl;
   delete coord;
#elif _OSG
   if ( _udcb )
   {
      gmtl::Quatf tempQuat = gmtl::make< gmtl::Quatf >( input );
      //std::cout << tempQuat << std::endl;
      osg::Quat quat( tempQuat[ 0 ], tempQuat[ 1 ], tempQuat[ 2 ], tempQuat[ 3 ] );
      _udcb->setQuat( quat );
   }
#elif _OPENSG
   std::cerr << " ERROR: cfdDCS::SetRotationMatrix is NOT implemented " << std::endl;
   exit( 1 );
#endif
}
////////////////////////////////////////////////
int cfdDCS::RemoveChild( cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdDCS::RemoveChild is NOT implemented " << std::endl;
   exit( 1 );
#endif
   std::vector< cfdNode* >::iterator oldChild;
   oldChild = std::find( childNodes.begin(), childNodes.end(), child );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      this->_dcs->removeChild( child->GetRawNode() );
      childNodes.erase( oldChild );
      child->SetParent( NULL );
      return 1;  
   }
   else
   {
      std::cout << " Child Not found " << std::endl;
      return -1;
   }
}
/////////////////////////////////////////////
int cfdDCS::AddChild( cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdDCS::AddChild is NOT implemented " << std::endl;
   exit( 1 );
   return -1;
#endif

   //add node to real graph rep
   int good = this->_dcs->addChild( child->GetRawNode() );
   if ( good )
   {
      //add the child to cfdscene
      childNodes.push_back( child );
      //set the parent in the cfdApp side
      child->SetParent( this );
      return 1;
   }
   
   return -1;
}
///////////////////////////////////////////////////////////////
void cfdDCS::InsertChild( int position, cfdNode* child )
{
#ifdef _OPENSG
   std::cerr << " ERROR: cfdDCS::InsertChild is NOT implemented " << std::endl;
   exit( 1 );
#endif

   this->_dcs->insertChild( position, child->GetRawNode() );
  
   std::vector< cfdNode* >::iterator newPosition;

   newPosition = std::find( childNodes.begin(), childNodes.end(), childNodes[ position ] );

   childNodes.insert( newPosition, child );
   child->SetParent( this );
}

/////////////////////////////////////
int cfdDCS::GetNumChildren( void )
{
   
#ifdef _OPENSG
   std::cerr << " ERROR: cfdDCS::GetNumChildren is NOT implemented " << std::endl;
   exit( 1 );
   return -1;
#endif

   int numChildren = this->_dcs->getNumChildren(); 
   if ( numChildren!=(int)childNodes.size() )
   {
      std::cout << " cfdDCS::ERROR: Number of children don't equal " 
               << numChildren << " : " << childNodes.size() << std::endl;
      exit( 1 );
   }
   return numChildren;
}
//////////////////////////////////////
const char* cfdDCS::GetName( void )
{
#ifdef _OPENSG
   return 0;
#endif
#ifdef _PERFORMER
   return _dcs->getName();
#elif _OSG
    return _dcs->getName().data();
#endif
}
////////////////////////////////////
void cfdDCS::SetName( char* name )
{
   const std::string test(name);
#ifdef _OPENSG
   std::cerr << " ERROR: cfdDCS::SetName is NOT implemented " << std::endl;
   exit( 1 );
#endif
#ifdef _PERFORMER
   _dcs->setName( test.c_str() );
#elif _OSG
   _dcs->setName( test );
#endif
}
////////////////////////////////////////////////////////////
int cfdDCS::ReplaceChild( cfdNode* childToBeReplaced,
                         cfdNode* newChild)
{
#ifdef _OPENSG
   cerr << " ERROR: cfdDCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
   std::vector< cfdNode* >::iterator oldChild;
   oldChild = std::find( childNodes.begin(), childNodes.end(), childToBeReplaced );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      // Just erases from the vector doesn't delete memory
      childNodes.erase( oldChild );
      this->_dcs->replaceChild( childToBeReplaced->GetRawNode(), 
                                      newChild->GetRawNode() );

      //add the child to cfdscene
      childNodes.push_back( newChild );
      // Set new parent for the new child
      newChild->SetParent( this );
      // Show that he no longer has a parent
      childToBeReplaced->SetParent( NULL );
      return 1;
   }
   else
   {
      std::cout << " Error : Child not found " << std::endl;
      return -1;
   }
}
// Reimplement for other graphs
#ifdef _PERFORMER
pfNode* cfdDCS::GetRawNode( void )
#elif _OSG
osg::Node* cfdDCS::GetRawNode(void)
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _dcs;
#elif _OSG
   return _dcs.get();
#elif _OPENSG
#endif
}
#ifdef _OSG
////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////
cfdDCS::cfdUpdateDCSCallback::cfdUpdateDCSCallback()
{
   _scale[0] = 1.;
   _scale[1] = 1.;
   _scale[2] = 1.;
   _trans[0] = 0;
   _trans[1] = 0;
   _trans[2] = 0;
   _h = 0;
   _p = 0;
   _r = 0;
}
//////////////////////////////////////////////////////////////////
cfdDCS::cfdUpdateDCSCallback::cfdUpdateDCSCallback( const cfdUpdateDCSCallback& input )
:osg::Object( input ), osg::NodeCallback( input )
{
   _scale[0] = input._scale[0];
   _scale[1] = input._scale[1];
   _scale[2] = input._scale[2];
   _trans[0] = input._trans[0];
   _trans[1] = input._trans[1];
   _trans[2] = input._trans[2];
   _h = input._h;
   _p = input._p;
   _r = input._r;
   quat = input.quat;
}
//////////////////////////////////////////////////////////////////
void cfdDCS::cfdUpdateDCSCallback::setRotationDegreeAngles(float h,
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
void cfdDCS::cfdUpdateDCSCallback::setQuat( osg::Quat& input )
{
   quat = input;
}
///////////////////////////////////////////////////////////////
void cfdDCS::cfdUpdateDCSCallback::setTranslation(float* trans)
{
   if ( trans )
   {
      _trans[0] = trans[0];
      _trans[1] = trans[1];
      _trans[2] = trans[2];
   }
}
////////////////////////////////////////////////////////////////
void cfdDCS::cfdUpdateDCSCallback::setScaleValues(float* scale)
{
   if ( scale )
   {
      _scale[0] = scale[0];
      _scale[1] = scale[1];
      _scale[2] = scale[2];
   }
}
//////////////////////////////////////////////////////////////////////////////////////
void cfdDCS::cfdUpdateDCSCallback::operator()(osg::Node* node, osg::NodeVisitor* nv)
{
   osg::ref_ptr<osg::MatrixTransform> dcs = dynamic_cast<osg::MatrixTransform*>(node);
   if ( dcs.valid() )
   {
      osg::Vec3f pitch(1,0,0);
      osg::Vec3f roll(0,1,0);
      osg::Vec3f yaw(0,0,1);
      
      osg::Matrixd scale = osg::Matrixd::scale(_scale[0],_scale[1],_scale[2]);
      osg::Matrixd rotateMat;

      rotateMat.makeRotate( quat );

      osg::Vec3f transMat( _trans[0], _trans[1], _trans[2] );
      scale.setTrans(transMat);

      dcs->setMatrix(rotateMat*scale);
      traverse(node,nv);
   }
 }
#endif

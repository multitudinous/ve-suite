/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
#include "cfdDCS.h"

#include <iostream>
#include <cstdlib>

#include <gmtl/Generate.h>
#include <gmtl/Coord.h>

#ifdef _PERFORMER
#include <vrj/Draw/Pf/PfUtil.h>

#include <Performer/pf.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pr/pfLinMath.h>

#elif _OSG
#include <osg/MatrixTransform>
#include <osg/Matrix>
#elif _OPENSG
#endif

using namespace gmtl;

cfdDCS::cfdDCS( float* scale, float* trans, float* rot )
:cfdGroup()
{
#ifdef _PERFORMER
   _dcs = new pfDCS();
   _group = dynamic_cast<pfGroup*>(_dcs);
#elif _OSG
   _dcs = new osg::MatrixTransform();
   _dcs->setMatrix(osg::Matrix::identity());
   _group = dynamic_cast<osg::Group*>(_dcs);
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
      this->_rotation[ i ] = input._rotation[ i ];
      this->_scale[ i ] = input._scale[ i ];
   }

   this->childNodes = input.childNodes;
   this->_vjMatrix = input._vjMatrix;
   this->_dcs = input._dcs;
#ifdef _PERFORMER
   
   _group = dynamic_cast<pfGroup*>(_dcs);
#elif _OSG
   _group = dynamic_cast<osg::Group*>(_dcs); 
   //_dcs->getMatrix()->makeIdentity();
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
         this->_rotation[ i ] = input._rotation[ i ];
         this->_scale[ i ] = input._scale[ i ];
      }
   
      for ( unsigned int i = 0; i < childNodes.size(); i++ )
      {
         delete childNodes.at( i );
      }   
      childNodes.clear();
   
      this->childNodes = input.childNodes;
      this->_vjMatrix = input._vjMatrix;
#ifdef _PERFORMER
      pfDelete( this->_dcs );
      this->_dcs = input._dcs;
      _group = dynamic_cast<pfGroup*>(_dcs);
#elif _OSG
      _dcs->unref();
      _dcs = input._dcs;
      _group = dynamic_cast<osg::Group*>(_dcs);
#elif _OPENSG
#endif
      //_group = _dcs;
      SetCFDNodeType(CFD_DCS);
   }
   return *this;
}
//////////////////////
cfdDCS::cfdDCS( void )
:cfdGroup()
{
#ifdef _PERFORMER
   _dcs = new pfDCS();
   _group = dynamic_cast<pfGroup*>(_dcs);
#elif _OSG
   _dcs = new osg::MatrixTransform();
   _group = dynamic_cast<osg::Group*>(_dcs);
   _dcs->setMatrix(osg::Matrix::identity());
#elif _OPENSG
#endif
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
float* cfdDCS::GetTranslationArray( void )
{
   return this->_translation;
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
   _dcs->getMatrix().translate( this->_translation[ 0 ],
                           this->_translation[ 1 ],
                           this->_translation[ 2 ] );
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
   //_dcs->getMatrix()->rotate();

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
   _dcs->getMatrix().scale(this->_scale[ 0 ],
                           this->_scale[ 1 ],
                           this->_scale[ 2 ] );
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
   _vjMatrix.set((float*)_dcs->getMatrix().ptr()); 
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
   if(_dcs){
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
   pfCoord* coord = new pfCoord();
   temp.getOrthoCoord( coord );
   _dcs->setRot( coord->hpr[ 0 ], coord->hpr[ 1 ], coord->hpr[ 2 ] );
   delete coord;
#elif _OSG
   osg::Matrix rot;
   rot.set(osg::Matrix::identity());;
   osg::Matrix orig;
   orig.set(osg::Matrix::identity());

   orig.set(_dcs->getMatrix());
   rot.set(input.getData());
   
   _dcs->setMatrix(rot* orig);
   std::cerr <<"Check to make sure rotation is working!!"<<std::endl;
   std::cerr <<"OSG cfdDCS::SetRotationMatrix."<<std::endl;
   
#elif _OPENSG
   std::cerr << " ERROR: cfdDCS::SetRotationMatrix is NOT implemented " << std::endl;
   exit( 1 );
#endif
}


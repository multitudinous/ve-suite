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
using namespace std;

#include <gmtl/Generate.h>
#include <gmtl/Coord.h>
#include <vrj/Draw/Pf/PfUtil.h>

#ifdef _PERFORMER
#include <Performer/pf.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pr/pfLinMath.h>
#elif _OSG
#elif _OPENSG
#endif

using namespace vrj;

cfdDCS::cfdDCS( float* scale, float* trans, float* rot ):cfdSceneNode()
{
#ifdef _PERFORMER
   this->_dcs = new pfDCS();
#elif _OSG
#elif _OPENSG
#endif
   this->SetTranslationArray( trans );
   this->SetRotationArray( rot );
   this->SetScaleArray( scale );
}

cfdDCS::cfdDCS( const cfdDCS& input )
{
   for ( int i = 0; i < 3; i++ )
   {
      this->_translation[ i ] = input._translation[ i ];
      this->_rotation[ i ] = input._rotation[ i ];
      this->_scale[ i ] = input._scale[ i ];
   }

   this->childNodes = input.childNodes;
   this->_vjMatrix = input._vjMatrix;

#ifdef _PERFORMER
   this->_dcs = input._dcs;
#elif _OSG
#elif _OPENSG
#endif
}

cfdDCS& cfdDCS::operator=( const cfdDCS& input)
{
   if ( this != &input )
   {
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
#elif _OSG
#elif _OPENSG
#endif
   }
   return *this;
}

cfdDCS::cfdDCS( void ):cfdSceneNode()
{
#ifdef _PERFORMER
   this->_dcs = new pfDCS();
#elif _OSG
#elif _OPENSG
#endif
}

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

float* cfdDCS::GetTranslationArray( void )
{
   return this->_translation;
}

float* cfdDCS::GetRotationArray( void )
{
   return this->_rotation;
}

float* cfdDCS::GetScaleArray( void )
{
   return this->_scale;
}

void cfdDCS::SetTranslationArray( float* trans )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_translation[ i ] = trans[ i ];
   }

   this->_dcs->setTrans( this->_translation[ 0 ],
                           this->_translation[ 1 ],
                           this->_translation[ 2 ] );
//std::cout << this->_translation[ 0 ]<< " : " << this->_translation[ 1 ] << " : " <<
//this->_translation[ 2 ]  << std::endl;
}

void cfdDCS::SetRotationArray( float* rot )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_rotation[ i ] = rot[ i ];
   }

   this->_dcs->setRot( this->_rotation[ 0 ],
                          this->_rotation[ 1 ],
                           this->_rotation[ 2 ] );
//std::cout << this->_rotation[ 0 ]<< " : " <<   
//                           this->_rotation[ 1 ] << " : " <<
//                           this->_rotation[ 2 ]  << std::endl;
}

void cfdDCS::SetScaleArray( float* scale )
{
   int i;
   for ( i = 0; i < 3; i++ )
   {
      this->_scale[ i ] = scale[ i ];
   }

   this->_dcs->setScale( this->_scale[ 0 ],
                           this->_scale[ 1 ],
                           this->_scale[ 2 ] );
//std::cout << this->_scale[ 0 ]<< " : " <<   
//                           this->_scale[ 1 ] << " : " <<
//                           this->_scale[ 2 ]  << std::endl;
}

#ifdef _PERFORMER
pfNode* cfdDCS::GetRawNode( void )
#elif _OSG
#elif _OPENSG
#endif
{
#ifdef _PERFORMER
   return _dcs;
#elif _OSG
   cerr << " ERROR: cfdDCS::GetRawNode is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::GetRawNode is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#endif
}

int cfdDCS::RemoveChild( cfdSceneNode* child )
{
#ifdef _PERFORMER
   vector< cfdSceneNode* >::iterator oldChild;
   oldChild = find( childNodes.begin(), childNodes.end(), child );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      this->_dcs->removeChild( (*oldChild)->GetRawNode() );
      childNodes.erase( oldChild );
      child->SetParent( NULL );
      return 1;  
   }
   else
   {
      cout << " Child Not found " << endl;
      return -1;
   }
#elif _OSG
   cerr << " ERROR: cfdDCS::RemoveChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::RemoveChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

int cfdDCS::AddChild( cfdSceneNode* child )
{
#ifdef _PERFORMER
   childNodes.push_back( child );
   this->_dcs->addChild( child->GetRawNode() );
   child->SetParent( this );
   return 1;
#elif _OSG
   cerr << " ERROR: cfdDCS::AddChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::AddChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

int cfdDCS::ReplaceChild( cfdSceneNode* childToBeReplaced, cfdSceneNode* newChild )
{
#ifdef _PERFORMER
   vector< cfdSceneNode* >::iterator oldChild;
   oldChild = find( childNodes.begin(), childNodes.end(), childToBeReplaced );
   
   // Check to make sure he is on this node
   if ( oldChild != childNodes.end() )
   {
      // Just erases from the vector doesn't delete memory
      childNodes.erase( oldChild );
      this->_dcs->replaceChild( childToBeReplaced->GetRawNode(), 
                                                newChild->GetRawNode() );

      // Set new parent for the new child
      newChild->SetParent( this );
      // Show that he no longer has a parent
      childToBeReplaced->SetParent( NULL );
      return 1;
   }
   else
   {
      cout << " Error : Child not found " << endl;
      return -1;
   }
#elif _OSG
   cerr << " ERROR: cfdDCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::ReplaceChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

Matrix44f cfdDCS::GetMat( void )
{
#ifdef _PERFORMER
   pfMatrix temp;
   this->_dcs->getMat( temp );
   _vjMatrix = GetVjMatrix( temp );
#elif _OSG
   // GetVjMatrix
   cerr << " ERROR: cfdDCS::GetMat is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   // GetVjMatrix
   cerr << " ERROR: cfdDCS::GetMat is NOT implemented " << endl;
   exit( 1 );
#endif
   return _vjMatrix;
}

int cfdDCS::SearchChild( cfdSceneNode* child )
{
#ifdef _PERFORMER
   for ( unsigned int i = 0; i < childNodes.size(); i++ )
      if ( childNodes[ i ] == child )
      {
         return (int)i;
      }
   // if not found
   return -1;
#elif _OSG
   cerr << " ERROR: cfdDCS::SearchChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::SearchChild is NOT implemented " << endl;
   exit( 1 );
   return -1;
#endif
}

int cfdDCS::GetNumChildren( void )
{
#ifdef _PERFORMER
   int numChildren = this->_dcs->getNumChildren();
   if ( numChildren != (int)this->childNodes.size() )
   {
      cout << " ERROR: Number of children don't equal " << endl;
      exit( 1 );
   }

   return numChildren;
#elif _OSG
   cerr << " ERROR: cfdDCS::GetNumChildren is NOT implemented " << endl;
   exit( 1 );
   return 1;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::GetNumChildren is NOT implemented " << endl;
   exit( 1 );
   return 1;
#endif
}

void cfdDCS::SetMat( Matrix44f& input )
{
#ifdef _PERFORMER
   pfMatrix temp = GetPfMatrix( input );
   this->_dcs->setMat( temp );
#elif _OSG
   cerr << " ERROR: cfdDCS::SetMat is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   cerr << " ERROR: cfdDCS::SetMat is NOT implemented " << endl;
   exit( 1 );
#endif
}

cfdSceneNode* cfdDCS::GetChild( int child )
{
#ifdef _PERFORMER
   // Need to catch the exceptiom
   // Need to fix
   return childNodes.at( child );
#elif _OSG
   cerr << " ERROR: cfdDCS::GetChild is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#elif _OPENSG
   cerr << " ERROR: cfdDCS::GetChild is NOT implemented " << endl;
   exit( 1 );
   return NULL;
#endif
}

void cfdDCS::SetRotationMatrix( Matrix44f& input )
{
   Coord4fZXY coord;
   gmtl::set( coord, input );
   // Need to set rotation to this matrix
#ifdef _PERFORMER
   // Need to find who calls this function
   // fix this
#elif _OSG
   cerr << " ERROR: cfdDCS::SetRotationMatrix is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   cerr << " ERROR: cfdDCS::SetRotationMatrix is NOT implemented " << endl;
   exit( 1 );
#endif
}

void cfdDCS::SetName( char* name )
{
#ifdef _PERFORMER
   this->_dcs->setName( name );
#elif _OSG
   cerr << " ERROR: cfdDCS::SetName is NOT implemented " << endl;
   exit( 1 );
#elif _OPENSG
   cerr << " ERROR: cfdDCS::SetName is NOT implemented " << endl;
   exit( 1 );
#endif
}

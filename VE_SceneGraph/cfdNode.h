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
 * File:          $RCSfile: cfdNode.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_NODE_H
#define CFD_NODE_H

#include "cfdSceneNode.h"

#ifdef _PERFORMER
class pfNode;
class pfFog;
#elif _OSG
namespace osg{
   class Node;
   class Fog;
}
#elif _OPENSG
#endif

class cfdNode: public cfdSceneNode{
public:
   
   cfdNode( void );
   cfdNode(cfdSceneNode::cfdNodeType nt);

   //biv--don't understand this method
   cfdNode( float*, float*, float* );
   
   //copy constructor
   cfdNode( const cfdNode& );
   virtual ~cfdNode( void );

   //equal operator
   cfdNode& operator=( const cfdNode& );
   

#ifdef _PERFORMER
   pfNode* GetRawNode( void );
   void clearGeodesFromNode( pfNode* );
#elif _OSG
   osg::Node* GetRawNode( void );
   void clearGeodesFromNode( osg::Node* );
#elif _OPENSG
#endif

   //biv--why is this stuff in this class?
   //should change to take a cfdNode and 
   //let internals call get raw node and then
   //traverse. . .
#ifdef _PERFORMER
   void pfTravNodeMaterial( pfNode* );
   void pfTravNodeFog( pfNode* node_1, pfFog* fog );
#elif _OSG
   void TravNodeMaterial(cfdNode*);
   void TravNodeFog(osg::Node* node_1, osg::Fog* fog);
#elif _OPENSG
#endif
   void SetNodeProperties( int, float, float* );
   void LoadFile( char* );
      
   cfdNode* Clone( int );   
protected:

   float op;
   float stlColor[ 3 ];
   int color;
};
#endif

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
 * File:          $RCSfile: cfdNode.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_NODE_H
#define CFD_NODE_H

#include "VE_SceneGraph/cfdSceneNode.h"

#ifdef _PERFORMER
class pfNode;
class pfFog;
#elif _OSG
namespace osg 
{ 
   class Fog; 
   class Node;
}
#include <osg/ref_ptr>
#elif _OPENSG
#endif

class WXPLUGIN_DECLSPEC cfdNode: public cfdSceneNode
{
   public:   
      cfdNode( void );
      //cfdNode(cfdSceneNode::cfdNodeType nt);

      //copy constructor
      cfdNode( const cfdNode& );
      virtual ~cfdNode( void );

      //equal operator
      cfdNode& operator= ( const cfdNode& );

      //equality operator
      //bool operator== ( const cfdNode& ) const;
#ifdef _PERFORMER
      virtual pfNode* GetRawNode( void );
#elif _OSG
      virtual osg::Node* GetRawNode( void );
#elif _OPENSG
#endif

#ifdef _PERFORMER
      void pfTravNodeMaterial( pfNode* );
      void pfTravNodeFog( pfNode* node_1, pfFog* fog );
#elif _OSG
      void TravNodeMaterial(osg::Node*);
      void TravNodeFog(osg::Node* node_1, osg::Fog* fog);
#elif _OPENSG
#endif
      void SetNodeProperties( int, float, float* );
      void LoadFile( char* );
      
      cfdNode* Clone( int );

   protected:
#ifdef _PERFORMER
      pfNode* _node;
#elif _OSG
      osg::ref_ptr<osg::Node> _node;
#elif _OPENSG
#endif
      float op;
      float stlColor[ 3 ];
      int color;
};
#endif

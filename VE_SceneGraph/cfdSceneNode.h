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
 * File:          $RCSfile: cfdSceneNode.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_SCENENODE_H
#define CFD_SCENENODE_H


#ifdef _PERFORMER
class pfNode;
class pfGeode;
class pfDCS;
class pfGroup;
class pfSwitch;
#elif _OSG
namespace osg
{
   class Node;
   class Geode;
   class Group;
   class MatrixTransform;
   class Switch;
};
#elif _OPENSG
#else
#error "Define either _PERFORMER, _OSG, or _OPENSG"
#endif

#include "VE_Installer/include/VEConfig.h"
namespace VE_SceneGraph
{
   class cfdNode;
   class cfdSequence;
}
/////////////////////////////////////////////
//This class holds and manages the nodes on//
//the graph.                               //
//If adding functionality(new node types)  //
//it should be added here in this class    //
//and then an appropriate wrapper class    //
//should be created.                       //
/////////////////////////////////////////////
namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS cfdSceneNode
{
public:
   enum cfdNodeType
   {
      CFD_NODE,
      CFD_GROUP,
      CFD_DCS,
      CFD_GEODE,
      CFD_SEQUENCE,
      CFD_SWITCH,
      CFD_OTHER
   };

   cfdSceneNode();
   cfdSceneNode(cfdNodeType nt);
   cfdSceneNode( const cfdSceneNode& );

   virtual ~cfdSceneNode( void );
   cfdSceneNode& operator=( const cfdSceneNode& );

   //get the internal cfd node type
   virtual cfdNodeType GetCFDNodeType(){return _nt;}

   //retrieve the underlying node depending on
   //scene graph
#ifdef _PERFORMER
   virtual pfNode* GetRawNode( void )=0;
#elif _OSG
   virtual osg::Node* GetRawNode(void)=0;
#elif _OPENSG
#endif
   //the parent node    
   cfdNode* GetParent( int );

   //set the parent
   void SetParent( cfdNode* );
   
   //set internal cfd node type
   virtual void SetCFDNodeType(cfdNodeType nt){_nt = nt;}

   //vpr::GUID guid;
protected:
   //our scene graph specific nodes
   int _numParents;
   cfdNodeType _nt;
   cfdNode* _parent;
};
}
#endif

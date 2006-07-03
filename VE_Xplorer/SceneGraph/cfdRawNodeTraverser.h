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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CFD_RAW_NODE_TRAVERSER_H_
#define _CFD_RAW_NODE_TRAVERSER_H_

#include "VE_Xplorer/SceneGraph/cfdNode.h"

#ifdef _PERFORMER
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Node>
#endif
namespace VE_SceneGraph{
   class VE_SCENEGRAPH_EXPORTS cfdRawNodeTraverser
   {
      public:
         cfdRawNodeTraverser();
         cfdRawNodeTraverser(const cfdRawNodeTraverser& cfdNT);
         virtual ~cfdRawNodeTraverser();
         enum TraversalStatus{CONT,SKIP,STOP};
   
      //the pre and post node callbacks
#ifdef _PERFORMER
      typedef void (*preNodeTraverseCallback)(cfdRawNodeTraverser*,pfNode*);
      typedef void (*postNodeTraverseCallback)(cfdRawNodeTraverser*,pfNode*);
#elif _OSG
      typedef void (*preNodeTraverseCallback)(cfdRawNodeTraverser*,osg::Node*);
      typedef void (*postNodeTraverseCallback)(cfdRawNodeTraverser*,osg::Node*);
#endif

      //set the pre node traverse callback
      virtual void setPreNodeTraverseCallback(preNodeTraverseCallback func)
      {
         _preFunc = func;
      }
      //set the post node traverse callback
      virtual void setPostNodeTraverseCallback(postNodeTraverseCallback func)
      {
         _postFunc = func;
      }
      
      //set the node to traverse
#ifdef _PERFORMER
      void setNode(pfNode* root,bool deepClone=false);
#elif _OSG      
      void setNode(osg::Node* root,bool deepClone=false);
#endif

      void setTraversalStatus(TraversalStatus ts){_ts = ts;}
      TraversalStatus traversalStatus(){return _ts;}

      //begin traversing the node
      void traverse();

      //equal operator
      cfdRawNodeTraverser& operator=(const cfdRawNodeTraverser& cfdNT);
   protected:
      //recurse the nodes

      TraversalStatus _ts;
#ifdef _PERFORMER
     virtual void _traverseNode(pfNode* currentNode);
     pfNode* _root;
#elif _OSG
     virtual void _traverseNode(osg::Node* currentNode);
     osg::ref_ptr<osg::Node> _root;
#endif
      preNodeTraverseCallback _preFunc;
      postNodeTraverseCallback _postFunc;
   };
}
#endif// _CFD_RAW_NODE_TRAVERSER_H_

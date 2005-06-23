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
 * File:          $RCSfile: cfdNodeTraverser.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CFD_NODE_TRAVERSER_H_
#define _CFD_NODE_TRAVERSER_H_

#include "VE_SceneGraph/cfdNode.h"

#ifdef _PERFORMER
#include <Performer/pf/pfNode.h>
#elif _OSG
#include <osg/Node>
#endif

class cfdNodeTraverser
{
public:
   cfdNodeTraverser();
   cfdNodeTraverser(const cfdNodeTraverser& cfdNT);
   virtual ~cfdNodeTraverser();
   
   //the pre and post node callbacks
   typedef void (*preNodeTraverseCallback)(cfdNodeTraverser*,cfdNode*);
   typedef void (*postNodeTraverseCallback)(cfdNodeTraverser*,cfdNode*);

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
   void setNode(cfdNode* root);

   //begin traversing the node
   void traverse();

   //equal operator
   cfdNodeTraverser& operator=(const cfdNodeTraverser& cfdNT);
protected:
   //recurse the nodes
   virtual void _traverseNode(cfdNode* currentNode);

   cfdNode* _root;
   preNodeTraverseCallback _preFunc;
   postNodeTraverseCallback _postFunc;
};
#endif// _CFD_NODE_TRAVERSER_H_

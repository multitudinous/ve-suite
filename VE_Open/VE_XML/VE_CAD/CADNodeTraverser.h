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
 * File:          $RCSfile: CADNodeTraverser.h,v $
 * Date modified: $Date: 2005-07-11 13:47:16 -0500 (Mon, 11 Jul 2005) $
 * Version:       $Rev: 2653 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef _CAD_NODE_TRAVERSER_H_
#define _CAD_NODE_TRAVERSER_H_

/*!\file CADNodeTraverser.h
  CADNodeTraverser API
  */
/*!\class VE_CAD::CADNodeTraverser
 * Base class for traversing VE_CAD::CADNode.
 */

namespace VE_CAD
{
   class CADNode;
}

#include "VE_Installer/include/VEConfig.h"
namespace VE_CAD{
   class VE_CAD_EXPORTS CADNodeTraverser
   {
      public:
         ///Constructor
         CADNodeTraverser();
         ///Copy Constructor
         CADNodeTraverser(const CADNodeTraverser& cfdNT);
         ///Destructor
         virtual ~CADNodeTraverser();

	    
   
      /*!\class VE_CAD::CADNodeTraverser::CADNodeTraverseCallback
       * Class that defines pre/post CADNode traversal tasks.
       */
      class CADNodeTraverseCallback{
         public:
            ///Constructor
            CADNodeTraverseCallback(){;}
	         
            ///Destructor
            virtual ~CADNodeTraverseCallback(){;}

            ///This is the function to override to do something to a node
            ///before (pre) or after (post) encountering a CADNode in the graph.
            ///\param cadNodeTraverser The CADNodeTraverser that is doing the traversing.
            ///\param node The VE_CAD::CADNode that is currently being encountered.
	         virtual void Apply(CADNodeTraverser* cadNodeTraverser,VE_CAD::CADNode* node)=0;
         protected:
      };
      ///TraversalStatus The status of the traverser.
      enum TraversalStatus{CONT,SKIP,STOP};

      ///Set the pre node traverse callback
      ///\param func The pre-traverse callback function.
      virtual void SetPreNodeTraverseCallback(CADNodeTraverser::CADNodeTraverseCallback* func);

      ///Set the post node traverse callback
      ///\param func The post-traverse callback function.
      virtual void SetPostNodeTraverseCallback(CADNodeTraverser::CADNodeTraverseCallback* func);

      ///Set the node to traverse
      ///\param root The root node to traverse.
      void SetRootNode(VE_CAD::CADNode* root);

      ///Set the traversal status.
      ///\param ts The new traversal status.
      void SetTraversalStatus(TraversalStatus ts){_ts = ts;}
      
      ///Get the traversal status.
      TraversalStatus GetTraversalStatus(){return _ts;}

      ///Begin traversing the node.
      void Traverse();

      ///equal operator
      CADNodeTraverser& operator=(const CADNodeTraverser& cfdNT);
   protected:

      ///Recurse the nodes internally.
      ///\param currentNode The node currently being traversed.
      virtual void _traverseNode(VE_CAD::CADNode* currentNode);

      TraversalStatus _ts;///<The status of the traverser.
      VE_CAD::CADNode* _root;///<The root node to traverse.
      CADNodeTraverser::CADNodeTraverseCallback* _preFunc;///<The pre traverse callback.
      CADNodeTraverser::CADNodeTraverseCallback* _postFunc;///<The post node traverse callback.
   };
}
#endif// _CFD_NODE_TRAVERSER_H_

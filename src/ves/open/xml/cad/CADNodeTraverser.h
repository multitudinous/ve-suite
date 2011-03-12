/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef _CAD_NODE_TRAVERSER_H_
#define _CAD_NODE_TRAVERSER_H_

/*!\file CADNodeTraverser.h
  CADNodeTraverser API
  */
/*!\class ves::open::xml::cad::CADNodeTraverser
 * Base class for traversing ves::open::xml::cad::CADNode.
 */
/*!\namespace ves::open::xml::cad
 * Contains nodes for creating/managing a CAD hierarchy.
 */


#include <ves/VEConfig.h>

#include <ves/open/xml/cad/CADNodePtr.h>
#include <ves/open/xml/cad/CADAssemblyPtr.h>


namespace ves
{
namespace open
{
namespace xml
{
namespace cad
{
class VE_CAD_EXPORTS CADNodeTraverser
{
public:
    ///Constructor
    CADNodeTraverser();
    ///Copy Constructor
    CADNodeTraverser( const CADNodeTraverser& cfdNT );
    ///Destructor
    virtual ~CADNodeTraverser();




    class VE_CAD_EXPORTS CADNodeTraverseCallback
    {
    public:
        ///Constructor
        CADNodeTraverseCallback()
        {
            ;
        }

        ///Destructor
        virtual ~CADNodeTraverseCallback()
        {
            ;
        }

        ///This is the function to override to do something to a node
        ///before (pre) or after (post) encountering a CADNode in the graph.
        ///\param cadNodeTraverser The CADNodeTraverser that is doing the traversing.
        ///\param node The CADNode that is currently being encountered.
        ///\param currentParent The CADNode that is the parent of the node being encountered.
        virtual void Apply( CADNodeTraverser* cadNodeTraverser, CADNodePtr node, CADAssemblyPtr currentParent ) = 0;
    protected:
    };
    ///TraversalStatus The status of the traverser.
    enum TraversalStatus
    {
        CONT,///<Continue traversing.
        SKIP,///<Skip traversing this CADNode.
        STOP///<Stop traversal of the graph.
    };

    ///Set the pre node traverse callback
    ///\param func The pre-traverse callback function.
    virtual void SetPreNodeTraverseCallback( CADNodeTraverseCallback* func );

    ///Set the post node traverse callback
    ///\param func The post-traverse callback function.
    virtual void SetPostNodeTraverseCallback( CADNodeTraverseCallback* func );

    ///Set the node to traverse
    ///\param root The root node to traverse.
    void SetRootNode( CADNodePtr root );

    ///Set the traversal status.
    ///\param ts The new traversal status.
    void SetTraversalStatus( TraversalStatus ts )
    {
        _ts = ts;
    }

    ///Get the traversal status.
    TraversalStatus GetTraversalStatus()
    {
        return _ts;
    }

    ///Begin traversing the node.
    void Traverse();

    ///equal operator
    CADNodeTraverser& operator=( const CADNodeTraverser& cfdNT );
protected:

    ///Recurse the nodes internally.
    ///\param currentNode The node currently being traversed.
    ///\param currentParent The CADNode that is the parent of the node being encountered.
    virtual void _traverseNode( CADNodePtr currentNode, CADAssemblyPtr currentParent = CADAssemblyPtr() );

    TraversalStatus _ts;///<The status of the traverser.
    CADNodePtr _root;///<The root node to traverse.
    CADNodeTraverser::CADNodeTraverseCallback* _preFunc;///<The pre traverse callback.
    CADNodeTraverser::CADNodeTraverseCallback* _postFunc;///<The post node traverse callback.
};
}
}
}
}
#endif// _CFD_NODE_TRAVERSER_H_

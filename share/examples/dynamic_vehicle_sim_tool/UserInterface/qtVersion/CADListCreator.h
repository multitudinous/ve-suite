/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#ifndef VES_CAD_LIST_CREATOR
#define VES_CAD_LIST_CREATOR

/*!\file CADListCreator.h
  CADListCreator API
  */
/*!\class dynamicvehicletool::CADListCreator
 * Class for creating wxTreeCtrl and wxTreeItem from ves::open::xml::cad::CADNode.
 */

#include <ves/open/xml/cad/CADNodePtr.h>
#include <ves/open/xml/cad/CADNodeTraverser.h>

//#include <wx/treectrl.h>
//#include <wx/window.h>

#include <vector>

namespace dynamicvehicletool
{
class CADListCreator: public ves::open::xml::cad::CADNodeTraverser
{
public:

    ///Constructor
    ///\param cadNode The root CADNode.
    ///\param parent The parent wxWindow
    ///\param id The ID for the tree control.
    CADListCreator( ves::open::xml::cad::CADNodePtr cadNode );

    ///Destructor
    virtual ~CADListCreator();

class TreeGraphPreCallback: public CADNodeTraverser::CADNodeTraverseCallback
    {
    public:
        ///Constructor
        TreeGraphPreCallback()
        {
            ;
        }

        ///Destructor
        virtual ~TreeGraphPreCallback()
        {
            ;
        }

        ///This is the function to override to do something to a node
        ///before (pre) or after (post) encountering a CADNode in the graph.
        ///\param cadNodeTraverser The CADListCreator that is doing the traversing.
        ///\param node The CADNode that is currently being encountered.
        ///\param currentParent The CADNode that is the parent of the node being encountered.
        void Apply( CADNodeTraverser* sceneGraphBuilder, ves::open::xml::cad::CADNodePtr node, ves::open::xml::cad::CADAssemblyPtr currentParent );
        
        std::vector< ves::open::xml::cad::CADNodePtr > m_nodeList;
        std::vector< std::string > m_nodeListNames;
    protected:
    };

    class TreeGraphPostCallback: public CADNodeTraverser::CADNodeTraverseCallback
    {
    public:
        ///Constructor
        TreeGraphPostCallback()
        {
            ;
        }
        
        ///Destructor
        virtual ~TreeGraphPostCallback()
        {
            ;
        }
        
        ///This is the function to override to do something to a node
        ///before (pre) or after (post) encountering a CADNode in the graph.
        ///\param cadNodeTraverser The CADListCreator that is doing the traversing.
        ///\param node The CADNode that is currently being encountered.
        ///\param currentParent The CADNode that is the parent of the node being encountered.
        void Apply( CADNodeTraverser* sceneGraphBuilder, ves::open::xml::cad::CADNodePtr node, ves::open::xml::cad::CADAssemblyPtr currentParent );
        
        std::vector< ves::open::xml::cad::CADNodePtr > m_nodeList;
    protected:
    };
    
    ///Search the tree for a CADNode.
    ///\param name The name to search for.
    ves::open::xml::cad::CADNodePtr GetCADNode( std::string name );

    ///Get the node list
    std::vector< ves::open::xml::cad::CADNodePtr >& GetNodeList();

    ///Get the node list
    std::vector< std::string >& GetNodeNameList();

    std::vector< ves::open::xml::cad::CADNodePtr > m_parentStack;
protected:
    ///The pre traverse callback that creates a wxTreeCtrl.
    CADListCreator::TreeGraphPreCallback* m_treeCtrlCreator;
    ///The post traverse callback that creates a wxTreeCtrl.
    CADListCreator::TreeGraphPostCallback* m_treePostCtrlCreator;
};
}
#endif//VES_CAD_LIST_CREATOR

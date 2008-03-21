/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef CREATE_GRAPH_DOT_VISITOR_H
#define CREATE_GRAPH_DOT_VISITOR_H

/*!\file CreateGraphDOTVisitor.h
*/

/*!\class ves::xplorer::scenegraph::CreateGraphDOTVisitor
*
*/

/*!\namespace ves::xplorer::scenegraph::
*
*/

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>

#include <string>
#include <fstream>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS CreateGraphDOTVisitor : public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param node The node to be traversed
    ///\param inputStream The filename to be written out by the visitor
    CreateGraphDOTVisitor( osg::Node* node, std::string& inputStream );

    ///Destructor
    virtual ~CreateGraphDOTVisitor();

    ///Apply function that gets called during the traversal
    ///\param node A parent node of the node being traversed
    virtual void apply( osg::Node& node );

private:
    ///Get the material string to write out
    ///\param node Node to look at
    ///\return Node data in a string
    std::string GetMaterialDataString( osg::Node* node );

    ///Get the texture string to write out
    ///\param node Node to look at
    ///\return Node data in a string
    std::string GetTextureDataString( osg::Node* node );

    ///Get the texture string to write out
    ///\param node Node to look at
    ///\return Node data in a string
    std::string GetStateSetDataString( osg::Node* node );
    
    ///Get the OSG Stats string to write out
    ///\param node Node to look at
    ///\return Node data in a string
    std::string GetOSGStats( osg::Node* node );

    ///DOT file stream to be written to
    std::ofstream m_dotFile;

};
}
}
}

#endif //CREATE_GRAPH_DOT_VISITOR_H

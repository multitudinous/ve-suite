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
* Date modified: $Date: 2007-05-09 08:48:57 -0500 (Wed, 09 May 2007) $
* Version:       $Rev: 7573 $
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CREATE_GRAPH_DOT_VISITOR_H
#define CREATE_GRAPH_DOT_VISITOR_H

/*!\file CreateGraphDOTVisitor.h
*/

/*!\class VE_SceneGraph::CreateGraphDOTVisitor
*
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

// --- OSG Includes --- //
#include <osg/NodeVisitor>

#include <string>
#include <fstream>

namespace VE_SceneGraph
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
    std::ofstream m_dotFile;///<DOT file strem to be written to
};
}

#endif //CREATE_GRAPH_DOT_VISITOR_H

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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
LocalToWorldNodePath::LocalToWorldNodePath( osg::Node* stopNode,
                                              osg::Node* startNode )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN ),
    mStopNode( stopNode )
{
    startNode->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
LocalToWorldNodePath::~LocalToWorldNodePath()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void LocalToWorldNodePath::apply( osg::Node& node )
{
    if( &node == mStopNode.get() )
    {
        osg::NodePath nodePath = getNodePath();
        if( nodePath.size() == 0 )
        {
            std::cerr << "LocalToWorldNodePath::apply = No nodes" << std::endl;
            return;
        }
        NodeAndPath nap( &node, nodePath );
        mNaplIncludeLocal.push_back( nap );
        /*for( size_t i = 0; i < nodePath.size(); ++i )
        {
            std::cout << "name " << nodePath.at(i)->getName() << " : " << nodePath.at(i)->className() << std::endl;
        }*/
        nodePath.erase( nodePath.end() - 1 );
        NodeAndPath nap2( &node, nodePath );
        mNaplExcludeLocal.push_back( nap2 );
        /*for( size_t i = 0; i < nodePath.size(); ++i )
        {
            std::cout << "name " << nodePath.at(i)->getName() << " : " << nodePath.at(i)->className() << std::endl;
        }*/
        return;
    }
    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
LocalToWorldNodePath::NodeAndPathList LocalToWorldNodePath::GetLocalToWorldNodePath(
    bool includeLocalNode )
{
    if( includeLocalNode )
    {
        return mNaplIncludeLocal;
    }

    return mNaplExcludeLocal;
}
////////////////////////////////////////////////////////////////////////////////

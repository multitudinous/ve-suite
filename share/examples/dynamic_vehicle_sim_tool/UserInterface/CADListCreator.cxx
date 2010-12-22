/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include "CADListCreator.h"

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/cad/CADAssembly.h>

#include <string>

using namespace ves::open::xml::cad;
using namespace dynamicvehicletool;

////////////////////////////////////////////////////////////////////////////////
CADListCreator::CADListCreator( CADNodePtr root )
{
    SetRootNode( root );
    m_treeCtrlCreator = new TreeGraphPreCallback();
    m_treePostCtrlCreator = new TreeGraphPostCallback();
    SetPreNodeTraverseCallback( m_treeCtrlCreator );
    SetPostNodeTraverseCallback( m_treePostCtrlCreator );
    Traverse();
}
////////////////////////////////////////////////////////////////////////////////
CADListCreator::~CADListCreator()
{
    if( m_treeCtrlCreator )
    {
        delete m_treeCtrlCreator;
        m_treeCtrlCreator = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
CADNodePtr CADListCreator::GetCADNode( std::string name )
{
    size_t nNodes = m_treeCtrlCreator->m_nodeList.size();
    for( size_t i = 0; i < nNodes; i++ )
    {
        if( m_treeCtrlCreator->m_nodeList.at( i )->GetNodeName() == name )
            return m_treeCtrlCreator->m_nodeList.at( i );
    }
    return CADNodePtr();
}
////////////////////////////////////////////////////////////////////////////////
std::vector< CADNodePtr >& CADListCreator::GetNodeList()
{
    return m_treeCtrlCreator->m_nodeList;
}
////////////////////////////////////////////////////////////////////////////////
std::vector< std::string >& CADListCreator::GetNodeNameList()
{
    return m_treeCtrlCreator->m_nodeListNames;
}
////////////////////////////////////////////////////////////////////////////////
void CADListCreator::TreeGraphPreCallback::Apply( CADNodeTraverser* treeBuilder, CADNodePtr node, CADAssemblyPtr currentParent )
{
    CADListCreator* treeGraph = dynamic_cast<CADListCreator*>( treeBuilder );
    if( !treeGraph )
        return;
    
    ///
    if( currentParent )
    {
        treeGraph->m_parentStack.push_back( currentParent );
    }
    
    m_nodeList.push_back( node );
    
    ///
    std::string nodeName;
    for( size_t i = 0; i < treeGraph->m_parentStack.size(); ++i )
    {
        nodeName += treeGraph->m_parentStack.at( i )->GetNodeName();
        nodeName += "/";
    }
    nodeName += node->GetNodeName();
    
    m_nodeListNames.push_back( nodeName );
}
////////////////////////////////////////////////////////////////////////////////
void CADListCreator::TreeGraphPostCallback::Apply( CADNodeTraverser* treeBuilder, CADNodePtr node, CADAssemblyPtr currentParent )
{
    CADListCreator* treeGraph = dynamic_cast<CADListCreator*>( treeBuilder );
    if( !treeGraph )
        return;

    if( currentParent )
    {
        treeGraph->m_parentStack.pop_back();
    }
}
////////////////////////////////////////////////////////////////////////////////

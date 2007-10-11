/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
* Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
* Version:       $Rev: 8206 $
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSNode.h>

// --- OSG Includes --- //
#include <osg/Node>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
SetStateOnNURBSNodeVisitor::SetStateOnNURBSNodeVisitor( osg::Node* node, 
    bool selectedState, bool dragState, std::pair< double, double > mousePoint, 
    std::pair< double, double > mouseDelta)
:
NodeVisitor( TRAVERSE_ALL_CHILDREN ),
m_mousePoint( mousePoint ),
m_selectedState( selectedState ),
m_mouseDelta( mouseDelta ),
m_dragState( dragState )
{
    node->accept( *this );    
}
////////////////////////////////////////////////////////////////////////////////
SetStateOnNURBSNodeVisitor::~SetStateOnNURBSNodeVisitor( void )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SetStateOnNURBSNodeVisitor::apply( osg::Node& node )
{
    osg::ref_ptr< osg::Group > tempGroup = node.asGroup();
    if( !tempGroup.valid() )
    {
        return;
    }

    NURBS::NURBSNode* tempNode = 
        dynamic_cast< NURBS::NURBSNode* >( tempGroup.get() );
    if( tempNode )
    {
        //process patches
        //std::cout << " found a patch " << std::endl;
        tempNode->SetSelectionStatus( m_selectedState );
        if( !m_selectedState )
        {
            return;
        }

        if( m_dragState )
        {
            tempNode->MoveSelectedControlPoint( 
                m_mouseDelta.first * 5, 0, -m_mouseDelta.second * 5);
            std::cout <<  "Have a selected point " 
                << tempNode->IsControlPointSelected() << std::endl;
        }
        else
        {
            tempNode->SetMousePosition( -1.0 + m_mousePoint.first*2.0,
                                        -1.0 + m_mousePoint.second*2.0 );
        }
    }
    else
    {
        osg::NodeVisitor::traverse( node );
    }
}
////////////////////////////////////////////////////////////////////////////////

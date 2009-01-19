/*************** <auto-copyright.rb BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/nurbs/NURBS.h>

// --- OSG Includes --- //
#include <osg/Node>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
SetStateOnNURBSNodeVisitor::SetStateOnNURBSNodeVisitor( osg::Node* node,
                                                        bool selectedState, bool dragState,
                                                        std::pair< double, double > mousePoint,
                                                        std::pair< double, double > mouseDelta )
 
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
    ves::xplorer::scenegraph::nurbs::NURBS* tempNode =
        dynamic_cast< ves::xplorer::scenegraph::nurbs::NURBS* >( &node );
    if( tempNode )
    {
        if( !m_selectedState )
        {
    //        std::cout<<"Released"<<std::endl;
            tempNode->ReleaseControlPointSelection();
            return;
        }
        osg::Matrix localToWorld = osg::computeLocalToWorld( getNodePath() );

        //process patches
        if( m_dragState && tempNode->HasSelectedControlPoint() )
        {
     //       std::cout<<"Draggin"<<std::endl;
            tempNode->MoveSelectedControlPoint( localToWorld, 
                                                osg::Vec3( 10*m_mouseDelta.first,
                                                           0,-m_mouseDelta.second*10 ) );
        }
    }
    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////

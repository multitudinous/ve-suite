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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SceneNode.h>
#include <ves/xplorer/scenegraph/DefaultTechnique.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/NodeVisitor>
#elif _OPENSG
#endif

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
SceneNode::SceneNode()
:
m_activeTechnique( "Default" )
{
    AddTechnique( "Default", new ves::xplorer::scenegraph::DefaultTechnique() );
}
////////////////////////////////////////////////////////////////////////////////
SceneNode::~SceneNode()
{
    //Delete techniques in map
    for( std::map< std::string, ves::xplorer::scenegraph::Technique* >::iterator 
         iter = m_techniques.begin(); iter != m_techniques.end(); ++iter )
    {
        ves::xplorer::scenegraph::Technique* tempTech = iter->second;
        delete tempTech;
    }

    m_techniques.clear();
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::DirtyTechniques()
{
    std::map< std::string, ves::xplorer::scenegraph::Technique* >::const_iterator itr;
    for( itr = m_techniques.begin(); itr != m_techniques.end(); ++itr )
    {
        itr->second->DirtyPasses();
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::AddTechnique(  std::string name, ves::xplorer::scenegraph::Technique* technique  )
{
    m_techniques[ std::string( name ) ] = technique;
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::SetTechnique( std::string name )
{
    m_activeTechnique = name;
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Technique* SceneNode::GetTechnique( std::string name )
{
    return m_techniques[ name ];
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::Technique* SceneNode::GetActiveTechnique()
{
    return m_techniques[ m_activeTechnique ];
}
////////////////////////////////////////////////////////////////////////////////

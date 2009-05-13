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
#include <ves/xplorer/scenegraph/SceneNode.h>

#include <ves/xplorer/scenegraph/technique/DefaultTechnique.h>

// --- OSG Includes --- //
#include <osg/NodeVisitor>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
SceneNode::SceneNode()
    :
    mActiveTechnique( "Default" )
{
    AddTechnique( "Default", new technique::DefaultTechnique() );
}
////////////////////////////////////////////////////////////////////////////////
SceneNode::~SceneNode()
{
    //Delete techniques in map
    std::map< std::string, technique::Technique* >::iterator itr;
    for( itr =mTechniques.begin(); itr != mTechniques.end(); ++itr )
    {
        technique::Technique* tempTech = itr->second;

        delete tempTech;
    }

    mTechniques.clear();
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::DirtyTechniques()
{
    std::map< std::string, technique::Technique* >::const_iterator itr;
    for( itr = mTechniques.begin(); itr != mTechniques.end(); ++itr )
    {
        itr->second->DirtyPasses();
    }
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::AddTechnique(
    const std::string& name, technique::Technique* technique )
{
    mTechniques[ name ] = technique;
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::RemoveTechnique( const std::string& name )
{
    mTechniques.erase( name );
}
////////////////////////////////////////////////////////////////////////////////
void SceneNode::SetTechnique( const std::string& name )
{
    mActiveTechnique = name;
}
////////////////////////////////////////////////////////////////////////////////
technique::Technique* const SceneNode::GetTechnique(
    const std::string& name ) const
{
    std::map< std::string, technique::Technique* >::const_iterator itr =
        mTechniques.find( name );
    if( itr != mTechniques.end() )
    {
        return itr->second;
    }
    
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
technique::Technique* const SceneNode::GetActiveTechnique() const
{
    std::map< std::string, technique::Technique* >::const_iterator itr =
        mTechniques.find( mActiveTechnique );
    if( itr != mTechniques.end() )
    {
        return itr->second;
    }
    
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////

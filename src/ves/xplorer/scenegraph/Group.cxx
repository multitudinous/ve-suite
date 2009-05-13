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
#include <ves/xplorer/scenegraph/Group.h>

#include <ves/xplorer/scenegraph/technique/Technique.h>

// --- OSG Includes --- //
#include <osg/Group>
#include <osg/Node>

// --- C/C++ Libraries --- //
#include <iostream>
#include <algorithm>
#include <string>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Group::Group( const Group& group, const osg::CopyOp& copyop )
    :
    osg::Group( group, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Group::Group()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Group::~Group()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
int Group::RemoveChild( SceneNode* child )
{
    return removeChild( dynamic_cast< osg::Node* >( child ) );
}
////////////////////////////////////////////////////////////////////////////////
int Group::AddChild( SceneNode* child )
{
    return addChild( dynamic_cast< Node* >( child ) );
}
////////////////////////////////////////////////////////////////////////////////
void Group::InsertChild( int position, SceneNode* child )
{
    insertChild( position, dynamic_cast< Node* >( child ) );
}
////////////////////////////////////////////////////////////////////////////////
int Group::GetNumChildren( void )
{
    return getNumChildren();
}
////////////////////////////////////////////////////////////////////////////////
const std::string Group::GetName( void )
{
    return getName().data();
}
////////////////////////////////////////////////////////////////////////////////
void Group::SetName( std::string name )
{
    setName( name );
}
////////////////////////////////////////////////////////////////////////////////
int Group::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
    return replaceChild(
        dynamic_cast< Node* >( childToBeReplaced ),
        dynamic_cast< Node* >( newChild ) );
}
////////////////////////////////////////////////////////////////////////////////
bool Group::SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild )
{
    return containsNode( dynamic_cast< osg::Node* >( searchChild ) );
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Group::GetParent( unsigned int position )
{
    return getParent( position );
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Group::GetChild( unsigned int position )
{
    return getChild( position );
}
////////////////////////////////////////////////////////////////////////////////
void Group::ToggleDisplay( bool onOff )
{
    std::string value = ( onOff == true ) ? "ON" : "OFF";

    ToggleDisplay( value );
}
////////////////////////////////////////////////////////////////////////////////
void Group::ToggleDisplay( std::string onOff )
{
    if( onOff == "ON" )
    {
        setNodeMask( 1 );
    }

    else if( onOff == "OFF" )
    {
        setNodeMask( 0 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Group::traverse( osg::NodeVisitor& nv )
{
    technique::Technique* technique = mTechniques[ mActiveTechnique ];

    technique->Traverse( nv, this );
}
////////////////////////////////////////////////////////////////////////////////
void Group::InheritedTraverse( osg::NodeVisitor& nv )
{
    typedef osg::Group inherited;
    inherited::traverse( nv );
}
////////////////////////////////////////////////////////////////////////////////

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
#include <ves/xplorer/scenegraph/Switch.h>

#include <ves/xplorer/scenegraph/technique/Technique.h>

// --- C/C++ Includes --- //
#include <iostream>
#include <algorithm>
#include <string>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
Switch::Switch()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Switch::Switch( const Switch& switchNode, const osg::CopyOp& copyop )
    :
    osg::Switch( switchNode, copyop )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Switch::~Switch()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Switch::SetVal( int whichChildIsActive )
{
    if( whichChildIsActive == OFF )
    {
        setAllChildrenOff();
    }
    else
    {
        setSingleChildOn( whichChildIsActive );
    }
}
////////////////////////////////////////////////////////////////////////////////
int Switch::RemoveChild( SceneNode* child )
{
    return removeChild( dynamic_cast< osg::Node* >( child ) );
}
////////////////////////////////////////////////////////////////////////////////
int Switch::AddChild( SceneNode* child )
{
    return addChild( dynamic_cast< Node* >( child ) );
}
////////////////////////////////////////////////////////////////////////////////
void Switch::InsertChild( int position, SceneNode* child )
{
    insertChild( position, dynamic_cast< Node* >( child ) );
}
////////////////////////////////////////////////////////////////////////////////
int Switch::GetNumChildren()
{
    return getNumChildren();
}
////////////////////////////////////////////////////////////////////////////////
const std::string Switch::GetName()
{
    return getName().data();
}
////////////////////////////////////////////////////////////////////////////////
void Switch::SetName( std::string name )
{
    setName( name );
}
////////////////////////////////////////////////////////////////////////////////
int Switch::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
    return replaceChild(
        dynamic_cast< Node* >( childToBeReplaced ),
        dynamic_cast< Node* >( newChild ) );
}
////////////////////////////////////////////////////////////////////////////////
bool Switch::SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild )
{
    return containsNode( dynamic_cast< osg::Node* >( searchChild ) );
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Switch::GetParent( unsigned int position )
{
    return getParent( position );
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Switch::GetChild( unsigned int position )
{
    return getChild( position );
}
////////////////////////////////////////////////////////////////////////////////
void Switch::traverse( osg::NodeVisitor& nv )
{
    Technique* technique = mTechniques[ mActiveTechnique ];

    technique->Traverse( nv, this );
}
////////////////////////////////////////////////////////////////////////////////
void Switch::InheritedTraverse( osg::NodeVisitor& nv )
{
    typedef osg::Switch inherited;
    inherited::traverse( nv );
}
////////////////////////////////////////////////////////////////////////////////

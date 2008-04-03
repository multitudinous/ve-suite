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
#include <ves/xplorer/scenegraph/Group.h>
#include <ves/xplorer/scenegraph/Technique.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/Group>
#include <osg/Node>
#elif _OPENSG
#endif

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
#ifdef _OSG
    return removeChild( dynamic_cast< osg::Node* >( child ) );
#elif _OPENSG
    cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Group::AddChild( SceneNode* child )
{
#ifdef _OSG
    return addChild( dynamic_cast< Node* >( child ) );
#elif _OPENSG
    cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Group::InsertChild( int position, SceneNode* child )
{
#ifdef _OSG
    insertChild( position, dynamic_cast< Node* >( child ) );
#elif _OPENSG
    cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Group::GetNumChildren( void )
{
#ifdef _OSG
    return getNumChildren();
#elif _OPENSG
    cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string Group::GetName( void )
{
#ifdef _OSG
    return getName().data();
#elif _OPENSG
    return 0;
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Group::SetName( std::string name )
{
#ifdef _OSG
    setName( name );
#elif _OPENSG
    std::cerr << " ERROR: Group::SetName is NOT implemented " << std::endl;
    exit( 1 );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Group::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
#ifdef _OSG
    return replaceChild( dynamic_cast< Node* >( childToBeReplaced ), dynamic_cast< Node* >( newChild ) );
#elif _OPENSG
    cerr << " ERROR: Group::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#endif
}
////////////////////////////////////////////////////////////////////////////////
bool Group::SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild )
{
#ifdef _OSG
    return containsNode( dynamic_cast< osg::Node* >( searchChild ) );
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Group::GetParent( unsigned int position )
{
#ifdef _OSG
    return getParent( position );
#elif _OPENSG
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Group::GetChild( unsigned int position )
{
#ifdef _OSG
    return getChild( position );
#elif _OPENSG
#endif
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
#ifdef _OSG
        setNodeMask( 1 );
#elif _OPENSG
#endif
    }

    else if( onOff == "OFF" )
    {
#ifdef _OSG
        setNodeMask( 0 );
#elif _OPENSG
#endif
    }
}
////////////////////////////////////////////////////////////////////////////////
void Group::traverse( osg::NodeVisitor& nv )
{
    ves::xplorer::scenegraph::Technique* technique = mTechniques[ mActiveTechnique ];

    technique->Traverse( nv, this );
}
////////////////////////////////////////////////////////////////////////////////
void Group::InheritedTraverse( osg::NodeVisitor& nv )
{
    typedef osg::Group inherited;
    inherited::traverse( nv );
}
////////////////////////////////////////////////////////////////////////////////

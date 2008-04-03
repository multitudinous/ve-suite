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
#include <ves/xplorer/scenegraph/Switch.h>
#include <ves/xplorer/scenegraph/Technique.h>

//C/C++ Libraries
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
Switch::Switch( const Switch& switchNode, const osg::CopyOp& copyop ):
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
#ifdef _OSG
    if( whichChildIsActive == OFF )
    {
        setAllChildrenOff();
    }
    else
    {
        setSingleChildOn( whichChildIsActive );
    }

#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::RemoveChild( SceneNode* child )
{
#ifdef _OPENSG
    cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#elif _OSG
    return removeChild( dynamic_cast< osg::Node* >( child ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::AddChild( SceneNode* child )
{
#ifdef _OPENSG
    cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#elif _OSG
    return addChild( dynamic_cast< Node* >( child ) );
#endif

}
////////////////////////////////////////////////////////////////////////////////
void Switch::InsertChild( int position, SceneNode* child )
{
#ifdef _OPENSG
    cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#elif _OSG
    insertChild( position, dynamic_cast< Node* >( child ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::GetNumChildren( void )
{
#ifdef _OPENSG
    cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#elif _OSG
    return getNumChildren();
#endif
}
////////////////////////////////////////////////////////////////////////////////
const std::string Switch::GetName( void )
{
#ifdef _OPENSG
    return 0;
#endif
#ifdef _PERFORMER
    return _Switch->getName();
#elif _OSG
    return getName().data();
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Switch::SetName( std::string name )
{
#ifdef _OPENSG
    std::cerr << " ERROR: Switch::SetName is NOT implemented " << std::endl;
    exit( 1 );
#endif
#ifdef _PERFORMER
    _Switch->setName( name.c_str() );
#elif _OSG
    setName( name );
#endif
}
////////////////////////////////////////////////////////////////////////////////
int Switch::ReplaceChild( SceneNode* childToBeReplaced, SceneNode* newChild )
{
#ifdef _OPENSG
    cerr << " ERROR: Switch::ReplaceChild is NOT implemented " << endl;
    exit( 1 );
    return -1;
#elif _OSG
    return replaceChild( dynamic_cast< Node* >( childToBeReplaced ), dynamic_cast< Node* >( newChild ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
bool Switch::SearchChild( ves::xplorer::scenegraph::SceneNode* searchChild )
{
#ifdef _OPENSG

#elif _OSG
    return containsNode( dynamic_cast< osg::Node* >( searchChild ) );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Group* Switch::GetParent( unsigned int position )
{
#ifdef _OPENSG

#elif _OSG
    return getParent( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
osg::Node* Switch::GetChild( unsigned int position )
{
#ifdef _OPENSG

#elif _OSG
    return getChild( position );
#endif
}
////////////////////////////////////////////////////////////////////////////////
void Switch::traverse( osg::NodeVisitor& nv )
{
    ves::xplorer::scenegraph::Technique* technique = mTechniques[ mActiveTechnique ];

    technique->Traverse( nv, this );
}
////////////////////////////////////////////////////////////////////////////////
void Switch::InheritedTraverse( osg::NodeVisitor& nv )
{
    typedef osg::Switch inherited;
    inherited::traverse( nv );
}
////////////////////////////////////////////////////////////////////////////////

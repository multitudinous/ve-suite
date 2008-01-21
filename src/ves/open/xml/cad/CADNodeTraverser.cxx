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

#include <ves/open/xml/cad/CADNodeTraverser.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADPart.h>
#include <iostream>

using namespace ves::open::xml::cad;

////////////////////////////////////
//Constructors                    //
////////////////////////////////////
CADNodeTraverser::CADNodeTraverser()
{
    _root = 0;
    _preFunc = 0;
    _postFunc = 0;
    _ts = CONT;
}
/////////////////////////////////////////////////////////////////
CADNodeTraverser::CADNodeTraverser( const CADNodeTraverser& cfdNT )
{
    _root = 0;
    _preFunc = 0;
    _postFunc = 0;
    _ts = cfdNT._ts;
    _root = cfdNT._root;
    _preFunc = cfdNT._preFunc;
    _postFunc = cfdNT._postFunc;
}
/////////////////////////////////////
//Destructor                       //
/////////////////////////////////////
CADNodeTraverser::~CADNodeTraverser()
{}
/////////////////////////////////////////////////////
//set the node to traverse                         //
/////////////////////////////////////////////////////
void CADNodeTraverser::SetRootNode( CADNode* root )
{
    _root = root;
}
/////////////////////////////////
//traverse the node            //
/////////////////////////////////
void CADNodeTraverser::Traverse()
{
    if( _root )
    {
        //recurse the root node
        _traverseNode( _root );
    }
    else
    {
        std::cout << "Error: CADNodeTraverser::traverse()!" << std::endl;
        std::cout << "Root node not set!" << std::endl;
        return;
    }
}
///////////////////////////////////////////////////////////////////////////////////////////
//depth first recursion of a node/scene graph                                            //
///////////////////////////////////////////////////////////////////////////////////////////
void CADNodeTraverser::_traverseNode( CADNode* cNode, void* currentParent )
{
    int nChildren = 0;
    //the pre-callback
    if( _preFunc )
    {
        _preFunc->Apply( this, cNode, currentParent );
        if( _ts == SKIP ||
                _ts == STOP )
        {
            //pre func tells us to stop traversing down this branch
            _ts = CONT;
            return;
        }
    }
    //these are the only CADNode types (so far)
    //that have children so we must traverse the child nodes!!!!


    if( cNode->GetNodeType() == std::string( "Assembly" ) )
    {
        ves::open::xml::cad::CADAssembly* assembly = dynamic_cast<ves::open::xml::cad::CADAssembly*>( cNode );
        if( assembly )
        {
            unsigned int nChildren = assembly->GetNumberOfChildren();
            //recurse the children of this node
            for( unsigned int i = 0; i < nChildren; i++ )
            {
                _traverseNode( assembly->GetChild( i ), assembly );
            }
        }
    }
    //the post-callback
    if( _postFunc )
    {
        _postFunc->Apply( this, cNode, currentParent );
    }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
void CADNodeTraverser::SetPreNodeTraverseCallback( CADNodeTraverser::CADNodeTraverseCallback* func )
{
    _preFunc = func;
}
///////////////////////////////////////////////////////////////////////////////////////////////////
void CADNodeTraverser::SetPostNodeTraverseCallback( CADNodeTraverser::CADNodeTraverseCallback* func )
{
    _postFunc = func;
}
//////////////////////////////////////////////////////////
//the equal operator                                    //
//////////////////////////////////////////////////////////
CADNodeTraverser&
CADNodeTraverser::operator=( const CADNodeTraverser& cfdNT )
{
    if( this != &cfdNT )
    {
        _root = cfdNT._root;
        _preFunc = cfdNT._preFunc;
        _postFunc = cfdNT._postFunc;
    }
    return *this;
}

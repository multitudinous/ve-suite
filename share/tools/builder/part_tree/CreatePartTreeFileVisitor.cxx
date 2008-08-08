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
#include "CreatePartTreeFileVisitor.h"

#include <osg/Group>
#include <osg/Node>

#include <iostream>
#include <sstream>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
CreatePartTreeFileVisitor::CreatePartTreeFileVisitor( osg::Node* node,
                                              std::string& inputStream )
        :
        NodeVisitor( TRAVERSE_ALL_CHILDREN ),
        mTabCounter( 0 )
{
    mDotFile.open( inputStream.c_str(), std::ios::out );
    mDotFile << "Begin Part Tree" << std::endl;
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
CreatePartTreeFileVisitor::~CreatePartTreeFileVisitor()
{
    mDotFile << "End Part Tree" << std::endl;
    mDotFile.close();
}
////////////////////////////////////////////////////////////////////////////////
void CreatePartTreeFileVisitor::apply( osg::Node& node )
{
    //If it is not a group then it is a low level node which is already recorded
    //in the dot file
    osg::ref_ptr< osg::Group > tempGroup = node.asGroup();
    if( !tempGroup.valid() )
    {
        return;
    }

    std::string nodeName = node.getName();
    if( nodeName.empty() )
    {
        nodeName = std::string( "Class" ) + node.className();
    }
    else
    {
        if( nodeName.compare(0,4, "body" ) )
        {
            if( nodeName.compare(0,4, "face" ) )
            {
                if( nodeName.compare(0,5, "quilt" ) )
                {
                    for( unsigned int i = 0; i < mTabCounter; ++i )
                    {
                        mDotFile << "\t";
                    }
                    mDotFile << nodeName << std::endl;
                }
            }
        }
    }
    
    std::string childName;
    for( size_t i = 0; i < tempGroup->getNumChildren(); ++i )
    {
        osg::ref_ptr< osg::Node > childNode = tempGroup->getChild( i );
        childName = childNode->getName();
        if( childName.empty() )
        {
            childName = std::string( "Class" ) + childNode->className();
        }
        
        if( !childNode->asGroup() )
        {
            if( !childNode->getName().empty() )
            {
                if( nodeName.compare(0,4, "body" ) )
                {    
                    if( nodeName.compare(0,4, "face" ) )
                    {
                        if( nodeName.compare(0,5, "quilt" ) )
                        {                            
                            for( unsigned int i = 0; i < mTabCounter+1; ++i )
                            {
                                mDotFile << "\t";
                            }
                            mDotFile << "\t" << childName << std::endl;
                        }
                    }
                }
            }
        }
    }
    
    mTabCounter += 1;
    osg::NodeVisitor::traverse( node );
    mTabCounter -= 1;
}
////////////////////////////////////////////////////////////////////////////////

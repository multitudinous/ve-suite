/*************** <auto-copyright.pl BEGIN do not edit this line> **************
*
* VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
* Date modified: $Date: 2007-05-09 08:48:57 -0500 (Wed, 09 May 2007) $
* Version:       $Rev: 7573 $
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
#include "VE_Xplorer/SceneGraph/CreateGraphDOTVisitor.h"

#include <osg/Group>
#include <osg/Node>

#include <algorithm>
#include <iostream>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
CreateGraphDOTVisitor::CreateGraphDOTVisitor( osg::Node* node, 
                                              std::string& inputStream )
:
NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    m_dotFile.open( inputStream.c_str(), std::ios::out );
    m_dotFile << "digraph VE_Suite_Tree" << std::endl << "{" << std::endl;
    node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
CreateGraphDOTVisitor::~CreateGraphDOTVisitor()
{
    m_dotFile << "}" << std::endl;
    m_dotFile.close();
}
////////////////////////////////////////////////////////////////////////////////
void CreateGraphDOTVisitor::apply( osg::Node& node )
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
    
    /*Get the properties
    ///Material
    osg::ref_ptr< osg::StateSet > stateset = node.getOrCreateStateSet();
    osg::ref_ptr< osg::Material > material = 
                 static_cast< osg::Material* >
                 ( stateset->getAttribute( osg::StateAttribute::MATERIAL ) );
    osg::ref_ptr< osg::Texture > texture =
                 static_cast< osg::Texture* >
                 ( stateset->getAttribute( osg::StateAttribute::TEXTURE ) );

    if(material.valid())
    {
       osg::Vec4 ambient =  material.getAmbient(osg::Material::FRONT);
       osg::Vec4 diffuse = material.getDiffuse(osg::Material::FRONT);
       osg::Vec4 specular = material.getSpecular(osg::Material::FRONTF);
       osg::Vec4 emission =  material.getEmission(osg::Material::FRONT);
       float shininess =  material.getShininess(osg::Material::FRONT);
    }

    if(texture.valid())
    {
      //Just do the dimensions for now
      //and that will tell if it is 1,2 or 3 D
      int s = texture.getTextureWidth();
      int t = texture.getTextureHeight();
      int s = texture.getTextureDepth();
    }

   
    */
    std::string childName;
    for( size_t i = 0; i < tempGroup->getNumChildren(); ++i )
    {
        osg::ref_ptr< osg::Node > childNode = tempGroup->getChild( i );
        childName = childNode->getName();
        if( childName.empty() )
        {
            childName = std::string( "Class" ) + childNode->className();
        }

        m_dotFile << "\"" << tempGroup.get() << "\" -> \"" 
                    << childNode.get() << "\";" << std::endl; 
        m_dotFile << "\"" << tempGroup.get() << "\" " << "[label=\"" 
                    << nodeName << "\"];" << std::endl;
        m_dotFile << "\"" << childNode.get() << "\" " << "[label=\"" 
                    << childName << "\"];" << std::endl;
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////

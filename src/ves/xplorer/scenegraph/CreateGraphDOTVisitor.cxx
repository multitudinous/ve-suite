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
#include <ves/xplorer/scenegraph/CreateGraphDOTVisitor.h>

#include <osg/Group>
#include <osg/Node>
#include <osg/Material>
#include <osg/Texture>
#include <osg/StateSet>

#include <algorithm>
#include <iostream>
#include <sstream>

using namespace ves::xplorer::scenegraph;

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
    
    //Get the properties
    ///Material
    std::string childName;
    std::string childTextureData, childMaterialData;
    for( size_t i = 0; i < tempGroup->getNumChildren(); ++i )
    {
        osg::ref_ptr< osg::Node > childNode = tempGroup->getChild( i );
        childName = childNode->getName();
        if( childName.empty() )
        {
            childName = std::string( "Class" ) + childNode->className();
        }

        //Write the link
        m_dotFile << "\"" << tempGroup.get() << "\" -> \"" 
            << childNode.get() << "\";" << std::endl; 
        if ( !childNode->asGroup() )
        {
            //Write the child node label
            m_dotFile << "\"" << childNode.get() << "\" " << "[label=\"" 
                << childName << "\\n"
                << GetMaterialDataString( childNode.get() ) << "\\n" 
                << GetTextureDataString( childNode.get() ) << "\"];" << std::endl;
        }
    }

    //Write the label info for the parent
    m_dotFile << "\"" << tempGroup.get() << "\" " << "[label=\"" 
        << nodeName << "\\n" 
        << GetMaterialDataString( tempGroup.get() ) << "\\n" 
        << GetTextureDataString( tempGroup.get() ) << "\"];" << std::endl;
    
    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
std::string CreateGraphDOTVisitor::GetMaterialDataString( osg::Node* node )
{
    std::ostringstream materialData;
    
    osg::ref_ptr< osg::StateSet > stateset = node->getOrCreateStateSet();
    osg::ref_ptr< osg::Material > material = static_cast< osg::Material* >
        ( stateset->getAttribute( osg::StateAttribute::MATERIAL ) );

    if( material.valid() )
    {
        osg::Vec4 ambient =  material->getAmbient(osg::Material::FRONT);
        osg::Vec4 diffuse = material->getDiffuse(osg::Material::FRONT);
        osg::Vec4 specular = material->getSpecular(osg::Material::FRONT);
        osg::Vec4 emission =  material->getEmission(osg::Material::FRONT);
        float shininess =  material->getShininess(osg::Material::FRONT);
        materialData << "Material Properties" << "\\n"
            << "Ambient = " << ambient[ 0 ] << ", " 
            << ambient[ 1 ] << ", " 
            << ambient[ 2 ] << ", " 
            << ambient[ 3 ] << "\\n"
            << "Diffuse = " << diffuse[ 0 ] << ", " 
            << diffuse[ 1 ] << ", " 
            << diffuse[ 2 ] << ", " 
            << diffuse[ 3 ] << "\\n"
            << "Specular = " << specular[ 0 ] << ", " 
            << specular[ 1 ] << ", " 
            << specular[ 2 ] << ", " 
            << specular[ 3 ] << "\\n"
            << "Shininess = " << shininess;
    }
    else
    {
        materialData << "No Material Data";
    }
    return materialData.str();
}
////////////////////////////////////////////////////////////////////////////////
std::string CreateGraphDOTVisitor::GetTextureDataString( osg::Node* node )
{
    std::ostringstream textureData;
    
    osg::ref_ptr< osg::StateSet > stateset = node->getOrCreateStateSet();
    osg::ref_ptr< osg::Texture > texture = static_cast< osg::Texture* >
        ( stateset->getAttribute( osg::StateAttribute::TEXTURE ) );
    
    if( texture.valid() )
    {
        //Just do the dimensions for now
        //and that will tell if it is 1,2 or 3 D
        int w = texture->getTextureWidth();
        int h = texture->getTextureHeight();
        int d = texture->getTextureDepth();
        textureData << "Texture Properties" << "\\n"
            << "Width = " << w << "\\n" 
            << "Height = " << h << "\\n"
            << "Depth = " << d;
    }
    else
    {
        textureData << "No Texture Data";
    }
    return textureData.str();
}
////////////////////////////////////////////////////////////////////////////////

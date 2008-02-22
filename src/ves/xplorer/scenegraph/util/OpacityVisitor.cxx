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
#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Geometry>
#include <osg/Material>
#include <osg/Texture>
#include <osg/TexEnv>
#include <osg/Array>
#include <osg/BlendFunc>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
OpacityVisitor::OpacityVisitor( osg::Node* osg_node, bool state, float alpha )
        :NodeVisitor( TRAVERSE_ALL_CHILDREN ),
         transparent( state ),
         m_alpha( alpha )
{
    osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
OpacityVisitor::~OpacityVisitor()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void OpacityVisitor::apply( osg::Geode& node )
{
    osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();
    osg::ref_ptr< osg::Material > geode_material = static_cast< osg::Material* >( geode_stateset->getAttribute( osg::StateAttribute::MATERIAL ) );

    if( geode_material.valid() )
    {
        geode_material->setAlpha( osg::Material::FRONT_AND_BACK, m_alpha );
        geode_stateset->setAttribute( geode_material.get(), osg::StateAttribute::ON );
        SetupBlendingForStateSet( geode_stateset.get() );
    }

    for( size_t i = 0; i < node.getNumDrawables(); i++ )
    {
        osg::ref_ptr< osg::StateSet > drawable_stateset = node.getDrawable( i )->getOrCreateStateSet();
        osg::ref_ptr< osg::Material > drawable_material = static_cast< osg::Material* >( drawable_stateset->getAttribute( osg::StateAttribute::MATERIAL ) );
        osg::ref_ptr< osg::Vec4Array > color_array = static_cast< osg::Vec4Array* >( node.getDrawable( i )->asGeometry()->getColorArray() );
        osg::StateSet::TextureAttributeList drawable_tal = drawable_stateset->getTextureAttributeList();

        SetupBlendingForStateSet( drawable_stateset.get() );

        if( color_array.valid() )
        {
            for( size_t j = 0; j < color_array->size(); j++ )
            {
                color_array->at( j ).a() = m_alpha;
                node.getDrawable( i )->asGeometry()->setColorArray( color_array.get() );
            }
        }

        if( drawable_material.valid() )
        {
            drawable_material->setAlpha( osg::Material::FRONT_AND_BACK, m_alpha );
            drawable_stateset->setAttribute( drawable_material.get(), osg::StateAttribute::ON );
        }

        for( size_t k = 0; k < drawable_tal.size(); k++ )
        {
            osg::ref_ptr< osg::TexEnv > texenv = static_cast< osg::TexEnv* >( drawable_stateset->getTextureAttribute( k, osg::StateAttribute::TEXENV ) );

            if( texenv.valid() )
            {
                if( transparent == true )
                {
                    texenv->setMode( osg::TexEnv::BLEND );
                }

                else
                {
                    texenv->setMode( osg::TexEnv::DECAL );
                }
            }

            else
            {
                if( transparent == true )
                {
                    texenv = new osg::TexEnv( osg::TexEnv::BLEND );
                }

                else
                {
                    texenv = new osg::TexEnv( osg::TexEnv::DECAL );
                }
            }

            drawable_stateset->setTextureAttribute( k, texenv.get(), osg::StateAttribute::ON );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void OpacityVisitor::apply( osg::Group& node )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = node.getDescriptions();
    bool isAssembly = false;
    //Find if the node is an assembly
    for( size_t i = 0; i < descriptorsList.size(); i++ )
    {
        if( descriptorsList.at( i ) == "Assembly" )
        {
            isAssembly = true;
            break;
        }
    }

    if( !isAssembly )
    {
        osg::ref_ptr< osg::StateSet > stateset = node.getOrCreateStateSet();
        osg::ref_ptr< osg::Material > material = 
            static_cast< osg::Material* >( stateset->
                getAttribute( osg::StateAttribute::MATERIAL ) );
        
        if( material.valid() )
        {
            material->setAlpha( osg::Material::FRONT_AND_BACK, m_alpha );
            stateset->setAttribute( material.get(), osg::StateAttribute::ON );
            SetupBlendingForStateSet( stateset.get() );
        }
    }

    osg::NodeVisitor::apply( node );
}
////////////////////////////////////////////////////////////////////////
void OpacityVisitor::SetupBlendingForStateSet( osg::StateSet* stateset)
{
    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc;
    bf->setFunction( osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
    stateset->setAttributeAndModes( bf.get(), osg::StateAttribute::ON );

    if( transparent )
    {
        stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    }   
    else
    {
        stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
    }
}

////////////////////////////////////////////////////////////////////////////////

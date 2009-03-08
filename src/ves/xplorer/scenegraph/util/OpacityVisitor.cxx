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
#include <osg/BlendColor>

#include <osg/io_utils>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
OpacityVisitor::OpacityVisitor( osg::Node* osg_node, bool storeState, 
    bool state, float alpha )
        :NodeVisitor( TRAVERSE_ALL_CHILDREN ),
         transparent( state ),
         m_alpha( alpha ),
         mStoreState( storeState )
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
    osg::ref_ptr< osg::Material > geode_material = 
        static_cast< osg::Material* >( geode_stateset->
        getAttribute( osg::StateAttribute::MATERIAL ) );

    if( geode_material.valid() )
    {
        geode_material->setAlpha( osg::Material::FRONT_AND_BACK, m_alpha );
        geode_stateset->setAttribute( geode_material.get(), 
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        //The stateset only needs set at the part level in VE-Suite.
        //The alpha an material information can be set at the higher level
        //because otherwise the renderbins end up being nested and cause odd
        //problems.
        SetupBlendingForStateSet( geode_stateset.get() );
    }

    for( size_t i = 0; i < node.getNumDrawables(); i++ )
    {
        //Stateset for the drawable
        osg::ref_ptr< osg::StateSet > drawable_stateset = 
            node.getDrawable( i )->getOrCreateStateSet();

        //Material from the stateset
        osg::ref_ptr< osg::Material > drawable_material = 
            static_cast< osg::Material* >( 
                drawable_stateset->getAttribute( 
                osg::StateAttribute::MATERIAL ) );

        //Colors for the stateset
        osg::ref_ptr< osg::Vec4Array > color_array;
        osg::ref_ptr< osg::Geometry > geom = 
        node.getDrawable( i )->asGeometry();
        if( geom.valid() )
        {
            color_array = 
                dynamic_cast< osg::Vec4Array* >( geom->getColorArray() );
        }

        //Texture for the stateset
        osg::StateSet::TextureAttributeList drawable_tal = 
            drawable_stateset->getTextureAttributeList();

        //See if this drawable had a texture with opacity. If so then do not 
        //change the render bin information
        bool transparentTexture = false;
        for( size_t k = 0; k < drawable_tal.size(); k++ )
        {
            osg::ref_ptr< osg::Texture > texture = 
            static_cast< osg::Texture* >( drawable_stateset->
                getTextureAttribute( k, osg::StateAttribute::TEXTURE ) );
            
            for( unsigned int j=0; j<texture->getNumImages(); ++j )
            {
                transparentTexture = 
                    texture->getImage( j )->isImageTranslucent();
                if( transparentTexture )
                {
                    break;
                }
            }
            
            if( transparentTexture )
            {
                break;
            }
        }
        
        //The stateset only needs set at the part level in VE-Suite.
        //The alpha an material information can be set at the higher level
        //because otherwise the renderbins end up being nested and cause odd
        //problems.
        if( !transparentTexture )
        {
            SetupBlendingForStateSet( drawable_stateset.get() );
        }

        if( mStoreState )
        {
            //The first time we come through here store the original state
            //for the colors and the materials so that we can determine
            //if we should mess with the opacity
            if( color_array.valid() )
            {
                osg::Vec4Array* tempColorArray = 
                    new osg::Vec4Array( *(color_array.get()), 
                                   osg::CopyOp::DEEP_COPY_ALL );
               node.getDrawable( i )->setUserData( tempColorArray );
            }

            if( drawable_material.valid() )
            {
                drawable_stateset->setUserData( 
                    new osg::Material( (*drawable_material.get()), 
                    osg::CopyOp::DEEP_COPY_ALL ) );
            }
        }

        if( color_array.valid() )
        {
            osg::ref_ptr< osg::Vec4Array > temp_color_array = 
                static_cast< osg::Vec4Array* >( 
                node.getDrawable( i )->getUserData() );
            for( size_t j = 0; j < color_array->size(); j++ )
            {
                //If it is opaque then change the surface but if it is 
                //transparent then do not mess with the surface
                if( temp_color_array->at( j ).a() == 1.0f )
                {
                    color_array->at( j ).a() = m_alpha;                        
                }
            }
            node.getDrawable( i )->asGeometry()->
                setColorArray( color_array.get() );
        }

        if( drawable_material.valid() )
        {
            osg::ref_ptr< osg::Material > temp_drawable_material = 
                static_cast< osg::Material* >( 
                drawable_stateset->getUserData() );
                
            //If it is opaque then change the surface but if it is 
            //transparent then do not mess with the surface
            if( ( temp_drawable_material->getAmbient( osg::Material::FRONT_AND_BACK )[3] == 1.0f ) && 
               ( temp_drawable_material->getDiffuse( osg::Material::FRONT_AND_BACK )[3] == 1.0f ) && 
               ( temp_drawable_material->getSpecular( osg::Material::FRONT_AND_BACK )[3] == 1.0f ) && 
               ( temp_drawable_material->getEmission( osg::Material::FRONT_AND_BACK )[3] == 1.0f ) )
            {
                drawable_material->setAlpha( 
                    osg::Material::FRONT_AND_BACK, m_alpha );
                drawable_stateset->setAttribute( drawable_material.get(), 
                                                osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
            }
        }            

        //This sets the gl blend mode for the textures on geometry so
        //that when transparency is needed the texture renders properly
        for( size_t k = 0; k < drawable_tal.size(); k++ )
        {
            osg::ref_ptr< osg::TexEnv > texenv = 
                static_cast< osg::TexEnv* >( drawable_stateset->
                    getTextureAttribute( k, osg::StateAttribute::TEXENV ) );

            if( !texenv.valid() )
            {
                texenv = new osg::TexEnv();
            }

            texenv->setMode( osg::TexEnv::MODULATE );
            drawable_stateset->setTextureAttribute( k, texenv.get(), 
                osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        }
    }
    /*
    osg::ref_ptr< osg::BlendColor > bc = 
        static_cast< osg::BlendColor* >( geode_stateset->
            getAttribute( osg::StateAttribute::BLENDCOLOR ) );
    if(  bc.valid() )
    {
        bc->setConstantColor( osg::Vec4( 1.0f, 1.0f, 1.0f, m_alpha ) );
        geode_stateset->setAttribute( bc.get(), 
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        SetupSTLBlendingForStateSet( geode_stateset.get() );
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void OpacityVisitor::apply( osg::Group& node )
{
    /*osg::Node::DescriptionList descriptorsList;
    descriptorsList = node.getDescriptions();
    bool isPart = false;
    //Find if the node is an assembly
    for( size_t i = 0; i < descriptorsList.size(); i++ )
    {
        if( descriptorsList.at( i ) == "Part" )
        {
            isPart = true;
            break;
        }
    }*/

    ///Only process if it is a part
    //if( isPart )
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

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////
void OpacityVisitor::SetupBlendingForStateSet( osg::StateSet* stateset )
{
    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( osg::BlendFunc::SRC_ALPHA, 
        osg::BlendFunc::ONE_MINUS_SRC_ALPHA );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes( bf.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

    if( transparent )
    {
        stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
        stateset->setNestRenderBins( false );
    }   
    else
    {
        stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
        stateset->setNestRenderBins( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
void OpacityVisitor::SetupSTLBlendingForStateSet( osg::StateSet* stateset )
{
    osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc();
    bf->setFunction( osg::BlendFunc::CONSTANT_COLOR, 
                    osg::BlendFunc::ONE_MINUS_CONSTANT_ALPHA );
    stateset->setMode( GL_BLEND, osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    stateset->setAttributeAndModes( bf.get(), osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    if( transparent )
    {
        stateset->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
        stateset->setNestRenderBins( false );
    }   
    else
    {
        stateset->setRenderBinDetails( 0, std::string( "RenderBin" ) );
        stateset->setNestRenderBins( true );
    }
}

////////////////////////////////////////////////////////////////////////////////

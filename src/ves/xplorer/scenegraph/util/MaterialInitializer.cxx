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
#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Material>
#include <osg/Geometry>

// --- C/C++ Libraries --- //
#include <iostream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
MaterialInitializer::MaterialInitializer( osg::Node* osg_node )
        :
        NodeVisitor( TRAVERSE_ALL_CHILDREN ),
        mFileHasMaterial( false )
{
    osg_node->accept( *this );
    //std::cout << " has material " << mFileHasMaterial << std::endl;
    if( !mFileHasMaterial )
    {
        osg::ref_ptr< osg::StateSet > stateset = osg_node->getOrCreateStateSet();
        osg::ref_ptr< osg::Material > material;
        material = new osg::Material();
        material->setAmbient( osg::Material::FRONT_AND_BACK, osg::Vec4( 0.56862f, 0.56842f, 0.56842f, 1.0f ) );
        material->setColorMode( osg::Material::AMBIENT_AND_DIFFUSE );
        stateset->setAttribute( material.get(), osg::StateAttribute::ON );        
    }
}
////////////////////////////////////////////////////////////////////////////////
MaterialInitializer::~MaterialInitializer()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MaterialInitializer::apply( osg::Geode& node )
{
    osg::ref_ptr< osg::StateSet > geode_stateset = node.getOrCreateStateSet();
    osg::ref_ptr< osg::Material > geode_material = 
    static_cast< osg::Material* >( geode_stateset->
                                  getAttribute( osg::StateAttribute::MATERIAL ) );
    
    if( geode_material.valid() )
    {
        mFileHasMaterial = true;
        return;
    }
    
    for( size_t i = 0; i < node.getNumDrawables(); i++ )
    {
        //Stateset for the drawable
        osg::ref_ptr< osg::StateSet > drawable_stateset = 
            node.getDrawable( i )->getOrCreateStateSet();
        //Material from the stateset
        osg::ref_ptr< osg::Material > drawable_material = 
            static_cast< osg::Material* >( 
            drawable_stateset->getAttribute( osg::StateAttribute::MATERIAL ) );

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

        if( color_array.valid() )
        {
            for( size_t j = 0; j < color_array->size(); j++ )
            {
                mFileHasMaterial = true;
                return;
            }
        }
        
        if( drawable_material.valid() )
        {
            mFileHasMaterial = true;
            return;
        }            
        
        //This sets the gl blend mode for the textures on geometry so
        //that when transparency is needed the texture renders properly
        for( size_t k = 0; k < drawable_tal.size(); k++ )
        {
            mFileHasMaterial = true;
            return;
        }
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
void MaterialInitializer::apply( osg::Node& node )
{
    osg::ref_ptr< osg::StateSet > stateset = node.getOrCreateStateSet();
    osg::ref_ptr< osg::Material > material = 
    static_cast< osg::Material* >( stateset->
                                  getAttribute( osg::StateAttribute::MATERIAL ) );
    
    if( material.valid() )
    {
        mFileHasMaterial = true;
        return;
    }
    
    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////

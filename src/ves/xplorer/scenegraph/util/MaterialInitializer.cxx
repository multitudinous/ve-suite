/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

    if( !mFileHasMaterial )
    {
        osg::ref_ptr< osg::StateSet > stateset = osg_node->getOrCreateStateSet();
        osg::ref_ptr< osg::Material > material;
        material = new osg::Material();
        material->setAmbient( osg::Material::FRONT_AND_BACK,
                              osg::Vec4( 0.56862f, 0.56842f, 0.56842f, 1.0f ) );
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
    ///Guard to stop any checking of nodes if this flag has already been set.
    if( mFileHasMaterial )
    {
        return;
    }

    //Stateset for the geode
    mFileHasMaterial = CheckStateSet( node.getStateSet() );
    if( mFileHasMaterial )
    {
        return;
    }

    for( size_t i = 0; i < node.getNumDrawables(); i++ )
    {
        //Stateset for the drawable of the geode
        mFileHasMaterial = CheckStateSet( node.getDrawable( i )->getStateSet() );
        if( mFileHasMaterial )
        {
            return;
        }

        //StateSet for the Drawables Geometry
        osg::ref_ptr< osg::Geometry > geom =
            node.getDrawable( i )->asGeometry();
        if( geom.valid() )
        {
            mFileHasMaterial = CheckStateSet( geom->getStateSet() );
            if( mFileHasMaterial )
            {
                return;
            }

            osg::ref_ptr< osg::Vec4Array > color_array =
                dynamic_cast< osg::Vec4Array* >( geom->getColorArray() );
            if( color_array.valid() )
            {
                if( color_array->size() > 0 )
                {
                    mFileHasMaterial = true;
                    return;
                }
            }
        }
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
void MaterialInitializer::apply( osg::Node& node )
{
    ///Guard to stop any checking of nodes if this flag has already been set.
    if( mFileHasMaterial )
    {
        return;
    }

    mFileHasMaterial = CheckStateSet( node.getStateSet() );
    if( mFileHasMaterial )
    {
        return;
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
bool MaterialInitializer::CheckStateSet( osg::StateSet* stateSet )
{
    osg::ref_ptr< osg::StateSet > tempStateSet = stateSet;
    if( tempStateSet.valid() )
    {
        osg::ref_ptr< osg::Material > material =
            static_cast< osg::Material* >( tempStateSet->
                                           getAttribute( osg::StateAttribute::MATERIAL ) );

        if( material.valid() )
        {
            return true;
        }

        osg::StateSet::TextureAttributeList stateSetTal =
            tempStateSet->getTextureAttributeList();
        if( stateSetTal.size() > 0 )
        {
            return true;
        }
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////

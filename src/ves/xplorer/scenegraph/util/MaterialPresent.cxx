/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

// --- VES Includes --- //
#include <ves/xplorer/scenegraph/util/MaterialPresent.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Material>
#include <osg/Geometry>
#include <osg/Texture2D>

// --- STL Includes --- //
#include <iostream>
#include <sstream>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
MaterialPresent::MaterialPresent( osg::Node* osg_node )
    :
    NodeVisitor( TRAVERSE_ALL_CHILDREN ),
    mFileHasMaterial( false )
{
    osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
MaterialPresent::~MaterialPresent()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool MaterialPresent::FileHasMaterial()
{
    return mFileHasMaterial;
}
////////////////////////////////////////////////////////////////////////////////
void MaterialPresent::apply( osg::Geode& node )
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
            mFileHasMaterial = CheckStateSet(  geom->getStateSet() );
            if( mFileHasMaterial )
            {
                return;
            }

            /*osg::ref_ptr< osg::Vec4Array > color_array = 
                dynamic_cast< osg::Vec4Array* >( geom->getColorArray() );
            if( color_array.valid() )
            {
                if( color_array->size() > 0 )
                {
                    mFileHasMaterial = true;
                    return;
                }
            }*/
        }
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
void MaterialPresent::apply( osg::Node& node )
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
bool MaterialPresent::CheckStateSet( osg::StateSet* stateSet )
{
    osg::ref_ptr< osg::StateSet > tempStateSet = stateSet;
    if( tempStateSet.valid() )
    {
        /*
        osg::ref_ptr< osg::Material > material = 
            static_cast< osg::Material* >( tempStateSet->
            getAttribute( osg::StateAttribute::MATERIAL ) );
        if( material.valid() )
        {
            return true;
        }
        */

        osg::StateSet::TextureAttributeList stateSetTal =
            tempStateSet->getTextureAttributeList();
        for( unsigned int i = 0; i < stateSetTal.size(); ++i )
        {
            osg::StateAttribute* sa = stateSet->getTextureAttribute(
                i, osg::StateAttribute::TEXTURE );
            //Only worry about 2D textures for now
            osg::Texture2D* tex2D = dynamic_cast< osg::Texture2D* >( sa );
            if( tex2D )
            {
                stateSet->setTextureAttributeAndModes(
                    i, tex2D, osg::StateAttribute::ON );
                std::stringstream ss;
                ss << "tex" << i;
                stateSet->addUniform( new osg::Uniform( ss.str().c_str(), i ) );
            }
        }

        if( stateSetTal.size() > 0 )
        {
            return true;
        }
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////

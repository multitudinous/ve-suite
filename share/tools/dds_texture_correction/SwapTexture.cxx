/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include "SwapTexture.h"

// --- OSG Stuff --- //
#include <osg/Geode>
#include <osg/Group>
#include <osg/Material>
#include <osg/Geometry>
#include <osg/Texture2D>

#include <osgDB/ReadFile>
#include <osgDB/WriteFile>
#include <osgDB/ReaderWriter>

// --- C/C++ Libraries --- //
#include <iostream>

#include <boost/filesystem/path.hpp>

using namespace ves::xplorer::scenegraph::util;

////////////////////////////////////////////////////////////////////////////////
SwapTexture::SwapTexture( osg::Node* osg_node )
        :
        NodeVisitor( TRAVERSE_ALL_CHILDREN )
{
    //This enables the visitor to traverse "off" nodes
    setNodeMaskOverride( 1 );
    osg_node->accept( *this );
}
////////////////////////////////////////////////////////////////////////////////
SwapTexture::~SwapTexture()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SwapTexture::apply( osg::Geode& node )
{
    //Stateset for the geode
    CheckStateSet( node.getStateSet() );
    
    for( size_t i = 0; i < node.getNumDrawables(); ++i )
    {
        //Stateset for the drawable of the geode
        CheckStateSet( node.getDrawable( i )->getStateSet() );
    }

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
void SwapTexture::apply( osg::Node& node )
{
    CheckStateSet( node.getStateSet() );

    osg::NodeVisitor::traverse( node );
}
////////////////////////////////////////////////////////////////////////////////
void SwapTexture::CheckStateSet( osg::StateSet* stateSet )
{
    osg::ref_ptr< osg::StateSet > tempStateSet = stateSet;
    if( !tempStateSet.valid() )
    {
        return;
    }

    osg::ref_ptr< osgDB::ReaderWriter::Options > opt = 
        new osgDB::ReaderWriter::Options();
    opt->setOptionString( "dds_flip" );
    //osgDB::Registry::instance()->setOptions( opt );
    osg::ref_ptr< osg::Image > tgaImage;
    std::string fileName;
    osg::ref_ptr< osg::Image > ddsImage;
    
    osg::StateSet::TextureAttributeList stateSetTal =
        tempStateSet->getTextureAttributeList();
    for( unsigned int i = 0; i < stateSetTal.size(); ++i )
    {
        osg::StateAttribute* sa = stateSet->getTextureAttribute(
            i, osg::StateAttribute::TEXTURE );
        //Only worry about 2D textures for now
        if( sa )
        {
            osg::Texture2D* tex2D = dynamic_cast< osg::Texture2D* >( sa );
            if( tex2D )
            {
                tgaImage = tex2D->getImage();
                if( tgaImage.valid() )
                {
                    fileName = tgaImage->getFileName();
                    boost::filesystem::path newFileName( fileName, boost::filesystem::native );
                    if( newFileName.extension() == ".tga" )
                    {
                        std::cout << "Texture file name = " 
                            << fileName << std::endl;
                        newFileName.replace_extension( "dds" );
                        std::cout << "New texture file name = " 
                            << newFileName.string() << std::endl;
                        
                        ddsImage = 
                            osgDB::readImageFile( newFileName.string(), opt );
                        tex2D->setImage( ddsImage.get() );
                        //Write it back out if we do not want to include the image 
                        //file in the ive file
                        osgDB::writeImageFile( *(ddsImage.get()),  newFileName.string());
                    }
                }
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////

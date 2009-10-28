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

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/rtt/Unit.h>
#include <ves/xplorer/scenegraph/rtt/Utility.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Image>
#include <osg/Program>
#include <osg/Texture2D>
#include <osg/FrameBufferObject>

//#include <osgDB/WriteFile>
//#include <osgDB/Registry>

// --- C/C++ Includes --- //
#include <cmath>
#include <iostream>

using namespace ves::xplorer::scenegraph::rtt;

////////////////////////////////////////////////////////////////////////////////
Unit::Unit()
    :
    osg::Group(),
    mActive( true ),
    mInputTextureIndexForViewportReference( 0 ),
    mGeode( NULL ),
    mDrawable( NULL ),
    mViewport( NULL )
{
    //Set default name
    setName( "Nameless_PPU" );

    //Create default geode
    mGeode = new osg::Geode();
    mGeode->setCullingActive( false );
#if !VES_USE_FBO_CAMERA
    addChild( mGeode.get() );
#endif
    //mModelViewMatrix = new osg::RefMatrix( osg::Matrixd::identity() );

    //No culling, because we do not need it
    setCullingActive( false );

    //Setup default empty program, so we do not use any program from above
    getOrCreateStateSet()->setAttribute(
        new osg::Program(),
        osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
}
////////////////////////////////////////////////////////////////////////////////
Unit::Unit( const Unit& unit, const osg::CopyOp& copyop )
    :
    osg::Group( unit, copyop ),
    mActive( unit.mActive ),
    mInputTextureIndexForViewportReference(
        unit.mInputTextureIndexForViewportReference ),
    mInputTextures( unit.mInputTextures ),
    mOutputTextures( unit.mOutputTextures ),
    mGeode( unit.mGeode ),
    mDrawable( unit.mDrawable ),
    mViewport( unit.mViewport )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Unit::~Unit()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool Unit::SetInputToUniform(
    Unit* unit, const std::string& uniform, bool add )
{
    if( uniform.length() < 1 ) 
    {
        return false;
    }

    //Add this unit as a child of the parent if required
    if( add && !unit->containsNode( this ) )
    {
        unit->addChild( this );
    }

    //Add the uniform
    //mInputToUniformMap[ unit ] =
    //    std::pair< std::string, unsigned int >(
    //        uniform, mInputToUniformMap.size() );
    mInputToUniformMap.push_back( std::pair< std::string, osg::ref_ptr< Unit > >( uniform, unit ) );
    return true;  
}
////////////////////////////////////////////////////////////////////////////////
/*void Unit::RemoveInputToUniform( Unit* parent, bool remove )
{
    InputToUniformMap::iterator itr = mInputToUniformMap.find( parent );
    if( itr != mInputToUniformMap.end() )
    {
        const std::string& uniform = itr->second.first;

        osg::StateSet* stateset = mGeode->getOrCreateStateSet();
        if( stateset->getUniform( uniform ) )
        {
            //Remove from the stateset
            stateset->removeUniform( uniform );

            //If we have to remove the parent
            if( remove )
            {
                itr->first->removeChild( this );
            }

            //Remove the element from the list
            mInputToUniformMap.erase( itr );
        }
        else
        {
            std::cout << "Unit::RemoveInputToUniform: "
                      << uniform
                      << " not found in stateset!" << std::endl;
        }
    }
    else
    {
        std::cout << "Unit::RemoveInputToUniform: "
                  << parent->getName()
                  << " not found in mInputToUniformMap!" << std::endl;
    }
}*/
////////////////////////////////////////////////////////////////////////////////
const Unit::InputToUniformMap& Unit::GetInputToUniformMap() const
{
    return mInputToUniformMap;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture* const Unit::GetInputTexture( int inputIndex ) const
{
    if( inputIndex < mInputTextures.size() )
    {
        return mInputTextures.at( inputIndex ).get();
    }
    /*TextureMap::const_iterator itr = mInputTextures.find( inputIndex );
    if( itr != mInputTextures.end() )
    {
        return itr->second.get();
    }*/
    
    std::cout << "Unit::GetInputTexture: "
              << "texture " << inputIndex
              << " not found in mInputTextures!"
              << std::endl;

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
const Unit::TextureMap& Unit::GetInputTextureMap() const
{
    return mInputTextures;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture* const Unit::GetOutputTexture( int mrt ) const
{
    if( mrt < mOutputTextures.size() )
    {
        return mOutputTextures.at( mrt ).get();
    }
    /*TextureMap::const_iterator itr = mOutputTextures.find( mrt );
    if( itr != mOutputTextures.end() )
    {
        return itr->second.get();
    }*/

    std::cout << "Unit::GetOutputTexture: "
              << "texture " << mrt
              << " not found in mOutputTextures!"
              << std::endl;

    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
const Unit::TextureMap& Unit::GetOutputTextureMap() const
{
    return mOutputTextures;
}
////////////////////////////////////////////////////////////////////////////////
void Unit::Initialize()
{
    //Collect all inputs from the units above
    SetInputTexturesFromParents();

    //Check if we have input reference size
    if( GetInputTextureIndexForViewportReference() >= 0 &&
        GetInputTexture( GetInputTextureIndexForViewportReference() ) )
    {
        //If no viewport, so create it
        if( !mViewport.valid() )
        {
            mViewport = new osg::Viewport( 0, 0, 0, 0 );
        }

        //Change viewport sizes
        mViewport->width() = GetInputTexture(
            GetInputTextureIndexForViewportReference() )->getTextureWidth();
        mViewport->height() = GetInputTexture(
            GetInputTextureIndexForViewportReference() )->getTextureHeight();

        //Just notice that the viewport size is changed
        NoticeChangeViewport();
    }

    //Reassign input and shaders
    AssignInputTexture();
    if( mViewport.valid() )
    {
        getOrCreateStateSet()->setAttribute(
            mViewport.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Unit::Update()
{
    Initialize();

    UpdateUniforms();
}
////////////////////////////////////////////////////////////////////////////////
void Unit::SetInputTextureIndexForViewportReference( int index )
{
    if( index != mInputTextureIndexForViewportReference )
    {
        if( index < 0 )
        {
            mInputTextureIndexForViewportReference = -1;
        }
        else
        {
            mInputTextureIndexForViewportReference = index;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
const int Unit::GetInputTextureIndexForViewportReference() const
{
    return mInputTextureIndexForViewportReference;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* const Unit::GetGeode() const
{
    return mGeode.get();
}
////////////////////////////////////////////////////////////////////////////////
void Unit::SetInputTexturesFromParents()
{
    //Scan the input to uniform map
    rtt::Unit* unit( NULL );
    Unit::InputToUniformMap::iterator itumItr = mInputToUniformMap.begin();
    for( itumItr; itumItr != mInputToUniformMap.end(); ++itumItr )
    {
        //unit = itumItr->first.get();
        unit = itumItr->second.get();
        if( unit )
        {
            //Add each found texture as input
            const Unit::TextureMap& textureMap = unit->GetOutputTextureMap();
            Unit::TextureMap::const_iterator tmItr = textureMap.begin();
            for( tmItr; tmItr != textureMap.end(); ++tmItr )
            {
                //osg::Texture* texture = tmItr->second.get();
                osg::Texture* texture = tmItr->get();
                if( texture )
                {
                    //mInputTextures[ mInputTextures.size() ] =
                    //    tmItr->second.get();
                    mInputTextures.push_back( texture );
                }
                else
                {
                    osg::notify( osg::WARN )
                        << "rtt::Unit::SetInputTexturesFromParents(): "
                        << unit->getName()
                        << " has invalid output texture!"
                        << std::endl;
                }
            }
        }
        else
        {
            osg::notify( osg::WARN )
                << "rtt::Unit::SetInputTexturesFromParents(): "
                << "This is a bad unit."
                << std::endl;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Unit::UpdateUniforms()
{
    osg::StateSet* stateset = mGeode->getOrCreateStateSet();

    //Setup input texture uniforms
    InputToUniformMap::iterator itr = mInputToUniformMap.begin();
    for( itr; itr != mInputToUniformMap.end(); ++itr )
    {
        //Only valid inputs
        //if( itr->first.valid() )
        if( itr->second.valid() )
        {
            size_t index = itr - mInputToUniformMap.begin();
            //Setup uniform
            osg::Uniform* texture = stateset->getOrCreateUniform(
                //itr->second.first,
                itr->first,
                rtt::ConvertTextureToUniformType(
                    //itr->first->GetOutputTexture( 0 ) ) );
                    itr->second->GetOutputTexture( 0 ) ) );

            //texture->set( static_cast< int >( itr->second.second ) );
            texture->set( static_cast< int >( index ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Unit::NoticeChangeViewport()
{
    //Change size of the result texture according to the viewport
    TextureMap::iterator itr = mOutputTextures.begin();
    for( itr; itr != mOutputTextures.end(); ++itr )
    {
        //if( itr->second.valid() )
        if( itr->valid() )
        {
            //If texture type is a 2D texture
            //if( dynamic_cast< osg::Texture2D* >( itr->second.get() ) != NULL )
            if( dynamic_cast< osg::Texture2D* >( itr->get() ) != NULL )
            {
                //Change size
                osg::Texture2D* texture =
                    //dynamic_cast< osg::Texture2D* >( itr->second.get() );
                    dynamic_cast< osg::Texture2D* >( itr->get() );
                texture->setTextureSize(
                    static_cast< int >( mViewport->width() ),
                    static_cast< int >( mViewport->height() ) );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Unit::AssignInputTexture()
{
    //Here the textures will be applied
    osg::StateSet* stateset = getOrCreateStateSet();

    //For all entries
    TextureMap::iterator itr = mInputTextures.begin();
    for( itr; itr != mInputTextures.end(); ++itr )
    {
        //Set texture if it is valid
        //if( itr->second.valid() )
        if( itr->valid() )
        {
            size_t index = itr - mInputTextures.begin();
            stateset->setTextureAttributeAndModes(
                //itr->first, itr->second.get(),
                index, itr->get(),
                osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void Unit::CreateTexturedQuadDrawable(
    const osg::Vec3& corner,
    const osg::Vec3& widthVec,
    const osg::Vec3& heightVec,
    float l, float b, float r, float t )
{
    //Get the vertex coordinates for the quad
    osg::ref_ptr< osg::Vec3Array > quadVertices = new osg::Vec3Array();
    quadVertices->resize( 4 );

    (*quadVertices)[ 0 ] = corner;
    (*quadVertices)[ 1 ] = corner + widthVec;
    (*quadVertices)[ 2 ] = corner + widthVec + heightVec;
    (*quadVertices)[ 3 ] = corner + heightVec;

    //Get the texture coordinates for the quad
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );

    (*quadTexCoords)[ 0 ].set( l, b );
    (*quadTexCoords)[ 1 ].set( r, b );
    (*quadTexCoords)[ 2 ].set( r, t );
    (*quadTexCoords)[ 3 ].set( l, t );

    //Create the quad geometry
    osg::Geometry* quadGeometry = new osg::Geometry();
    quadGeometry->setVertexArray( quadVertices.get() );
    quadGeometry->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::QUADS, 0, quadVertices->size() ) );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    quadGeometry->setUseDisplayList( true );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OFF );

    //Set the stateset for the quad
    osg::ref_ptr< osg::StateSet > stateset =
        quadGeometry->getOrCreateStateSet();
    stateset->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    mDrawable = quadGeometry;
}
////////////////////////////////////////////////////////////////////////////////

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
#include <apps/xplorer/rtt/Unit.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Image>
#include <osg/Program>
#include <osg/Texture2D>

//#include <osgDB/WriteFile>
//#include <osgDB/Registry>

// --- C/C++ Includes --- //
#include <cmath>

using namespace ves::xplorer::rtt;

////////////////////////////////////////////////////////////////////////////////
Unit::DrawCallback::DrawCallback()
    :
    osg::Drawable::DrawCallback(),
    mUnit( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Unit::DrawCallback::DrawCallback( Unit* unit )
    :
    osg::Drawable::DrawCallback(),
    mUnit( unit )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Unit::DrawCallback::DrawCallback(
    const Unit::DrawCallback& drawCallback, const osg::CopyOp& copyop )
    :
    osg::Drawable::DrawCallback( drawCallback, copyop ),
    mUnit( drawCallback.mUnit )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Unit::DrawCallback::~DrawCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Unit::DrawCallback::drawImplementation(
    osg::RenderInfo& ri, const osg::Drawable* dr ) const
{
    //Set matricies used for the unit
    ri.getState()->applyProjectionMatrix( mUnit->mProjectionMatrix.get() );
    ri.getState()->applyModelViewMatrix( mUnit->mModelViewMatrix.get() );
}
////////////////////////////////////////////////////////////////////////////////
Unit::Unit()
    :
    osg::Group(),
    mActive( true ),
    mShader( NULL ),
    mDrawable( NULL ),
    mViewport( NULL ),
    mGeode( NULL ),
    mProjectionMatrix( new osg::RefMatrix(
        osg::Matrix::ortho( 0.0, 1.0, 0.0, 1.0, 0.0, 1.0 ) ) ),
    mModelViewMatrix( new osg::RefMatrix( osg::Matrix::identity() ) )
{
    //Set default name
    setName( "Unit" );

    //No culling
    setCullingActive( false );
}
////////////////////////////////////////////////////////////////////////////////
Unit::Unit( const Unit& unit, const osg::CopyOp& copyop )
    :
    osg::Group( unit, copyop ),
    mActive( unit.mActive ),
    mInputTextures( unit.mInputTextures ),
    mOutputTextures( unit.mOutputTextures ),
    mShader( unit.mShader ),
    mDrawable( unit.mDrawable ),
    mViewport( unit.mViewport ),
    mGeode( unit.mGeode )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
Unit::~Unit()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture* const Unit::GetInputTexture( int inputIndex ) const
{
    TextureMap::const_iterator itr = mInputTextures.find( inputIndex );
    if( itr != mInputTextures.end() )
    {
        return itr->second.get();
    }
    
    return NULL;
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture* const Unit::GetOutputTexture( int mrt ) const
{
    TextureMap::const_iterator itr = mOutputTextures.find( mrt );
    if( itr != mOutputTextures.end() )
    {
        return itr->second.get();
    }

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
    // collect all inputs from the units above
    SetInputTexturesFromParents();

    /*
    // check if we have input reference size
    if( getInputTextureIndexForViewportReference() >= 0 &&
         getInputTexture( getInputTextureIndexForViewportReference() ) )
    {
        // if no viewport, so create it
        if (!mViewport.valid())
            mViewport = new osg::Viewport(0,0,0,0);

        // change viewport sizes
        mViewport->width() = getInputTexture(getInputTextureIndexForViewportReference())->getTextureWidth();
        mViewport->height() = getInputTexture(getInputTextureIndexForViewportReference())->getTextureHeight();

        // just notice that the viewport size is changed
        noticeChangeViewport();
    }

    // reassign input and shaders
    assignInputTexture();
    assignShader();
    assignViewport();
    */
}
////////////////////////////////////////////////////////////////////////////////
void Unit::SetInputTexturesFromParents()
{
    //Scan all parents and look for units
    rtt::Unit* unit( NULL );
    for( unsigned int i = 0; i < getNumParents(); ++i )
    {
        unit = dynamic_cast< rtt::Unit* >( getParent( i ) );
        if( unit )
        {
            //Add each found texture as input
            const Unit::TextureMap& textureMap = unit->GetOutputTextureMap();
            Unit::TextureMap::const_iterator itr = textureMap.begin();
            for( itr; itr != textureMap.end(); ++itr )
            {
                osg::Texture* texture = itr->second.get();
                if( texture )
                {
                    mInputTextures[ mInputTextures.size() ] = itr->second.get();
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
    }
}
////////////////////////////////////////////////////////////////////////////////

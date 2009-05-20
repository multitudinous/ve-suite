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

// --- My Includes --- //
#include "DepthOfFieldTechnique.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //
#include <osg/Texture2D>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
DepthOfFieldTechnique::DepthOfFieldTechnique()
    :
    ves::xplorer::scenegraph::Technique(),
    mTextureDimensionsUniform(
        new osg::Uniform( osg::Uniform::INT_VEC2, "textureDimensions" ) ),
    mMaxCircleOfConfusionUniform(
        new osg::Uniform( osg::Uniform::FLOAT, "maxCoC" ) )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
DepthOfFieldTechnique::~DepthOfFieldTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DepthOfFieldTechnique::DefinePasses()
{
    //Implement pass #1
    {
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

        stateset->setRenderBinDetails( 1, std::string( "RenderBin" ) );

        stateset->setTextureAttributeAndModes( 0,
            ( ves::xplorer::scenegraph::ResourceManager::instance()->get
            < osg::Texture2D, osg::ref_ptr >( "CameraViewTexture" ) ).get(),
            osg::StateAttribute::ON );

        stateset->setTextureAttributeAndModes( 1,
            ( ves::xplorer::scenegraph::ResourceManager::instance()->get
            < osg::Texture2D, osg::ref_ptr >( "DepthTexture" ) ).get(),
            osg::StateAttribute::ON );

        stateset->setAttribute(
            ( ves::xplorer::scenegraph::ResourceManager::instance()->get
            < osg::Program, osg::ref_ptr >( "CameraViewProgram" ) ).get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        stateset->setMode(
            GL_LIGHTING,
            osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::Uniform > textureZeroUniform =
            new osg::Uniform( "texture0", 0 );
        osg::ref_ptr< osg::Uniform > textureOneUniform =
            new osg::Uniform( "texture1", 1 );

        stateset->addUniform( textureZeroUniform.get() );
        stateset->addUniform( textureOneUniform.get() );
        stateset->addUniform( mTextureDimensionsUniform.get() );
        stateset->addUniform( mMaxCircleOfConfusionUniform.get() );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const DepthOfFieldTechnique::GetTextureDimensionsUniform() const
{
    return mTextureDimensionsUniform.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Uniform* const DepthOfFieldTechnique::GetMaxCircleOfConfusionUniform() const
{
    return mMaxCircleOfConfusionUniform.get();
}
////////////////////////////////////////////////////////////////////////////////

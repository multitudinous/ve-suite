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
#include "DepthHelperTechnique.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ResourceManager.h>

// --- OSG Includes --- //
#include <osg/Texture2D>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
DepthHelperTechnique::DepthHelperTechnique()
    :
    ves::xplorer::scenegraph::technique::Technique()
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
DepthHelperTechnique::~DepthHelperTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void DepthHelperTechnique::DefinePasses()
{
    //Implement pass #1
    {
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

        stateset->setRenderBinDetails( 2, std::string( "RenderBin" ) );

        stateset->setTextureAttributeAndModes( 0,
            ( ves::xplorer::scenegraph::ResourceManager::instance()->get
            < osg::Texture2D, osg::ref_ptr >( "DepthTexture" ) ).get(),
            osg::StateAttribute::ON );

        stateset->setAttribute(
            ( ves::xplorer::scenegraph::ResourceManager::instance()->get
            < osg::Program, osg::ref_ptr >( "RenderBlurProgram" ) ).get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        stateset->setMode(
            GL_LIGHTING,
            osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::Uniform > textureOneUniform =
            new osg::Uniform( "texture1", 0 );

        stateset->addUniform( textureOneUniform.get() );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////

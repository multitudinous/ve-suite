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
#include "Shaders.h"
#include "VertFrag.h"

// --- OSG Includes --- //
#include <osg/Node>
#include <osg/StateSet>

////////////////////////////////////////////////////////////////////////////////
Shaders::Shaders()
:
m_phong( new osg::StateSet() ),
m_xray( new osg::StateSet() )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Shaders::~Shaders()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Initialize()
{
    osg::ref_ptr< osg::Program > phongProgram = new osg::Program();
    osg::ref_ptr< osg::Program > xrayProgram = new osg::Program();

    m_phong->setAttribute( phongProgram.get(), osg::StateAttribute::ON );
    m_xray->setAttribute( xrayProgram.get(), osg::StateAttribute::ON );

    osg::ref_ptr< osg::Shader > phongVertexShader =
        new osg::Shader( osg::Shader::VERTEX, phongVertex );
    osg::ref_ptr< osg::Shader > phongFragmentShader =
        new osg::Shader( osg::Shader::FRAGMENT, phongFragment );
    phongProgram->addShader( phongVertexShader.get() );
    phongProgram->addShader( phongFragmentShader.get() );

    osg::ref_ptr< osg::Shader > xrayVertexShader =
        new osg::Shader( osg::Shader::VERTEX, xrayVertex );
    osg::ref_ptr< osg::Shader > xrayFragmentShader =
        new osg::Shader( osg::Shader::FRAGMENT, xrayFragment );
    xrayProgram->addShader( xrayVertexShader.get() );
    xrayProgram->addShader( xrayFragmentShader.get() );

    osg::ref_ptr< osg::Uniform > ambientMaterial = new osg::Uniform(
        "ambientMaterial", osg::Vec3( 0.368627, 0.368421, 0.368421 ) );
    m_phong->addUniform( ambientMaterial.get() );

    osg::ref_ptr< osg::Uniform > diffuseMaterial = new osg::Uniform(
        "diffuseMaterial", osg::Vec3( 0.886275, 0.885003, 0.885003 ) );
    m_phong->addUniform( diffuseMaterial.get() );

    osg::ref_ptr< osg::Uniform > specularMaterial = new osg::Uniform(
        "specularMaterial", osg::Vec3( 0.490196, 0.488722, 0.488722 ) );
    m_phong->addUniform( specularMaterial.get() );

    float specularPowerValue = 20.0;
    osg::ref_ptr< osg::Uniform > specularPower = new osg::Uniform(
        "specularPower", specularPowerValue );
    m_phong->addUniform( specularPower.get() );

    m_xray->setRenderBinDetails( 10, std::string( "DepthSortedBin" ) );
    m_xray->setMode( GL_BLEND, osg::StateAttribute::ON );
    
    m_phong->setRenderBinDetails( 0, std::string( "RenderBin" ) );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::Phong( osg::Node* node )
{
    node->setStateSet( m_phong.get() );
}
////////////////////////////////////////////////////////////////////////////////
void Shaders::XRay( osg::Node* node )
{
    node->setStateSet( m_xray.get() );
}
////////////////////////////////////////////////////////////////////////////////

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
 * Date modified: $Date: 2009-05-13 14:11:06 -0600 (Wed, 13 May 2009) $
 * Version:       $Rev: 12682 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: GlowTechnique.cxx 12682 2009-05-13 20:11:06Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/technique/GlowTechnique.h>

// --- OSG Includes --- //
#include <osg/Depth>

using namespace ves::xplorer::scenegraph::technique;

////////////////////////////////////////////////////////////////////////////////
GlowTechnique::GlowTechnique( osg::ref_ptr< osg::StateSet > stateSet )
    :
    Technique(),
    m_stateSet( stateSet )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
GlowTechnique::~GlowTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void GlowTechnique::DefinePasses()
{
    osg::Vec4 glowColor( 1.0, 0.0, 0.0, 1.0 );
    //Pass 1
    {
        //m_stateSet->setRenderBinDetails( -1, "RenderBin" );

        m_stateSet->addUniform( new osg::Uniform( "glowColor", glowColor ) );

        AddPass( m_stateSet.get() );
    }
    //Pass 2
    {
        std::string fragmentSource =
        "uniform vec4 glowColor; \n"

        "void main() \n"
        "{ \n"
            "gl_FragData[ 2 ] = glowColor; \n"
        "} \n";

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragmentShader.get() );

        stateset->setAttributeAndModes( program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::Depth > depth = new osg::Depth();
        depth->setFunction( osg::Depth::ALWAYS );
        depth->setWriteMask( false );

        stateset->setAttributeAndModes( depth.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        stateset->addUniform( new osg::Uniform( "glowColor", glowColor ) );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////

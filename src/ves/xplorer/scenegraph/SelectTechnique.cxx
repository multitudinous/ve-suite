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
#include <ves/xplorer/scenegraph/SelectTechnique.h>

// --- OSG Includes --- //
#include <osg/Stencil>
#include <osg/LineWidth>
#include <osg/Material>
#include <osg/PolygonMode>
#include <osg/Depth>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
SelectTechnique::SelectTechnique( osg::ref_ptr< osg::StateSet > stateSet )
    :
    mStateSet( stateSet )
{
    DefinePasses();
}
////////////////////////////////////////////////////////////////////////////////
SelectTechnique::~SelectTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SelectTechnique::DefinePasses()
{
    /*
    osg::Vec4 glowColor( 1.0, 0.0, 0.0, 1.0 );
    //Pass 1
    {
        //mStateSet->setRenderBinDetails( -1, "RenderBin" );

        mStateSet->addUniform( new osg::Uniform( "glowColor", glowColor ) );

        AddPass( mStateSet.get() );
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
    */

    //Implement pass #1
    {
        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::ALWAYS,    //comparison function
                              1,                       //reference value
                              ~0u );                   //comparison mask
        stencil->setOperation( osg::Stencil::KEEP,     //stencil fail
                               osg::Stencil::KEEP,     //stencil pass/depth fail
                               osg::Stencil::REPLACE );//stencil pass/depth pass

        mStateSet->setMode( GL_STENCIL_TEST,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        mStateSet->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        AddPass( mStateSet.get() );
    }

    //Implement pass #2
    {
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

        std::string fragmentSource =
        "uniform vec4 selectionColor; \n"

        "void main() \n"
        "{ \n"
            "gl_FragColor = selectionColor; \n"
        "} \n";

        osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
        fragmentShader->setType( osg::Shader::FRAGMENT );
        fragmentShader->setShaderSource( fragmentSource );

        osg::ref_ptr< osg::Program > program = new osg::Program();
        program->addShader( fragmentShader.get() );

        stateset->setAttributeAndModes( program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::Vec4d selectionColor( 1.0, 0.75, 1.0, 1.0 );
        stateset->addUniform(
            new osg::Uniform( "selectionColor", selectionColor ) );

        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::NOTEQUAL,  //comparison function
                              1,                       //reference value
                              ~0u );                   //comparison mask
        stencil->setOperation( osg::Stencil::KEEP,     //stencil fail
                               osg::Stencil::KEEP,     //stencil pass/depth fail
                               osg::Stencil::REPLACE );//stencil pass/depth pass

        stateset->setMode( GL_STENCIL_TEST,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::PolygonMode > polygonMode = new osg::PolygonMode();
        polygonMode->setMode(
            osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );

        stateset->setAttributeAndModes( polygonMode.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::LineWidth > linewidth = new osg::LineWidth();
        linewidth->setWidth( 2.0 );
        stateset->setAttributeAndModes( linewidth.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        /*
        stateset->setMode( GL_LIGHTING,
            osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::Material > material = new osg::Material();
        material->setColorMode( osg::Material::EMISSION );
        material->setEmission(
            osg::Material::FRONT_AND_BACK, osg::Vec4( 1.0, 0.75, 1.0, 1.0 ) );

        stateset->setAttributeAndModes( material.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        */

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////

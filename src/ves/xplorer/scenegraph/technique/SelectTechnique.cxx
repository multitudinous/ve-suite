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
#include <ves/xplorer/scenegraph/technique/SelectTechnique.h>

// --- OSG Includes --- //
#include <osg/Point>
#include <osg/LineWidth>
#include <osg/PolygonMode>
#include <osg/Depth>
#include <osg/Stencil>

using namespace ves::xplorer::scenegraph::technique;

////////////////////////////////////////////////////////////////////////////////
SelectTechnique::SelectTechnique( osg::ref_ptr< osg::StateSet > stateSet )
    :
    Technique(),
    m_lineAndPointSize( 4.0 ),
    m_stateSet( stateSet )
{
    m_color = new osg::Uniform( "color", osg::Vec4f( 1.0, 0.0, 1.0, 1.0 ) );

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
    //Implement pass #1
    //Render the geometry with current StateSet and mask off stencil buffer
    {
        //Setup the stencil function and operation
        osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
        stencil->setFunction( osg::Stencil::ALWAYS,    //comparison function
                              1,                       //reference value
                              ~0u );                   //comparison mask
        stencil->setOperation( osg::Stencil::REPLACE,  //stencil fail
                               osg::Stencil::REPLACE,  //stencil pass/depth fail
                               osg::Stencil::REPLACE );//stencil pass/depth pass

        m_stateSet->setMode( GL_STENCIL_TEST,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        m_stateSet->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        AddPass( m_stateSet.get() );
    }

    //Create the shader used to render the lines and points
    std::string fragmentSource =
    "uniform vec4 color; \n"

    "void main() \n"
    "{ \n"
        "gl_FragColor = color; \n"
    "} \n";

    osg::ref_ptr< osg::Shader > fragmentShader = new osg::Shader();
    fragmentShader->setType( osg::Shader::FRAGMENT );
    fragmentShader->setShaderSource( fragmentSource );

    osg::ref_ptr< osg::Program > program = new osg::Program();
    program->addShader( fragmentShader.get() );

    //Create the stencil used to render the lines and points
    osg::ref_ptr< osg::Stencil > stencil = new osg::Stencil();
    stencil->setFunction( osg::Stencil::NOTEQUAL,  //comparison function
                          1,                       //reference value
                          ~0u );                   //comparison mask
    stencil->setOperation( osg::Stencil::KEEP,     //stencil fail
                           osg::Stencil::KEEP,     //stencil pass/depth fail
                           osg::Stencil::REPLACE );//stencil pass/depth pass

    //Implement pass #2
    //Render the geometry as lines where the stencil buffer has not been masked
    {
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setAttributeAndModes( program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        stateset->addUniform( m_color.get() );

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
        linewidth->setWidth( m_lineAndPointSize );
        stateset->setAttributeAndModes( linewidth.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        AddPass( stateset.get() );
    }

    //Implement pass #3
    //Render the geometry as points where the stencil buffer has not been masked
    {
        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        stateset->setAttributeAndModes( program.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        stateset->addUniform( m_color.get() );

        stateset->setMode( GL_STENCIL_TEST,
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
        stateset->setAttributeAndModes( stencil.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        osg::ref_ptr< osg::PolygonMode > polygonMode = new osg::PolygonMode();
        polygonMode->setMode(
            osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::POINT );

        stateset->setAttributeAndModes( polygonMode.get(),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        stateset->setAttribute( new osg::Point( m_lineAndPointSize ),
            osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void SelectTechnique::UseColor( osg::Vec4f color )
{
    m_color->set( color );
}
////////////////////////////////////////////////////////////////////////////////

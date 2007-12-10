/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/SelectTechnique.h>

// --- OSG Includes --- //
#include <osg/BlendFunc>
#include <osg/CameraNode>
#include <osg/Texture2D>
#include <osg/LineWidth>
#include <osg/Material>
#include <osg/PolygonMode>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
SelectTechnique::SelectTechnique( osg::ref_ptr< osg::StateSet > stateSet )
        :
        m_stateSet( stateSet )
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
    //Implement pass #1
    {
        AddPass( m_stateSet.get() );
    }

    //Implement pass #2
    {
        char vertexPass[] =
            "varying vec3 Normal; \n"

            "void main() \n"
            "{ \n"
            "gl_Position = ftransform(); \n"
            "} \n";

        char fragmentPass[] =
            "void main() \n"
            "{ \n"
            "gl_FragColor = vec4( 0.0, 1.0, 0.0, 1.0 ); \n"
            "} \n";

        osg::ref_ptr< osg::StateSet > stateSet = new osg::StateSet();
        osg::ref_ptr< osg::Program > program = new osg::Program;
        osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, vertexPass );
        osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, fragmentPass );
        osg::ref_ptr< osg::LineWidth > linewidth = new osg::LineWidth();
        osg::ref_ptr< osg::PolygonMode > polymode = new osg::PolygonMode();

        program->addShader( vertex_shader.get() );
        program->addShader( fragment_shader.get() );

        linewidth->setWidth( 2.0f );
        polymode->setMode( osg::PolygonMode::FRONT_AND_BACK, osg::PolygonMode::LINE );

        stateSet->setAttributeAndModes( linewidth.get(), osg::StateAttribute::ON );
        stateSet->setAttributeAndModes( polymode.get(), osg::StateAttribute::ON );
        stateSet->setAttributeAndModes( program.get(), osg::StateAttribute::ON );

        AddPass( stateSet.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////

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
#include "VE_Xplorer/SceneGraph/SelectTechnique.h"
#include "VE_Xplorer/SceneGraph/DCS.h"

// --- OSG Includes --- //
#include <osg/BlendFunc>

using namespace VE_SceneGraph;

////////////////////////////////////////////////////////////////////////////////
SelectTechnique::SelectTechnique( VE_SceneGraph::DCS* dcs )
{
    DefinePasses( dcs );
}
////////////////////////////////////////////////////////////////////////////////
SelectTechnique::~SelectTechnique()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SelectTechnique::DefinePasses( VE_SceneGraph::DCS* dcs )
{
    //Implement pass #1
    {
        /*
        char select_vertex_pass1[] =
            "varying vec3 ECPosition; \n"
            "varying vec3 LightDirection; \n"
            "varying vec3 Normal; \n"

            "void main() \n"
            "{ \n"
                "gl_Position = ftransform(); \n"

                "ECPosition = vec3( gl_ModelViewMatrix * gl_Vertex ); \n"
                "LightDirection = -ECPosition; \n"
                "Normal = vec3( gl_NormalMatrix * gl_Normal ); \n"
            "} \n";

        char select_fragment_pass1[] =
            "varying vec3 ECPosition; \n"
            "varying vec3 LightDirection; \n"
            "varying vec3 Normal; \n"

            "void main() \n"
            "{ \n"
                "vec3 diffColor = vec3( 0.2, 0.2, 0.2 ); \n"
                "float rollOff = 0.9; \n"      
                "vec3 specColor = vec3( 0.7, 0.75, 0.7 ); \n"
                "vec3 subColor = vec3( 0.0, 1.0, 0.0 ); \n"

                "vec3 Nn = normalize( Normal ); \n"
                "vec3 Ln = normalize( LightDirection ); \n"
                "float ldn = dot( Nn, Ln ); \n"
                "float diffComp = max( ldn, 0.0 ); \n"
                "vec3 diffContrib = diffComp * diffColor; \n"
                "float subLamb = smoothstep( -rollOff, 1.00000, ldn ) - smoothstep( 0.000000, 1.00000, ldn ); \n"
                "subLamb = max( subLamb, 0.0 ); \n"
                "vec3 subContrib = subColor * subLamb; \n"
                "vec3 Vn = normalize( -ECPosition ); \n"
                "float vdn = 1.0 - dot( Nn, Vn ); \n"
                "vec3 vecColor = vec3( vdn, vdn, vdn ); \n"
                "vec3 Diffuse = subContrib + diffContrib; \n"
                "vec3 Specular = specColor * vecColor; \n"

                "gl_FragColor = vec4( Diffuse + Specular, 1.0 ); \n"
            "} \n";
            */

        osg::ref_ptr< osg::StateSet > stateset = dcs->getOrCreateStateSet();
        //osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();
        //osg::ref_ptr< osg::Program > program = new osg::Program;

        //osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, select_vertex_pass1 );
        //program->addShader( vertex_shader.get() );

        //osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, select_fragment_pass1 );
        //program->addShader( fragment_shader.get() );

        //stateset->setAttributeAndModes( program.get() );

        AddPass( stateset.get() );
    }

    //Implement pass #2
    {
        char select_vertex_pass2[] =
            "varying vec3 Normal; \n"

            "void main() \n"
            "{ \n"
                "float GlowThickness = 0.05; \n"

                "//Normal ( view space ) \n"
                "Normal = vec3( gl_NormalMatrix * gl_Normal ); \n"

                "//Displaced position ( view space ) \n"
                "vec3 P = vec3( gl_ModelViewMatrix * gl_Vertex ) + ( GlowThickness * Normal ); \n"

                "gl_Position = gl_ProjectionMatrix * vec4( P, 1.0 ); \n"
            "} \n";

        char select_fragment_pass2[] =
            "varying vec3 Normal; \n"

            "void main() \n"
            "{ \n"
                "vec4 GlowColor = vec4( 0.0, 0.5, 0.0, 1.0 ); \n"
                "vec4 GlowAmbient = vec4( 0.0, 0.0, 0.0, 1.0 ); \n"

                "vec3 N = normalize( Normal ); \n"

                "//Glow axis \n"
                "vec3 A = vec3( 0, 0, 1 ); \n"
                "float Power = dot( N, A ); \n"
                "Power *= Power; \n"
                "Power -= 1.0; \n"
                "Power *= Power; \n"

                "//Inverse Glow Effect \n"
                "//Power = pow( 1.0 - pow( Power, 2.0 ), 2.0 ); \n"
                "//Power = pow( pow( Power, 2.0 ) - 1.0, 2.0 ); \n"

                "gl_FragColor = GlowColor * Power + GlowAmbient; \n"
            "} \n";

        osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;
        osg::ref_ptr< osg::Program > program = new osg::Program;

        osg::ref_ptr< osg::Shader > vertex_shader = new osg::Shader( osg::Shader::VERTEX, select_vertex_pass2 );
        program->addShader( vertex_shader.get() );

        osg::ref_ptr< osg::Shader > fragment_shader = new osg::Shader( osg::Shader::FRAGMENT, select_fragment_pass2 );
        program->addShader( fragment_shader.get() );

        osg::ref_ptr< osg::BlendFunc > bf = new osg::BlendFunc( GL_ONE, GL_ONE );
        stateset->setAttribute( bf.get() );
        stateset->setMode( GL_BLEND, osg::StateAttribute::ON );
        //stateset->setRenderingHint( osg::StateSet::TRANSPARENT_BIN ); 

        stateset->setAttributeAndModes( program.get() );

        AddPass( stateset.get() );
    }
}
////////////////////////////////////////////////////////////////////////////////

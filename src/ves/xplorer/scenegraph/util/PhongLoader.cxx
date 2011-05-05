/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#include <ves/xplorer/scenegraph/util/PhongLoader.h>

#include <iostream>
#include <sstream>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/Program.h>
#include <ves/open/xml/shader/Uniform.h>
using namespace ves::open::xml::shader;
using namespace ves::xplorer::scenegraph::util;
//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
PhongLoader::PhongLoader()
        : ShaderHelper()
{}
////////////////////////////////////////////////////////////////////////////////
PhongLoader::PhongLoader( const PhongLoader& rhs )
        : ShaderHelper( rhs )
{}
////////////////////////////////////////////////////////////////////////////////
PhongLoader& PhongLoader::operator=( const PhongLoader& rhs )
{
    if( this != &rhs )
    {
        ShaderHelper::operator=( rhs );
    }
    return *this;
}
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
PhongLoader::~PhongLoader()
{}
////////////////////////////////////////////////////////////////////////////////
void PhongLoader::_loadShader( std::string vertexSource, std::string fragmentSource )
{
    ShaderPtr vertShader(  new Shader() );
    vertShader->SetShaderType( "Vertex" );
    vertShader->SetShaderSource( vertexSource );

    ShaderPtr fragShader(  new Shader() );
    fragShader->SetShaderType( "Fragment" );
    fragShader->SetShaderSource( fragmentSource );

    UniformPtr amaterial(  new Uniform() );
    amaterial->SetType( "Float" );
    amaterial->SetName( "ambientMaterial" );
    amaterial->SetSize( 3 );
    std::vector<float> ambient;
    ambient.push_back( 0.368627 );
    ambient.push_back( 0.368421 );
    ambient.push_back( 0.368421 );
    amaterial->SetValues( ambient );

    UniformPtr dmaterial(  new Uniform() );
    dmaterial->SetType( "Float" );
    dmaterial->SetName( "diffuseMaterial" );
    dmaterial->SetSize( 3 );
    std::vector<float> diffuse;
    diffuse.push_back( 0.886275 );
    diffuse.push_back( 0.885003 );
    diffuse.push_back( 0.885003 );
    dmaterial->SetValues( diffuse );

    UniformPtr smaterial(  new Uniform() );
    smaterial->SetType( "Float" );
    smaterial->SetName( "specularMaterial" );
    smaterial->SetSize( 3 );
    std::vector<float> specular;
    specular.push_back( 0.490196 );
    specular.push_back( 0.488722 );
    specular.push_back( 0.488722 );
    smaterial->SetValues( specular );

    UniformPtr specularValues(  new Uniform() );
    specularValues->SetType( "Float" );
    specularValues->SetName( "specularPower" );
    specularValues->SetSize( 1 );
    std::vector<float> specularPower;
    specularPower.push_back( 20.0 );
    specularValues->SetValues( specularPower );

    //fragShader->AddUniform( amaterial );
    //fragShader->AddUniform( dmaterial );
    //fragShader->AddUniform( smaterial );
    //fragShader->AddUniform( specularValues );

    vertShader->AddUniform( amaterial );
    vertShader->AddUniform( dmaterial );
    vertShader->AddUniform( smaterial );
    vertShader->AddUniform( specularValues );
    
    ProgramPtr glslProgram(  new Program() );
    glslProgram->SetProgramName( "Phong Shader" );
    glslProgram->SetVertexShader( vertShader );
    glslProgram->SetFragmentShader( fragShader );

    LoadGLSLProgram( glslProgram );

    //enable 2 sided lighting fix
    if( m_twoSidedLighting )
    {
        m_ss->setMode( GL_VERTEX_PROGRAM_TWO_SIDE, osg::StateAttribute::ON );
    }
}
////////////////////////////////////////////////////////////////////////////////
void PhongLoader::SyncShaderAndStateSet()
{
    std::string vSource(
        "uniform vec3 ambientMaterial;\n"
        "uniform vec3 diffuseMaterial;\n"
        "uniform vec3 specularMaterial;\n"
        "uniform float specularPower;\n"

        "varying vec3 color;\n"
        "varying vec3 lightPos;\n"
        "varying vec3 objPos;\n"
        "varying vec3 eyePos;\n"
        "varying vec3 normal;\n"
        "void main()\n"
        "{\n"
        "    gl_Position=ftransform();\n"

        "    color=gl_Color.xyz;\n"
        "    objPos=gl_Vertex.xyz;\n"
        "    eyePos=vec3(gl_ModelViewMatrix*gl_Vertex);\n"
        "    lightPos=gl_LightSource[0].position.xyz;\n"
        "    normal=vec3(gl_NormalMatrix*gl_Normal);\n"

        "    vec3 N=normalize(normal);\n"
        "    vec3 L=normalize(lightPos);\n"
        "    float NDotL=max(dot(N,L),0.0);\n"
        
        "    vec3 V=normalize(eyePos);\n"
        "    vec3 R=reflect(V,N);\n"
        "    float RDotL=max(dot(R,L),0.0);\n"
        
        "    vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*ambientMaterial*color;\n"
        "    vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*diffuseMaterial*color*NDotL;\n"
        "    vec3 TotalSpecular=gl_LightSource[0].specular.rgb*specularMaterial*pow(RDotL,specularPower);\n"

        //"    gl_FrontSecondaryColor=vec4(1.0);\n"
        //"    gl_BackSecondaryColor=vec4(0.0);\n"
        //"    gl_BackColor = vec4( color, 1.0);\n"
        //"    gl_FrontColor = vec4( color, 1.0);\n"
        "    gl_FrontColor=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0);\n"

        //Now compute the back face for two sided lighting
        "    N = -N;\n"
        "    NDotL=max(dot(N,L),0.0);\n"
        "    R=reflect(V,N);\n"
        "    RDotL=max(dot(R,L),0.0);\n"
        
        "    TotalAmbient=gl_LightSource[0].ambient.rgb*ambientMaterial*color;\n"
        "    TotalDiffuse=gl_LightSource[0].diffuse.rgb*diffuseMaterial*color*NDotL;\n"
        "    TotalSpecular=gl_LightSource[0].specular.rgb*specularMaterial*pow(RDotL,specularPower);\n"

        "    gl_BackColor=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0);\n"
        "}\n"
    );
    std::string fSource(
        //"uniform vec3 ambientMaterial;\n"
        //"uniform vec3 diffuseMaterial;\n"
        //"uniform vec3 specularMaterial;\n"
        //"uniform float specularPower;\n"

        //"varying vec3 color;\n"
        //"varying vec3 lightPos;\n"
        //"varying vec3 objPos;\n"
        //"varying vec3 eyePos;\n"
        //"varying vec3 normal;\n"

        //Use gl_FrontFacing to determine if the fragment is front facing
        "void main()\n"
        "{\n"
        //"    vec3 N=normalize(normal);\n"
        //"    if(gl_SecondaryColor.r < .5)\n"
        //"    {\n"
        //"       N=-N; \n"
        //"    }\n"
        //"    vec3 L=normalize(lightPos);\n"
        //"    float NDotL=max(dot(N,L),0.0);\n"

        //"    vec3 V=normalize(eyePos);\n"
        //"    vec3 R=reflect(V,N);\n"
        //"    float RDotL=max(dot(R,L),0.0);\n"

        //"    vec3 TotalAmbient=gl_LightSource[0].ambient.rgb*ambientMaterial*color;\n"
        //"    vec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*diffuseMaterial*color*NDotL;\n"
        //"    vec3 TotalSpecular=gl_LightSource[0].specular.rgb*specularMaterial*pow(RDotL,specularPower);\n"

        //"    gl_FragData[ 0 ]=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0);\n"
        "    gl_FragData[ 0 ]=gl_Color;\n"
        "    //To handle the glow\n"
        "    gl_FragData[ 1 ] = vec4( 0, 0, 0, 1 );\n"
        " }\n"
    );
    _loadShader( vSource, fSource );
}
////////////////////////////////////////////////////////////////////////////////

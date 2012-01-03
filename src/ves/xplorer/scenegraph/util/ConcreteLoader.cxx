/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include <ves/xplorer/scenegraph/util/ConcreteLoader.h>
#include <ves/xplorer/scenegraph/util/PerlinNoiseTexture.h>
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
ConcreteLoader::ConcreteLoader()
        : ShaderHelper()

{
    m_noise = new PerlinNoiseTexture( 64, 64, 64 );
}
///////////////////////////////////////////////////
ConcreteLoader::ConcreteLoader( const ConcreteLoader& rhs )
        : ShaderHelper( rhs )
{}
//////////////////////////////////////////////////////////////
ConcreteLoader& ConcreteLoader::operator=( const ConcreteLoader& rhs )
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
ConcreteLoader::~ConcreteLoader()
{
    if( m_noise )
    {
        delete m_noise;
        m_noise = 0;
    }
}
/////////////////////////////////////////////////////////////////////////////////
void ConcreteLoader::_loadShader( std::string vertexSource, std::string fragmentSource )
{
    ShaderPtr vertShader(  new Shader() );
    vertShader->SetShaderType( "Vertex" );
    vertShader->SetShaderSource( vertexSource );

    ShaderPtr fragShader(  new Shader() );
    fragShader->SetShaderType( "Fragment" );
    fragShader->SetShaderSource( fragmentSource );

    UniformPtr lightPosition(  new Uniform() );
    lightPosition->SetType( "Float" );
    lightPosition->SetName( "LightPos" );
    lightPosition->SetSize( 3 );
    std::vector<float> lightPos;
    lightPos.push_back( 10.0 );
    lightPos.push_back( 10.0 );
    lightPos.push_back( 10.0 );
    lightPosition->SetValues( lightPos );

    UniformPtr scale(  new Uniform() );
    scale->SetType( "Float" );
    scale->SetName( "Scale" );
    scale->SetSize( 1 );
    std::vector<float> value;
    value.push_back( 1.0 );
    scale->SetValues( value );

    UniformPtr noiseScale(  new Uniform() );
    noiseScale->SetType( "Float" );
    noiseScale->SetName( "NoiseScale" );
    noiseScale->SetSize( 1 );
    std::vector<float> ns;
    ns.push_back( 1.0 );
    noiseScale->SetValues( ns );

    UniformPtr specularValues(  new Uniform() );
    specularValues->SetType( "Float" );
    specularValues->SetName( "specularPower" );
    specularValues->SetSize( 1 );
    std::vector<float> specularPower;
    specularPower.push_back( 20.0 );
    specularValues->SetValues( specularPower );

    fragShader->AddUniform( noiseScale );
    //fragShader->AddUniform(dmaterial);
    vertShader->AddUniform( scale );
    vertShader->AddUniform( lightPosition );
    ProgramPtr glslProgram(  new Program() );
    glslProgram->SetProgramName( "Phong Shader" );
    glslProgram->SetVertexShader( vertShader );
    glslProgram->SetFragmentShader( fragShader );

    LoadGLSLProgram( glslProgram );
    m_ss->addUniform( new osg::Uniform( "Noise", 0 ) );
    m_ss->setTextureAttributeAndModes( 0, m_noise->GetNoiseTexture() );
}
/////////////////////////////////////////
void ConcreteLoader::SyncShaderAndStateSet()
{
    std::string vSource(
        "varying float LightIntensity;\n"
        "varying vec3  MCposition;\n"

        "uniform vec3  LightPos;\n"
        "uniform float Scale;\n"

        "void main()\n"
        "{\n"
        "vec3 ECposition = vec3(gl_ModelViewMatrix * gl_Vertex);\n"
        "MCposition      = vec3(gl_Vertex) * Scale;\n"
        "vec3 tnorm      = normalize(vec3(gl_NormalMatrix * gl_Normal));\n"
        "LightIntensity  = dot(normalize(LightPos - ECposition), tnorm);\n"
        "LightIntensity *= 1.5;\n"
        "gl_Position     = ftransform();\n"
        "}\n"
    );
    std::string fSource(
        "varying float LightIntensity; \n"
        "varying vec3  MCposition;\n"

        "uniform sampler3D Noise;\n"
        "uniform float NoiseScale;\n"

        "void main()\n"
        "{\n"
        "vec4  noisevec  = texture3D(Noise, NoiseScale * MCposition);\n"
        "float intensity = min(1.0, noisevec[3] * 18.0);\n"
        "vec3  color     = vec3(intensity * LightIntensity);\n"

        "gl_FragColor    = vec4(color, 1.0);\n"
        "}\n"

    );
    _loadShader( vSource, fSource );
}

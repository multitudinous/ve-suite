/*************** <auto-copyright.rb BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <fstream>
#include <iostream>
#include <string>

#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/shader/ShaderCreator.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>

#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/ShaderPtr.h>
#include <ves/open/xml/shader/Program.h>
#include <ves/open/xml/shader/TextureImage.h>
#include <ves/open/xml/shader/Uniform.h>

XERCES_CPP_NAMESPACE_USE

int main( int argc, char* argv[] )
{
    try
    {
        XMLPlatformUtils::Initialize();
    }
    catch(const XMLException &toCatch)
    {
        std::cerr << "Error during Xerces-c Initialization.\n"
            << "  Exception message:"
            << XMLString::transcode(toCatch.getMessage()) << std::endl;
        return false;
    }

    ves::open::xml::shader::Program program;
    program.SetProgramName( "testVEAFile" );
    
    ves::open::xml::shader::ShaderPtr vertexShader = new ves::open::xml::shader::Shader();
    vertexShader->SetShaderType( "Vertex" );

    std::ostringstream vertexSource;
    
    vertexSource << std::endl << "\t\tvarying vec3 lightPos;" << std::endl 
        << "\t\tvarying vec3 objPos;" << std::endl
        << "\t\tvarying vec3 eyePos;" << std::endl
        << "\t\tvarying vec3 normal;" << std::endl
        << "\t\tvarying vec2 texCoord;" << std::endl
        << "\t\tvoid main()" << std::endl
        << "\t\t{" << std::endl
        << "\t\tgl_Position=ftransform();" << std::endl
        << "\t\tobjPos=gl_Vertex.xyz;" << std::endl
        << "\t\teyePos=vec3(gl_ModelViewMatrix*gl_Vertex);" << std::endl
        << "\t\tnormal=vec3(gl_NormalMatrix*gl_Normal);" << std::endl
        << "\t\tlightPos=gl_LightSource[0].position.xyz;" << std::endl
        << "\t\ttexCoord=gl_MultiTexCoord0.xy;" << std::endl
        << "\t\t//texCoord=gl_Vertex.xy;" << std::endl
        << "\t\t}" << std::endl;

    vertexShader->SetShaderSource( vertexSource.str() );
    
    program.SetVertexShader( vertexShader );
    
    ves::open::xml::shader::ShaderPtr fragmentShader  = new ves::open::xml::shader::Shader();
    fragmentShader->SetShaderType( "Fragment" );
    std::ostringstream fragmentSource;
    fragmentSource << std::endl << "\t\tuniform vec3 ambientMaterial;" << std::endl
        << "\t\tuniform vec3 diffuseMaterial;" << std::endl
        << "\t\tuniform vec3 specularMaterial;" << std::endl
        << "\t\tuniform float specularPower;" << std::endl
    
        << "\t\tuniform sampler2D baseMap;" << std::endl
    
        << "\t\tvarying vec3 lightPos;" << std::endl
        << "\t\tvarying vec3 objPos;" << std::endl
        << "\t\tvarying vec3 eyePos;" << std::endl
        << "\t\tvarying vec3 normal;" << std::endl
        << "\t\tvarying vec2 texCoord;" << std::endl
    
        << "\t\tvoid main()" << std::endl
        << "\t\t{" << std::endl
        << "\t\tvec3 N=normalize(normal);" << std::endl
        << "\t\tvec3 L=normalize(lightPos);" << std::endl
        << "\t\tfloat NDotL=max(dot(N,L),0.0); " << std::endl
        
        << "\t\tvec3 V=normalize(eyePos);" << std::endl
        << "\t\tvec3 R=reflect(V,N);" << std::endl
        << "\t\tfloat RDotL=max(dot(R,L),0.0);" << std::endl
        
        << "\t\tvec3 baseColor=vec3(texture2D(baseMap,texCoord));" << std::endl
        
        << "\t\tvec3 TotalAmbient=gl_LightSource[0].ambient.rgb*gl_FrontMaterial.ambient.rgb*baseColor; " << std::endl
        << "\t\tvec3 TotalDiffuse=gl_LightSource[0].diffuse.rgb*gl_FrontMaterial.diffuse.rgb*baseColor*NDotL; " << std::endl
        << "\t\tvec3 TotalSpecular=gl_LightSource[0].specular.rgb*gl_FrontMaterial.specular.rgb*pow(RDotL,specularPower);" << std::endl
        
        << "\t\tgl_FragColor=vec4(TotalAmbient+TotalDiffuse+TotalSpecular,1.0);  " << std::endl
        << "\t\t}" << std::endl;
    
    fragmentShader->SetShaderSource( fragmentSource.str() );

    {
        ves::open::xml::shader::Uniform uniformFloat;
        uniformFloat.SetType( "Float" );
        uniformFloat.SetSize( 1 );
        uniformFloat.SetName( "specularPower" );
        std::vector< float > vals;
        vals.push_back( 10.0 );
        uniformFloat.SetValues( vals );
        fragmentShader->AddUniform( uniformFloat );
    }
    
    {
        ves::open::xml::shader::Uniform uniformSampler;
        uniformSampler.SetType( "Sampler" );
        uniformSampler.SetSize( 1 );
        uniformSampler.SetName( "baseMap" );
        uniformSampler.SetTextureUnit( 0 );
        fragmentShader->AddUniform( uniformSampler );
    }
    
    {
        ves::open::xml::shader::TextureImage textureImage;
        textureImage.SetTextureImageType( "2D" );
        textureImage.SetDimension( 1 );
        textureImage.SetImageFile( "temp.jpg", "FRONT" );
        textureImage.SetTextureUnit( 0 );
        textureImage.SetWrapMode( "S", "Repeat" );
        textureImage.SetWrapMode( "T", "Repeat" );
        textureImage.SetWrapMode( "R", "Repeat" );
        textureImage.SetFilterMode( "MIN", "Linear" );
        textureImage.SetFilterMode( "MAG", "Linear" );
        fragmentShader->AddTextureImage( textureImage );
    }

    program.SetFragmentShader( fragmentShader );

    std::vector< std::pair< ves::open::xml::XMLObject*, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObject*, std::string >( 
        &program, "Program" ) );
    
    ///Initialize VE-Open
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new ves::open::xml::XMLCreator() );
    ves::open::xml::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new ves::open::xml::shader::ShaderCreator() );

    ves::open::xml::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    std::string filename( "test.vea" );
    netowrkWriter.WriteXMLDocument( nodes, filename, "Command" );
    
    return 0;
}


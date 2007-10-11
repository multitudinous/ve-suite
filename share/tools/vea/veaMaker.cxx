/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 * Date modified: $Date: 2007-08-24 11:53:30 -0500 (Fri, 24 Aug 2007) $
 * Version:       $Rev: 8827 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <fstream>
#include <iostream>
#include <string>

#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/Shader/ShaderCreator.h>

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

    VE_XML::VE_Shader::Program program;
    program.SetProgramName( "testVEAFile" );
    
    VE_XML::VE_Shader::ShaderPtr vertexShader = new VE_XML::VE_Shader::Shader();
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
    
    VE_XML::VE_Shader::ShaderPtr fragmentShader  = new VE_XML::VE_Shader::Shader();
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
        VE_XML::VE_Shader::Uniform uniformFloat;
        uniformFloat.SetType( "Float" );
        uniformFloat.SetSize( 1 );
        uniformFloat.SetName( "specularPower" );
        std::vector< float > vals;
        vals.push_back( 10.0 );
        uniformFloat.SetValues( vals );
        fragmentShader->AddUniform( uniformFloat );
    }
    
    {
        VE_XML::VE_Shader::Uniform uniformSampler;
        uniformSampler.SetType( "Sampler" );
        uniformSampler.SetSize( 1 );
        uniformSampler.SetName( "baseMap" );
        uniformSampler.SetTextureUnit( 0 );
        fragmentShader->AddUniform( uniformSampler );
    }
    
    {
        VE_XML::VE_Shader::TextureImage textureImage;
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

    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &program, "Program" ) );
    
    ///Initialize VE-Open
    VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "XML", new VE_XML::XMLCreator() );
    VE_XML::XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new VE_XML::VE_Shader::ShaderCreator() );

    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    std::string filename( "test.vea" );
    netowrkWriter.WriteXMLDocument( nodes, filename, "Command" );
    
    return 0;
}


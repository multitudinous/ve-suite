/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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

#include <ves/xplorer/scenegraph/util/ShaderHelper.h>
#include <ves/xplorer/scenegraph/util/PerlinNoiseTexture.h>
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Shader>
#include <osg/Uniform>
#include <osg/Texture>
#include <osg/Texture1D>
#include <osg/Texture2D>
#include <osg/Texture3D>
#include <osg/TextureCubeMap>
#include <osg/TexGen>
#include <osgDB/ReadFile>
#elif _PEFORMER
#endif

#include <iostream>
#include <sstream>
#include <ves/open/xml/shader/Shader.h>
#include <ves/open/xml/shader/Program.h>
#include <ves/open/xml/shader/Uniform.h>
#include <ves/open/xml/shader/TextureImage.h>
using namespace VE_XML::VE_Shader;
using namespace VE_SceneGraph::Utilities;
//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
ShaderHelper::ShaderHelper()
{
}

///////////////////////////////////////////////////
ShaderHelper::ShaderHelper(const ShaderHelper& rhs)
{
   for(size_t i = 0; i < rhs._vertexUniformNames.size(); i++)
   {
      _vertexUniformNames.push_back(rhs._vertexUniformNames.at(i));
   }
   for(size_t i = 0; i < rhs._fragmentUniformNames.size(); i++)
   {
      _fragmentUniformNames.push_back(rhs._fragmentUniformNames.at(i));
   }
#ifdef _OSG
   if(rhs._vshader.valid())
      _vshader = new osg::Shader(*rhs._vshader.get());
   
   if(rhs._fshader.valid())
      _fshader = new osg::Shader(*rhs._fshader.get());
   
   _glslProgram = new osg::Program(*_glslProgram.get());
   _ss = new osg::StateSet(*rhs._ss);
#elif _PERFORMER
#endif
}
//////////////////////////////////////////////////////////////
ShaderHelper& ShaderHelper::operator=(const ShaderHelper& rhs)
{
   if(this != &rhs)
   {
      _vertexUniformNames.clear();
      _fragmentUniformNames.clear();
      for(size_t i = 0; i < rhs._vertexUniformNames.size(); i++)
      {
         _vertexUniformNames.push_back(rhs._vertexUniformNames.at(i));
      }
      for(size_t i = 0; i < rhs._fragmentUniformNames.size(); i++)
      {
         _fragmentUniformNames.push_back(rhs._fragmentUniformNames.at(i));
      }
#ifdef _OSG
      
      if(rhs._vshader.valid())
         _vshader = rhs._vshader;

      if(rhs._fshader.valid())
         _fshader = rhs._fshader;
     
      _glslProgram = rhs._glslProgram;
      _ss = rhs._ss;
#elif _PERFORMER
#endif
   }
   return *this;
}
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
ShaderHelper::~ShaderHelper()
{
   _vertexUniformNames.clear();
   _fragmentUniformNames.clear();
}
#ifdef _OSG
/////////////////////////////////////////////////////
void ShaderHelper::SetStateSet(osg::StateSet* shader)
{
   _ss = shader;
}
#elif _PERFORMER
#endif
////////////////////////////////////////////
void ShaderHelper::LoadTransparencyProgram()
{
#ifdef _OSG
   VE_XML::VE_Shader::Shader* vertShader = new VE_XML::VE_Shader::Shader();
   vertShader->SetShaderType("Vertex");
   std::string vertexSource("	varying vec3 N;\n"
		                    "varying vec3 I;\n"
	                       "varying vec4 currentColor;\n"

		                     "void main()\n"
		                     "{\n"
			                  "vec4 P=gl_ModelViewMatrix*gl_Vertex;\n"
			                  "I=P.xyz;\n"
			                  "N=gl_NormalMatrix*gl_Normal;\n"
                          "currentColor=gl_Color;\n"
			                  "gl_Position=ftransform();\n"
		                     "}\n");
   vertShader->SetShaderSource(vertexSource);
   
   VE_XML::VE_Shader::Shader* fragShader = new VE_XML::VE_Shader::Shader();
   fragShader->SetShaderType("Fragment");
   std::string fragmentSource("varying vec3 N;\n"
		                     " varying vec3 I;\n"
                           "varying vec4 currentColor;\n"	
                           "void main()\n"
                           "{\n"
    			                   "float opac=dot(normalize(-N),normalize(-I));\n"
    			                   "opac=abs(opac);\n"
    			                   "opac=1.0-pow(opac,0.8);\n"
   			                   "vec4 Cs=currentColor;\n"
    			                   "gl_FragColor=opac*Cs;\n"
		                      "}\n"	   
                            );
   fragShader->SetShaderSource(fragmentSource);

   VE_XML::VE_Shader::Program* glslProgram = new VE_XML::VE_Shader::Program();
   glslProgram->SetProgramName("Dataset Transparency");
   glslProgram->SetVertexShader(vertShader);
   glslProgram->SetFragmentShader(fragShader);
   LoadGLSLProgram(glslProgram);
#elif _PERFORMER
#endif
}
///////////////////////////////////////////////////////////////////
void ShaderHelper::LoadGLSLProgram(VE_XML::VE_Shader::Program* glslProgram)
{
#ifdef _OSG
   //std::cout<<"Loading GLSLProgram: "<<glslProgram->GetProgramName()<<std::endl;
   if(!_ss.valid())
   {
      _ss = new osg::StateSet();
   }
   else
   {
      _ss->clear();
   }

   if(!_glslProgram.valid())
   {
      _glslProgram = new osg::Program();
   }
   _glslProgram->setName(glslProgram->GetProgramName());

   if(glslProgram->GetFragmentShader())
   {
      _createGLSLShader(glslProgram->GetFragmentShader());
   }
   if(glslProgram->GetVertexShader())
   {
      _createGLSLShader(glslProgram->GetVertexShader());
   }
   ///two-sided lighting hack until gl_FrontFacing works in glsl...
   ///only works if the shader implements it though...
   _ss->setMode(GL_VERTEX_PROGRAM_TWO_SIDE,osg::StateAttribute::ON);
#elif _PERFORMER
   std::cout<<"Not implemented for Performer yet!!!"<<std::endl;
#endif
   _attachGLSLProgramToStateSet();
}
///////////////////////////////////////////////////////////////
void ShaderHelper::_createGLSLShader(VE_XML::VE_Shader::ShaderPtr shader)
{
   if(shader->GetShaderSource().empty())
   {
      std::cout<<"Couldn't load shader from inline source because the source is empty!"<<std::endl;
      return;
   }
   _extractUniformsFromShader(shader);
#ifdef _OSG
   if(shader->GetShaderType() == std::string("Fragment")){
      if(!_fshader)
      {
         _fshader = new osg::Shader(osg::Shader::FRAGMENT,shader->GetShaderSource());
      }else{
         _fshader->setShaderSource(shader->GetShaderSource());
      }
      _glslProgram->addShader(_fshader.get());
   }else if(shader->GetShaderType() == std::string("Vertex")){
      if(!_vshader)
      {
         _vshader = new osg::Shader(osg::Shader::VERTEX,shader->GetShaderSource());
      }else{
         _vshader->setShaderSource(shader->GetShaderSource());
      }
      _glslProgram->addShader(_vshader.get());
   }
#elif _PERFORMER
   std::cout<<"Not implemented for Performer yet!!!"<<std::endl;
#endif
}
///////////////////////////////////////////////////////////////////////////////////
void ShaderHelper::_extractTextureFromShader(VE_XML::VE_Shader::TextureImage textureImage)
{
#ifdef _OSG
   //create the image
   unsigned int tUnit = textureImage.GetTextureUnit();
   unsigned int dimension = textureImage.GetDimension();

   //std::cout<<"Reading image file: "<<std::endl;
   osg::ref_ptr<osg::Image> textureImageData = osgDB::readImageFile(textureImage.GetImageFile());
   //std::cout<<"Read image file: "<<std::endl;
   osg::ref_ptr<osg::Texture> genericTexture;
   std::string textureType("");
   textureImage.GetType(textureType);

   //std::cout<<"Extracting: "<<textureType<<std::endl;
   if(textureType == "1D" )
   {
      osg::ref_ptr<osg::Texture1D> texture1D = new osg::Texture1D();
      //we need to set the wrapping and filters still!!!!
      texture1D->setImage(textureImageData.get());

      //Set the texture unit on the state set
      genericTexture = texture1D.get();
   }
   else if(textureType == "2D")
   {
      osg::ref_ptr<osg::Texture2D> texture2D = new osg::Texture2D();
      //we need to set the wrapping and filters still!!!!
      texture2D->setImage(textureImageData.get());
      genericTexture = texture2D.get();
   }
   else if(textureType == "3D")
   {
      osg::ref_ptr<osg::Texture3D> texture3D = new osg::Texture3D();
      //we need to set the wrapping and filters still!!!!
      texture3D->setImage(textureImageData.get());
      genericTexture = texture3D.get();
   }
   else if(textureType == "Cube")
   {
      //std::cout<<"Cube map"<<std::endl;
      osg::ref_ptr<osg::TextureCubeMap> textureCubeMap = new osg::TextureCubeMap();
      textureCubeMap->setImage(osg::TextureCubeMap::POSITIVE_X, osgDB::readImageFile(textureImage.GetImageFile("Positive X")));
      textureCubeMap->setImage(osg::TextureCubeMap::NEGATIVE_X, osgDB::readImageFile(textureImage.GetImageFile("Negative X")));
      textureCubeMap->setImage(osg::TextureCubeMap::POSITIVE_Y, osgDB::readImageFile(textureImage.GetImageFile("Positive Y")));
      textureCubeMap->setImage(osg::TextureCubeMap::NEGATIVE_Y, osgDB::readImageFile(textureImage.GetImageFile("Negative Y")));
      textureCubeMap->setImage(osg::TextureCubeMap::POSITIVE_Z, osgDB::readImageFile(textureImage.GetImageFile("Positive Z")));
      textureCubeMap->setImage(osg::TextureCubeMap::NEGATIVE_Z, osgDB::readImageFile(textureImage.GetImageFile("Negative Z")));     
      
      //this should be adjustable parameter in the TextureImage interface
      osg::ref_ptr<osg::TexGen> textureCoordGeneration = new osg::TexGen;
      textureCoordGeneration->setMode(osg::TexGen::REFLECTION_MAP);
      _ss->setTextureAttributeAndModes(tUnit, textureCoordGeneration.get(),osg::StateAttribute::ON);
      
      genericTexture = textureCubeMap.get();
   }
   else if( textureType == "Perlin Noise")
   {
       PerlinNoiseTexture perlinNoise(64,64,64);
       osg::ref_ptr<osg::Texture3D> texture3D = 
			dynamic_cast<osg::Texture3D*>(perlinNoise.GetNoiseTexture());
      genericTexture = texture3D.get(); 
   }

   if(genericTexture.valid())
   {
      //std::cout<<"Setting up texture parameters for shader!"<<std::endl;
      std::string minFilter;
      textureImage.GetFilterMode("Minification",minFilter);
      
      std::string magFilter;
      textureImage.GetFilterMode("Magnification",magFilter);
      
      osg::Texture::FilterMode magMode = osg::Texture::LINEAR;
      osg::Texture::FilterMode minMode = osg::Texture::LINEAR;

      if(minFilter == "Nearest")
      {
         minMode =  osg::Texture::NEAREST;
      }

      if(magFilter == "Nearest")
      {
         magMode = osg::Texture::NEAREST;
      }
      //set the min/mag filters
      genericTexture->setFilter(osg::Texture::MAG_FILTER,magMode);
      genericTexture->setFilter(osg::Texture::MIN_FILTER,minMode);

      //set the wrap modes
      std::string sWrap;
      std::string tWrap;
      std::string rWrap;

      osg::Texture::WrapMode swrapMode = osg::Texture::CLAMP;

      if(textureImage.GetWrapMode("Wrap S",sWrap))
         _setWrapOnTexture(genericTexture.get(),osg::Texture::WRAP_S,sWrap);
      
      if(dimension != 1)
      {
         if(textureImage.GetWrapMode("Wrap T",tWrap))
            _setWrapOnTexture(genericTexture.get(),osg::Texture::WRAP_T,tWrap);
      }
      
      if(dimension == 3)
      {
         if(textureImage.GetWrapMode("Wrap R",rWrap))
            _setWrapOnTexture(genericTexture.get(),osg::Texture::WRAP_R,rWrap);
      }

      //std::cout<<"Is this the problem??"<<std::endl;
      //set the texture to the state set
      _ss->setTextureAttributeAndModes(tUnit,genericTexture.get(),osg::StateAttribute::ON);

      if(dimension == 1)
         _ss->setTextureMode(tUnit,GL_TEXTURE_1D,osg::StateAttribute::ON);
      
      if(dimension == 2)
         _ss->setTextureMode(tUnit,GL_TEXTURE_2D,osg::StateAttribute::ON);
      
      if(dimension == 3)
      {
         if(textureType == "3D" )
         {
            //std::cout<<"Probably"<<std::endl;
            _ss->setTextureMode(tUnit,GL_TEXTURE_3D,osg::StateAttribute::ON);
         }
      }
   }
#endif
}
///////////////////////////////////////////////////////////////////////
#ifdef _OSG
void ShaderHelper::_setWrapOnTexture(osg::Texture* texture,
                                 osg::Texture::WrapParameter param,
                                 std::string wrapMode)
{
   if(wrapMode == "Clamp to Border")
   {
      texture->setWrap(param,osg::Texture::CLAMP_TO_BORDER);
   }
   else if(wrapMode == "Clamp to Edge")
   {
      texture->setWrap(param,osg::Texture::CLAMP_TO_EDGE);
   }
   else if(wrapMode == "Repeat")
   {
      texture->setWrap(param,osg::Texture::REPEAT);
   }
   else if(wrapMode == "Mirror")
   {
      texture->setWrap(param,osg::Texture::MIRROR);
   }
   else if(wrapMode == "Clamp")
   {
      texture->setWrap(param,osg::Texture::CLAMP);
   }
}
#endif
/////////////////////////////////////////////////////////////////
void ShaderHelper::UpdateUniform(VE_XML::VE_Shader::Uniform* uniformData)
{
#ifdef _OSG
   std::string uniformName("");
   std::string uniformType("");
   unsigned int uniformSize = 0;
   std::vector<float> uniformValues;

   uniformName = uniformData->GetName();
   uniformType = uniformData->GetType();
   uniformSize = uniformData->GetSize();
   uniformValues = uniformData->GetValues();

   osg::ref_ptr<osg::Uniform> uniformToUpdate = _ss->getUniform(uniformName);
   if(uniformToUpdate.valid()){
      if(uniformType == "Float"){
         if(uniformSize == 1)
         {
            uniformToUpdate->set(uniformValues.at(0));
         }
         else if(uniformSize == 2)
         {
            uniformToUpdate->set(osg::Vec2f(uniformValues.at(0),uniformValues.at(0)));
         }
         else if(uniformSize == 3)
         {
            uniformToUpdate->set(osg::Vec3f(uniformValues.at(0),
                                            uniformValues.at(1),
                                            uniformValues.at(2)));
         }
         else if(uniformSize == 4)
         {
            uniformToUpdate->set(osg::Vec4f(uniformValues.at(0),
                                            uniformValues.at(1),
                                            uniformValues.at(2),
                                            uniformValues.at(3)));
         }
      }else if(uniformType == "Int"){
         if(uniformSize == 1)
         {
            uniformToUpdate->set(static_cast<int>(uniformValues.at(0)));
         }
         else if(uniformSize == 2)
         {
            uniformToUpdate->set(osg::Vec2(static_cast<int>(uniformValues.at(0)),
                                           static_cast<int>(uniformValues.at(1))));
         }
         else if(uniformSize == 3)
         {
            uniformToUpdate->set(osg::Vec3(static_cast<int>(uniformValues.at(0)),
                                           static_cast<int>(uniformValues.at(1)),
                                           static_cast<int>(uniformValues.at(2))));
         } 
         else if(uniformSize == 4)
         {
            uniformToUpdate->set(osg::Vec4(static_cast<int>(uniformValues.at(0)),
                                           static_cast<int>(uniformValues.at(1)),
                                           static_cast<int>(uniformValues.at(2)),
                                           static_cast<int>(uniformValues.at(3))));
         }
      }else if(uniformType == "Bool"){
         std::vector<bool> boolValues;
         for(size_t i =0; i <uniformSize; i++)
         {
            if(uniformValues.at(i) == 0.0)
            {
               boolValues.push_back(false);
            }
            else
            {
               boolValues.push_back(true);
            }
         }
         if(uniformSize == 1)
         {
            uniformToUpdate->set(boolValues.at(0));
         }
         else if(uniformSize == 2)
         {
            uniformToUpdate->set(osg::Vec2(boolValues.at(0),
                                           boolValues.at(1)));
         }
         else if(uniformSize == 3)
         {
            uniformToUpdate->set(osg::Vec3(boolValues.at(0),
                                           boolValues.at(1),
                                           boolValues.at(2)));
         }
         else if(uniformSize == 4)
         {
            uniformToUpdate->set(osg::Vec4(boolValues.at(0),
                                           boolValues.at(1),
                                           boolValues.at(2),
                                           boolValues.at(3)));
         }
      }else if(uniformType == "Sampler"){
         std::cout<<"Updating Sampler!!"<<std::endl;
      }
   }
#elif _PERFORMER
      std::cout<<"Not implemented for Performer yet!!!"<<std::endl;
#endif
}
////////////////////////////////////////////////////////////////////////
void ShaderHelper::_extractUniformsFromShader(VE_XML::VE_Shader::ShaderPtr shader)
{
   size_t nUniforms = shader->GetNumberOfUniforms();
   VE_XML::VE_Shader::Uniform* uniformData = 0;
   std::string uniformName("");
   std::string uniformType("");
   unsigned int uniformSize = 0;
   std::vector<float> uniformValues;
   for(size_t i = 0; i < nUniforms; i++)
   {
      uniformData = &shader->GetUniform(i);
      uniformName = uniformData->GetName();
      uniformType = uniformData->GetType();
      uniformSize = uniformData->GetSize();
      uniformValues = uniformData->GetValues();
#ifdef _OSG
      if(uniformType == "Float"){
         if(uniformSize == 1)
         {
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),uniformValues.at(0)));
         }else if(uniformSize == 2){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec2f(uniformValues.at(0),uniformValues.at(0))));
         }else if(uniformSize == 3){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec3f(uniformValues.at(0),
                                                             uniformValues.at(1),
                                                             uniformValues.at(2))));
         }else if(uniformSize == 4){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec4f(uniformValues.at(0),
                                                             uniformValues.at(1),
                                                             uniformValues.at(2),
                                                             uniformValues.at(3))));
         }
      }else if(uniformType == "Int"){
         if(uniformSize == 1)
         {
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),static_cast<int>(uniformValues.at(0))));
         }else if(uniformSize == 2){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec2(static_cast<int>(uniformValues.at(0)),
                                                                     static_cast<int>(uniformValues.at(1)))));
         }else if(uniformSize == 3){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec3(static_cast<int>(uniformValues.at(0)),
                                                             static_cast<int>(uniformValues.at(1)),
                                                             static_cast<int>(uniformValues.at(2)))));
         }else if(uniformSize == 4){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec4(static_cast<int>(uniformValues.at(0)),
                                                                     static_cast<int>(uniformValues.at(1)),
                                                                     static_cast<int>(uniformValues.at(2)),
                                                                     static_cast<int>(uniformValues.at(3)))));
         }
      }else if(uniformType == "Bool"){
         std::vector<bool> boolValues;
         for(size_t i =0; i <uniformSize; i++)
         {
            if(uniformValues.at(i) == 0.0)
            {
               boolValues.push_back(false);
            }
            else
            {
               boolValues.push_back(true);
            }
         }
         if(uniformSize == 1)
         {
            _ss->addUniform(new osg::Uniform(uniformName.c_str(), boolValues.at(0)));
         }else if(uniformSize == 2){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec2(boolValues.at(0),
                                                                    boolValues.at(1))));
         }else if(uniformSize == 3){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec3(boolValues.at(0),
                                                                    boolValues.at(1),
                                                                    boolValues.at(2))));
         }else if(uniformSize == 4){
            _ss->addUniform(new osg::Uniform(uniformName.c_str(),osg::Vec4(boolValues.at(0),
                                                                     boolValues.at(1),
                                                                     boolValues.at(2),
                                                                     boolValues.at(3))));
         }
      }else if(uniformType == "Sampler"){
         std::cout<<"Extracting Sampler!!"<<std::endl;
         std::cout<<"Unit: "<<uniformData->GetTextureUnit()<<std::endl;
         
         _extractTextureFromShader(shader->GetTextureImage(uniformData->GetTextureUnit()));
         std::cout<<"---Done---"<<std::endl;
      }
      
#elif _PERFORMER
      std::cout<<"Not implemented for Performer yet!!!"<<std::endl;
#endif
   }
}
/////////////////////////////////////////////////////////////////
void ShaderHelper::_attachGLSLProgramToStateSet(bool override)
{
#ifdef _OSG
   if(_ss.valid()){
      if(_glslProgram.valid()){
         //_glslProgram->setName(_name.c_str());
         if(override){
            _ss->setAttributeAndModes(_glslProgram.get(),osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
         }else{
             _ss->setAttributeAndModes(_glslProgram.get(),osg::StateAttribute::ON);
         }
      }
   }
#elif _PERFORMER
   std::cout<<"Not implemented for Performer yet!!!"<<std::endl;
#endif
}
#ifdef _OSG
//////////////////////////////////////////////////////////////
osg::StateSet* ShaderHelper::GetProgramStateSet()
{
   if(_ss.valid())
   {
      return _ss.get();
   }
   else
   {
      std::cout<<"State set is invalid for shader!!"<<std::endl;
   }
   return 0;
}
#elif _PERFORMER
#endif

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
 * File:          $RCSfile: OSGShaderMaterial.cxx,v $
 * Date modified: $Date: 2005-11-03 17:22:13 -0600 (Thu, 03 Nov 2005) $
 * Version:       $Rev: 3259 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_SceneGraph/Utilities/ShaderHelper.h"
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Shader>
#include <osg/Uniform>
#include <osg/Texture>
#elif _PEFORMER
#endif

#include <iostream>
#include <sstream>
#include "VE_Open/XML/Shader/Shader.h"
#include "VE_Open/XML/Shader/Program.h"
#include "VE_Open/XML/Shader/Uniform.h"
using namespace VE_Shader;
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
///////////////////////////////////////////////////////////////////
void ShaderHelper::LoadGLSLProgram(VE_Shader::Program* glslProgram)
{
#ifdef _OSG
   std::cout<<"Loading GLSLProgram: "<<glslProgram->GetProgramName()<<std::endl;
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
   
#elif _PERFORMER
   std::cout<<"Not implemented for Performer yet!!!"<<std::endl;
#endif
   _attachGLSLProgramToStateSet();
}
///////////////////////////////////////////////////////////////
void ShaderHelper::_createGLSLShader(VE_Shader::Shader* shader)
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
////////////////////////////////////////////////////////////////////////
void ShaderHelper::_extractUniformsFromShader(VE_Shader::Shader* shader)
{
   size_t nUniforms = shader->GetNumberOfUniforms();
   VE_Shader::Uniform* uniformData = 0;
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
            if(uniformValues.at(i) == 0)
            {
               boolValues.push_back(false);
            }
            else
            {
               boolValues.push_back(false);
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

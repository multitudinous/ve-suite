/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Shader>
#include <iostream>
#include <sstream>
#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>


#include "VE_Xplorer/TextureBased/cfdOSGShaderManager.h"
using namespace VE_TextureBased;
//////////////////////////////////////////
//Constructors                          //
//////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager()
{
   _shaderDirectory = '\0';
   _bounds = 0;
   _tUnit =0;
   _useGLSL = true;
}
///////////////////////////////////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager(const cfdOSGShaderManager& sm)
{
   _useGLSL = sm._useGLSL;
   _ss = sm._ss;
   _fshader = new osg::Shader(*sm._fshader);
   _vshader = new osg::Shader(*sm._vshader);
   _programs = sm._programs;
   SetShaderDirectory(sm._shaderDirectory);
}
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
cfdOSGShaderManager::~cfdOSGShaderManager()
{
   if(!_shaderDirectory.empty())
   {
      _shaderDirectory.clear();
   }
   if(_bounds){
      delete [] _bounds;
      _bounds = 0;
   }
   _programs.clear();
}
///////////////////////////////////////////
void cfdOSGShaderManager::UseCG(bool useCG)
{
   _useGLSL = (useCG)?false:true;
}
////////////////////////////////////////////////////////////////////////////////////////////
osg::Shader* cfdOSGShaderManager::_createGLSLShaderFromInline(const std::string inlineSource,
                                                           bool isFrag)
{
   if(isFrag)
   {
      //_fshader = new osg::Shader(osg::Shader::FRAGMENT,inlineSource);
      //_fshader->loadShaderSourceFromFile(filename.c_str());
      return new osg::Shader(osg::Shader::FRAGMENT,inlineSource);//_fshader.get();
   }
   else
   {
      //_vshader = new osg::Shader(osg::Shader::VERTEX,inlineSource);
      //_vshader->loadShaderSourceFromFile(filename.c_str());
      return new osg::Shader(osg::Shader::VERTEX,inlineSource);//_vshader.get();
   }
   return 0;
}
//////////////////////////////////////////////////////////////////////////////////////
osg::Shader* cfdOSGShaderManager::_createGLSLShaderFromFile(const std::string filename,
                                                           bool isFrag)
{
   if(isFrag)
   {
      _fshader = new osg::Shader(osg::Shader::FRAGMENT);
      _fshader->loadShaderSourceFromFile(filename.c_str());
      return _fshader.get();
   }
   else
   {
      _vshader = new osg::Shader(osg::Shader::VERTEX);
      _vshader->loadShaderSourceFromFile(filename.c_str());
      return _vshader.get();
   }
   return 0;
}
//////////////////////////////////////////////////////////////////
void cfdOSGShaderManager::SetActiveShaderProgram(std::string name)
{
   //Probably don't need this memeber yet
   //_activeShaderProgram = name;
   if(_ss.valid())
   {
      if(_ss->getAttribute(osg::StateAttribute::PROGRAM))
      {
         _ss->removeAttribute(osg::StateAttribute::PROGRAM);
      }
      
      if(_programs[name].valid())
	   {
         _ss->setAttributeAndModes(_programs[name].get(),osg::StateAttribute::ON);
      }
   }
}
///////////////////////////////////////////////////////////////////////////////////////////////////
void cfdOSGShaderManager::AddShaderProgram(std::string name,osg::ref_ptr<osg::Program> glslProgram)
{
   _programs[name] = glslProgram;
   _programs[name]->setName(name);
   ///check here if vector vis has problems
   SetActiveShaderProgram(name);
   //_setupGLSLShaderProgram(_programs[name].get(),name);
}
/////////////////////////////////////////////////////////////////////////
void cfdOSGShaderManager::_setupGLSLShaderProgram(osg::Program* glslProgram,
                                                  const std::string pgName,
                                                  bool override)
{
   ///This is an old function!!!!!
   std::cout<<"Depricated function!!!"<<std::endl;
   std::cout<<"cfdOSGShaderManager::_setupGLSLShaderProgram!!!!!"<<std::endl;
   std::cout<<"Use cfdOSGShaderManager::SetActiveShaderProgram instead!!"<<std::endl;
   if(_ss.valid())
   {
      if(_ss->getAttribute(osg::StateAttribute::PROGRAM))
      {
         _ss->removeAttribute(osg::StateAttribute::PROGRAM);
      }
      if(glslProgram)
	   {
         glslProgram->setName(pgName.c_str());
         if(override)
         {
            _ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
         }
		   else
         {
            _ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON);
         }
      }
   }
}
/////////////////////////////////////////////////////////////////////////
/*void cfdOSGShaderManager::_setupGLSLShaderProgram(osg::StateSet* ss,
                                                  osg::Program* glslProgram,
                                                  const std::string pgName,bool override)
{
   if(ss)
   {
      ///remove any old shader programs
      if(ss->getAttribute(osg::StateAttribute::PROGRAM)
	   {
         ss->removeAttribute(osg::StateAttribute::PROGRAM);
	   }
      if(glslProgram)
	   {
         glslProgram->setName(pgName.c_str());
         if(override)
         {
            ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
         }
		   else
         {
            ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON);
         }
      }
   }
}*/
//////////////////////////////////////////////////
void cfdOSGShaderManager::SetBounds(float* bounds)
{
   if(!_bounds){
      _bounds = new float[6];
   }
   _bounds[0] = bounds[0];
   _bounds[1] = bounds[1];
   _bounds[2] = bounds[2];
   _bounds[3] = bounds[3];
   _bounds[4] = bounds[4];
   _bounds[5] = bounds[5];

}
///////////////////////////////////////////////////////
osg::StateSet* cfdOSGShaderManager::GetShaderStateSet()
{
   if(_ss.valid()){
      return _ss.get();
   }
   return 0;
}
///////////////////////////////////////////////////////////
void cfdOSGShaderManager::SetShaderDirectory(std::string shadDir)
{
   _shaderDirectory = shadDir;
}
//////////////////////////////////////////////////////////
//equal operator                                        //
//////////////////////////////////////////////////////////
//not fully implemented!!!!!!!!!!!!!!!!!!!!!!!!!
cfdOSGShaderManager& cfdOSGShaderManager::operator=(const
	                                            cfdOSGShaderManager& sm)
{
   if(this != &sm){
      _ss = sm._ss;
      SetShaderDirectory(sm._shaderDirectory);
   }
   return *this;
}
#endif //_OSG
#endif

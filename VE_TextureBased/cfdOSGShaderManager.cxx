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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/StateSet>
#include <osg/Shader>
#include <iostream>
#include <strstream>
#include <sstream>
#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>


#include "VE_TextureBased/cfdOSGShaderManager.h"
using namespace VE_TextureBased;
//////////////////////////////////////////
//Constructors                          //
//////////////////////////////////////////
cfdOSGShaderManager::cfdOSGShaderManager()
{
   _shaderDirectory = 0;
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
   SetShaderDirectory(sm._shaderDirectory);
}
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
cfdOSGShaderManager::~cfdOSGShaderManager()
{
   if(_bounds){
      delete [] _bounds;
      _bounds = 0;
   }
}
///////////////////////////////////////////
void cfdOSGShaderManager::UseCG(bool useCG)
{
   _useGLSL = (useCG)?false:true;
}
//////////////////////////////////////////////////////////////////////////////////////
osg::Shader* cfdOSGShaderManager::_createGLSLShaderFromFile(const std::string filename,
                                                           bool isFrag)
{
   if(isFrag){
      _fshader = new osg::Shader(osg::Shader::FRAGMENT);
      _fshader->loadShaderSourceFromFile(filename.c_str());
      return _fshader.get();
   }else{
      _vshader = new osg::Shader(osg::Shader::VERTEX);
      _vshader->loadShaderSourceFromFile(filename.c_str());
      return _vshader.get();
   }
   return 0;
}
/////////////////////////////////////////////////////////////////////////
void cfdOSGShaderManager::_setupGLSLShaderProgram(osg::StateSet* ss,
                                             osg::Program* glslProgram,
                                             const std::string pgName,bool override)
{
   if(ss){
      if(glslProgram){
         glslProgram->setName(pgName.c_str());
         if(override){
            ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
         }else{
            ss->setAttributeAndModes(glslProgram,osg::StateAttribute::ON);
         }
      }
   }
}
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
void cfdOSGShaderManager::SetShaderDirectory(char* shadDir)
{
   if(_shaderDirectory){
      delete [] _shaderDirectory;
      _shaderDirectory = 0;
   }
   _shaderDirectory = new char[strlen(shadDir)+1];
   strcpy(_shaderDirectory,shadDir);
}

////////////////////////////////////////////////////////////////////
char* cfdOSGShaderManager::_createShaderPathForFile(char* shaderFile)
{
   char tempDirectory[1024];
   std::strstream directory;
   if(_shaderDirectory){
      //strcpy(tempDirectory,_shaderDirectory);
      directory<<_shaderDirectory;
   }else{
      //check vesuite home
      char* vesh = getenv("VE_SUITE_HOME");
      
      std::ostringstream vesuitehome;
      vesuitehome<<getenv("VE_SUITE_HOME");;
      boost::filesystem::path ves_pth(vesuitehome.str());
	   try
	   {
	      boost::filesystem::is_directory( ves_pth );
	   }
	   catch ( const std::exception& ex )
	   {
         std::cout << ex.what() << std::endl;
         std::cout<<"Couldn't locate VE_SUITE_HOME!!"<<std::endl;
         return 0;
	   }

      std::string glShadersDir("\\glsl_shaders\\");
      strcpy(tempDirectory,vesuitehome.str().c_str());
      strcat(tempDirectory,"\\VE_TextureBased\\glsl_shaders\\");

      //check if the path to TextureBased directory exists
      boost::filesystem::path devDir( tempDirectory );
	   try
	   {
         //development version
         boost::filesystem::is_directory( devDir );
         directory<<devDir.string();
	   }
	   catch ( const std::exception& ex )
	   {
         //must be running the install version
         std::cout << ex.what() << std::endl;
         boost::filesystem::path instDir( std::string(vesuitehome.str()+glShadersDir));
         try
         {
            boost::filesystem::is_directory( instDir );
            directory<<instDir.string();
         }
         catch( const std::exception& ex )
         {
            std::cout << ex.what() << std::endl;
            std::cout<<"Couldn't locate the glsl shader directory!!"<<std::endl;
            return 0;
         }
	   }
   }
   directory<<shaderFile<<'\0';
   return directory.str();
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

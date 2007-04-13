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
#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/TexEnv>
#include <osg/TexMat>
#include <osg/TexGen>
#include <osg/AlphaFunc>
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/cfdUpdateTextureCallback.h"
#include "VE_Xplorer/TextureBased/cfdOSGTransferShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdSimpleTextureCallback.h"
using namespace VE_TextureBased;
//the shader inline source
static const char* volumeTransferFragSource = {
   
   "uniform sampler1D mat1Func;\n" 
   "uniform sampler1D mat2Func;\n"
   "uniform sampler1D mat3Func;\n"
   "uniform sampler1D mat4Func;\n"
   "uniform sampler3D densityTexture;\n"
   "void main(void)\n"
   "{\n"
      "//get the density for each material\n"
      "vec4 density = texture3D(densityTexture,gl_TexCoord[0].xyz);\n"
   
      "vec4 red = texture1D(mat4Func,density.x);\n"
      "vec4 dye = vec4(red.x,0,0,red.a);\n"

      "vec4 ink = texture1D(mat1Func,density.y);\n"
   
      "vec4 hole = texture1D(mat2Func,density.z);\n"
      "ink = clamp((ink - hole),vec4(0,0,0,0),vec4(1,1,1,1));\n"

      "gl_FragColor = clamp(ink +dye,vec4(0,0,0,0),vec4(1,1,1,1));\n"
      "gl_FragColor.w *= gl_Color.a;\n"
   "}\n"
};
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::cfdOSGTransferShaderManager()
:cfdOSGShaderManager()
{
   _reinit = true;
   _useTM = false;
   _fieldSize[0] = 0;
   _fieldSize[1] = 0;
   _fieldSize[2] = 0;
   _tm = 0;
   //_utCbk = 0;
}
//////////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::cfdOSGTransferShaderManager(const
                               cfdOSGTransferShaderManager& sm)
:cfdOSGShaderManager(sm)
{
   _property = new osg::Texture3D(*(sm._property.get()));
   int nTransferFuncts = sm._transferFunctions.size();
   for(int i = 0; i < nTransferFuncts; i++){
      _transferFunctions.push_back(sm._transferFunctions.at(i));
   }
   _fieldSize[0] = sm._fieldSize[0];
   _fieldSize[1] = sm._fieldSize[1];
   _fieldSize[2] = sm._fieldSize[2];
   _reinit = sm._reinit;
   _texMat = sm._texMat;
   _useTM = sm._useTM;
   _tm = sm._tm;
   _utCbk = sm._utCbk;
}
///////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::~cfdOSGTransferShaderManager()
{
   if(_transferFunctions.size())
   {
      _transferFunctions.clear();
   }
}
////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::InitTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   _reinit = true;
}
///////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::UpdateTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   if(_utCbk.valid()){
      _utCbk->SetTextureManager(_tm);
   }
   _reinit = false;
}
/////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetTextureMatrix(osg::TexMat* tmat)
{
   _texMat = tmat;
   if(_ss.valid()){
      unsigned int ntf = _transferFunctions.size();
      _ss->setTextureAttributeAndModes(ntf,_texMat.get(),
                                   osg::StateAttribute::ON);
   }
}
///////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetPropertyTexture(osg::Texture3D* property)
{
   //_property = property;
}
////////////////////////////////////////
void cfdOSGTransferShaderManager::Init()
{
   _initTransferFunctions();
   _initPropertyTexture();
   if(!_ss.valid() && _reinit &&_property.valid()){
      _ss = new osg::StateSet();
      _ss->setDataVariance(osg::Object::DYNAMIC);
      _ss->setMode(GL_BLEND,osg::StateAttribute::ON);
      _ss->setAttributeAndModes(new osg::AlphaFunc(osg::AlphaFunc::GEQUAL,.1));
      _ss->setMode(GL_LIGHTING,osg::StateAttribute::OFF|osg::StateAttribute::OVERRIDE);
      osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
      bf->setFunction(osg::BlendFunc::SRC_ALPHA, 
                    osg::BlendFunc::ONE_MINUS_SRC_ALPHA);

      _ss->setAttributeAndModes(bf.get());
      _ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
      
      int nTransferFunctions = _transferFunctions.size();
      for(int i =0; i < nTransferFunctions; i++){
         _ss->setTextureAttributeAndModes(i,_transferFunctions.at(i).get(),
                                      osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE);
//         _ss->setTextureMode(i,GL_TEXTURE_1D,
//                          osg::StateAttribute::OVERRIDE|osg::StateAttribute::OFF);
//          _ss->setTextureMode(i,GL_TEXTURE_2D,
//                           osg::StateAttribute::OVERRIDE|osg::StateAttribute::OFF);


         _ss->setTextureMode(i,GL_TEXTURE_3D,
                          osg::StateAttribute::OVERRIDE|osg::StateAttribute::ON);
         _ss->setTextureMode(i,GL_TEXTURE_GEN_S,
                           osg::StateAttribute::ON);
         _ss->setTextureMode(i,GL_TEXTURE_GEN_T,
                           osg::StateAttribute::ON);
         _ss->setTextureMode(i,GL_TEXTURE_GEN_R,
                           osg::StateAttribute::ON);
     
      }
      
      _ss->setDataVariance(osg::Object::DYNAMIC);
      _tUnit = nTransferFunctions;
      _setupStateSetForGLSL();
   }
   _reinit = false;
}
/////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_setupStateSetForGLSL()
{
   std::cout<<"Using glsl..."<<std::endl;
   _ss->addUniform(new osg::Uniform("mat1Func",0));
   _ss->addUniform(new osg::Uniform("mat2Func",1));
   _ss->addUniform(new osg::Uniform("mat3Func",2));
   _ss->addUniform(new osg::Uniform("mat4Func",3));
   _ss->addUniform(new osg::Uniform("densityTexture",4));
   
   osg::ref_ptr<osg::Shader> vTransfers = _createGLSLShaderFromInline(volumeTransferFragSource,true);
   osg::ref_ptr<osg::Program> glslProgram = new osg::Program();
   glslProgram->addShader(vTransfers.get());
   AddShaderProgram("3D Texture Advection Transfer Functions",glslProgram); 
}
/////////////////////////////////////////////////////////////////
osg::Texture3D* cfdOSGTransferShaderManager::GetPropertyTexture()
{
   if(_property.valid()){
      return _property.get();
   }
   return 0;
}
//////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_initTransferFunctions()
{
   if(_transferFunctions.empty()){
      //create 4 transfer functions
      _createTransferFunction(); 
      _createTransferFunction(); 
      _createTransferFunction(); 
      _createTransferFunction(true);
   }
}
///////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_createTransferFunction(bool useGamma,
                                                    bool clearList)
{
   if(clearList)
   {
      if(_transferFunctions.size())
      {
         _transferFunctions.clear();
      }
   }
   GLint ga[256];
   GLfloat taoA = .10;
   for(unsigned int i = 0; i < 256; i++)
   {
     if(i < taoA*255)
     {
        ga[i] = 0;
     }
     else
     {
        ga[i] = 255.0*(i-/*255  */taoA*255)/(255.0-taoA*255);
     }
  }
   GLubyte* lutex = new GLubyte[256*256*4];
   //gamma table
   GLubyte gTable[256];
   double gamma = 2.4;
   //double y = 0;
   for (int i=0; i<256; i++) 
   {       
      double y = (double)(i)/255.0;   
      y = pow(y, 1.0/gamma);     
      gTable[i] = (int) floor(255.0 * y + 0.5);  
   }

   //diagonal is the old 1D transfer function
   //Probably should initialize the offdiagonals better but leaving for now
   for(unsigned int j = 0; j < 256; ++j)
   {
      if(useGamma)
      {
         for (int i = 0; i < 256; ++i)
         {
           if(i==j)
           {
              lutex[j*4*256 + i*4    ] = 
              lutex[j*4*256 + i*4 + 1] = 
              lutex[j*4*256 + i*4 + 2] = 
              lutex[j*4*256 + i*4 + 3] = (GLubyte)gTable[i];
           }
           else
           {
              lutex[j*4*256 + i*4    ] = 
              lutex[j*4*256 + i*4 + 1] = 
              lutex[j*4*256 + i*4 + 2] = 
              lutex[j*4*256 + i*4 + 3] = 0;
           }
           //std::cout<<lutex[j*4*256 + i]<<",";
         }
         //std::cout<<std::endl;
      }
      else
      {
         for (int i = 0; i < 256; i++)
         {
            if(i == j)
            {
               lutex[j*(256*4) + i*4    ] =
               lutex[j*(256*4) + i*4 + 1] = 
               lutex[j*(256*4) + i*4 + 2] = (GLubyte)gTable[i];
               lutex[j*(256*4) + i*4 + 3] = (GLubyte)ga[i];
            }
            else
            {
               lutex[j*(256*4) + i*4    ] =
               lutex[j*(256*4) + i*4 + 1] = 
               lutex[j*(256*4) + i*4 + 2] = 
               lutex[j*(256*4) + i*4 + 3] = 0;
            }
            //std::cout<<lutex[j*(256*4) + i*4]<<",";
         }
         //std::cout<<std::endl;
      }
   }

   osg::ref_ptr<osg::Image> imageField = new osg::Image();
   imageField->allocateImage(256,256,1,GL_RGBA,GL_UNSIGNED_BYTE);
   imageField->setImage(256,256,1,GL_RGBA,GL_RGBA,GL_UNSIGNED_BYTE,lutex,
                        osg::Image::USE_NEW_DELETE,1);
   imageField->setDataVariance(osg::Object::DYNAMIC);

   osg::ref_ptr<osg::Texture2D> trans = new osg::Texture2D;
   trans->setDataVariance(osg::Object::DYNAMIC);
   trans->setFilter(osg::Texture2D::MIN_FILTER,
                    osg::Texture2D::LINEAR);

   trans->setFilter(osg::Texture2D::MAG_FILTER,
                    osg::Texture2D::LINEAR);

   trans->setWrap(osg::Texture2D::WRAP_S,
                  osg::Texture2D::CLAMP);
   trans->setWrap(osg::Texture2D::WRAP_T,
                  osg::Texture2D::CLAMP);

   trans->setInternalFormat(GL_RGBA);
   trans->setImage(imageField.get());

   _transferFunctions.push_back(trans);
}
//////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetUseTextureManagerForProperty(bool tf)
{
   _useTM = tf;
}
///////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_initPropertyTexture()
{
   if(_fieldSize[0] && 
      _fieldSize[1] &&
      _fieldSize[2])
   { 
      unsigned int dataSize = _fieldSize[0]*_fieldSize[1]*_fieldSize[2];
      //unsigned char* data = new unsigned char[dataSize*4];
      unsigned int i=0;
      unsigned int j=0;
      unsigned int k = 0;
      
      osg::ref_ptr<osg::Image> propertyField = new osg::Image();

      propertyField->allocateImage(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2],
                                GL_RGBA,GL_UNSIGNED_BYTE);

      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
		                      GL_RGBA,
		                      GL_RGBA,
			                   GL_UNSIGNED_BYTE,
                           0,
                           osg::Image::USE_NEW_DELETE,1);
      propertyField->setDataVariance(osg::Object::DYNAMIC);
      _property = new osg::Texture3D();
      _property->setDataVariance(osg::Object::DYNAMIC);
      
      _property->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _property->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _property->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setInternalFormat(GL_RGBA);
      _property->setTextureSize(_fieldSize[0],
                             _fieldSize[1],
                             _fieldSize[2]);
      _property->setImage(propertyField.get());
      osg::ref_ptr<cfdSimpleTextureCallback> sCallback = new cfdSimpleTextureCallback();
      sCallback->setTextureSize(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2]);
      _property->setSubloadCallback(sCallback.get());
      
   }else{
      std::cout<<"Invalid field size!!"<<std::endl;
      std::cout<<"cfdOSGTransferShaderManager::_initPropertyTexture"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                                         float param, int whichFunction)
{
   cfdUpdateableOSGTexture1d* temp;
   if(!_transferFunctions.size())
   {
      std::cout<<"Transfer functions not initialized!!!"<<std::endl;
      return;
   }
   switch(whichFunction)
   {
      case 0:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(0)->getSubloadCallback());
         break;
      case 1:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(1)->getSubloadCallback());
         break;
      case 2:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(2)->getSubloadCallback());
         break;
      case 3:
      default:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(3)->getSubloadCallback());
         break;
   };
   temp->UpdateParam(type,param);
}
/////////////////////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetFieldSize(unsigned int x,unsigned int y,unsigned  int z)
{
   _fieldSize[0] = x;
   _fieldSize[1] = y;
   _fieldSize[2] = z;
}
/////////////////////////////////////////////////////////////////////////
cfdOSGTransferShaderManager& cfdOSGTransferShaderManager::operator=(const cfdOSGTransferShaderManager& sm)

{
   if(this != &sm){
      cfdOSGShaderManager::operator =(sm);
      _property = sm._property;
      int nTransferFuncts = sm._transferFunctions.size();
      for(int i = 0; i < nTransferFuncts; i++){
         _transferFunctions.push_back(sm._transferFunctions.at(i));
      }
      
      _reinit = sm._reinit;
      _fieldSize[0] = sm._fieldSize[0];
      _fieldSize[1] = sm._fieldSize[1];
      _fieldSize[2] = sm._fieldSize[2];
      _tm = sm._tm;
      _utCbk = sm._utCbk;
   }
   return *this;
}
#endif//_OSG
#endif

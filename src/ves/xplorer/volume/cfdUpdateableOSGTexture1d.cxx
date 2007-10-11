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
#ifdef _OSG
#include <osg/Texture1D>
#include <osg/Image>
#include <osg/State>
#include <ves/xplorer/volume/cfdUpdateableOSGTexture1d.h>
#include <iostream>
using namespace VE_TextureBased;
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d::cfdUpdateableOSGTexture1d()
{
   _alphaCutoff = .1;
   _gamma = 1.4;
   _lastAlpha = 0.0;
   _lastGamma = 0.0;
   _type = GAMMA_CORRECTION;
   _textureWidth = 256;
   _oWidth = -1;
   _data = 0;
   _updateData();
}
////////////////////////////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d::cfdUpdateableOSGTexture1d(const
                                                cfdUpdateableOSGTexture1d& cb)
{
   _lastAlpha = cb._lastAlpha;
   _lastGamma = cb._lastGamma;
   _alphaCutoff = cb._alphaCutoff;
   _gamma = cb._gamma;
   _type = cb._type;
   _textureWidth = cb._textureWidth;
   _oWidth = cb._oWidth;
   _data = 0;
}
//////////////////////////////////////////////////////////
//Destructor                                            //
//////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d::~cfdUpdateableOSGTexture1d()
{
   if(_data){
      delete [] _data;
      _data = 0;
   }
}
/////////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::UpdateParam(TransType type,GLfloat param)
{
   if(type == GAMMA_CORRECTION){
      SetGamma(param);
   }else{
      SetAlphaCutoff(param);
   }
}
///////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetGamma(GLfloat gamma)
{  
   _gamma = gamma;   
   if(_needsUpdate()){
      _updateData();
      _lastGamma = _gamma;
   }
}
///////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetAlphaCutoff(GLfloat aCutoff)
{
   _alphaCutoff = aCutoff;
   if(_needsUpdate()){
      _updateData();
      _lastAlpha = _alphaCutoff;
   }
}
//////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::subload(const osg::Texture1D& texture,
                                        osg::State& state) const
{
   glTexImage1D(GL_TEXTURE_1D,0,GL_RGBA,256,0,GL_RGBA,GL_UNSIGNED_BYTE,_data);

}
///////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::load(const osg::Texture1D& texture,
                                     osg::State&) const
{
   glTexSubImage1D(GL_TEXTURE_1D,0,0,256,GL_RGBA,GL_UNSIGNED_BYTE,_data);
}
///////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetTransferFunctionType(TransType type)
{
   _type = type;
}
////////////////////////////////////////////////////
bool cfdUpdateableOSGTexture1d::_needsUpdate() const
{
   if(_type == GAMMA_CORRECTION){
      return ((_lastGamma == _gamma)?false:true);
   }else{
      return ((_lastAlpha == _alphaCutoff)?false:true);
   }
   return false;
}
/////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::_updateData()
{
   if(!_data){
      _data = new unsigned char[256];
   }
   if(_type == GAMMA_CORRECTION){
      int* gTable = new int[256];
      for (int i=0; i<256; i++) {       
         double y = (double)(i)/((double)(_textureWidth-1.0));   
         y = pow(y, 1.0/_gamma);     
          gTable[i] = (int) floor((_textureWidth-1.0) * y + 0.5);  
      }
      for (int i = 0; i < 256; i++){
          _data[i] = (GLubyte)gTable[i]; 
      }
      if(gTable){
         delete [] gTable;
         gTable = 0;
      }
   }else{
      int cutoff = _alphaCutoff*256;
      int twminusone = 256-1;
      for(int i = 0; i < _textureWidth; i++){
         if(i < cutoff){
            _data[i] = (unsigned char)0;
         }else{
           _data[i] = (unsigned char)(twminusone*(twminusone - i)/(twminusone-cutoff));
         }
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
//equal operator
////////////////////////////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d& cfdUpdateableOSGTexture1d::operator=(const cfdUpdateableOSGTexture1d& cb)
{
   if(this != &cb){
      _lastAlpha = cb._lastAlpha;
      _lastGamma = cb._lastGamma;
      _alphaCutoff = cb._alphaCutoff;
      _gamma = cb._gamma;
      _type = cb._type;
      _textureWidth = cb._textureWidth;
      _oWidth = cb._oWidth;
      if(!_data){
         _data = new unsigned char[256];
      }
      for(int i = 0; i < 256; i++){
         _data[i] = cb._data[i];
      }
   }
   return *this;
}
#endif

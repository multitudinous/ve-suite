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
#ifdef _PERFORMER
#elif _OSG
#include "VE_Xplorer/TextureBased/cfdOSGPingPongTexture3d.h"
#include <osg/Image>
#include <osg/State>
#include <osg/Texture3D>
#include <osg/StateAttribute>
static bool start = false;
using namespace VE_TextureBased;
//////////////////////////////////////////////////
//Constructors                                  //
//////////////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdOSGPingPongTexture3D()
{
    _pingUnit = 0;
    _pongUnit = 0;
}
//////////////////////////////////////////////////////
cfdOSGPingPongTexture3D::cfdOSGPingPongTexture3D(const
                           cfdOSGPingPongTexture3D& pp)
{
   _previous = pp._previous;
   _current = pp._current;
   _pingUnit = pp._pingUnit;
   _pongUnit = pp._pongUnit;
}
///////////////////////////////////////////////////
//Destructor                                     //
///////////////////////////////////////////////////
cfdOSGPingPongTexture3D::~cfdOSGPingPongTexture3D()
{
}
//////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::SetPingTexture(unsigned int unit,
                                        osg::Node* ping)
{
   _pingUnit = unit;
   _previous = ping;
}
//////////////////////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::SetPongTexture(unsigned int unit,
                                        osg::Node* pong)
{
   _pongUnit = unit;
   _current = pong;
}
/////////////////////////////////////////////////////////////
osg::Texture3D* cfdOSGPingPongTexture3D::GetCurrentTexture()
{
   if(_current.valid()){
      osg::ref_ptr<osg::Texture3D> texture = 
         dynamic_cast<osg::Texture3D*>(_current->getStateSet()->getTextureAttribute(_pongUnit,
                                                          osg::StateAttribute::TEXTURE));
      return texture.get();
   }
   return 0;
}
////////////////////////////////////////////////
void cfdOSGPingPongTexture3D::PingPongTextures()
{
   osg::ref_ptr<osg::StateSet> tmpStateSet = _previous->getStateSet();
   unsigned int tmpUnit = 0;
   tmpUnit = _pingUnit;
   _pingUnit = _pongUnit;
   _previous->setStateSet(_current->getStateSet());
   _pongUnit = tmpUnit;
   _current->setStateSet(tmpStateSet.get());
   return;
}
/////////////////////////////////////////////////////////////////////   
//equal operator                                                   //
/////////////////////////////////////////////////////////////////////   
cfdOSGPingPongTexture3D&
cfdOSGPingPongTexture3D::operator=(const cfdOSGPingPongTexture3D& pp)
{
   if(this != &pp){
      _previous = pp._previous;
      _current = pp._current;
   }
   return *this;
}
#endif //_OSG

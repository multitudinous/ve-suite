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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
//class to update the billboard center appropriately` 
#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/Billboard>
#include <osg/NodeCallback>
#include <osg/Vec3f>
#include "VE_TextureBased/cfdVolumeCenterCallback.h"
using namespace VE_TextureBased;
///////////////////////////////////////////////////////////////////
//Constructor                                                    //
///////////////////////////////////////////////////////////////////
cfdVolumeCenterCallback::cfdVolumeCenterCallback(osg::Vec3f center)
:_center(center)
{
   _translate[0] = 0;
   _translate[1] = 0;
   _translate[2] = 0;
}
///////////////////////////////////////////////////////////
void cfdVolumeCenterCallback::Translate(float* translation)
{
   _translate[0] = translation[0];
   _translate[1] = translation[2];
   _translate[2] = translation[2];
}
//////////////////////////////////////////////////////////////////////////////
void cfdVolumeCenterCallback::operator()(osg::Node* node,osg::NodeVisitor* nv)
{
   osg::ref_ptr<osg::Billboard> billboard = dynamic_cast<osg::Billboard*>(node);
   if(billboard.valid())
   {
      billboard->setPosition(0,_center + _translate);
   }
   traverse(node,nv);
}
#endif //_OSG
#endif

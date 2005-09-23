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
#ifdef _OSG
#include "VE_TextureBased/cfdVolumeBillboard.h"
using namespace VE_TextureBased;
bool cfdVolumeBillboard::computeMatrix(osg::Matrix& modelview,
                                   const osg::Vec3& eye_local,
                                   const osg::Vec3& pos_local) const
{
   return osg::Billboard::computeMatrix(modelview,eye_local,pos_local);
   //osg::Billboard::computeMatrix(modelview,eye_local,pos_local);
   osg::Matrix matrix;
   osg::Vec3 ev(eye_local-pos_local);
   osg::Vec3f eyeTemp; 
   osg::Vec3f center;
   osg::Vec3f up;
   modelview.getLookAt(eyeTemp, center, up);
   //billboards matrix is basically looking at 
   //the camera from the rotation center
   matrix.makeLookAt(_positionList[0],eyeTemp,up);
   
   //matrix.setTrans(pos_local);
   modelview.preMult(matrix);
   return true;
   
}
#endif

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
 * File:          $RCSfile: cfdQuatCam.cxx,v $
 * Date modified: $Date: 2004-08-02 12:14:18 -0700 (Mon, 02 Aug 2004) $
 * Version:       $Rev: 737 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <cfdQuatCam.h>

using namespace gmtl;
using namespace vrj;

cfdQuatCam::cfdQuatCam(pfMatrix m, double* worldTrans, float* rotPts)
{
   NextPosQuat.makeRot(m);
   for (int i=0; i<3; i++)
      vjVecNextTrans[i] = worldTrans[i];
   for (int j=0; j<4; j++)
      rotPoints[j] = rotPts[j];
}

cfdQuatCam::cfdQuatCam(float angle, float x, float y, float z, float* worldTrans)
{
   NextPosQuat.makeRot(angle, x, y, z);
   for (int i=0; i<3; i++)
      vjVecNextTrans[i] = worldTrans[i];
}


void cfdQuatCam::SetCamPos(double* worldTrans, pfDCS* worldDCS)
{
   for (int i=0; i<3; i++)
      vjVecLastTrans[i] = worldTrans[i];
   pfMatrix m;
   worldDCS->getMat(m);  
   LastPosQuat.makeRot(m);
}


pfDCS* cfdQuatCam::MoveCam(double* worldTrans, float t, pfDCS* dummy)
{
   TransLerp(t);
   RotSlerp(t);
   dummy->setMat(m2);
   dummy->setTrans(-vjVecCurrTrans[0], -vjVecCurrTrans[1], -vjVecCurrTrans[2]);
   return dummy;
}

void cfdQuatCam::RotSlerp(float t)
{  
   CurPosQuat = new pfQuat();
   CurPosQuat->slerp(t, LastPosQuat, NextPosQuat);
   CurPosQuat->getRot(m2);
}


void cfdQuatCam::TransLerp(float t)
{
   gmtl::lerp(vjVecCurrTrans, t, vjVecLastTrans, vjVecNextTrans); 
}


void cfdQuatCam::UpdateTrans(cfdNavigate* nav)
{
   nav->worldTrans[0] = (double)vjVecCurrTrans[0];
   nav->worldTrans[1] = (double)vjVecCurrTrans[1];
   nav->worldTrans[2] = (double)vjVecCurrTrans[2];
   nav->UpdateLoc(nav->worldTrans);

   //return worldTrans;
}

void cfdQuatCam::UpdateRotation()
{
   if (rotPoints[1]<0.000001)
   {
      angle = gmtl::Math::aCos(rotPoints[0])*57.29877951;
   }
   else
   {
      angle = 360 - (gmtl::Math::aCos(rotPoints[0])*57.29877951);
   }
}   





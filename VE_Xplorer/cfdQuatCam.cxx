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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "VE_Xplorer/cfdQuatCam.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_SceneGraph/cfdDCS.h"
#ifdef _PERFORMER
#include <vrj/Draw/Pf/PfUtil.h>
#include <Performer/pf.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pr/pfLinMath.h>
#elif _OSG
#include <osg/MatrixTransform>
#include <osg/Matrix>
#include <osg/Vec3f>
#include <osg/NodeVisitor>
#elif _OPENSG
#endif
using namespace gmtl;
using namespace VE_SceneGraph;
using namespace VE_Xplorer;

cfdQuatCam::cfdQuatCam(Matrix44f& m, double* worldTrans, float* rotPts)
{
   
   set(NextPosQuat,m);
   for (int i=0; i<3; i++)
      vjVecNextTrans[i] = worldTrans[i];
   for (int j=0; j<4; j++)
      rotPoints[j] = rotPts[j];
}

//Currently not used, needs to be fixed using gmtl functionality
/*cfdQuatCam::cfdQuatCam(float angle, float x, float y, float z, float* worldTrans)
{
   NextPosQuat.makeRot(angle, x, y, z);
   for (int i=0; i<3; i++)
      vjVecNextTrans[i] = worldTrans[i];
}*/


void cfdQuatCam::SetCamPos(double* worldTrans, VE_SceneGraph::cfdDCS* worldDCS)
{
   for (int i=0; i<3; i++)
      vjVecLastTrans[i] = worldTrans[i];
   
   Matrix44f vjm;  
   vjm = worldDCS->GetMat();   
   set(LastPosQuat,vjm);
}


void cfdQuatCam::MoveCam(double* worldTrans, float t, VE_SceneGraph::cfdDCS* dummy)
{
   TransLerp(t);
   RotSlerp(t);
}

void cfdQuatCam::RotSlerp(float t)
{  
   gmtl::slerp(CurPosQuat,t,LastPosQuat,NextPosQuat);;
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
}

void cfdQuatCam::UpdateRotation( cfdNavigate* nav, VE_SceneGraph::cfdDCS* worldDCS)
{
   Matrix44f temp;
   temp = makeRot<gmtl::Matrix44f>( CurPosQuat );
   worldDCS->SetRotationMatrix( temp );
   
#ifdef _PERFORMER
   pfCoord* coord = new pfCoord();
   rotvec[0] = coord->hpr[ 0 ];
   rotvec[1] = coord->hpr[ 1 ];
   rotvec[2] = coord->hpr[ 2 ];
   nav->worldRot[0] = rotvec[0];
   nav->worldRot[1] = rotvec[1];
   nav->worldRot[2] = rotvec[2];
#elif _OSG
   std::cerr << " ERROR: cfdQuatCam is NOT implemented for OSG " << std::endl;

   //rotvec[0] = gmtl::Math::rad2Deg( makeZRot(temp) );
   //rotvec[1] = gmtl::Math::rad2Deg( makeXRot(temp) );
   //rotvec[2] = gmtl::Math::rad2Deg( makeYRot(temp) );
#endif


}   





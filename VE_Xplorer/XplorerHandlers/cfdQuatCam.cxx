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

#include "VE_Xplorer/XplorerHandlers/cfdQuatCam.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
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

cfdQuatCam::cfdQuatCam(gmtl::Matrix44f& m, float* worldTrans)
{
   nextMatrix = m;
   gmtl::Vec3f scaleXVec( nextMatrix[ 0 ][ 0 ], nextMatrix[ 1 ][ 0 ], nextMatrix[ 2 ][ 0 ] );
   gmtl::Vec3f scaleYVec( nextMatrix[ 0 ][ 1 ], nextMatrix[ 1 ][ 1 ], nextMatrix[ 2 ][ 1 ] );
   gmtl::Vec3f scaleZVec( nextMatrix[ 0 ][ 2 ], nextMatrix[ 1 ][ 2 ], nextMatrix[ 2 ][ 2 ] );
   float tempScale = 1.0f/gmtl::length( scaleXVec );
   gmtl::Matrix44f tempScaleMat;
   gmtl::setScale( tempScaleMat, tempScale );
   gmtl::Matrix44f unScaleInput = tempScaleMat * nextMatrix;   
   set(NextPosQuat,unScaleInput);
   for (int i=0; i<3; i++)
      vjVecNextTrans[i] = worldTrans[i];
/*
         gmtl::Vec3f scaleXVec( input[ 0 ][ 0 ], input[ 1 ][ 0 ], input[ 2 ][ 0 ] );
         gmtl::Vec3f scaleYVec( input[ 0 ][ 1 ], input[ 1 ][ 1 ], input[ 2 ][ 1 ] );
         gmtl::Vec3f scaleZVec( input[ 0 ][ 2 ], input[ 1 ][ 2 ], input[ 2 ][ 2 ] );
         float tempScale = 1.0f/gmtl::length( scaleXVec );
         gmtl::Matrix44f tempScaleMat;
         gmtl::setScale( tempScaleMat, tempScale );
         gmtl::Matrix44f unScaleInput = tempScaleMat * input;
*/

}

void cfdQuatCam::SetCamPos(float* worldTrans, VE_SceneGraph::DCS* worldDCS)
{
   for (int i=0; i<3; i++)
      vjVecLastTrans[i] = worldTrans[i];
   
   gmtl::Matrix44f vjm;  
   vjm = worldDCS->GetMat();
   gmtl::Vec3f scaleXVec( vjm[ 0 ][ 0 ], vjm[ 1 ][ 0 ], vjm[ 2 ][ 0 ] );
   gmtl::Vec3f scaleYVec( vjm[ 0 ][ 1 ], vjm[ 1 ][ 1 ], vjm[ 2 ][ 1 ] );
   gmtl::Vec3f scaleZVec( vjm[ 0 ][ 2 ], vjm[ 1 ][ 2 ], vjm[ 2 ][ 2 ] );
   float tempScale = 1.0f/gmtl::length( scaleXVec );
   gmtl::Matrix44f tempScaleMat;
   gmtl::setScale( tempScaleMat, tempScale );
   gmtl::Matrix44f unScaleInput = tempScaleMat * vjm;   

   set(LastPosQuat,unScaleInput);
}


void cfdQuatCam::MoveCam( float t )
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

void cfdQuatCam::UpdateRotation( VE_SceneGraph::DCS* worldDCS)
{
   Matrix44f temp;
   temp = makeRot<gmtl::Matrix44f>( CurPosQuat );
   worldDCS->SetRotationMatrix( temp );
   float tempTrans[3] ;
   /*tempTrans[0] = -vjVecCurrTrans[0];
   tempTrans[1] = -vjVecCurrTrans[1];
   tempTrans[2] = -vjVecCurrTrans[2];*/
	tempTrans[0] = vjVecCurrTrans[0];
   tempTrans[1] = vjVecCurrTrans[1];
   tempTrans[2] = vjVecCurrTrans[2];
   worldDCS->SetTranslationArray(tempTrans/*vjVecCurrTrans.getData()*/);
}  

gmtl::Matrix44f cfdQuatCam::GetMatrix( void )
{
   return nextMatrix;
} 

gmtl::Vec3f cfdQuatCam::GetTrans( void )
{
   return vjVecNextTrans;
}

gmtl::Vec3f cfdQuatCam::GetLastTrans( void )
{
   return vjVecLastTrans;
}


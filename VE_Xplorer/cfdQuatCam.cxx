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





#ifndef _CFD_QUAT_CAM_H_
#define _CFD_QUAT_CAM_H_

#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>
#include <vector>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <gmtl/Math.h>
#include <gmtl/Vec.h>
#include <gmtl/Point.h>
#include <gmtl/Xforms.h>
#include <gmtl/Output.h>
#include <gmtl/Matrix.h>
#include <gmtl/Coord.h>
#include <gmtl/Generate.h>
#include <Performer/pr.h>
#include <Performer/pf/pfLightSource.h>
#include <Performer/pr/pfLinMath.h>
#include <Performer/pf/pfNode.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pf/pfSwitch.h>
#include <Performer/pf/pfGroup.h>
#include <Performer/pfdu.h>
#include <Performer/pfutil.h>
#include <Performer/prmath.h>

// --- VR Juggler Stuff --- //
#include <vrj/Kernel/Kernel.h>
#include <vrj/Draw/Pf/PfApp.h>    /* the performer application base type */
#include <vrj/Util/Debug.h>
#include <vrj/Display/Projection.h>  /* for setNearFar (for setting clipping planes) */
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>
#include "cfdNavigate.h"

using namespace vrj;


class cfdQuatCam
{
public:

   //Constructors
   cfdQuatCam(pfMatrix, double*, float*);
   cfdQuatCam(float, float, float, float, float*);
   
   
   //Destructor
   ~cfdQuatCam();

   void SetCamPos(double*, pfDCS*);

   pfDCS* MoveCam(double*, float, pfDCS*);

   void RotSlerp(float);

   void TransLerp(float);

   void UpdateTrans(cfdNavigate*);

   void UpdateRotation();

   gmtl::Vec3f  vjVecCurrTrans;

   pfMatrix m2;

   float rotPoints[4];

   float angle;

private:
   pfQuat LastPosQuat;
   pfQuat NextPosQuat;
   pfQuat* CurPosQuat;
   gmtl::Vec3f  vjVecNextTrans;
   gmtl::Vec3f  vjVecLastTrans;

};
#endif

 

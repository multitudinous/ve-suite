#ifndef _CFD_QUAT_CAM_HANDLER_H_
#define _CFD_QUAT_CAM_HANDLER_H_

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

#include "cfdQuatCam.h"
#include "cfdNavigate.h"

using namespace vrj;


class cfdPoints
{
public:
   
   //Constructor
   cfdPoints(double*, pfMatrix);

   //Destructor
   ~cfdPoints();
    pfMatrix& matrix(){return m;}
//private:
   pfMatrix       m;
   gmtl::Vec3f    ptrans; 

};


class cfdQuatCamHandler
{
public:

   //Constructors
   cfdQuatCamHandler();
      
   //Destructor
   ~cfdQuatCamHandler();

   void LoadData(double*, pfDCS*);

   void WriteToFile(char*);

   void LoadFromFile(char*);

   pfDCS* Relocate(int runSlerp, pfDCS* worldDCS, int cfdIso_value, cfdNavigate* nav); 

   std::vector<cfdPoints*> cfdPointsVec;
   std::vector<cfdQuatCam*> QuatCams;
   int run;
   float rotvec[3];
   float angle;

private:


   cfdQuatCam* thisQuatCam;
   pfMatrix m;
   cfdPoints*  nextPoint;

   float t;
};
#endif

 

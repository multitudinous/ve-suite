/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdQuatCamHandler.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

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
#include <vrj/Util/Debug.h>
#include <vrj/Display/Projection.h>  /* for setNearFar (for setting clipping planes) */
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

#include "cfdQuatCam.h"
#include "cfdNavigate.h"
#ifdef _CFDCOMMANDARRAY
class cfdCommandArray;
#endif //_CFDCOMMANDARRAY


using namespace vrj;
using namespace gmtl;


class cfdPoints
{
public:
   
   //Constructor
   cfdPoints(double*, Matrix44f&);

   //Destructor
   ~cfdPoints();
    Matrix44f& matrix(){return m;}
    Vec3f& ptrans(){return trans;}
private:
    Matrix44f      m;
    Vec3f    trans; 

};


class cfdQuatCamHandler
{
public:

   //Constructors
   cfdQuatCamHandler();
      
   //Destructor
   ~cfdQuatCamHandler();

#ifdef _CFDCOMMANDARRAY
   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
#endif //_CFDCOMMANDARRAY

   void LoadData(double*, pfDCS*);

   void WriteToFile(char*);

   void LoadFromFile(char*);

   void Relocate(int runSlerp, pfDCS* worldDCS, int cfdIso_value, cfdNavigate* nav); 

   std::vector<cfdPoints*> cfdPointsVec;
   std::vector<cfdQuatCam*> QuatCams;
   int run;
   float rotvec[3];
   float angle;

private:


   cfdQuatCam* thisQuatCam;
   cfdPoints*  nextPoint;

   float t;
};
#endif

 

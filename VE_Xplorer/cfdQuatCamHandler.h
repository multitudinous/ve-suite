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

#include <gmtl/Math.h>
#include <gmtl/Vec.h>
#include <gmtl/Point.h>
#include <gmtl/Xforms.h>
#include <gmtl/Output.h>
#include <gmtl/Matrix.h>
#include <gmtl/Coord.h>
#include <gmtl/Generate.h>

class cfdDCS;
class cfdQuatCam;
class cfdNavigate;
class cfdCommandArray;
class cfdReadParam;

#include <vector>

#include "cfdGlobalBase.h"

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


class cfdQuatCamHandler : public cfdGlobalBase
{
public:

   //Constructors
   cfdQuatCamHandler( cfdDCS*, cfdNavigate*, char* );
      
   //Destructor
   ~cfdQuatCamHandler();

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   void CreateObjects( void );
   
   void LoadData(double*, cfdDCS*);

   void WriteToFile(char*);

   void LoadFromFile(char*);

   void Relocate(int runSlerp, cfdDCS* worldDCS, int cfdIso_value, cfdNavigate* nav); 

   std::vector<cfdPoints*> cfdPointsVec;
   std::vector<cfdQuatCam*> QuatCams;
   int run;
   float rotvec[3];
   float angle;

private:


   cfdQuatCam* thisQuatCam;
   cfdPoints*  nextPoint;
   cfdDCS*     _worldDCS;
   cfdNavigate* _nav;
   cfdReadParam* _readParam;
   char*       _param;
   float t;
   char quatCamFileName[ 100 ];
};
#endif

 

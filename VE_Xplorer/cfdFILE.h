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
 * File:          $RCSfile: cfdFILE.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_FILE_H
#define CFD_FILE_H

#include <vector>

class fileInfo;
class cfdDCS;
class cfdNode;
// Need to fix this class
// BIG PROBLEMS here
//class pfNode;
//class pfMaterial;

class cfdFILE 
{
   public:
      cfdFILE( fileInfo*, cfdDCS* );
      cfdFILE( char*, cfdDCS* );
      cfdFILE( float, float [ 3 ], char * );

  ~cfdFILE();

  void Initialize(float);

  //void pfTravNodeMaterial(pfNode*, pfMaterial*, int );

  cfdDCS* getpfDCS( void );
  cfdNode* GetcfdNode( void );

   void SetFILEProperties( int, int, float* );
  void setOpac(float op_val);
  float getOpacity();

  //pfLightModel *matLight;
  //pfMaterial *fmaterial;
  //pfMaterial *bmaterial;
  //std::vector< pfMaterial *> matList;
  cfdNode* node;
  cfdDCS*  DCS;
  //pfMaterial *mat1, *mat0;
  int mat_count;
  int color;
  int transparent;
  float stlColor [ 3 ];

 private:
  float op;
};

#endif

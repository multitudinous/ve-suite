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
 * Date modified: $Date: 2004/03/23 16:29:15 $
 * Version:       $Revision: 1.9 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_FILE_H
#define CFD_FILE_H

#include <vector>

class fileInfo;
class pfNode;
class pfMaterial;
class pfNode;
class pfDCS;
//class pfLightModel;

class cfdFILE 
{
 public:
  cfdFILE( fileInfo *, pfDCS * );
  cfdFILE( float, float [ 3 ], char * );

  ~cfdFILE();

  void Initialize(float);

  void pfTravNodeMaterial(pfNode*, pfMaterial*, int );

  pfDCS * getpfDCS();
  pfNode * getpfNode();

  void setOpac(float op_val);
  float getOpacity();

  //pfLightModel *matLight;
  pfMaterial *fmaterial;
  pfMaterial *bmaterial;
  std::vector< pfMaterial *> matList;
  pfNode *node;
  pfDCS *DCS;
  pfMaterial *mat1, *mat0;
  int mat_count;
  int color;
  int transparent;
  float stlColor [ 3 ];

 private:
  float op;
};

#endif

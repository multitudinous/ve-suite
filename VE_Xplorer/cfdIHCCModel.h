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
#ifndef CFD_IHCCMODEL_H
#define CFD_IHCCMODEL_H

#include "cfdObjects.h"
#include <vector>

using namespace std;
class vtkLookupTable;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkActor;

class fileInfo;
class pfDCS;
class pfGroup;

class cfdIHCCGauge;
class cfdIHCCContour;

#ifndef _USE_CFD_SEQUENCE
class pfSequence;
#else
class cfdSequence;
#endif

class cfdIHCCModel: public cfdObjects
{
   public:
      cfdIHCCModel( fileInfo *, pfDCS * );

      ~cfdIHCCModel( void );

      void RunModel( void );
      void UpdateModelVariables( double* );
      void MakeLookupTable( void );
      void MakeSequence( void );
      void RemoveSequence( void );
      void Update( void );

      double variables[ 6 ];

      double min, max;
      vtkLookupTable* lut;
      vtkPolyData* pData;
      vtkPolyDataMapper* mapper;
      vtkActor* actor;
      double definedRange[ 2 ];
	   vector< double > solutions;
	   vector< double > times;
#ifndef _USE_CFD_SEQUENCE
      pfSequence* sequence;
#else
      cfdSequence* sequence;
#endif
      cfdIHCCGauge* gauge_acid;
      cfdIHCCGauge* gauge_time;
      cfdIHCCContour* contours;
      pfGroup* ihccModelNode;
};

#endif

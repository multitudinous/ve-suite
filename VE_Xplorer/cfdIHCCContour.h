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
 * File:          $RCSfile: cfdIHCCContour.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_IHCCCONTOUR_H
#define CFD_IHCCCONTOUR_H

#include <vector>
#include "cfdObjects.h"
class vtkLookupTable;
class vtkPolyData;
class vtkPolyDataMapper;
class vtkActor;
using namespace std;

class cfdIHCCContour: public cfdObjects
{
   public:
      cfdIHCCContour( void );

      ~cfdIHCCContour( void );

      void RunModel( void );
      void UpdateModelVariables( double* );
      void MakeLookupTable( void );
      void MakeSequence( void );
      void Update( void );
      void SetDataVector( vector< double >, double* x );
      double variables[ 6 ];

      double min, max;
      vtkLookupTable* lut;
      vtkPolyData* pData;
      vtkPolyDataMapper* mapper;
      vtkActor* actor;
      double definedRange[ 2 ];
      vector< double > solutions;
};

#endif

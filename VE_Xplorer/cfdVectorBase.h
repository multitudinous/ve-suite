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
 * File:          $RCSfile: cfdVectorBase.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VECTOR_BASE_H
#define CFD_VECTOR_BASE_H

#include "cfdObjects.h"

class vtkGlyph3D;
class vtkGeometryFilter;
class vtkPolyDataMapper;
class vtkMaskPoints;

#ifdef _CFDCOMMANDARRAY
class cfdCommandArray;
#endif //_CFDCOMMANDARRAY

class cfdVectorBase : public cfdObjects
{
 public:
   cfdVectorBase();

   virtual ~cfdVectorBase();

   // pure virtual int functions to be specified in concrete implementations

#ifdef _CFDCOMMANDARRAY
   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray* commandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();
#endif //_CFDCOMMANDARRAY

   // update the actor
   virtual void Update() = 0;

   static void SetThreshHoldPercentages( int min, int max );

   static void SetThreshHoldValues( double * );
   static double* GetThreshHoldValues( void );

   static void UpdateThreshHoldValues();

   static void SetVectorRatioFactor( int );
   static int GetVectorRatioFactor();

   static void SetScaleByVectorFlag( int );
   static int GetScaleByVectorFlag( void );

 protected:
   vtkGlyph3D*       glyph;
   vtkGeometryFilter* filter;
   vtkPolyDataMapper* mapper;
   vtkMaskPoints*    ptmask;

   void SetGlyphWithThreshold();
   void SetGlyphAttributes();
   float GetVectorScaleFactor();

   //int GetVectorRatioFactor();

   static int vectorThreshHoldMinPercentage;
   static int vectorThreshHoldMaxPercentage;
   static double vectorThreshHoldValues[ 2 ];
   static int vectorRatioFactor;
   static int scaleByVector;
};

#endif

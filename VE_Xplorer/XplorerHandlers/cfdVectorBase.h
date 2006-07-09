/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_VECTOR_BASE_H
#define CFD_VECTOR_BASE_H

#include "VE_Xplorer/XplorerHandlers/cfdObjects.h"

class vtkGlyph3D;
class vtkGeometryFilter;
class vtkPolyDataMapper;
class vtkMaskPoints;
class vtkTriangleFilter;
class vtkStripper;
class vtkThresholdPoints;

namespace VE_Xplorer
{
   class cfdCommandArray;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdVectorBase : public cfdObjects
{
public:
   cfdVectorBase();

   virtual ~cfdVectorBase();

   // pure virtual int functions to be specified in concrete implementations

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray* commandArray );

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand();

   // update the actor
   virtual void Update() = 0;

   void SetThreshHoldPercentages( int minThresh, int maxThresh );

   void SetThreshHoldValues( double * );
   double* GetThreshHoldValues( void );

   void UpdateThreshHoldValues();

   void SetVectorRatioFactor( int );
   int GetVectorRatioFactor();

   void SetScaleByVectorFlag( int );
   int GetScaleByVectorFlag( void );

   void SetVectorScale( float );
   float GetVectorScale();

protected:
   vtkGlyph3D*          glyph;
   vtkGeometryFilter*   filter;
   vtkPolyDataMapper*   mapper;
   vtkMaskPoints*       ptmask;
   vtkTriangleFilter*   tris;
   vtkStripper*         strip;
   vtkThresholdPoints*  tfilter;
   void SetGlyphWithThreshold();
   void SetGlyphAttributes();
   float GetVectorScaleFactor();

   int _vectorThreshHoldMinPercentage;
   int _vectorThreshHoldMaxPercentage;
   double _vectorThreshHoldValues[ 2 ];
   int _vectorRatioFactor;
   int _scaleByVector;
   float _vectorScale;
};
}
#endif

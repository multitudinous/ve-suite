/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdTransientVectorActor.h"
#include "cfdDataSet.h"

#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkGlyph3D.h>
#include <vtkMaskPoints.h>
#include <vtkThresholdPoints.h>
#include <vpr/Util/Debug.h>

void cfdTransientVectorActor::definePipeline( )
{
   //the threshold stuff
   vtkMaskPoints * pMask = vtkMaskPoints::New();
   pMask->SetInput( this->dataset );
   int pMaskVal = 5; // gm engine spray
   //int pMaskVal = 1;
   pMask->SetOnRatio( pMaskVal );
   pMask->RandomModeOn();

   vtkThresholdPoints * thresh = vtkThresholdPoints::New();
   thresh->SetInput( pMask->GetOutput() );
   float threshVal = 0.0001;
   thresh->ThresholdByUpper( threshVal );
   thresh->Update();

   vtkGlyph3D * arrowGlyph  = vtkGlyph3D::New();
   arrowGlyph->SetInput( thresh->GetOutput() );
   thresh->Delete();
   //arrowGlyph->SetInput( pMask->GetOutput());
   arrowGlyph->SetSource( this->arrow );
   arrowGlyph->SetScaleModeToScaleByVector();
   arrowGlyph->SetScaleFactor( 0.09 ); // gm engine spray
   //arrowGlyph->SetScaleFactor( this->arrowSize );
   //arrowGlyph->SetVectorModeToUseVector();
   arrowGlyph->SetScaleModeToDataScalingOff();
   arrowGlyph->Update();
   
   pMask->Delete();

   this->mapper->SetInput( arrowGlyph->GetOutput() );
   arrowGlyph->Delete();

   // Set the mapper to the right colors
   //this->mapper->SetColorModeToMapScalars();
   //this->mapper->SetScalarRange( polydata->GetScalarRange() );
   
   // Specify the lookup table
   vtkLookupTable * lut = vtkLookupTable::New();
   lut->SetNumberOfColors(256); //default is 256
   for ( int i=0; i<256; i++ )   //all white arrows
   {
      lut->SetTableValue( i, 1.0, 1.0, 1.0, 1.0 );
   }

   this->mapper->SetLookupTable( lut );
   this->mapper->Update();
   lut->Delete();
}


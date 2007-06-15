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
/* a Plot3D file reader and viewer.                   -Song Li */ 

#include "Plot3Dviewer.h"
#include "vtkActorToPF.h"

#include <vtkPolyDataNormals.h>
#include <vtkStructuredGridGeometryFilter.h>
#include <vtkPLOT3DReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

Plot3Dviewer::Plot3Dviewer( )
{
}

Plot3Dviewer::~Plot3Dviewer( )
{
}

cfdGeode * Plot3Dviewer::Init(vtkPLOT3DReader *p )
{
   vtkStructuredGridGeometryFilter *cFilter = vtkStructuredGridGeometryFilter::New( );
     cFilter->SetInput( p->GetOutput( ) );
     cFilter->SetExtent( 0, 100, 0, 100, 0, 100 );
     cFilter->Update( );

  vtkPolyDataNormals *cNormal = vtkPolyDataNormals::New( );
     cNormal->SetInput( cFilter->GetOutput( ) );

  vtkPolyDataMapper *cMapper = vtkPolyDataMapper::New( );
     cMapper->SetInput( cNormal->GetOutput( ) );
     cMapper->ScalarVisibilityOff( );

  vtkActor *cActor = vtkActor::New( );
     cActor->SetMapper( cMapper );
     cActor->GetProperty( )->SetColor( 0.5, 0.5, 0.5 );
     cActor->GetProperty( )->SetAmbient( 0.2f );
     cActor->GetProperty( )->SetDiffuse( 0.8f );
     cActor->GetProperty( )->SetSpecular( 0.3f );
     cActor->GetProperty( )->SetSpecularPower( 20.0f );
     cActor->GetProperty( )->SetOpacity( 1.0f );

 
  
  cfdGeode* cGeode = new cfdGeode();
  cGeode->TranslateTocfdGeode(cActor);

  cFilter->Delete( );
  cNormal->Delete( );
  cMapper->Delete( );
  cActor->Delete( );
  
  return cGeode;
}

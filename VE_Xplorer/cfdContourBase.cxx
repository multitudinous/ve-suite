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
 * File:          $RCSfile: cfdContourBase.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdContourBase.h"
#include "cfdDataSet.h"

#ifdef _CFDCOMMANDARRAY
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#endif //_CFDCOMMANDARRAY

#include <vpr/Util/Debug.h>

#include <vtkPolyData.h>
#include <vtkContourFilter.h>                // contour lines
#include <vtkBandedPolyDataContourFilter.h>  // banded contours
#include <vtkGeometryFilter.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

// this class requires that the dataset has a scalar field.
cfdContourBase::cfdContourBase()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdContourBase constructor"
                          << std::endl << vprDEBUG_FLUSH;

   this->filter = vtkGeometryFilter::New();
   this->cfilter = vtkContourFilter::New();              // for contourlines
   this->bfilter = vtkBandedPolyDataContourFilter::New();// for banded contours

   this->mapper = vtkPolyDataMapper::New();
   this->mapper->SetInput( this->filter->GetOutput() );
   this->mapper->SetColorModeToMapScalars();

   this->actor = vtkActor::New();
   this->actor->SetMapper( this->mapper );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );
}

cfdContourBase::~cfdContourBase()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdContourBase destructor"
                          << std::endl  << vprDEBUG_FLUSH;

   this->filter->Delete();
   this->filter = NULL;
   
   this->cfilter->Delete();
   this->cfilter = NULL;
   
   this->bfilter->Delete();
   this->bfilter = NULL;
   
   this->mapper->Delete();
   this->mapper = NULL;

   this->actor->Delete();
   this->actor = NULL;
}

void cfdContourBase::SetMapperInput( vtkPolyData * polydata )
{
   if ( this->fillType == 0 )
   {
/*
      // convert any type of data to polygonal type
      this->filter->SetInput( polydata );
      this->filter->Update();
      this->mapper->SetInput( this->filter->GetOutput() );
*/
      this->mapper->SetInput( polydata );
   }
   else if ( this->fillType == 1 )  // banded contours
   {
      this->bfilter->SetInput( polydata );
      double range[2];
      this->GetActiveMeshedVolume()->GetUserRange( range );
      this->bfilter->GenerateValues( 10, range[0], range[1] );
      this->bfilter->SetScalarModeToValue();
      this->bfilter->GenerateContourEdgesOn();
      this->bfilter->Update();
      this->mapper->SetInput( this->bfilter->GetOutput() );
   }
   else if ( this->fillType == 2 )  // contourlines
   {
      this->cfilter->SetInput( polydata );
      double range[2];
      this->GetActiveMeshedVolume()->GetUserRange( range );
      this->cfilter->GenerateValues( 10, range[0], range[1] );
      this->cfilter->Update();
      this->mapper->SetInput( this->cfilter->GetOutput() );
   }
}

#ifdef _CFDCOMMANDARRAY
bool cfdVectorBase::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   bool flag = cfdObjects::CheckCommandId( commandArray );
   
   if ( commandArray->GetCommandValue( CFD_ID ) == CHANGE_CONTOUR_FILL )
   {
      vprDEBUG(vprDBG_ALL,0) << "CHANGE_CONTOUR_FILL to type " 
                             << commandArray->GetCommandValue( CFD_ISOVALUE )
                             << std::endl << vprDEBUG_FLUSH;

      cfdContourBase::SetFillType( commandArray->GetCommandValue( CFD_ISOVALUE ) );
      return true;
   }
   return flag;
}

void cfdVectorBase::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
}
#endif //_CFDCOMMANDARRAY

/////////////////// STATIC member functions follow ///////////////////

// initial definition of the static variable
int cfdContourBase::fillType = 0;

void cfdContourBase::SetFillType( const int type )
{
   if ( -1 < type && type < 3 )
      fillType = type;
   else
   {
      vprDEBUG(vprDBG_ALL, 0)
         << "cfdContourBase: requested fillType (" << type
         << ") is not available, using 0 instead"
         << std::endl << vprDEBUG_FLUSH;
      fillType = 0;
   }
}


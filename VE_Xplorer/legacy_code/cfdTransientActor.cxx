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
#include "cfdTransientActor.h"
#include "cfdDataSet.h"
#include "cfdTransientVizHandler.h"
#include "readWriteVtkThings.h"

#include <vtkActor.h>
#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkPointData.h>
#include <vtkProperty.h>
#include <vpr/Util/Debug.h>

cfdTransientActor::cfdTransientActor()
{
   this->actor = NULL;
   this->activeDataSet = NULL;
   this->param = NULL;
}

cfdTransientActor::~cfdTransientActor()
{
   if ( this->actor != NULL )
   { 
      this->actor->Delete();
      this->actor = NULL;
   }
}

vtkActor * cfdTransientActor::CreateActor( char *vtkFilename )
{
   vprDEBUG(vprDBG_ALL,1) << " cfdTransientActor::creating actor for file: "
      << vtkFilename << std::endl << vprDEBUG_FLUSH;

   if ( this->param == NULL ) 
   {
      vprDEBUG(vprDBG_ALL,0) << "Parameter file is not set"
                             << std::endl << vprDEBUG_FLUSH;
      return NULL;
   }

   this->dataset = this->param->GetDataSetWithName( vtkFilename )->GetDataSet();

   if ( this->dataset == NULL ) 
   {
      vprDEBUG(vprDBG_ALL,0) << "Can't find this dataset"
                             << std::endl << vprDEBUG_FLUSH;
      return NULL;
   }

   if ( this->dataset->GetNumberOfPoints() == 0 ) 
   {
      vprDEBUG(vprDBG_ALL,0) << "No points in this dataset"
                             << std::endl << vprDEBUG_FLUSH;
      return NULL;
   }
      
   vprDEBUG(vprDBG_ALL,2) << " Active Data Set : \"" 
      << this->activeDataSet->GetFileName() << "\""
      << std::endl << vprDEBUG_FLUSH;
            
   vprDEBUG(vprDBG_ALL,1) << " Active Scalar Name : " 
      << this->activeDataSet->GetDataSet()->GetPointData()
                                          ->GetScalars()->GetName()
      << std::endl << vprDEBUG_FLUSH;
            
   int scalarIndex = this->dataset->GetPointData()->SetActiveScalars( 
      this->activeDataSet->GetDataSet()->GetPointData()->GetScalars()->GetName() );

   vprDEBUG(vprDBG_ALL,1) << " scalarIndex = " << scalarIndex
                          << std::endl << vprDEBUG_FLUSH;

   if ( scalarIndex == -1 )
   {
      std::cerr  << "ERROR: You must activate a valid scalar to proceed"
                             << std::endl;
      return NULL;
   }
   
   this->mapper = vtkPolyDataMapper::New();

   this->definePipeline();

   this->actor = vtkActor::New();
   this->actor->SetMapper( this->mapper );
   this->actor->GetProperty()->SetSpecularPower( 20.0f );

   this->mapper->Delete();

   return this->actor;
}

void cfdTransientActor::SetArrow( vtkPolyData * arrow )
{
   this->arrow = arrow;
}

void cfdTransientActor::SetArrowSize( float size )
{
   this->arrowSize = size;
}

void cfdTransientActor::SetActiveDataSet( cfdDataSet * dataSet )
{
   this->activeDataSet = dataSet;
}

void cfdTransientActor::SetParameterFile( cfdTransientVizHandler *param, int member )
{
   this->param = param;
   this->member = member;
}


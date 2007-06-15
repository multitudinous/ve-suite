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
//#include <string>
#include "cfdFrame.h"
#include "cfdFILE.h"
#include "cfdTransientVizHandler.h"
#include "cfdTransientInfo.h"
#include "cfdTransientVectorActor.h"
#include "cfdTransientScalarActor.h"
#include "cfdTransientParticleActor.h"
#include "cfdTransientParticleActor_fluent.h"
#include "cfdDCS.h"
#include "cfdNode.h"
#include "cfdGeode.h"

#include <vpr/Util/Debug.h>
#include <gmtl/Matrix.h>

//vtk includes
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkProperty.h>

using namespace gmtl;

cfdFrame::cfdFrame()
{
   //initialize data members
   this->vtkFilename = NULL;
   this->transientActor = NULL;
   this->geode = NULL;
   this->actor = NULL;
   this->node = NULL;
   this->dcs = NULL;   
   this->geomFile = NULL;
   this->frameDataType = 0;
   this->activeDataSet = NULL;
   this->activeMeshedVolume = NULL;
   this->activeParticleData = NULL;
   this->activeSurfaceData = NULL;
}

cfdFrame::~cfdFrame()
{
   // this implies that there would also be a node
   // pfDelete will also delete the node as well as the dcs

/*   if ( this->geode != NULL) 
   {
      pfDelete( this->geode );
   }
*/   
   if ( this->transientActor != NULL) 
   {
      delete this->transientActor;
      this->transientActor = NULL;
   }
   
   if ( this->vtkFilename != NULL) 
   {
      delete [] this->vtkFilename;
      this->vtkFilename = NULL;
   }
   
   if ( this->geomFile != NULL) 
   {
      delete this->geomFile;
      this->geomFile = NULL;
   }
}

int cfdFrame::CreateFrame( void )
{
   //check if the file name has been set
   if ( ! this->vtkFilename )
   {
      std::cerr << "No vtk file specified for this frame!!!!" << std::endl;
      return 0;
   }

   vprDEBUG(vprDBG_ALL,1) << "Creating frame from file: " << this->vtkFilename
                          << std::endl << vprDEBUG_FLUSH;

   // depending on frameDataType, create actor
   if ( this->frameDataType == VTK_SCALAR ||
        this->frameDataType == VTK_VECTOR ||
        this->frameDataType == VTK_PARTICLE ||
        this->frameDataType == VTK_FLUENTPARTICLE )
   {
      if ( this->frameDataType == VTK_SCALAR )
      {
         vprDEBUG(vprDBG_ALL,1) <<" Scalar data" << std::endl << vprDEBUG_FLUSH;
         this->transientActor = new cfdTransientScalarActor();
         this->transientActor->SetActiveDataSet( this->activeDataSet );
      }
      else if ( this->frameDataType == VTK_VECTOR )
      {
         vprDEBUG(vprDBG_ALL,1) <<" Vector data"<< std::endl << vprDEBUG_FLUSH;
         this->transientActor = new cfdTransientVectorActor();
         this->transientActor->SetActiveDataSet( this->activeDataSet );
      }
      else if ( this->frameDataType == VTK_PARTICLE )
      {
         vprDEBUG(vprDBG_ALL,1) << " Particle data"
                                << std::endl << vprDEBUG_FLUSH;
         this->transientActor = new cfdTransientParticleActor();
         this->transientActor->SetActiveDataSet( this->activeParticleData );
      }
      else if ( this->frameDataType == VTK_FLUENTPARTICLE )
      {
         vprDEBUG(vprDBG_ALL,1) << " Fluent Particle data"
                                << std::endl << vprDEBUG_FLUSH;
         this->transientActor = new cfdTransientParticleActor_fluent();
         this->transientActor->SetActiveDataSet( this->activeParticleData );
      }
      this->transientActor->SetParameterFile( this->param, this->member );
      this->transientActor->SetArrow( this->arrow );
      this->transientActor->SetArrowSize( this->arrowSize );
      this->actor = this->transientActor->CreateActor( this->vtkFilename );

      //if ( this->displayType() == VTK_WIRE )
      //   this->actor->GetProperty()->SetRepresentationToWireframe();

      //create the new group node for the frame
      if ( this->actor != NULL )
      {
         this->geode = new cfdGeode();

         //get the geometry information from vtk and convert
         //it to performer geometry
         //then add it to the graph
         this->geode->TranslateTocfdGeode( this->actor );
      }
   }
   else if ( this->frameDataType == GEOM )
   {
      vprDEBUG(vprDBG_ALL,1) << " Geometry data" << std::endl << vprDEBUG_FLUSH;

      float op = 1.0;
      if ( this->param->transientInfo[ this->member ]->trans )
         op = 0.2;

      this->geomFile = new cfdFILE( op, 
                     this->param->transientInfo[ this->member ]->stlColor, 
                     this->vtkFilename );

      vprDEBUG(vprDBG_ALL,1) 
         << " geometry file : " << this->vtkFilename 
         << " : trans = " << this->param->transientInfo[ this->member ]->trans 
         << " : op = " << op
         << " : this->member : " << this->member
         << " : GetGeometryDCS() = " 
         << this->param->transientInfo[ this->member ]->GetGeometryDCS()
         << std::endl << vprDEBUG_FLUSH;

      this->node = this->geomFile->GetcfdNode();
      this->dcs = new cfdDCS();
      Matrix44f m;
      m = this->param->transientInfo[ this->member ]->GetGeometryDCS()->GetMat();
      this->dcs->SetMat( m );
      this->dcs->AddChild( this->node );
   }
   else
   {
      std::cerr << "ERROR: unknown frameDataType = " << this->frameDataType
                << std::endl;
      return 0;
   }

   return 1;
}

void cfdFrame::CreateFrame( char* vtkFileName )
{
   //clear out the pointer
   if( this->vtkFilename )
   {
      delete [] this->vtkFilename;
      this->vtkFilename = NULL;
   }

   this->vtkFilename = new char[ strlen( vtkFileName )+1 ];
   strcpy( this->vtkFilename, vtkFileName );

   if ( ! this->CreateFrame() )
   {
      vprDEBUG(vprDBG_ALL,0) << " ERROR !!! **** Frame not created!!"
                             << std::endl << vprDEBUG_FLUSH;
   }
}

void cfdFrame::SetParameterFile( cfdTransientVizHandler *param, int member)
{
   this->param = param;
   this->member = member;
}

void cfdFrame::SetArrow( vtkPolyData * arrow )
{
   this->arrow = arrow;
}

void cfdFrame::SetArrowSize( float size )
{
   this->arrowSize = size;
}

void cfdFrame::SetActiveDataSets( cfdDataSet * activeDataSet,
                                  cfdDataSet * activeMeshedVolume,
                                  cfdDataSet * activeParticleData,
                                  cfdDataSet * activeSurfaceData )
{
   vprDEBUG(vprDBG_ALL,1) 
      << " cfdFrame::SetActiveDataSets, activeDataSet: " << activeDataSet
      << "\tactiveMeshedVolume: " << activeMeshedVolume
      << "\tactiveParticleData: " << activeParticleData
      << "\tactiveSurfaceData:  " << activeSurfaceData
      << std::endl << vprDEBUG_FLUSH;
   this->activeDataSet = activeDataSet;
   this->activeMeshedVolume = activeMeshedVolume;
   this->activeParticleData = activeParticleData;
   this->activeSurfaceData = activeSurfaceData;
}

cfdNode* cfdFrame::GetcfdNode( void )
{
   vprDEBUG(vprDBG_ALL,2) << " Using frameDataType = " << this->frameDataType
                          << std::endl << vprDEBUG_FLUSH;
   if ( this->frameDataType == GEOM )
   {
      return this->dcs;
   }
   else
   {
      return this->geode;
   }
}

void cfdFrame::SetFrameDataType( int type )
{
   this->frameDataType = type;
   vprDEBUG(vprDBG_ALL,1) << " cfdFrame: frameDataType has been set to "
                          << this->frameDataType
                          << std::endl << vprDEBUG_FLUSH;
}

int cfdFrame::GetFrameDataType()
{
   return this->frameDataType;
}



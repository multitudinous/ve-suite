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
 * File:          $RCSfile: cfdScalarBarActor.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdScalarBarActor.h"
#ifdef _CFDCOMMANDARRAY
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#endif //_CFDCOMMANDARRAY


#include <Performer/pf/pfDCS.h>
#include <Performer/pf.h>
#include <Performer/pf/pfGeode.h>

#include "vtkActorToPF.h"

#include <vtkFloatArray.h>
#include <vtkLookupTable.h>
#include <vtkPointData.h>
#include <vtkTransform.h>
#include <vtkTriangleStrip.h>
#include <vtkUnstructuredGrid.h>
#include <vtkGeometryFilter.h>
#include <vtkTransformFilter.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkVectorText.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vpr/Util/Debug.h>

cfdScalarBarActor::cfdScalarBarActor()
{
  vprDEBUG(vprDBG_ALL,2) << "constructing cfdScalarBarActor" 
                         << std::endl << vprDEBUG_FLUSH;

  // Initialize the all the variables
  this->SetPosition( -5.0f, 6.0f, 0.0f );
  this->zrot = 90;
  this->height = 3.0f;
  this->width = 0.5f;
  this->numColors = 256;
  this->numPts = 2*(this->numColors + 1);
  this->numTextLabels = 5;

  this->lut = vtkLookupTable::New();
  this->lut->SetNumberOfColors(numColors); //default is 256
  this->lut->SetHueRange(2.0f/3.0f, 0.0f); //a blue-to-red scale
  //this->lut->Build();// will build in SetRange

  this->titleTextScale = 0.2f;

  this->titleScalar = vtkVectorText::New();

  this->scalarBar = new pfDCS();
}
 
cfdScalarBarActor::~cfdScalarBarActor()
{
   vprDEBUG(vprDBG_ALL,2) << "deconstructing cfdScalarBarActor"
                          << std::endl << vprDEBUG_FLUSH;

   for ( int i=0; i<this->numTextLabels; i++ )
   {
      this->scalarBar->removeChild( this->pfLabelActor[i] );
      pfDelete( this->pfLabelActor[i] );
   }
   vprDEBUG(vprDBG_ALL,2) << "   scalarBar->getNumChildren() = " 
      << this->scalarBar->getNumChildren() 
      << std::endl << vprDEBUG_FLUSH;

   this->scalarBar->removeChild( this->pfaPolyActor );
   pfDelete( this->pfaPolyActor );

   vprDEBUG(vprDBG_ALL,2) << "   scalarBar->getNumChildren() = " 
      << this->scalarBar->getNumChildren() 
      << std::endl << vprDEBUG_FLUSH;

   this->scalarBar->removeChild( this->pftitleActor );
   pfDelete( this->pftitleActor );

   vprDEBUG(vprDBG_ALL,2) << "   scalarBar->getNumChildren() = " 
      << this->scalarBar->getNumChildren() 
      << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vprDBG_ALL,2) << "   titleScalar->Delete()"
                          << std::endl << vprDEBUG_FLUSH;
   this->titleScalar->Delete();

   vprDEBUG(vprDBG_ALL,2) << "   lut->Delete()" 
                          << std::endl << vprDEBUG_FLUSH;
   this->lut->Delete();
   this->lut = NULL;

   vprDEBUG(vprDBG_ALL,2) << "   pfDelete( scalarBar )"
                          << std::endl << vprDEBUG_FLUSH;
   pfDelete( this->scalarBar );

   vprDEBUG(vprDBG_ALL,2) << "   finished deconstructing cfdScalarBarActor"
                          << std::endl << vprDEBUG_FLUSH;
}

 void cfdScalarBarActor::SetPosition(float x[3])
{
   this->SetPosition(x[0], x[1], x[2]);
}

void cfdScalarBarActor::SetPosition(float x, float y, float z)
{
   this->itsX[0] = x;
   this->itsX[1] = y;
   this->itsX[2] = z;  
}

void cfdScalarBarActor::GetPosition(float x[3])
{
   this->GetPosition(x[0], x[1], x[2]);
}

void cfdScalarBarActor::GetPosition(float &x, float &y, float &z)
{
   x = this->itsX[0];
   y = this->itsX[1];
   z = this->itsX[2];
}

void cfdScalarBarActor::SetZRotation( float rot )
{
   this->zrot = rot;
}

void cfdScalarBarActor::SetWidth(float w)
{
   this->width = w;
}

float cfdScalarBarActor::GetWidth() const
{
   return this->width;
}

void cfdScalarBarActor::SetHeight(float h)
{
   this->height = h;
}

float cfdScalarBarActor::GetHeight() const
{
   return this->height;
}

void cfdScalarBarActor::SetMaximumNumberOfColors(int nC)
{
   this->numColors = nC;
}

int cfdScalarBarActor::GetMaximumNumberOfColors() const
{
   return this->numColors;
}

void cfdScalarBarActor::SetRange(double r[2])
{
   this->SetRange(r[0], r[1]);
}

void cfdScalarBarActor::SetRange(double r0, double r1)
{
   this->range[0] = r0;
   this->range[1] = r1;
   this->dScalar = (this->range[1] - this->range[0])/((float)(this->numPts-2)*0.5);
   this->lut->SetTableRange( this->range[0], this->range[1] );
   //this->lut->Build();
   vprDEBUG(vprDBG_ALL,1) 
      << "cfdScalarBarActor::SetRange, range = " 
      << this->range[0] << " and " << this->range[1]
      << std::endl << vprDEBUG_FLUSH;
}

void cfdScalarBarActor::GetRange(double r[2])
{
   this->GetRange(r[0], r[1]);
}

void cfdScalarBarActor::GetRange(double &r0, double &r1)
{
   r0 = range[0];
   r1 = range[1];
}

void cfdScalarBarActor::SetLookupTable( vtkLookupTable * lookup )
{
   vprDEBUG(vprDBG_ALL,1) << "cfdScalarBarActor::SetLookupTable" 
                          << std::endl << vprDEBUG_FLUSH;

   if ( this->lut )
      this->lut->Delete();

   this->lut = vtkLookupTable::New();
   this->lut->DeepCopy( lookup );
}

vtkLookupTable * cfdScalarBarActor::GetLookupTable()
{
   return this->lut;
}

void cfdScalarBarActor::SetTitleTextScale( float scale )
{
   this->titleTextScale = scale;
}

float cfdScalarBarActor::GetTitleTextScale() const
{
   return this->titleTextScale;
}

void cfdScalarBarActor::SetVtkVectorText( char text[] )
{
   this->titleScalar->SetText( text );
}

void cfdScalarBarActor::Execute()
{
   int i;

   vtkPoints *polyPoints = vtkPoints::New();

   vtkFloatArray *polyScalars = vtkFloatArray::New();
   polyScalars->SetNumberOfTuples( this->numPts );

   vtkTriangleStrip *aPoly = vtkTriangleStrip::New();
   ( aPoly->GetPointIds() )->SetNumberOfIds( this->numPts );

   // for all the even points -- left side of bar?
   float scalarValue = this->range[0];
   for ( i=0; i<this->numPts; i+=2 )
   {
      polyPoints->InsertPoint( i, 0.0, 0.0, (float)i/(float)(this->numPts-2) );

      polyScalars->InsertComponent( i,0,scalarValue );

      scalarValue += this->dScalar;
      ( aPoly->GetPointIds() )->SetId( i, i );
   }

   // for all the odd points -- right side of bar?
   scalarValue = this->range[0];
   //for ( i=1; i<=this->numPts; i+=2 )
   for ( i=1; i<this->numPts; i+=2 )
   {
      // use same z-coordinate as previous even point
      polyPoints->InsertPoint( i, this->width, 0.0, (float)(i-1)/(float)(this->numPts-2) );

      polyScalars->InsertComponent( i,0, scalarValue );

      scalarValue += this->dScalar;
      ( aPoly->GetPointIds() )->SetId( i, i );
   }

   vtkUnstructuredGrid *aPolyGrid = vtkUnstructuredGrid::New();
   aPolyGrid->Allocate( 1, 1 );
   aPolyGrid->InsertNextCell( aPoly->GetCellType(), aPoly->GetPointIds() );
   aPolyGrid->SetPoints( polyPoints );
   ( aPolyGrid->GetPointData() )->SetScalars( polyScalars );

   vtkTransform *aPolyTransform = vtkTransform::New();
   aPolyTransform->RotateZ( this->zrot );
   aPolyTransform->Translate( this->itsX[0], this->itsX[1], this->itsX[2] );
   aPolyTransform->Scale( 1.0f, 1.0f, this->height );

   vtkTransformFilter *transformPoly = vtkTransformFilter::New();
   transformPoly->SetInput( aPolyGrid );
   transformPoly->SetTransform( aPolyTransform );

   vtkGeometryFilter *aPolyFilter = vtkGeometryFilter::New();
   aPolyFilter->SetInput( transformPoly->GetOutput() );

   this->lut->Build();

   vtkPolyDataMapper *aPolyMapper = vtkPolyDataMapper::New();
   aPolyMapper->SetInput( aPolyFilter->GetOutput() );
   aPolyMapper->SetScalarRange( this->range );
   aPolyMapper->SetLookupTable( this->lut );
   aPolyMapper->Update();

   vtkActor *aPolyActor = vtkActor::New();
   aPolyActor->SetMapper( aPolyMapper ); 

   polyPoints->Delete();
   polyScalars->Delete();
   aPoly->Delete();
   aPolyGrid->Delete();
   aPolyTransform->Delete();
   transformPoly->Delete();
   aPolyFilter->Delete();
   aPolyMapper->Delete();

   // creating the title for scalar bar properties
   vtkTransform *titleTransform = vtkTransform::New();
   titleTransform->Identity();
   titleTransform->RotateZ( this->zrot );
   titleTransform->RotateX( 90.0f );
   titleTransform->Translate( this->itsX[0], 
                              this->itsX[2] + this->height*1.1f,
                             -this->itsX[1] );

   titleTransform->Scale( this->titleTextScale,
                          this->titleTextScale,
                          this->titleTextScale );

   vtkTransformPolyDataFilter *titleFilter = vtkTransformPolyDataFilter::New();
   titleFilter->SetTransform( titleTransform );

   titleFilter->SetInput( titleScalar->GetOutput() );

   vtkPolyDataMapper *titleMapper = vtkPolyDataMapper::New();
   titleMapper->SetInput( titleFilter->GetOutput() );

   vtkActor *titleActor = vtkActor::New();
   titleActor->SetMapper( titleMapper );

   vtkVectorText **labelScalar = new vtkVectorText * [ this->numTextLabels ];
   for ( i=0; i<this->numTextLabels; i++ )
   {
      labelScalar[i] = vtkVectorText::New();
      this->pfLabelActor[i] = new pfGeode;
   }
   
   // creating the numerical labels on the scalar bar legend
   char ** labelText = new char * [ this->numTextLabels ];
   for ( i = 0; i < this->numTextLabels; i ++ )
   {
      labelText[i] = new char[20];  // enough space for a formatted number
   }

   float labelIncrement = (this->range[1] - this->range[0]) / 
                          (float)(this->numTextLabels - 1);

   vtkTransform *labelTransform = vtkTransform::New();
   vtkTransformPolyDataFilter *labelFilter = vtkTransformPolyDataFilter::New();
   vtkPolyDataMapper *labelMapper = vtkPolyDataMapper::New();
   vtkActor *labelActor = vtkActor::New();

   for ( i=0; i<this->numTextLabels; i++ )
   {
      if      ( range[1]-range[0] < 0.0016 )  
         sprintf( labelText[i], "%8.3e", this->range[0] + (float)i*labelIncrement );
      else if ( range[1]-range[0] < 0.016 )  
         sprintf( labelText[i], "%8.4f", this->range[0] + (float)i*labelIncrement );
      else if ( range[1]-range[0] < 0.16 ) 
         sprintf( labelText[i], "%8.3f", this->range[0] + (float)i*labelIncrement );
      else if ( range[1]-range[0] < 1.6 ) 
         sprintf( labelText[i], "%8.2f", this->range[0] + (float)i*labelIncrement );
      else if ( range[1]-range[0] < 16 ) 
         sprintf( labelText[i], "%8.2f", this->range[0] + (float)i*labelIncrement );
      else                       
         sprintf( labelText[i], "%8.0f", this->range[0] + (float)i*labelIncrement );

      labelTransform->Identity();
      labelTransform->RotateZ( this->zrot );
      labelTransform->RotateX( 90.0f );
      float dLabel = this->height/(float)(this->numTextLabels - 1);
      labelTransform->Translate( this->itsX[0]+width, this->itsX[2] + (float)i*dLabel, -this->itsX[1] );
      float labelScale = this->height / 15.0;
      labelTransform->Scale( labelScale, labelScale, labelScale );

      labelScalar[i]->SetText( labelText[i] );

      labelFilter->SetTransform( labelTransform );

      labelFilter->SetInput( labelScalar[i]->GetOutput() );
      labelMapper->SetInput( labelFilter->GetOutput() );
      labelActor->SetMapper( labelMapper );

      vtkActorToPF(labelActor, this->pfLabelActor[i]);
      this->scalarBar->addChild(this->pfLabelActor[i]);
   }

   for ( i = 0; i < this->numTextLabels; i ++ )
   {
      delete [] labelText[i];
   }
   delete [] labelText;

   this->pfaPolyActor = new pfGeode;
   vtkActorToPF(aPolyActor, this->pfaPolyActor);
   this->scalarBar->addChild(this->pfaPolyActor); 

   this->pftitleActor = new pfGeode;
   vtkActorToPF(titleActor, this->pftitleActor);
   this->scalarBar->addChild(this->pftitleActor); 

   aPolyActor->Delete();
   titleTransform->Delete();
   titleFilter->Delete();
   titleMapper->Delete();
   titleActor->Delete();
   labelTransform->Delete();
   labelFilter->Delete();
   labelMapper->Delete();
   labelActor->Delete();
   for ( i=0; i<this->numTextLabels; i++ )
   {
      labelScalar[i]->Delete();
   }
   delete [] labelScalar; 
   labelScalar = NULL;
}

pfDCS * cfdScalarBarActor::getpfDCS(void )
{
   return this->scalarBar;
}


#ifdef _CFDCOMMANDARRAY
bool cfdScalarBarActor::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( CFD_ID ) == SCALAR_BAR_TOGGLE )
   {
      if ( this->scalarBarActor )
      { 
          this->rootNode->removeChild( this->scalarBarActor->getpfDCS() );
          //this->worldDCS->removeChild( this->scalarBarActor->getpfDCS() );
          delete this->scalarBarActor;
          this->scalarBarActor = NULL;
      }
      else
      { 
          RefreshScalarBar();
      }

      this->setId( -1 );
   }
   return flag;
}

void cfdScalarBarActor::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << endl;
}

void cfdScalarBarActor::RefreshScalarBar()
{
   if ( this->scalarBarActor )
   {
      this->rootNode->removeChild( this->scalarBarActor->getpfDCS() );
      //this->worldDCS->removeChild( this->scalarBarActor->getpfDCS() );
      delete this->scalarBarActor;
      this->scalarBarActor = NULL;
   }

   if ( cfdObjects::GetActiveDataSet() == NULL ||
        cfdObjects::GetActiveDataSet()->GetNumberOfScalars() == 0 )
   {
      vprDEBUG(vprDBG_ALL,0) << " RefreshScalarBar: no data" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   this->scalarBarActor = new cfdScalarBarActor();

   // if the param file specified scalarBar settings, apply them here...
   if ( this->paramReader->scalarBarH != 0.0 )
   {
      this->scalarBarActor->SetPosition( this->paramReader->scalarBarPos );
      this->scalarBarActor->SetZRotation( this->paramReader->scalarBarZRot );
      this->scalarBarActor->SetHeight( this->paramReader->scalarBarH );
      this->scalarBarActor->SetWidth( this->paramReader->scalarBarW );
      this->scalarBarActor->SetTitleTextScale( 
                                     this->paramReader->scalarBarH / 15.0 );
   }

   this->scalarBarActor->SetRange( 
                  cfdObjects::GetActiveDataSet()->GetDisplayedScalarRange() );

   this->scalarBarActor->SetLookupTable( 
                  cfdObjects::GetActiveDataSet()->GetLookupTable() );

   vprDEBUG(vprDBG_ALL,1) << " RefreshScalarBar: " 
      << "cfdObjects::GetActiveDataSet()->GetLookupTable() = "
      << cfdObjects::GetActiveDataSet()->GetLookupTable()
      << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL,1) << "RefreshScalarBar: " 
      << "cfdObjects::GetActiveDataSet()->GetParent()->GetLookupTable() = "
      << cfdObjects::GetActiveDataSet()->GetParent()->GetLookupTable()
      << std::endl << vprDEBUG_FLUSH;

   // give a name to display over the scalarBar
   static char legend[50];
   strcpy( legend, cfdObjects::GetActiveDataSet()->GetDataSet()
                       ->GetPointData()->GetScalars()->GetName() );

   vprDEBUG(vprDBG_ALL,1) << "RefreshScalarBar: " 
                          << "desired scalar bar name: " << legend 
                          << std::endl << vprDEBUG_FLUSH;

   this->scalarBarActor->SetVtkVectorText( legend );

   this->scalarBarActor->Execute();

   // give the scalarBar DCS a name so that it can be detected during a CLEAR_ALL
   this->scalarBarActor->getpfDCS()->setName("scalarBar");
   this->rootNode->addChild( this->scalarBarActor->getpfDCS() );
   //this->worldDCS->addChild( this->scalarBarActor->getpfDCS() );
}

#endif //_CFDCOMMANDARRAY

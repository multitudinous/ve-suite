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
 * File:          $RCSfile: cfdMenu.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// .NAME cfdMenu - create menu objects.
// .SECTION Description
// A class to build a virtual menu using vtk functions and 
// render using Performer.  VTK objects(vtkActor) are translated into
// Performer objects(pfGeode).
#include "cfdMenu.h"
#include <Performer/pf/pfDCS.h>
#include "vtkActorToPF.h"

#include <vtkTransform.h>
#include <vtkUnstructuredGrid.h>
#include <vtkAppendPolyData.h>
#include <vtkCellCenters.h>
#include <vtkCubeSource.h>
#include <vtkGeometryFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkVectorText.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
//#include <vtkPointSet.h>
#include <vtkPolyData.h>

#include <vpr/Util/Debug.h>

cfdMenu::cfdMenu( char *menuFile, char *menuConfig )
{
   this->id = 0;

   // Read menu data
   this->reader = vtkUnstructuredGridReader::New();
   this->reader->SetFileName( menuFile );
   this->reader->Update();

   this->grid = ( vtkUnstructuredGrid * ) reader->GetOutput();
   this->grid->GetBounds( this->bound );

   this->cell = vtkCubeSource::New();
   this->cellFilter = vtkGeometryFilter::New();
   this->cellNormal = vtkPolyDataNormals::New();
   this->cellMapper = vtkPolyDataMapper::New();
   this->cellActor = vtkActor::New();

   this->outlineGeode = new pfGeode;
   this->shadedGeode = new pfGeode;
   this->labelGeode = new pfGeode;
   this->cellGeode = new pfGeode;

   this->menuDCS = new pfDCS;
   this->menuDCS->addChild( this->GetOutline() );
   this->menuDCS->addChild( this->GetShaded() );
   this->menuDCS->addChild( this->GetLabel( menuConfig ) );
   this->menuDCS->addChild( this->GetCell() );
   this->menuDCS->setTrans(0.0,0.0,-2.0);   //to fit Deere's screen
}


cfdMenu::~cfdMenu()
{
   this->reader->Delete();
   this->cell->Delete();
   this->cellFilter->Delete();
   this->cellNormal->Delete();
   this->cellMapper->Delete();
   this->cellActor->Delete();

   pfDelete( this->outlineGeode );
   pfDelete( this->shadedGeode );
   pfDelete( this->labelGeode );
   pfDelete( this->cellGeode );
   pfDelete( this->menuDCS );
}


pfDCS * cfdMenu::GetpfDCS()
{
   return this->menuDCS;
}


void cfdMenu::UpdateHitCell( double x[3] )
{
   double temp_x[3];
   double bounds[6];
   
   temp_x[0] = x[0];
   temp_x[1] = x[1];
   temp_x[2] = x[2] + 2.0;
   
   int i;

   for ( i = 0; i < this->grid->GetNumberOfCells(); i++ ) 
   {
      this->grid->GetCellBounds( i, bounds );

      vprDEBUG(vprDBG_ALL, 3) << " cfdMenu : " << bounds[0] << " : "
         << bounds[1] << " : " << bounds[2] << " : " << bounds[3] << " : "
         << bounds[4] << " : " << bounds[5] << std::endl << vprDEBUG_FLUSH;

      if ( temp_x[0] > bounds[0] && temp_x[0] < bounds[1] )
      {
         if ( temp_x[2] > bounds[4] && temp_x[2] < bounds[5] ) 
         {
            this->id = i;
            break;
         }
      }
   }
  
   /*
   // WARNING!!!! -> This method is NOT thread safe
   int   subID;
   float pcoords[3];
   float *weights = NULL;
   this->id = this->grid->FindCell( temp_x, NULL, cell, -1, 0.00001f, subID, pcoords, weights ); //to fit Deere's screen
   */

   if ( this->id >= 0 )
   {
      this->UpdateCell();
   }
}


double * cfdMenu::GetBound()
{
  return this->bound;
}


int cfdMenu::GetCellId()
{
  return this->id;
}


void cfdMenu::UpdateCell()
{
   double *temp1;
   double  temp2[ 6 ];
   temp1 = this->grid->GetCell( this->id )->GetBounds();
   for ( int i = 0; i < 6; i++ )
      temp2[ i ] = temp1[ i ];
  cell->SetBounds( temp2 );
  
  //cellActor->Update();
  cellActor->GetMapper()->GetInput()->Update(); //changed to fit vtk4.0

  vtkActorToPF( this->cellActor, this->cellGeode );
}


pfGeode * cfdMenu::GetOutline()
{
  vtkGeometryFilter *cFilter = vtkGeometryFilter::New();
     cFilter->SetInput( this->grid );

  vtkPolyDataNormals *cNormal = vtkPolyDataNormals::New();
     cNormal->SetInput( cFilter->GetOutput() );

  vtkPolyDataMapper *cMapper = vtkPolyDataMapper::New();
     cMapper->SetInput( cNormal->GetOutput() );
     cMapper->ScalarVisibilityOff();

  vtkActor *cActor = vtkActor::New();
     cActor->SetMapper( cMapper );
     cActor->GetProperty()->SetColor( 1.0f, 1.0f, 1.0f );
     cActor->GetProperty()->SetAmbient( 0.2f );
     cActor->GetProperty()->SetDiffuse( 0.8f );
     cActor->GetProperty()->SetSpecular( 0.3f );
     cActor->GetProperty()->SetSpecularPower( 20.0f );
     cActor->GetProperty()->SetRepresentationToWireframe();

  vtkActorToPF( cActor, this->outlineGeode );

  cFilter->Delete();
  cNormal->Delete();
  cMapper->Delete();
  cActor->Delete();

  return this->outlineGeode;
}


pfGeode * cfdMenu::GetShaded(  )
{
  vtkGeometryFilter *cFilter = vtkGeometryFilter::New();
     cFilter->SetInput( this->grid );

  vtkPolyDataNormals *cNormal = vtkPolyDataNormals::New();
     cNormal->SetInput( cFilter->GetOutput() );

  vtkPolyDataMapper *cMapper = vtkPolyDataMapper::New();
     cMapper->SetInput( cNormal->GetOutput() );
     cMapper->ScalarVisibilityOff();

  vtkActor *cActor = vtkActor::New();
     cActor->SetMapper( cMapper );
     cActor->GetProperty()->SetColor( 0.0f, 0.0f, 1.0f );
     cActor->GetProperty()->SetAmbient( 0.2f );
     cActor->GetProperty()->SetDiffuse( 0.8f );
     cActor->GetProperty()->SetSpecular( 0.3f );
     cActor->GetProperty()->SetSpecularPower( 20.0f );
     cActor->GetProperty()->SetOpacity( 0.2f );

  vtkActorToPF( cActor, this->shadedGeode );

  cFilter->Delete();
  cNormal->Delete();
  cMapper->Delete();
  cActor->Delete();

  return this->shadedGeode;
}

pfGeode * cfdMenu::GetLabel( char* menuConfig )
{
   int  MAX_MENU_ARRAY;
   char menuText[ 30 ][ 50 ];
   char tag[ 20 ];
   char menu_item_text[ 15 ];
   int  menu_item_number;

   FILE *fp;
   fp = fopen( menuConfig,"r");
   if (fp)
   {
      fscanf(fp,"%s%d\n",tag,&MAX_MENU_ARRAY);

      for ( int i = 0; i < MAX_MENU_ARRAY; i++)
      {
         fscanf(fp,"%s%d%s\n",tag,&menu_item_number,menu_item_text);
         if (tag == "MENU_ITEM");
         sprintf(menuText[menu_item_number], menu_item_text);
      }
   }
   else
   {
      vprDEBUG(vprDBG_ALL, 0) << "Can not find the config file!"
                              << std::endl << vprDEBUG_FLUSH;
   }

   fclose(fp);
   // Render menu
   int   maxNoOfCell = this->grid->GetNumberOfCells();
   double menuPos[3];

   // Get cell center of each cells for text labelling
   vtkCellCenters *centers = vtkCellCenters::New();
   centers->SetInput( this->grid );
   centers->VertexCellsOn();
   centers->Update();

   vtkAppendPolyData *textAppend = vtkAppendPolyData::New();

   // Looping to set the texts into polydata
   for ( int i=0; i<MAX_MENU_ARRAY; i++ )
   {
      if ( i < maxNoOfCell )
      {
         ((vtkPolyData*)centers->GetOutput())->GetPoint( i, menuPos );
         vtkPolyData * pData = this->GetText( menuText[i], menuPos );
         textAppend->AddInput( pData );
         pData->Delete();
      }
   }

   vtkPolyDataMapper *textMapper = vtkPolyDataMapper::New();
   textMapper->SetInput( textAppend->GetOutput() );

   vtkActor *textActor = vtkActor::New();
   textActor->SetMapper( textMapper );
   textActor->GetProperty()->SetColor( 1.0f, 1.0f, 1.0f );
   textActor->GetProperty()->SetSpecularPower( 40.0f );

   vtkActorToPF( textActor, this->labelGeode );

   centers->Delete();
   textAppend->Delete();
   textMapper->Delete();
   textActor->Delete();

   return this->labelGeode;
}  


vtkPolyData * cfdMenu::GetText( char menuText[ ], double tPos[3] )
{
   vtkVectorText *atext = vtkVectorText::New();
   atext->SetText( menuText );

   vtkTransform *aLabelTransform = vtkTransform::New();
   aLabelTransform->Identity();
   aLabelTransform->Translate( (float)tPos[0]-0.45f, (float)tPos[1], (float)tPos[2]+0.25f );
   aLabelTransform->RotateX( 90.0f );
   aLabelTransform->Scale( 0.07f, 0.07f, 0.07f );

   vtkTransformPolyDataFilter *labelTransform = vtkTransformPolyDataFilter::New();
   labelTransform->SetTransform( aLabelTransform );
   labelTransform->SetInput( atext->GetOutput() );
   labelTransform->Update();

   atext->Delete();
   aLabelTransform->Delete();

   vtkPolyData * pData = vtkPolyData::New();
   pData->DeepCopy( labelTransform->GetOutput() );
   labelTransform->Delete();

   return pData;
}


pfGeode * cfdMenu::GetCell()
{
   double *temp1;
   double  temp2[ 6 ];
   temp1 = this->grid->GetCell( this->id )->GetBounds();
   for ( int i = 0; i < 6; i++ )
      temp2[ i ] = temp1[ i ];
  cell->SetBounds( temp2 );
  //cell->SetBounds( this->grid->GetCell( this->id )->GetBounds() );

  cellFilter->SetInput( cell->GetOutput() );

  cellNormal->SetInput( cellFilter->GetOutput() );

  cellMapper->SetInput( cellNormal->GetOutput() );

  cellActor->SetMapper( cellMapper );

  cellActor->GetProperty()->SetColor( 1.0f, 1.0f, 1.0f );
  cellActor->GetProperty()->SetAmbient( 0.2f );
  cellActor->GetProperty()->SetDiffuse( 0.8f );
  cellActor->GetProperty()->SetSpecular( 0.3f );
  cellActor->GetProperty()->SetSpecularPower( 20.0f );
  cellActor->GetProperty()->SetOpacity( 0.2f );

  //cellActor->Update();
  cellActor->GetMapper()->GetInput()->Update();

  vtkActorToPF( this->cellActor, this->cellGeode );

  return this->cellGeode;
}

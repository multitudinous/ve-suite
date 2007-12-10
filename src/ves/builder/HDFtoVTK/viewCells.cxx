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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/builder/HDFtoVTK/viewCells.h>

#include <vtkPoints.h>
#include <vtkUnstructuredGrid.h>
#include <vtkShrinkFilter.h>        // for visualization
#include <vtkStripper.h>            // for visualization
#include <vtkDataSetMapper.h>         // for visualization
#include <vtkActor.h>              // for visualization
#include <vtkRenderWindowInteractor.h>   // for visualization
#include <vtkPolyDataMapper.h>  // for visualization
#include <vtkRectilinearGrid.h> // for visualization
#include <vtkRectilinearGridGeometryFilter.h>   // for visualization
#include <vtkCubeSource.h>
#include <vtkOutlineCornerFilter.h>
#include <vtkVectorText.h>
#include <vtkFollower.h>

#include <vtkIdList.h>
#include <vtkGenericCell.h>
#include <vtkRenderer.h>
#include <vtkRenderWindow.h>
#include <vtkPolyData.h>
#include <vtkCamera.h>
#include <vtkProperty.h>
#include <vtkCellArray.h>
#include <vtkScalarsToColors.h>
#include <vtkLookupTable.h>

#include <iostream>
void viewCells( vtkDataSet *dataset, const float shrinkFactor )
{
    std::cout << "\nviewCells: Preparing to view mesh..." << std::endl;
    int numCells = dataset->GetNumberOfCells();
    std::cout << "     The number of cells is " << numCells << std::endl;
    int numPts = dataset->GetNumberOfPoints();
    std::cout << "     The number of points is " << numPts << std::endl;

    if( numCells == 0 ) return;

    //Create one-time graphics stuff
    vtkRenderer* ren1 = vtkRenderer::New();
    vtkRenderWindow* renWin = vtkRenderWindow::New();
    renWin->AddRenderer( ren1 );
    vtkRenderWindowInteractor* iren = vtkRenderWindowInteractor::New();
    iren->SetRenderWindow( renWin );

    //Create actor from dataset and add to the renderer
    vtkActor *actor = AddToRenderer( dataset, ren1, shrinkFactor );

    vtkActor * axesActor = NULL;
    vtkFollower * xActor = NULL;
    vtkFollower * yActor = NULL;
    vtkFollower * zActor = NULL;
    vtkActor * C6Actor = NULL;

    int hasCaveCorners = 1;
    if( hasCaveCorners )
    {
        // Create the cube corners and the associated mapper and actor.
        vtkCubeSource * C6 = vtkCubeSource::New();
        C6->SetBounds( -5, 5, -5, 5, 0, 10 );
        vtkOutlineCornerFilter * C6CornerFilter = vtkOutlineCornerFilter::New();
        C6CornerFilter->SetInput( C6->GetOutput() );
        vtkPolyDataMapper * C6Mapper = vtkPolyDataMapper::New();
        C6Mapper->SetInput( C6CornerFilter->GetOutput() );

        C6Actor = vtkActor::New();
        C6Actor->SetMapper( C6Mapper );

        // intermediate cleanup
        C6->Delete();
        C6CornerFilter->Delete();
        C6Mapper->Delete();

        // Create the axes symbol actor.
        axesActor = vtkActor::New();
        GetAxesSymbol( axesActor );

        // Create the axes labels
        xActor = vtkFollower::New();
        yActor = vtkFollower::New();
        zActor = vtkFollower::New();
        GetAxesLabels( xActor, yActor, zActor );

        // Add the actors to the renderer.
        ren1->AddActor( C6Actor );
        ren1->AddActor( axesActor );
        ren1->AddActor( xActor );
        ren1->AddActor( yActor );
        ren1->AddActor( zActor );

        // Force the axis labels to always face camera
        xActor->SetCamera( ren1->GetActiveCamera() );
        yActor->SetCamera( ren1->GetActiveCamera() );
        zActor->SetCamera( ren1->GetActiveCamera() );

        // Set the position of the camera in world coordinates. The default position is (0,0,1).
        ren1->GetActiveCamera()->SetPosition( 0, -10, 5 );

        //Set the view up direction for the camera. The default is (0,1,0)
        ren1->GetActiveCamera()->SetViewUp( 0, 0, 1 );

        ren1->GetActiveCamera()->Zoom( 0.3 );
    }


    // Reset the clipping range of the camera; set the camera of the follower; render.
    ren1->ResetCameraClippingRange();

    std::cout << "\nWith cursor on the graphics window, press 'e' to exit the viewer" << std::endl;

    // interact with data
    renWin->SetSize( 800, 800 );
    renWin->Render();
    iren->Start();

    // delete all the instances that have been created.
    if( hasCaveCorners )
    {
        C6Actor->Delete();
        axesActor->Delete();
        xActor->Delete();
        yActor->Delete();
        zActor->Delete();
    }

    actor->Delete();
    ren1->Delete();
    renWin->Delete();
    iren->Delete();
}
void GetAxesSymbol( vtkActor * axesActor )
{
    // Create the axes and the associated mapper and actor.
    vtkPoints *newPts = vtkPoints::New();
    newPts->Allocate( 6 );

    vtkCellArray *newLines = vtkCellArray::New();
    newLines->Allocate( 3 );

    float x[3];
    vtkIdType pts[2];

    pts[0] = 0;
    x[0] = 0;
    x[1] = 0;
    x[2] = 0;
    newPts->InsertPoint( pts[0], x );

    pts[1] = 1;
    x[0] = 1;
    x[1] = 0;
    x[2] = 0;
    newPts->InsertPoint( pts[1], x );
    newLines->InsertNextCell( 2, pts );

    pts[0] = 2;
    x[0] = 0;
    x[1] = 0;
    x[2] = 0;
    newPts->InsertPoint( pts[0], x );

    pts[1] = 3;
    x[0] = 0;
    x[1] = 1;
    x[2] = 0;
    newPts->InsertPoint( pts[1], x );
    newLines->InsertNextCell( 2, pts );

    pts[0] = 4;
    x[0] = 0;
    x[1] = 0;
    x[2] = 0;
    newPts->InsertPoint( pts[0], x );

    pts[1] = 5;
    x[0] = 0;
    x[1] = 0;
    x[2] = 1;
    newPts->InsertPoint( pts[1], x );
    newLines->InsertNextCell( 2, pts );

    vtkPolyData *output = vtkPolyData::New();
    output->SetPoints( newPts );
    newPts->Delete();

    output->SetLines( newLines );
    newLines->Delete();

    vtkPolyDataMapper * axesMapper = vtkPolyDataMapper::New();
    axesMapper->SetInput( output );

    axesActor->SetMapper( axesMapper );

    output->Delete();
    axesMapper->Delete();
}


void GetAxesLabels( vtkFollower * xActor, vtkFollower * yActor, vtkFollower * zActor )
{
    // Create the 3D text and the associated mapper and follower
    vtkVectorText * xText = vtkVectorText::New();
    xText->SetText( "X" );
    vtkVectorText * yText = vtkVectorText::New();
    yText->SetText( "Y" );
    vtkVectorText * zText = vtkVectorText::New();
    zText->SetText( "Z" );

    vtkPolyDataMapper * xMapper = vtkPolyDataMapper::New();
    xMapper->SetInput( xText->GetOutput() );

    xActor->SetMapper( xMapper );
    xActor->SetScale( 0.2, 0.2, 0.2 );
    xActor->AddPosition( 1, -0.1, 0 );

    vtkPolyDataMapper * yMapper = vtkPolyDataMapper::New();
    yMapper->SetInput( yText->GetOutput() );

    yActor->SetMapper( yMapper );
    yActor->SetScale( 0.2, 0.2, 0.2 );
    yActor->AddPosition( 0, 1 - 0.1, 0 );

    vtkPolyDataMapper * zMapper = vtkPolyDataMapper::New();
    zMapper->SetInput( zText->GetOutput() );

    zActor->SetMapper( zMapper );
    zActor->SetScale( 0.2, 0.2, 0.2 );
    zActor->AddPosition( 0, -0.1, 1 );

    xText->Delete();
    yText->Delete();
    zText->Delete();

    xMapper->Delete();
    yMapper->Delete();
    zMapper->Delete();
}
vtkActor * AddToRenderer( vtkDataSet *dataset, vtkRenderer* ren1, const float shrinkFactor )
{
    vtkActor * actor = NULL;

    int numCells = dataset->GetNumberOfCells();

    if( numCells == 0 )
    {
        std::cout << "\tNothing to plot in AddToRenderer: The number of cells is " << numCells << std::endl;
        return actor;
    }

    vtkDataSetMapper *map = vtkDataSetMapper::New();
//    By default, VTK uses OpenGL display lists which results in another copy of the data being stored
//    in memory. For most large datasets you will be better off saving memory by not using display lists.
//    You can turn off display lists by turning on ImmediateModeRendering.
    map->ImmediateModeRenderingOn();

    std::cout << "Using shrinkFactor = " << shrinkFactor << std::endl;

    vtkShrinkFilter *shrink = NULL;
    if( shrinkFactor > 0.0 && shrinkFactor < 1.0 )
    {
        //cout << "Using shrinkFactor = " << shrinkFactor << std::endl;
        shrink = vtkShrinkFilter::New();
        shrink->SetInput( dataset );
        shrink->SetShrinkFactor( shrinkFactor );
        shrink->GetOutput()->ReleaseDataFlagOn();

        map->SetInput( shrink->GetOutput() );    //don't use this with stock version of vtk3.2: has code error!
    }
    else map->SetInput( dataset );


    actor = vtkActor::New();
    actor->SetMapper( map );
    actor->GetProperty()->SetColor( 1, 0, 0 );

    //Add the actors to the renderer, set the viewport and background
    ren1->AddActor( actor );

    if( shrink ) shrink->Delete();
    map->Delete();

    return actor;

}

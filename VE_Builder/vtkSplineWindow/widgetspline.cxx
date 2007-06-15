/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
//#include "vktObject.h"
#include "vtkCommand.h"
#include "vtkInteractorStyleImage.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkProperty.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"
#include "vtkSplineWidget.h"
#include <iostream>
using std::cout;
using std::cin;
using std::endl;


int main()
{
int NumberInputPoints=4, NumberOutputPoints=200, Window=500;
double xValue, yValue, zValue = 0;


//user inputs coordinates for NumberInputPoints 
vtkPoints* inputPoints = vtkPoints::New();

for (int i =0; i < NumberInputPoints; i++)
		
	{
		cout <<"Enter in X coordinate of point " << i + 1 << endl;
		cin>> xValue;
		
		
		cout <<"Enter in Y coordinate of point " << i + 1 << endl;
		cin>> yValue;
		
		
		cout << "Coordinate (" << xValue << ","<< yValue <<") entered."<<endl;
		
		inputPoints->InsertPoint(i,xValue,yValue,zValue);
		}


// a renderer and render window
vtkRenderer *ren1 = vtkRenderer::New();
vtkRenderWindow *renWin = vtkRenderWindow::New();
  renWin->AddRenderer(ren1);


// an interactor
vtkRenderWindowInteractor *iren = vtkRenderWindowInteractor::New();
  iren->SetRenderWindow(renWin);
  /*
  iren->RemoveObserver(16);
  iren->RemoveObserver(17);
  iren->SetDolly(0);
  iren->RemoveObserver(vtkCommand::RightButtonPressEvent);  
  iren->RemoveObserver(vtkCommand::RightButtonReleaseEvent);

  vtkObject::RemoveObserver(vtkCommand::MiddleButtonPressEvent);
  vtkObject::RemoveObserver(vtkCommand::MiddleButtonReleaseEvent);
  iren->AddObserver(vtkCommand::RightButtonReleaseEvent, DisableEvent);
  iren->AddObserver(vtkCommand::RightButtonPressEvent, DisableEvent );  
  iren->AddObserver(vtkCommand::MiddleButtonPressEvent, DisableEvent );
  iren->AddObserver(vtkCommand::MiddleButtonReleaseEvent, DisableEvent );
  */

  
  
  
//prevent rotation, vtkInteractionStyleImage renders an imageActor
//with a view that is always normal to the XY plane  
vtkInteractorStyleImage *isi = vtkInteractorStyleImage::New();
  /*isi->RemoveObservers(vtkCommand::PickEvent);
  isi->RemoveObservers(vtkCommand::InteractionEvent);
  isi->RemoveObservers(vtkCommand::StartPickEvent);
  isi->RemoveObservers(vtkCommand::RightButtonReleaseEvent);
  isi->RemoveObservers(vtkCommand::RightButtonPressEvent);  
  isi->RemoveObservers(vtkCommand::MiddleButtonPressEvent);
  isi->RemoveObservers(vtkCommand::MiddleButtonReleaseEvent);
  isi->AddObserver(vtkCommand::RightButtonReleaseEvent, NULL );
  isi->AddObserver(vtkCommand::RightButtonPressEvent, NULL );  
  isi->AddObserver(vtkCommand::MiddleButtonPressEvent, NULL );
  isi->AddObserver(vtkCommand::MiddleButtonReleaseEvent, NULL );
  */
  iren->SetInteractorStyle(isi);
  
  
  
//need some subclass of vtkDataSet (such as vtkPolyData) as defined 
//for input by the spline widget class
vtkPolyData *poly = vtkPolyData::New();
  poly->SetPoints(inputPoints);
    
     
//handle properties
vtkProperty* handleProperty = vtkProperty::New();
  handleProperty->SetColor(1, 0, 0);
  handleProperty->SetDiffuse(0.7);
  handleProperty->SetSpecular(0.4);
  handleProperty->SetSpecularPower(20);

//line properties
vtkProperty* lineProperty = vtkProperty::New();
  lineProperty->SetColor(0, 0, 0);
  lineProperty->SetDiffuse(0.7);
  lineProperty->SetSpecular(0.4);
  lineProperty->SetSpecularPower(20);
  lineProperty->SetLineWidth(2.0);


//Create Spline Widget. 
//
//First the SetInteractor() function must be invoked to define which interactor
//the widget will use (This case only one). Then the input from a vtkDataSet
// must be called for data definition.
//
// Initializing the handles allows the user to create
//and define all the points on the spline with a callback to a vtkPoints
//object. 
//
//By turning the KeyPressActivationOff, the user will not be able to
//turn the widgets on or off (done with the letter 'i'). However, the spline
//widget is turned on by the spline->On(); command so the window brings up the
//spline initially.
//
//Increasing the resolution will control how many line segments are used
//to define the spline.
  
vtkSplineWidget* spline = vtkSplineWidget::New();
  spline->SetInteractor(iren);
  spline->SetInput(poly);
  //spline->SetProjectionNormalToZAxes();
  spline->InitializeHandles(inputPoints);  
  spline->SetPriority(1.0);
  spline->KeyPressActivationOff();
  spline->SetHandleProperty(handleProperty);
  spline->SetLineProperty(lineProperty);
  spline->SetHandleSize(0.007);
  spline->SetResolution(NumberOutputPoints);
  spline->On();
  
  
  
//set background color
 ren1->SetBackground(1, 1, 1);


//Set size of the renderwindow
 renWin->SetSize(Window,Window);


//render
 iren->Initialize();
 renWin->Render();


//mouse interaction
 iren->Start();


//clean up instances created
  spline->Delete();
  iren->Delete();
  ren1->Delete();
  renWin->Delete();
  inputPoints->Delete();
  lineProperty->Delete();
  handleProperty->Delete();
  poly->Delete();
  isi->Delete();


    
return 0;
}



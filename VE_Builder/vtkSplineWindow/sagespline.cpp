//#include "vktObject.h"
#include "vtkCamera.h"
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
vtkPoints* inputPoints1 = vtkPoints::New();
vtkPoints* inputPoints2 = vtkPoints::New();
vtkPoints* inputPoints3 = vtkPoints::New();

inputPoints1->InsertPoint(0, 0, 0.7 ,0);
inputPoints1->InsertPoint(1, 0.3, 0.8, 0);
inputPoints1->InsertPoint(2, 0.7, 0.9, 0);
inputPoints1->InsertPoint(3, 1, 1, 0);

inputPoints2->InsertPoint(0, 0, 0.4, 0);
inputPoints2->InsertPoint(1, 0.3, 0.5, 0);
inputPoints2->InsertPoint(2, 0.7, 0.6, 0);
inputPoints2->InsertPoint(3, 1, 0.7, 0);

inputPoints3->InsertPoint(0, 0,0.1, 0);
inputPoints3->InsertPoint(1, 0.3, 0.2, 0);
inputPoints3->InsertPoint(2, 0.7, 0.3, 0);
inputPoints3->InsertPoint(3, 1, 0.4, 0);

vtkCamera* camera = vtkCamera::New();
  camera->SetDistance(2);
  camera->Dolly(0);
  camera->Roll(0);
  camera->Azimuth(0);
  camera->Yaw(0);
  camera->Elevation(0);
  camera->Pitch(0);
 // camera->SetViewAngle(0);
  

// a renderer and render window
vtkRenderer *ren1 = vtkRenderer::New();
vtkRenderWindow *renWin = vtkRenderWindow::New();
  renWin->AddRenderer(ren1);


// an interactor
vtkRenderWindowInteractor *iren = vtkRenderWindowInteractor::New();
  iren->SetRenderWindow(renWin);


//TESTING camera locking capabilities
  ren1->SetActiveCamera(camera);
  ren1->ResetCamera();
  
//prevent rotation, vtkInteractionStyleImage renders an imageActor
//with a view that is always normal to the XY plane  
vtkInteractorStyleImage *isi = vtkInteractorStyleImage::New();
  iren->SetInteractorStyle(isi);
    
  
//need some subclass of vtkDataSet (such as vtkPolyData) as defined 
//for input by the spline widget class
vtkPolyData *poly1 = vtkPolyData::New();
  poly1->SetPoints(inputPoints1);

vtkPolyData *poly2 = vtkPolyData::New();
  poly2->SetPoints(inputPoints2);

vtkPolyData *poly3 = vtkPolyData::New();
  poly3->SetPoints(inputPoints3);    
     
//handle properties
vtkProperty* handleProperty = vtkProperty::New();
  handleProperty->SetColor(.2, .7, .15);
  handleProperty->SetDiffuse(0.7);
  handleProperty->SetSpecular(0.4);
  handleProperty->SetSpecularPower(20);

//line properties
vtkProperty* lineProperty1 = vtkProperty::New();
  lineProperty1->SetColor(1, 0, 0);
  lineProperty1->SetDiffuse(0.7);
  lineProperty1->SetSpecular(0.4);
  lineProperty1->SetSpecularPower(20);
  lineProperty1->SetLineWidth(2.0);

vtkProperty* lineProperty2 = vtkProperty::New();
  lineProperty2->SetColor(0, 1, 0);
  lineProperty2->SetDiffuse(0.7);
  lineProperty2->SetSpecular(0.4);
  lineProperty2->SetSpecularPower(20);
  lineProperty2->SetLineWidth(2.0);

vtkProperty* lineProperty3 = vtkProperty::New();
  lineProperty3->SetColor(0, 0, 1);
  lineProperty3->SetDiffuse(0.7);
  lineProperty3->SetSpecular(0.4);
  lineProperty3->SetSpecularPower(20);
  lineProperty3->SetLineWidth(2.0);


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
  
vtkSplineWidget* spline1 = vtkSplineWidget::New();
  spline1->SetInteractor(iren);
  spline1->PlaceWidget(0,1,0,1,0,1);
  spline1->SetInput(poly1);
  //spline->SetProjectionNormalToZAxes();
  spline1->InitializeHandles(inputPoints1);  
  spline1->SetPriority(1.0);
  spline1->KeyPressActivationOff();
  spline1->SetHandleProperty(handleProperty);
  spline1->SetLineProperty(lineProperty1);
  spline1->SetHandleSize(0.007);
  spline1->SetResolution(NumberOutputPoints);
  spline1->On();
  

vtkSplineWidget* spline2 = vtkSplineWidget::New();
  spline2->SetInteractor(iren);
  spline2->PlaceWidget(0,1,0,1,0,1);
  spline2->SetInput(poly2);
  //spline->SetProjectionNormalToZAxes();
  spline2->InitializeHandles(inputPoints2);  
  spline2->SetPriority(1.0);
  spline2->KeyPressActivationOff();
  spline2->SetHandleProperty(handleProperty);
  spline2->SetLineProperty(lineProperty2);
  spline2->SetHandleSize(0.007);
  spline2->SetResolution(NumberOutputPoints);
  spline2->On();


vtkSplineWidget* spline3 = vtkSplineWidget::New();
  spline3->SetInteractor(iren);
  spline3->PlaceWidget(0,1,0,1,0,1);
  spline3->SetInput(poly3);
  //spline->SetProjectionNormalToZAxes();
  spline3->InitializeHandles(inputPoints3);  
  spline3->SetPriority(1.0);
  spline3->KeyPressActivationOff();
  spline3->SetHandleProperty(handleProperty);
  spline3->SetLineProperty(lineProperty3);
  spline3->SetHandleSize(0.007);
  spline3->SetResolution(NumberOutputPoints);
  spline3->On();
  
  
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
  spline1->Delete();
  spline2->Delete();
  spline3->Delete();
  iren->Delete();
  ren1->Delete();
  renWin->Delete();
  inputPoints1->Delete();
  inputPoints2->Delete();
  inputPoints3->Delete();
  lineProperty1->Delete();
  lineProperty2->Delete();
  lineProperty3->Delete();
  handleProperty->Delete();
  poly1->Delete();
  poly2->Delete();
  poly3->Delete();
  isi->Delete();
  camera->Delete();

    
return 0;
}



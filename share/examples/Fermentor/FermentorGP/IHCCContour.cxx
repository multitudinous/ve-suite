// --- My Includes --- //
#include "IHCCContour.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/vtkActorToOSG.h>

#include <ves/xplorer/util/readWriteVtkThings.h>

// --- VTK Includes --- //
#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkLookupTable.h>
#include <vtkTransformFilter.h>
#include <vtkTransform.h>
#include <vtkProperty.h>

////////////////////////////////////////////////////////////////////////////////
IHCCContour::IHCCContour()
{
    variables[ 0 ] = 200;  //Agitation (rpm)
    variables[ 1 ] = 1.25; //Air Concentration
    variables[ 2 ] = 6;    //Initial pH value
    variables[ 3 ] = 0.1;  //Nitrate Concentration
    variables[ 4 ] = 37;   //Temperate (Celsius)
    variables[ 5 ] = 240;  //Simulate [a text box] Hours in 10 seconds
    definedRange[ 0 ] = definedRange[ 1 ] = 0;
    mapper = vtkPolyDataMapper::New();
    actor = vtkActor::New();
    lut = NULL;
}
////////////////////////////////////////////////////////////////////////////////
IHCCContour::~IHCCContour()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/*
// Update variables passed in from the gui
void IHCCContour::UpdateModelVariables( double* input )
{
	for ( int i = 0; i < 6; i++ )
	{
		variables[ i ] = input[ i ];
	}
}
*/
////////////////////////////////////////////////////////////////////////////////
void IHCCContour::SetDataVector( std::vector< double > input, double* x )
{
    solutions = input;
    definedRange[ 0 ] = x[ 0 ];
    definedRange[ 1 ] = x[ 1 ];
    cout << definedRange[ 0 ] << " : " << definedRange[ 1 ] << endl;
}
////////////////////////////////////////////////////////////////////////////////
/*
void IHCCContour::RunModel()
{
	vector< double > solutions;
   double t;                        //time (in hours)
   int i;                           //looping index

   double c[ 8 ];
   //array of equation answers
   double r = variables[ 0 ];
   //Defines the agitiation (in rpm) in the fermentor
   double a = variables[ 1 ];
   //defines the concentration of air initially
   double p = variables[ 2 ];
   //defines the initial pH in the fermentor
   double n = variables[ 3 ];
   //defines the initial nitrate concentration
   double k = variables[ 4 ];
   //defines the initial temperature in celsius
   double numsteps = variables[ 5 ];
   //defines the number of iterations to perform
   min = 1000000000;
   max = 0;
   solutions.clear();
   for(t=0;t<numsteps;t++)         //=0.4)
   {
      c[1] = -0.000036*t*t*t + 0.0092*t*t - 0.072*t + 1;
      c[2] = -0.000091*r*r + 0.035*r -2.56;
      c[3] = -1*a*a + 2*a -2;
      c[4] = -0.41*p*p + 4.9*p - 13;
      c[5] = -17*n*n + 8.4*n - 0.004;
      c[6] = -0.01*k*k + 0.69*k - 7.8;
      c[7] = -1;
      c[0] = 1;

//      cout << "Timestep " << t << endl;

      for(i=1;i<8;i++)
      {
         c[0] = c[0] * c[i];
      }

	  if ( c[ 0 ] < min )
	  {
	     min = c[ 0 ];
	  }
	  
	  if ( c[ 0 ] > max )
	  {
	     max = c[ 0 ];
	  }
//      cout << "I calculated the concentration" << endl;
		solutions.push_back( c[ 0 ] );
   }
   definedRange[ 0 ] = min;
   definedRange[ 1 ] = max;
}

*/
////////////////////////////////////////////////////////////////////////////////
void IHCCContour::MakeLookupTable()
{
    cout << "lut range: " << definedRange[ 0 ] << " : " << definedRange[ 1 ] << endl;
    if( lut == NULL )
    {
        lut = vtkLookupTable::New();
    }

    //change color here, angran
    // set up the vtkLookupTable
    lut->SetNumberOfColors( 256 );            //default is 256
    // lut->SetHueRange( 2.0f/3.0f, 0.0f );      //a blue-to-red scale
    lut->SetHueRange( 0.5f, 0.65f);  
    // angran add
    lut->SetSaturationRange (0.1f,1.0f);
    //  lut->SetValueRange (0.0f,0.3f);

    lut->SetTableRange( definedRange );
    lut->Build();
}
////////////////////////////////////////////////////////////////////////////////
void IHCCContour::Update()
{
    // Loop over all the time steps and create the actors and geodes
    // read polydata
    // Translate 5 in y direction
    MakeLookupTable();
    vtkDataObject* pData = ves::xplorer::util::readVtkThing( "./Y_MultiCont_0.vtk" );
    vtkTransform* transform = vtkTransform::New();
    transform->Translate( 0, 5, 0 );
    transform->Update();

    // Transformation 
    vtkTransformFilter *transFilter = vtkTransformFilter::New();
    transFilter->SetInput( (vtkPointSet *)pData );
    transFilter->SetTransform( transform );
    transFilter->Update();
    mapper->SetInput( (vtkPolyData*)transFilter->GetOutput() );
    mapper->ScalarVisibilityOff();
    mapper->Update();

    for( int i = 0; i < static_cast< int >( solutions.size() ); ++i )
    {
        //std::cout << i << " : " << solutions[ i ] << std::endl;

        double* color = 0;
        lut->GetColor( solutions[ i ], color );
        vtkActor* actor = vtkActor::New();
        actor->SetMapper( mapper );
        actor->GetProperty()->SetSpecularPower( 20.0f );
        //cout << " Color : " << color[ 0 ] << " : " << color[ 1 ] << " : " << color[ 2 ] << endl;
        actor->GetProperty()->SetColor( color );
        osg::ref_ptr< ves::xplorer::scenegraph::Geode > geode =
            new ves::xplorer::scenegraph::Geode();
        geode->TranslateToGeode( actor );
        actor->Delete();
        //geodes.push_back( geode.get() );
    }

    lut->Delete();
    lut = NULL;
    pData->Delete();
    transform->Delete();
    transFilter->Delete();
}
////////////////////////////////////////////////////////////////////////////////

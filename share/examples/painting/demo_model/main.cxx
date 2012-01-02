#include <iostream>

#include "AirlessPaintModel.h"

int main( int argc, char* argv[] )
{
    std::cout << argc << " " << argv[ 0 ] << std::endl;

    double distance = 18.0;
    double pressure = 2000.0;
    double viscosity = 200.0;
    double diameter = 0.011;
    AirlessPaintModel model;
    model.SetGunToPartDistance( distance );
    model.SetAirPressure( pressure );
    model.SetViscosity( viscosity );
    model.SetTipDiameter( diameter );

    model.UpdateModel();
    
    double te = model.GetTE();
    double flowrate = model.GetFlowRate();
    
    double h1, h2, w1, w2;
    model.GetPatternDimensions( h1, h2, w1, w2 );

    std::cout << te << " " << flowrate << " " << h1 << " " 
        << h2 << " " << w1 << " " << w2 << std::endl;
    return 0;
}

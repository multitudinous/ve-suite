#include <iostream>
#include <fstream>

#include "AirlessPaintModel.h"

#include <boost/archive/text_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>

#include <boost/archive/xml_iarchive.hpp>
#include <boost/archive/xml_oarchive.hpp>

int main( int argc, char* argv[] )
{
    std::cout << argc << " " << argv[ 0 ] << std::endl;

    std::string filename("test.txt");
    AirlessPaintModel model;
    /*std::ifstream ifs(filename.c_str());
    boost::archive::text_iarchive ia(ifs);
    ia >> model;
    std::cout << model << std::endl;*/
    double distance = 18.0;
    double pressure = 2000.0;
    double viscosity = 200.0;
    double diameter = 0.011;
    //AirlessPaintModel model;
    model.SetGunToPartDistance( distance );
    model.SetAirPressure( pressure );
    model.SetViscosity( viscosity );
    model.SetTipDiameter( diameter );

    model.UpdateModel();
    
    double te = model.GetTE();
    double flowrate = model.GetFlowRate();
    
    double h1, h2, w1, w2;
    model.GetPatternDimensions( h1, h2, w1, w2 );

    std::cout << te << " " << flowrate << std::endl << h1 << " " 
        << h2 << " " << w1 << " " << w2 << std::endl;
    
    /*{
        std::ofstream ofs(filename.c_str());
        boost::archive::text_oarchive oa(ofs);
        oa << model;
    }*/
    
    {
        std::ofstream ofsx("test_xml.txt");
        boost::archive::xml_oarchive oax(ofsx);
        oax << boost::serialization::make_nvp("airless_model" , model);
    }
    
    return 0;
}

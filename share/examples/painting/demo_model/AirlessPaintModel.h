#pragma once

#include <string>
#include <iostream>
#include <map>
#include <vector>

#include <boost/serialization/base_object.hpp>
#include <boost/serialization/utility.hpp>
//#include <boost/serialization/list.hpp>
//#include <boost/serialization/assume_abstract.hpp>
#include <boost/serialization/nvp.hpp>
#include <boost/serialization/map.hpp>
#include <boost/serialization/vector.hpp>

class AirlessPaintModel
{
    friend std::ostream & operator<<(std::ostream& os, const AirlessPaintModel& gp )
    {
        return os << ' ' << gp.m_percentIncrease << ' ' << gp.m_measuredFlowRate << ' ';
    }
    
    friend class boost::serialization::access;
    template<class Archive>
    void serialize( Archive& ar, const unsigned int /* file_version */)
    {        
        ar & boost::serialization::make_nvp( "m_percentIncrease", m_percentIncrease ) 
            & boost::serialization::make_nvp( "m_measuredFlowRate", m_measuredFlowRate )
            & boost::serialization::make_nvp( "m_pressureIncreasePer750", m_pressureIncreasePer750 )
            & boost::serialization::make_nvp( "m_flowRateReductionMap", m_flowRateReductionMap )
            & boost::serialization::make_nvp( "m_teDecreaseMap", m_teDecreaseMap )
            & boost::serialization::make_nvp( "m_patternPercentIncrease", m_patternPercentIncrease )
            & boost::serialization::make_nvp( "m_patternPercentDecrease", m_patternPercentDecrease );
    }
public:
    ///Constructor
    AirlessPaintModel();
    ///Destructor
    ~AirlessPaintModel();

    ///Set the gun to part distance
    void SetGunToPartDistance( double& distance );
    
    ///Set the air pressure
    void SetAirPressure( double& pressure );
    
    ///Set the paint viscosity
    void SetViscosity( double& viscosity );
    
    ///Set the paint nozzle diameter
    void SetTipDiameter( double& diameter );
    
    ///Run the model to generate the model parameters
    void UpdateModel();
    
    ///Get the pattern dimensions
    void GetPatternDimensions( double& h1, double& h2, double& w1, double& w2 );
    
    ///Get TE
    double GetTE();
    
    ///Get the paint flowrate
    double GetFlowRate();

private:
    ///Calculate the paint droplet size
    std::string CalculateDropletSize( double& diameter, double& viscosity );
    ///Determine the droplet size bin
    std::string DetermineDropletBinLetter( double& size );

    ///Calculate the transfer effencieny
    double CalculateTransferEffenciency( double& pressure, std::string const& bin, double& distance );
    
    ///Calculate the fluid flowrate
    double CalculateFlowrate();
    
    ///Calculate the dimension of the paint pattern
    void CalculatePatternDimension();
    
    ///Determine the TE decrease based on droplet size bin
    double DetermineTEDecrease( std::string const& bin );

    double Interpolate( std::map< double, double >& dataMap, double input );
    
    ///////
    ///Model Constants
    double m_percentIncrease;
    double m_measuredFlowRate;
    double m_pressureIncreasePer750;
    std::map< double, double > m_flowRateReductionMap;
    std::map< double, double > m_teDecreaseMap;
    std::vector< double > m_patternPercentIncrease;
    std::vector< double > m_patternPercentDecrease;
    ///////
    
    ///The distance of the gun from the part
    double m_gunToPartDistance;
    ///The air pressure
    double m_airPressure;
    ///The paint viscosity
    double m_viscosity;
    ///The diameter of the nozzle used on the paint gun
    double m_tipDiameter;
    
    ///Paint pattern dimensions
    double m_h1, m_h2, m_w1, m_w2;
    ///Paint flowrate
    double m_flowrate;
    
    double m_te;
};

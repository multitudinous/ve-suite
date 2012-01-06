#pragma once

#include <string>

class AirlessPaintModel
{
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


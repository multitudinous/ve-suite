#include "AirlessPaintModel.h"

#include <cmath>
#include <iostream>
#include <map>

////////////////////////////////////////////////////////////////////////////////
AirlessPaintModel::AirlessPaintModel()
    :
    m_percentIncrease( 175.0f ),
    m_measuredFlowRate( 3.75f ),
    m_pressureIncreasePer750( 40. ),
    m_gunToPartDistance( 0. ),
    m_airPressure( 0. ),
    m_viscosity( 0. ),
    m_tipDiameter( 0. )
{
    //10	100	250	500	750	1000
    //0	-40	-60	-80	-90	-95
    //std::map< double, double > flowRateReductionMap;
    m_flowRateReductionMap[ 10. ] = 0.;
    m_flowRateReductionMap[ 100. ] = -40.;
    m_flowRateReductionMap[ 250. ] = -60.;
    m_flowRateReductionMap[ 500. ] = -80.;
    m_flowRateReductionMap[ 750. ] = -90.;
    m_flowRateReductionMap[ 1000. ] = -95.;
    
    //750	1500	2250	3000	3750	4500
    //-0.028	-0.026	-0.020	-0.018	-0.015	-0.013
    m_teDecreaseMap[ 750. ] = -0.028;
    m_teDecreaseMap[ 1500. ] = -0.026;
    m_teDecreaseMap[ 2250. ] = -0.020;
    m_teDecreaseMap[ 3000. ] = -0.018;
    m_teDecreaseMap[ 3750. ] = -0.015;
    m_teDecreaseMap[ 4500. ] = -0.013;
    
    //Distance Factor (Per Inch) H1	H2	W1	W2
    //Percent Decrease -10% -10% -10% -10%
    //IF(D6<D3,(D3-D6)*(J4)*(B9)+(B9)
    m_patternPercentDecrease.push_back( -10.0 );
    m_patternPercentDecrease.push_back( -10.0 );
    m_patternPercentDecrease.push_back( -10.0 );
    m_patternPercentDecrease.push_back( -10.0 );

    //Distance Factor (Per Inch) H1 H2 W1 W2
    //Percent Increase 2% 5% 2% 3%
    //IF(D6>D3,(D6-D3)*(J3)*(B9)+B9,)
    m_patternPercentIncrease.push_back( 2.0 );
    m_patternPercentIncrease.push_back( 5.0 );
    m_patternPercentIncrease.push_back( 2.0 );
    m_patternPercentIncrease.push_back( 3.0 );    
}
////////////////////////////////////////////////////////////////////////////////
AirlessPaintModel::~AirlessPaintModel()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::SetGunToPartDistance( double& distance )
{
    m_gunToPartDistance = distance;
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::SetAirPressure( double& pressure )
{
    ///Air pressure must be between 0 and 4500
    m_airPressure = pressure;
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::SetViscosity( double& viscosity )
{
    ///Viscosity must be between 10 and 1000
    m_viscosity = viscosity;
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::SetTipDiameter( double& diameter )
{
    ///Tip diameter must be between 0.007 and 0.039
    m_tipDiameter = diameter;
}
////////////////////////////////////////////////////////////////////////////////
std::string AirlessPaintModel::CalculateDropletSize( double& diameter, double& viscosity )
{
    //Min/max tip diameters
    double tipMin = 0.007;
    double tipMax = 0.039;
    
    //Viscosity = 10
    double min10Viscosity = 1.0;
    double max10Viscosity = 25.0;

    //Viscosity = 1000
    double min1000Viscosity = 60.0;
    double max1000Viscosity = 100.0;
    
    //=(B30-B31)/(A30-A31)
    double slope10Viscosity = (max10Viscosity - min10Viscosity)/(tipMax - tipMin);
    
    double actual10DropletSize = (diameter - tipMin) * slope10Viscosity + min10Viscosity;
    std::cout << "Droplet interpolated 10 viscosity = " << actual10DropletSize << std::endl;
    
    //=(B30-B31)/(A30-A31)
    double slope1000Viscosity = (max1000Viscosity - min1000Viscosity)/(tipMax - tipMin);
    
    double actual1000DropletSize = (diameter - tipMin) * slope1000Viscosity + min1000Viscosity;
    std::cout << "Droplet interpolated 1000 viscosity = " << actual1000DropletSize << std::endl;
    
    //=(F31-C31)/(B33-B34)
    double viscositySlope = (actual1000DropletSize - actual10DropletSize)/(1000.0 - 10.0);
    
    double dropletSize = (viscosity - 10.0) * viscositySlope + actual10DropletSize;
    std::cout << "Droplet size = " << dropletSize << std::endl;
    
    std::string dropletBin = DetermineDropletBinLetter( dropletSize );
    
    std::cout << "Droplet bin = " << dropletBin << std::endl;
    return dropletBin;
}
////////////////////////////////////////////////////////////////////////////////
std::string AirlessPaintModel::DetermineDropletBinLetter( double& size )
{
    //Category A: Over atomized Droplets 0 20
    if( (size >= 0.0) && (size <= 20.0) )
    {
        return "A";
    }
    
    //Category B: Small Droplets 20 40
    if( (size >= 20.1) && (size <= 40.0) )
    {
        return "B";
    }
    
    //Category C: Medium Droplets 40 60
    if( (size >= 40.1) && (size <= 60.0) )
    {
        return "C";
    }

    //Category D: Large Droplets 60 80
    if( (size >= 60.1) && (size <= 80.0) )
    {
        return "D";
    }
    
    //E Droplet: Splatter 80 100
    if( (size >= 80.1) && (size <= 100.0) )
    {
        return "E";
    }
    
    return std::string();
}
////////////////////////////////////////////////////////////////////////////////
double AirlessPaintModel::CalculateTransferEffenciency( double& pressure, std::string const& bin, double& distance )
{
    double teDescreasePerInch = Interpolate( m_teDecreaseMap, pressure );    

    double dropletSizeDecrease = DetermineTEDecrease( bin );
    
    double nominalTE = 100.0;
    
    double  transferEffeciency = 
        (teDescreasePerInch * dropletSizeDecrease * distance) * (nominalTE * 0.01) + 1.0;
    std::cout << "Transfer effeciency = " << transferEffeciency << std::endl;
    return transferEffeciency;
}
////////////////////////////////////////////////////////////////////////////////
double AirlessPaintModel::DetermineTEDecrease( std::string const& bin )
{
    if( bin == "A" )
    {
        return 1.0;
    }
    
    if( bin == "B" )
    {
        return 0.80;
    }
    
    if( bin == "C" )
    {
        return 0.50;
    }
    
    if( bin == "D" )
    {
        return 0.40;
    }
    
    if( bin == "E" )
    {
        return 0.20;
    }
    
    return 0.0;
}
////////////////////////////////////////////////////////////////////////////////
double AirlessPaintModel::CalculateFlowrate()
{
    double flowRateReduction = Interpolate( m_flowRateReductionMap, m_viscosity );

    std::cout << "Flowrate reduction = " << flowRateReduction << std::endl;

    double numberOfNozzleIncrements = ( m_tipDiameter - 0.0070 ) / 0.0020;
    //=B29*(C29*0.01)*A33+B29
    double baseFlowRate = m_measuredFlowRate * ( m_percentIncrease * 0.01 ) * numberOfNozzleIncrements + m_measuredFlowRate;
    std::cout << "Base flowrate = " << baseFlowRate << std::endl;

    //=L8*0.01+F30+L8
    double flowRate = baseFlowRate * 0.01 * flowRateReduction + baseFlowRate;
    std::cout << "Reduced flowrate by viscosity constant = " << flowRate << std::endl;
    
    //=(O9*K10)*G23+O9
    flowRate = ((m_airPressure/750) - 1) * m_pressureIncreasePer750 * 0.01 * flowRate + flowRate;
    std::cout << "Final flowrate = " << flowRate << std::endl; 
    
    return flowRate;
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::CalculatePatternDimension()
{    
	//H1	H2	W1 	W2
    //Nominal	6.00	8.40	2.40	3.60
    double nomh1, nomh2, nomw1, nomw2;
    nomh1 = 6.00;
    nomh2 = 8.40;
    nomw1 = 2.40;
    nomw2 = 3.60;
    double nominalDistance = 12.0;

    if( m_gunToPartDistance == nominalDistance )
    {
        m_h1 = nomh1;
        m_h2 = nomh2;
        m_w1 = nomw1;
        m_w2 = nomw2;
    }
    else if( m_gunToPartDistance < nominalDistance )
    {
        //Distance Factor (Per Inch) H1	H2	W1	W2
        //Percent Decrease -10% -10% -10% -10%
        //IF(D6<D3,(D3-D6)*(J4)*(B9)+(B9)
        m_h1 = (nominalDistance - m_gunToPartDistance) * (m_patternPercentDecrease.at( 0 ) * 0.01) * nomh1 + nomh1;
        m_h2 = (nominalDistance - m_gunToPartDistance) * (m_patternPercentDecrease.at( 1 ) * 0.01) * nomh2 + nomh2;
        m_w1 = (nominalDistance - m_gunToPartDistance) * (m_patternPercentDecrease.at( 2 ) * 0.01) * nomw1 + nomw1;
        m_w2 = (nominalDistance - m_gunToPartDistance) * (m_patternPercentDecrease.at( 3 ) * 0.01) * nomw2 + nomw2;
    }
    else if( m_gunToPartDistance > nominalDistance )
    {
        //Distance Factor (Per Inch) H1 H2 W1 W2
        //Percent Increase 2% 5% 2% 3%
        //IF(D6>D3,(D6-D3)*(J3)*(B9)+B9,)
        m_h1 = (m_gunToPartDistance - nominalDistance) * (m_patternPercentIncrease.at( 0 ) * 0.01) * nomh1 + nomh1;
        m_h2 = (m_gunToPartDistance - nominalDistance) * (m_patternPercentIncrease.at( 1 ) * 0.01) * nomh2 + nomh2;
        m_w1 = (m_gunToPartDistance - nominalDistance) * (m_patternPercentIncrease.at( 2 ) * 0.01) * nomw1 + nomw1;
        m_w2 = (m_gunToPartDistance - nominalDistance) * (m_patternPercentIncrease.at( 3 ) * 0.01) * nomw2 + nomw2;
    }
    
    std::cout << "Pattern = " << m_h1 << " " << m_h2 << " " 
        << m_w1 << " " << m_w2 << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::UpdateModel()
{
    m_flowrate = CalculateFlowrate();
    
    std::string const dropletBin = 
        CalculateDropletSize( m_tipDiameter, m_viscosity );
    
    m_te = CalculateTransferEffenciency( m_airPressure, dropletBin, m_gunToPartDistance );
    
    CalculatePatternDimension();
}
////////////////////////////////////////////////////////////////////////////////
void AirlessPaintModel::GetPatternDimensions( double& h1, double& h2, double& w1, double& w2 )
{
    h1 = m_h1;
    h2 = m_h2;
    w1 = m_w1;
    w2 = m_w2;
}
////////////////////////////////////////////////////////////////////////////////
double AirlessPaintModel::GetTE()
{
    return m_te;
}
////////////////////////////////////////////////////////////////////////////////
double AirlessPaintModel::GetFlowRate()
{
    return m_flowrate;
}
////////////////////////////////////////////////////////////////////////////////
double AirlessPaintModel::Interpolate( std::map< double, double >& dataMap, double input )
{
    double result = 0.0;
    typedef std::map< double, double >::const_iterator BoundIter;
    BoundIter equalPoint = dataMap.find( input );
    if( equalPoint == dataMap.end() )
    {
        BoundIter upperBound = dataMap.upper_bound( input );
        BoundIter lowerBound = upperBound;
        lowerBound--;
        
        result = lowerBound->second + ( input - lowerBound->first ) * 
        ( ( upperBound->second - lowerBound->second ) / ( upperBound->first - lowerBound->first ) );
    }
    else
    {
        result = equalPoint->second;
    }
    return result;
}
////////////////////////////////////////////////////////////////////////////////
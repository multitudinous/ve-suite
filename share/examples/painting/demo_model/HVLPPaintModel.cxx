#include "HVLPPaintModel.h"

#include <cmath>
#include <iostream>
#include <map>

////////////////////////////////////////////////////////////////////////////////
HVLPPaintModel::HVLPPaintModel()
    :
    m_percentIncrease( 25.0f ),
    m_measuredFlowRate( 12.0f ),
    m_pressureIncreasePer750( 40. ),
    m_needlePosition( 100.0 ),
    m_atomizingAirPressure( 100.0 ),
    m_gunToPartDistance( 0. ),
    m_airPressure( 0. ),
    m_viscosity( 0. ),
    m_tipDiameter( 0. )
{
    ///Viscosity reduction map
    //Scale Max			0	100	250	500	750	1000
    //Flow Rate Reduction %			0.00	-0.25	-0.40	-0.60	-0.80	-0.90
    m_viscosityReductionMap[    0. ] =   0.;
    m_viscosityReductionMap[  100. ] = -25.;
    m_viscosityReductionMap[  250. ] = -40.;
    m_viscosityReductionMap[  500. ] = -60.;
    m_viscosityReductionMap[  750. ] = -80.;
    m_viscosityReductionMap[ 1000. ] = -90.;
    
    //Fluid Needle %			0	20	40	60	80	100
    //Flow Rate Reduction %			-1.00	-0.60	-0.20	-0.10	-0.05	0.00
    m_fluidNeedleReductionMap[   0. ] = -100.;
    m_fluidNeedleReductionMap[  20. ] =  -60.;
    m_fluidNeedleReductionMap[  40. ] =  -20.;
    m_fluidNeedleReductionMap[  60. ] =  -10.;
    m_fluidNeedleReductionMap[  80. ] =   -5.;
    m_fluidNeedleReductionMap[ 100. ] =  0.00;

    //Atomizing Air Pressure Reduction			-0.05	0.04	0.03	0.02	0.01	0.00
    m_atomizingPressureNeedleReductionMap[   0. ] = -5.;
    m_atomizingPressureNeedleReductionMap[  20. ] =  4.;
    m_atomizingPressureNeedleReductionMap[  40. ] =  3.;
    m_atomizingPressureNeedleReductionMap[  60. ] =  2.;
    m_atomizingPressureNeedleReductionMap[  80. ] =  1.;
    m_atomizingPressureNeedleReductionMap[ 100. ] =  0.00;

    //% Flow Rate Increase			125%	70%	50%	35%	25%	10%
    //Scale Min			0	100	250	500	750	1000
    m_fluidPressureReductionMap[    0. ] = 125.0;
    m_fluidPressureReductionMap[  100. ] =  70.0;
    m_fluidPressureReductionMap[  250. ] =  50.0;
    m_fluidPressureReductionMap[  500. ] =  35.0;
    m_fluidPressureReductionMap[  750. ] =  25.0;
    m_fluidPressureReductionMap[ 1000. ] =  10.0;
    
    //-0.05	-0.025	0	0	0	0
    //Scale Min			0	100	250	500	750	1000
    m_atomizingPressureReductionMap[    0. ] = -5.0;
    m_atomizingPressureReductionMap[  100. ] = -2.5;
    m_atomizingPressureReductionMap[  250. ] =  0.;
    m_atomizingPressureReductionMap[  500. ] =  0.;
    m_atomizingPressureReductionMap[  750. ] =  0.;
    m_atomizingPressureReductionMap[ 1000. ] =  0.;
        
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
HVLPPaintModel::~HVLPPaintModel()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::SetGunToPartDistance( double& distance )
{
    m_gunToPartDistance = distance;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::SetAirPressure( double& pressure )
{
    ///Air pressure must be between 0 and 4500
    m_airPressure = pressure;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::SetViscosity( double& viscosity )
{
    ///Viscosity must be between 10 and 1000
    m_viscosity = viscosity;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::SetTipDiameter( double& diameter )
{
    ///Tip diameter must be between 0.007 and 0.039
    m_tipDiameter = diameter;
}
////////////////////////////////////////////////////////////////////////////////
std::string HVLPPaintModel::CalculateDropletSize( double& diameter, double& viscosity )
{
    //Min/max tip diameters
    double tipMin = 0.000;
    double tipMax = 5.000;
    
    //Viscosity = 0
    double min0Viscosity = 0.0;
    double max0Viscosity = 150.0;

    //Viscosity = 1000
    double min1000Viscosity = 15.0;
    double max1000Viscosity = 500.0;
    
    //=(B30-B31)/(A30-A31)
    double slope0Viscosity = (max0Viscosity - min0Viscosity)/(tipMax - tipMin);
    
    double actual0DropletSize = (diameter - tipMin) * slope0Viscosity + min0Viscosity;
    std::cout << "Droplet interpolated 10 viscosity = " << actual0DropletSize << std::endl;
    
    //=(B30-B31)/(A30-A31)
    double slope1000Viscosity = (max1000Viscosity - min1000Viscosity)/(tipMax - tipMin);
    
    double actual1000DropletSize = (diameter - tipMin) * slope1000Viscosity + min1000Viscosity;
    std::cout << "Droplet interpolated 1000 viscosity = " << actual1000DropletSize << std::endl;
    
    //=(F31-C31)/(B33-B34)
    double viscositySlope = (actual1000DropletSize - actual0DropletSize)/(1000.0 - 0.0);
    
    double dropletSize = (viscosity - 0.0) * viscositySlope + actual0DropletSize;
    std::cout << "Droplet size = " << dropletSize << std::endl;
    
    std::string dropletBin = DetermineDropletBinLetter( dropletSize );
    
    std::cout << "Droplet bin = " << dropletBin << std::endl;
    return dropletBin;
}
////////////////////////////////////////////////////////////////////////////////
std::string HVLPPaintModel::DetermineDropletBinLetter( double& size )
{
    //Category A: Over atomized Droplets 0 20
    if( (size >= 0.0) && (size <= 10.0) )
    {
        return "A";
    }
    
    //Category B: Small Droplets 20 40
    if( (size >= 10.1) && (size <= 19.0) )
    {
        return "B";
    }
    
    //Category C: Medium Droplets 40 60
    if( (size >= 19.1) && (size <= 29.0) )
    {
        return "C";
    }

    //Category D: Large Droplets 60 80
    if( (size >= 29.1) && (size <= 59.0) )
    {
        return "D";
    }
    
    //E Droplet: Splatter 80 100
    if( (size >= 59.1) && (size <= 500.0) )
    {
        return "E";
    }
    
    return std::string();
}
////////////////////////////////////////////////////////////////////////////////
double HVLPPaintModel::CalculateTransferEffenciency( double& pressure, std::string const& bin, double& distance )
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
double HVLPPaintModel::DetermineTEDecrease( std::string const& bin )
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
double HVLPPaintModel::CalculateFlowrate()
{
    double viscosityReduction = Interpolate( m_viscosityReductionMap, m_viscosity );
    
    std::cout << "Viscosity reduction = " << viscosityReduction << std::endl;

    double flowRateReduction = Interpolate( m_fluidNeedleReductionMap, m_needlePosition );

    std::cout << "Flowrate reduction = " << flowRateReduction << std::endl;

    double atomizingNeedleReduction = Interpolate( m_atomizingPressureNeedleReductionMap, m_needlePosition );

    std::cout << "Atomizing Needle reduction = " << atomizingNeedleReduction << std::endl;

    double fluidPressureFlowRateReduction = Interpolate( m_fluidPressureReductionMap, m_viscosity );

    std::cout << "Fluid pressure flowrate reduction = " << fluidPressureFlowRateReduction << std::endl;

    double atomizingPressureReduction = Interpolate( m_atomizingPressureReductionMap, m_viscosity );

    std::cout << "Atomizing pressure reduction = " << atomizingPressureReduction << std::endl;

    double numberOfNozzleIncrements = ( m_tipDiameter - 0.80 ) / 0.20;

    //=B29*(C29*0.01)*A33+B29
    double baseFlowRate = m_measuredFlowRate * ( m_percentIncrease * 0.01 ) * numberOfNozzleIncrements + m_measuredFlowRate;
    std::cout << "Base flowrate = " << baseFlowRate << std::endl;

    ///Take into account viscosity on flowrate
    //=W10*(S11)+W10
    double flowRate = ( baseFlowRate * 0.01 * viscosityReduction ) + baseFlowRate;
    std::cout << "Reduced flowrate by viscosity constant = " << flowRate << std::endl;
    
    ///Take into account needle position on flowrate
    ///=(W11*S12)+W11
    flowRate = ( flowRate * 0.01 * flowRateReduction ) + flowRate;

    ///Take into account fluid pressure on flowrate
    ///=(W12*S14)*((P14-10)/10)+W12
    flowRate = ( flowRate * 0.01 * fluidPressureFlowRateReduction ) * ( m_airPressure - 10. )/10. + flowRate;

    ///Take into atomizing air pressure on flowrate
    //=(W14*S15)*(P15/10)+W14
    flowRate = ( flowRate * 0.01 * atomizingPressureReduction ) * (m_atomizingAirPressure/10.0) + flowRate;

    //=(O9*K10)*G23+O9
    //flowRate = ((m_airPressure/750) - 1) * m_pressureIncreasePer750 * 0.01 * flowRate + flowRate;
    std::cout << "Final flowrate = " << flowRate << std::endl; 
    
    return flowRate;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::CalculatePatternDimension()
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
void HVLPPaintModel::UpdateModel()
{
    m_flowrate = CalculateFlowrate();
    
    double atomizingRatio = m_flowrate / m_atomizingAirPressure;

    std::string const dropletBin = 
        CalculateDropletSize( atomizingRatio, m_viscosity );
    
    //m_te = CalculateTransferEffenciency( m_airPressure, dropletBin, m_gunToPartDistance );
    
    //CalculatePatternDimension();
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::GetPatternDimensions( double& h1, double& h2, double& w1, double& w2 )
{
    h1 = m_h1;
    h2 = m_h2;
    w1 = m_w1;
    w2 = m_w2;
}
////////////////////////////////////////////////////////////////////////////////
double HVLPPaintModel::GetTE()
{
    return m_te;
}
////////////////////////////////////////////////////////////////////////////////
double HVLPPaintModel::GetFlowRate()
{
    return m_flowrate;
}
////////////////////////////////////////////////////////////////////////////////
double HVLPPaintModel::Interpolate( std::map< double, double >& dataMap, double input )
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

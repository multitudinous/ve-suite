#include "HVLPPaintModel.h"

#include <cmath>
#include <iostream>
#include <map>

////////////////////////////////////////////////////////////////////////////////
HVLPPaintModel::HVLPPaintModel()
    :
    m_percentIncrease( 25.0f ),
    m_measuredFlowRate( 12.0f ),
    m_needlePosition( 75.0 ),
    m_atomizingAirPressure( 60.0 ),
    m_gunToPartDistance( 6. ),
    m_airPressure( 0. ),
    m_viscosity( 0. ),
    m_tipDiameter( 0. ),
    m_dropletBin( "A" ),
    m_patternBin( "A" ),
    m_fanAdjustment( 100.0 )
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
    m_atomizingPressureNeedleReductionMap[  20. ] = -4.;
    m_atomizingPressureNeedleReductionMap[  40. ] = -3.;
    m_atomizingPressureNeedleReductionMap[  60. ] = -2.;
    m_atomizingPressureNeedleReductionMap[  80. ] = -1.;
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
    //Fluid Pressure 	0	20	40	60	80	100
    //TE Decrease/ Inch	-0.0275	-0.026	-0.02	-0.0175	-0.015	-0.0125
    m_teDecreaseMap[   0.0 ] = -0.0275;
    m_teDecreaseMap[  20.0 ] = -0.026;
    m_teDecreaseMap[  40.0 ] = -0.020;
    m_teDecreaseMap[  60.0 ] = -0.0175;
    m_teDecreaseMap[  80.0 ] = -0.015;
    m_teDecreaseMap[ 100.0 ] = -0.0125;
    
    //Table 4: Distance Factor (Per Inch) H1	H2	W1	W2
    //Percent Decrease -10% -10% -10% -10%
    //IF(D6<D3,(D3-D6)*(J4)*(B9)+(B9)
    m_patternPercentDecrease.push_back( -12.0 );
    m_patternPercentDecrease.push_back( -15.0 );
    m_patternPercentDecrease.push_back(  -6.0 );
    m_patternPercentDecrease.push_back(  -8.0 );

    //Table 4: Distance Factor (Per Inch) H1 H2 W1 W2
    //Percent Increase 2% 5% 2% 3%
    //IF(D6>D3,(D6-D3)*(J3)*(B9)+B9,)
    m_patternPercentIncrease.push_back( 2.0 );
    m_patternPercentIncrease.push_back( 5.0 );
    m_patternPercentIncrease.push_back( 2.0 );
    m_patternPercentIncrease.push_back( 3.0 );
    
    ///Table 3: Base numbers for the pattern shape
    //H1	H2	W1 	W2
    //% Change	8.00	150%	40%	60%
    m_patternBasePercents.push_back(   8.0 );
    m_patternBasePercents.push_back( 150.0 );
    m_patternBasePercents.push_back(  40.0 );
    m_patternBasePercents.push_back(  60.0 );

    ///Calculate patter shape constants
    
    //Table5: Pattern Shape						
    //Function: Reduce the table 4 pattern dimensions based on the yellow highlighted values.						
    /*H1	H2	W1	W2
    A			100% 100%	50%	100%
    B			80%	100%	60%	100%
    C			100% 100%	100%	100%
    D			75%	100%	50%	100%
    E			30%	60%	80%	100%*/
    m_dropletSizeH1PatternMap[ "A" ] = 100.0;
    m_dropletSizeH1PatternMap[ "B" ] =  80.0;
    m_dropletSizeH1PatternMap[ "C" ] = 100.0;
    m_dropletSizeH1PatternMap[ "D" ] =  75.0;
    m_dropletSizeH1PatternMap[ "E" ] =  30.0;

    m_dropletSizeH2PatternMap[ "A" ] = 100.0;
    m_dropletSizeH2PatternMap[ "B" ] = 100.0;
    m_dropletSizeH2PatternMap[ "C" ] = 100.0;
    m_dropletSizeH2PatternMap[ "D" ] = 100.0;
    m_dropletSizeH2PatternMap[ "E" ] =  60.0;

    m_dropletSizeW1PatternMap[ "A" ] =  50.0;
    m_dropletSizeW1PatternMap[ "B" ] =  60.0;
    m_dropletSizeW1PatternMap[ "C" ] = 100.0;
    m_dropletSizeW1PatternMap[ "D" ] =  50.0;
    m_dropletSizeW1PatternMap[ "E" ] =  80.0;

    m_dropletSizeW2PatternMap[ "A" ] = 100.0;
    m_dropletSizeW2PatternMap[ "B" ] = 100.0;
    m_dropletSizeW2PatternMap[ "C" ] = 100.0;
    m_dropletSizeW2PatternMap[ "D" ] = 100.0;
    m_dropletSizeW2PatternMap[ "E" ] = 100.0;
    
    //Table 6: Needle Travel Distance						
    //Function: The corresponding % change relationships will effect the pattern dimensions from table 5						
    //Adjusted % Change: The actual % change relationship based on Table 2 trigger pull % (cell D13). 						
    /*H1	H2	W1	W2
    100			0%	0%	0%	0%
    80			-5%	-5%	-5%	-5%
    60			-10%	-10%	-10%	-10%
    40			-25%	-25%	-25%	-25%
    20			-50%	-50%	-50%	-50%
    0			-100%	-100%	-100%	-100%
    Adjusted % Change			0%	0%	0%	0%*/
    m_needleTravelH1PatternMap[ 100.0 ] =    0.0;
    m_needleTravelH1PatternMap[  80.0 ] =   -5.0;
    m_needleTravelH1PatternMap[  60.0 ] =  -10.0;
    m_needleTravelH1PatternMap[  40.0 ] =  -25.0;
    m_needleTravelH1PatternMap[  20.0 ] =  -50.0;
    m_needleTravelH1PatternMap[   0.0 ] = -100.0;

    m_needleTravelH2PatternMap[ 100.0 ] =    0.0;
    m_needleTravelH2PatternMap[  80.0 ] =   -5.0;
    m_needleTravelH2PatternMap[  60.0 ] =  -10.0;
    m_needleTravelH2PatternMap[  40.0 ] =  -25.0;
    m_needleTravelH2PatternMap[  20.0 ] =  -50.0;
    m_needleTravelH2PatternMap[   0.0 ] = -100.0;

    m_needleTravelW1PatternMap[ 100.0 ] =    0.0;
    m_needleTravelW1PatternMap[  80.0 ] =   -5.0;
    m_needleTravelW1PatternMap[  60.0 ] =  -10.0;
    m_needleTravelW1PatternMap[  40.0 ] =  -25.0;
    m_needleTravelW1PatternMap[  20.0 ] =  -50.0;
    m_needleTravelW1PatternMap[   0.0 ] = -100.0;

    m_needleTravelW2PatternMap[ 100.0 ] =    0.0;
    m_needleTravelW2PatternMap[  80.0 ] =   -5.0;
    m_needleTravelW2PatternMap[  60.0 ] =  -10.0;
    m_needleTravelW2PatternMap[  40.0 ] =  -25.0;
    m_needleTravelW2PatternMap[  20.0 ] =  -50.0;
    m_needleTravelW2PatternMap[   0.0 ] = -100.0;
    
    //Table 7: Fan Adjustment Knob						
    //Function: The corresponding % change relationships will effect the pattern dimensions from table 6.						
    //Adjusted % Change: The actual % change relationship based on Table 2 Fan Pattern % (cell D14). 						
    /*H1	H2	W1	W2
    100			0%	0%	0%	0%
    80			-5%	-5%	-5%	-5%
    60			-10%	-10%	-10%	-10%
    40			-25%	-25%	-15%	-15%
    20			-50%	-50%	-20%	-20%
    0			-75%	-75%	-37%	-42%
    Adjusted % Change			0%	0%	0%	0%*/
    m_fanAdjustmentH1PatternMap[ 100.0 ] =    0.0;
    m_fanAdjustmentH1PatternMap[  80.0 ] =   -5.0;
    m_fanAdjustmentH1PatternMap[  60.0 ] =  -10.0;
    m_fanAdjustmentH1PatternMap[  40.0 ] =  -25.0;
    m_fanAdjustmentH1PatternMap[  20.0 ] =  -50.0;
    m_fanAdjustmentH1PatternMap[   0.0 ] =  -75.0;

    m_fanAdjustmentH2PatternMap[ 100.0 ] =    0.0;
    m_fanAdjustmentH2PatternMap[  80.0 ] =   -5.0;
    m_fanAdjustmentH2PatternMap[  60.0 ] =  -10.0;
    m_fanAdjustmentH2PatternMap[  40.0 ] =  -25.0;
    m_fanAdjustmentH2PatternMap[  20.0 ] =  -50.0;
    m_fanAdjustmentH2PatternMap[   0.0 ] =  -75.0;

    m_fanAdjustmentW1PatternMap[ 100.0 ] =    0.0;
    m_fanAdjustmentW1PatternMap[  80.0 ] =   -5.0;
    m_fanAdjustmentW1PatternMap[  60.0 ] =  -10.0;
    m_fanAdjustmentW1PatternMap[  40.0 ] =  -15.0;
    m_fanAdjustmentW1PatternMap[  20.0 ] =  -20.0;
    m_fanAdjustmentW1PatternMap[   0.0 ] =  -37.0;

    m_fanAdjustmentW2PatternMap[ 100.0 ] =    0.0;
    m_fanAdjustmentW2PatternMap[  80.0 ] =   -5.0;
    m_fanAdjustmentW2PatternMap[  60.0 ] =  -10.0;
    m_fanAdjustmentW2PatternMap[  40.0 ] =  -15.0;
    m_fanAdjustmentW2PatternMap[  20.0 ] =  -20.0;
    m_fanAdjustmentW2PatternMap[   0.0 ] =  -42.0;
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
    
    m_patternBin = DeterminePatternShapeBinLetter( dropletSize );
    
    std::cout << "Droplet bin = " << dropletBin << " " << m_patternBin << std::endl;
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
    if( (size > 10.0) && (size <= 19.0) )
    {
        return "B";
    }
    
    //Category C: Medium Droplets 40 60
    if( (size > 19.0) && (size <= 29.0) )
    {
        return "C";
    }

    //Category D: Large Droplets 60 80
    if( (size > 29.0) && (size <= 59.0) )
    {
        return "D";
    }
    
    //E Droplet: Splatter 80 100
    if( (size > 59.0) && (size <= 500.0) )
    {
        return "E";
    }
    
    return std::string();
}
////////////////////////////////////////////////////////////////////////////////
std::string HVLPPaintModel::DeterminePatternShapeBinLetter( double& size )
{
    //Category A: Elliptical (Dumbbell)			
    if( (size >= 0.0) && (size <= 4.0) )
    {
        return "A";
    }
    
    //Category B: Eliptical Reduced Size			
    if( (size > 4.0) && (size <= 9.0) )
    {
        return "B";
    }
    
    //Category C: Elliptical			
    if( (size > 9.0) && (size <= 69.0) )
    {
        return "C";
    }
    
    //Category D: H1,W1 Circular / H2, W2 Eliptical			
    if( (size > 69.0) && (size <= 89.0) )
    {
        return "D";
    }
    
    //E Droplet: Circular Pattern			
    if( (size > 89.0) && (size <= 500.0) )
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

    double fluidPressureFlowRateReduction = Interpolate( m_fluidPressureReductionMap, m_viscosity );

    std::cout << "Fluid pressure flowrate reduction = " << fluidPressureFlowRateReduction << std::endl;

    double atomizingPressureReduction = Interpolate( m_atomizingPressureReductionMap, m_viscosity );

    std::cout << "Atomizing pressure reduction = " << atomizingPressureReduction << std::endl;

    double flowRateReduction = Interpolate( m_fluidNeedleReductionMap, m_needlePosition );
    
    std::cout << "Flowrate reduction = " << flowRateReduction << std::endl;
    
    double atomizingNeedleReduction = Interpolate( m_atomizingPressureNeedleReductionMap, m_needlePosition );
    
    std::cout << "Atomizing Needle reduction = " << atomizingNeedleReduction << std::endl;

    
    double numberOfNozzleIncrements = ( m_tipDiameter - 0.80 ) / 0.20;

    //=B29*(C29*0.01)*A33+B29
    double baseFlowRate = m_measuredFlowRate * ( m_percentIncrease * 0.01 ) * numberOfNozzleIncrements + m_measuredFlowRate;
    std::cout << "Base flowrate = " << baseFlowRate << std::endl;

    ///Take into account viscosity on flowrate
    //=W10*(S11)+W10
    double flowRate = ( baseFlowRate * 0.01 * viscosityReduction ) + baseFlowRate;
    std::cout << "Reduced flowrate by viscosity constant = " << flowRate << std::endl;
    
    ///Take into account fluid pressure on flowrate
    ///=(W12*S14)*((P14-10)/10)+W12
    flowRate = ( flowRate * 0.01 * fluidPressureFlowRateReduction ) * ( m_airPressure - 10. )/10. + flowRate;

    ///Take into account atomizing air pressure on flowrate
    m_atomizingAirPressure = atomizingNeedleReduction * 0.01 * m_atomizingAirPressure + m_atomizingAirPressure;
    //=(W14*S15)*(P15/10)+W14
    flowRate = ( flowRate * 0.01 * atomizingPressureReduction ) * (m_atomizingAirPressure/10.0) + flowRate;
    
    ///Take into account needle position on flowrate
    ///=(W11*S12)+W11
    flowRate = ( flowRate * 0.01 * flowRateReduction ) + flowRate;

    //=(O9*K10)*G23+O9
    //flowRate = ((m_airPressure/750) - 1) * m_pressureIncreasePer750 * 0.01 * flowRate + flowRate;
    std::cout << "Final flowrate = " << flowRate << std::endl; 
    std::cout << "AAP = " << m_atomizingAirPressure << std::endl; 

    return flowRate;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::CalculatePatternDimension()
{    
	//H1	H2	W1 	W2
    //Standard	8	12	3	5
    double nomh1, nomh2, nomw1, nomw2;
    nomh1 =  m_patternBasePercents.at( 0 );
    nomh2 =  nomh1 * m_patternBasePercents.at( 1 ) * 0.01;
    nomw1 =  nomh1 * m_patternBasePercents.at( 2 ) * 0.01;
    nomw2 =  nomh1 * m_patternBasePercents.at( 3 ) * 0.01;
    double nominalDistance = 6.0;

    //std::cout << "Pattern = " << nomh1 << " " << nomh2 << " " 
    //<< nomw1 << " " << nomw2 << std::endl;

    ///Take into account gun to part distance
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
    
    //std::cout << "Pattern = " << m_h1 << " " << m_h2 << " " 
    //<< m_w1 << " " << m_w2 << std::endl;

    ///Take into account Table 5: pattern shape
    m_h1 = m_h1 * m_dropletSizeH1PatternMap[ m_patternBin ] * 0.01;
    m_h2 = m_h2 * m_dropletSizeH2PatternMap[ m_patternBin ] * 0.01;
    m_w1 = m_w1 * m_dropletSizeW1PatternMap[ m_patternBin ] * 0.01;
    m_w2 = m_w2 * m_dropletSizeW2PatternMap[ m_patternBin ] * 0.01;

    //std::cout << "Pattern = " << m_h1 << " " << m_h2 << " " 
    //<< m_w1 << " " << m_w2 << std::endl;

    ///Take into account Table 6: Needle Travel Distance - these are negative
    ///percentages so we have to add in the base value of the pattern
    m_h1 = m_h1 * Interpolate( m_needleTravelH1PatternMap, m_needlePosition ) * 0.01 + m_h1;
    m_h2 = m_h2 * Interpolate( m_needleTravelH2PatternMap, m_needlePosition ) * 0.01 + m_h2;
    m_w1 = m_w1 * Interpolate( m_needleTravelW1PatternMap, m_needlePosition ) * 0.01 + m_w1;
    m_w2 = m_w2 * Interpolate( m_needleTravelW2PatternMap, m_needlePosition ) * 0.01 + m_w2;

    //std::cout << "Pattern = " << m_h1 << " " << m_h2 << " " 
    //<< m_w1 << " " << m_w2 << std::endl;

    ///Take into account Table 7: Fan adjustment knob - these are negative
    ///percentages so we have to add in the base value of the pattern
    m_h1 = m_h1 * Interpolate( m_fanAdjustmentH1PatternMap, m_fanAdjustment ) * 0.01 + m_h1;
    m_h2 = m_h2 * Interpolate( m_fanAdjustmentH2PatternMap, m_fanAdjustment ) * 0.01 + m_h2;
    m_w1 = m_w1 * Interpolate( m_fanAdjustmentW1PatternMap, m_fanAdjustment ) * 0.01 + m_w1;
    m_w2 = m_w2 * Interpolate( m_fanAdjustmentW2PatternMap, m_fanAdjustment ) * 0.01 + m_w2;
    
    ///Lets see what it looks like
    std::cout << "Pattern = " << m_h1 << " " << m_h2 << " " 
        << m_w1 << " " << m_w2 << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void HVLPPaintModel::UpdateModel()
{
    m_flowrate = CalculateFlowrate();
    
    double atomizingRatio = m_flowrate / m_atomizingAirPressure;

    m_dropletBin = 
        CalculateDropletSize( atomizingRatio, m_viscosity );
    
    m_te = CalculateTransferEffenciency( m_airPressure, m_dropletBin, m_gunToPartDistance );
    
    CalculatePatternDimension();
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

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

class HVLPPaintModel
{
    friend std::ostream & operator<<(std::ostream& os, const HVLPPaintModel& gp )
    {
        return os << ' ' << gp.m_percentIncrease << ' ' << gp.m_measuredFlowRate << ' ';
    }
    
    friend class boost::serialization::access;
    template<class Archive>
    void serialize( Archive& ar, const unsigned int /* file_version */)
    {        
        ar  & boost::serialization::make_nvp( "Table_1_m_percentIncrease", m_percentIncrease )
            & boost::serialization::make_nvp( "Table_1_m_measuredFlowRate", m_measuredFlowRate )
            & boost::serialization::make_nvp( "Table_2_m_viscosityReductionMap", m_viscosityReductionMap ) 
            & boost::serialization::make_nvp( "Table_3_m_fluidPressureReductionMap", m_fluidPressureReductionMap )
            & boost::serialization::make_nvp( "Table_4_m_atomizingPressureReductionMap", m_atomizingPressureReductionMap )
            & boost::serialization::make_nvp( "Table_5_m_fluidNeedleReductionMap", m_fluidNeedleReductionMap )
            & boost::serialization::make_nvp( "Table_6_m_atomizingPressureNeedleReductionMap", m_atomizingPressureNeedleReductionMap )
            
            & boost::serialization::make_nvp( "Table_1_m_teDecreaseMap", m_teDecreaseMap )
            
            & boost::serialization::make_nvp( "Table_4_m_patternPercentIncrease", m_patternPercentIncrease )
            & boost::serialization::make_nvp( "Table_4_m_patternPercentDecrease", m_patternPercentDecrease )
            & boost::serialization::make_nvp( "Table_3_m_patternBasePercents", m_patternBasePercents )
            & boost::serialization::make_nvp( "Table_5_m_dropletSizeH1PatternMap", m_dropletSizeH1PatternMap )
            & boost::serialization::make_nvp( "Table_5_m_dropletSizeH2PatternMap", m_dropletSizeH2PatternMap )
            & boost::serialization::make_nvp( "Table_5_m_dropletSizeW1PatternMap", m_dropletSizeW1PatternMap )
            & boost::serialization::make_nvp( "Table_5_m_dropletSizeW2PatternMap", m_dropletSizeW2PatternMap )
            & boost::serialization::make_nvp( "Table_6_m_needleTravelH1PatternMap", m_needleTravelH1PatternMap )
            & boost::serialization::make_nvp( "Table_6_m_needleTravelH2PatternMap", m_needleTravelH2PatternMap )
            & boost::serialization::make_nvp( "Table_6_m_needleTravelW1PatternMap", m_needleTravelW1PatternMap )
            & boost::serialization::make_nvp( "Table_6_m_needleTravelW2PatternMap", m_needleTravelW2PatternMap )
            & boost::serialization::make_nvp( "Table_7_m_fanAdjustmentH1PatternMap", m_fanAdjustmentH1PatternMap )
            & boost::serialization::make_nvp( "Table_7_m_fanAdjustmentH2PatternMap", m_fanAdjustmentH2PatternMap )
            & boost::serialization::make_nvp( "Table_7_m_fanAdjustmentW1PatternMap", m_fanAdjustmentW1PatternMap )
            & boost::serialization::make_nvp( "Table_7_m_fanAdjustmentW2PatternMap", m_fanAdjustmentW2PatternMap );
    }
public:
    ///Constructor
    HVLPPaintModel();
    ///Destructor
    ~HVLPPaintModel();

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
    ///Determine the pattern shape bin letter
    std::string DeterminePatternShapeBinLetter( double& size );

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

    std::map< double, double > m_viscosityReductionMap;
    std::map< double, double > m_fluidNeedleReductionMap;
    std::map< double, double > m_atomizingPressureReductionMap;
    std::map< double, double > m_fluidPressureReductionMap;
    std::map< double, double > m_atomizingPressureNeedleReductionMap;
    std::map< double, double > m_teDecreaseMap;
    std::vector< double > m_patternPercentIncrease;
    std::vector< double > m_patternPercentDecrease;
    std::vector< double > m_patternBasePercents;

    ///Pattern shape constants
    std::map< std::string, double > m_dropletSizeH1PatternMap;
    std::map< std::string, double > m_dropletSizeH2PatternMap;
    std::map< std::string, double > m_dropletSizeW1PatternMap;
    std::map< std::string, double > m_dropletSizeW2PatternMap;

    ///Needle travel constants
    std::map< double, double > m_needleTravelH1PatternMap;
    std::map< double, double > m_needleTravelH2PatternMap;
    std::map< double, double > m_needleTravelW1PatternMap;
    std::map< double, double > m_needleTravelW2PatternMap;

    ///Fan adjustment constants
    std::map< double, double > m_fanAdjustmentH1PatternMap;
    std::map< double, double > m_fanAdjustmentH2PatternMap;
    std::map< double, double > m_fanAdjustmentW1PatternMap;
    std::map< double, double > m_fanAdjustmentW2PatternMap;
    ///////
    double m_needlePosition;
    double m_atomizingAirPressure;
    
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
    
    std::string m_dropletBin;
    
    std::string m_patternBin;

    double m_fanAdjustment;
};


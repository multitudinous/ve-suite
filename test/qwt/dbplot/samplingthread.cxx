
// --- QWT Includes --- //
#include <qwt_math.h>

// --- DB Plot Includes --- //
#include "samplingthread.h"
#include "SensorData.h"

// --- Boost Includes --- //
#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>

// --- STL Includes --- //
#include <string>
#include <iostream>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
SamplingThread::SamplingThread( SensorData* sensorData, QObject* parent )
    :
    QwtSamplingThread( parent ),
    m_sensorData( sensorData ),
    m_timeValue( 0.0 )
{
    m_infile.open( "/Users/kochjb/dev/ve-suite/trunk/test/qwt/dbplot/log_0000.asc" );

    //Ignore first two lines
    m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
    m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );

    //Get the first timestamp
    std::string time; m_infile >> time;
    m_timeValue = boost::lexical_cast< double >( time );
}
////////////////////////////////////////////////////////////////////////////////
SamplingThread::~SamplingThread()
{
    m_infile.close();
}
////////////////////////////////////////////////////////////////////////////////
void SamplingThread::sample( double elapsed )
{
    //std::cout << elapsed << std::endl;

    while( m_timeValue < elapsed )
    {
        //Get the sensor
        m_infile.ignore( 3 );
        std::string sensor; m_infile >> sensor;
        if( sensor == "1A1" )
        {
            //Get the desired value
            QPointF newPoint( m_timeValue, 0.0 );
            m_infile.ignore( 32 );
            std::string val1, val2; m_infile >> val2 >> val1;
            long n = std::strtoul( ( val1 + val2 ).c_str(), NULL, 16 );
            newPoint.setY( 0.1 * n );
            m_sensorData->append( newPoint );
        }

        //Get the next timestamp
        m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
        std::string time; m_infile >> time;
        m_timeValue = boost::lexical_cast< double >( time );
    }
}
////////////////////////////////////////////////////////////////////////////////

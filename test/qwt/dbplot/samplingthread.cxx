
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
    m_infile.open( "C:/dev/ve-suite/trunk/test/qwt/dbplot/log_0000.asc" );

    //Ignore first two lines
    m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
    m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
}
////////////////////////////////////////////////////////////////////////////////
SamplingThread::~SamplingThread()
{
    if( m_infile.is_open() )
    {
        m_infile.close();
    }
}
////////////////////////////////////////////////////////////////////////////////
void SamplingThread::sample( double elapsed )
{
    static bool fileParsed( false );
    while( m_timeValue < elapsed && !fileParsed )
    {
        //Get the next timestamp
        std::string time; m_infile >> time;
        m_timeValue = boost::lexical_cast< double >( time );

        //Get the sensor
        m_infile.ignore( 3 );
        std::string sensor; m_infile >> sensor;
        if( sensor == "1A1" )
        {
            //Get the desired value
            m_infile.ignore( 32 );
            std::string val1, val2; m_infile >> val2 >> val1;
            long n = std::strtoul( ( val1 + val2 ).c_str(), NULL, 16 );
            m_sensorData->append( QPointF( m_timeValue, 0.1 * n ) );
        }

        //Move to the next line
        if( m_infile.ignore(
                std::numeric_limits< std::streamsize >::max(), '\n' ).eof() )
        {
            fileParsed = true;
            m_infile.close();
        }
    }
}
////////////////////////////////////////////////////////////////////////////////

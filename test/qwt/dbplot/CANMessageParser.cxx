
// --- DB Plot Includes --- //
#include "CANMessageParser.h"
#include "SensorData.h"

// --- QWT Includes --- //


// --- Boost Includes --- //
#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp>
#include <boost/filesystem/path.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>
#include <boost/dynamic_bitset.hpp>

// --- POCO Includes --- //
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>

// --- STL Includes --- //
#include <string>
#include <iostream>
#include <vector>
#include <bitset>

using namespace Poco::Data;

////////////////////////////////////////////////////////////////////////////////
CANMessageParser::CANMessageParser( QObject* parent )
    :
    QwtSamplingThread( parent ),
    m_timeValue( 0.0 )
{
    SQLite::Connector::registerConnector();
    std::string filename =
        "/Users/kochjb/dev/ve-suite/trunk/test/qwt/dbplot/BG480E.db";
    m_canDB = new Session( "SQLite", filename );

    std::stringstream m_signalQryStr;
    m_signalQryStr
        << "select t1.[Message Name], "
        <<        "t1.[CAN Id (hex)], "
        <<        "t2.[Signal Name], "
        <<        "t2.[Start Bit], "
        <<        "t2.Length, "
        <<        "t2.Factor, "
        <<        "t2.Unit "
        << "from luMessageBG480E t1, "
        <<      "luSignalBG480E t2 "
        << "where t1.id = t2.messageId "
        << "order by t1.[Message Name], "
        <<          "cast( t2.[Start Bit] as integer ) asc";

    *( m_canDB.get() ) << m_signalQryStr.str(), into( m_canSchema ), now;

    m_infile.open(
        "/Users/kochjb/dev/ve-suite/trunk/test/qwt/dbplot/log_0000.asc" );

    //Ignore first two lines
    m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
    m_infile.ignore( std::numeric_limits< std::streamsize >::max(), '\n' );
}
////////////////////////////////////////////////////////////////////////////////
CANMessageParser::~CANMessageParser()
{
    SQLite::Connector::unregisterConnector();

    if( m_infile.is_open() )
    {
        m_infile.close();
    }
}
////////////////////////////////////////////////////////////////////////////////
void CANMessageParser::sample( double elapsed )
{
    static bool fileParsed( false );
    while( m_timeValue < elapsed && !fileParsed )
    {
        //Get the next timestamp
        std::string time; m_infile >> time;
        m_timeValue = boost::lexical_cast< double >( time );
        //std::cout << "Time: " << m_timeValue << std::endl;

        std::string skip;

        //Get the message
        m_infile >> skip;
        std::string canId;
        m_infile >> canId;
        if( canId == "1A1" )
        {
        //Remove trailing "x" (hex specifier) from id
        if( canId.length() > 8 )
        {
            canId.erase( 8, 1 );
        }
        //std::cout << "CAN Id: " << canId << std::endl;

        m_infile >> skip >> skip >> skip;

        //Get the values
        std::vector< std::string > byte;
        byte.resize( 8 );
        //This is dumb
        m_infile >> byte[ 1 ] >> byte[ 0 ] >> byte[ 3 ] >> byte[ 2 ]
                 >> byte[ 5 ] >> byte[ 4 ] >> byte[ 7 ] >> byte[ 6 ];
        std::ostringstream bitSS;
        std::copy( byte.begin(), byte.end(),
            std::ostream_iterator< std::string >( bitSS ) );
        //std::cout << "Hex: " << bitSS.str() << std::endl;
        //Don't know if this will work on all platforms, especially windows
        std::bitset< 64 > bits( std::strtoull( bitSS.str().c_str(), NULL, 16 ) );
        std::string binStr = bits.to_string();
        //std::cout << "Binary: " << binStr << std::endl;

        std::pair< CAN_SCHEMA::const_iterator, CAN_SCHEMA::const_iterator > itp;
        itp = m_canSchema.equal_range( canId );
        CAN_SCHEMA::const_iterator itr = itp.first;
        for( ; itr != itp.second; ++itr )
        {
            CAN_Signal const& canSignal = itr->second;
            //std::cout << "Signal Name: "
                      //<< canSignal.GetSignalName().convert< std::string >()
                      //<< std::endl;
            unsigned int startBit = canSignal.GetStartBit();
            //std::cout << "Start Bit: " << startBit << std::endl;
            unsigned int length = canSignal.GetLength();
            //std::cout << "Length: " << length << std::endl;
            double factor = canSignal.GetFactor();
            //std::cout << "Factor: " << factor << std::endl;
            std::string signalBits( binStr.substr( startBit, length ) );
            //std::cout << "Signal Bits: " << signalBits << std::endl;
            boost::dynamic_bitset<> bs( signalBits );
            double value = factor * bs.to_ulong();
            //std::cout << "Value: " << value << " "
                      //<< canSignal.GetUnit().convert< std::string >()
                      //<< std::endl;

            //m_sensorData->append( QPointF( m_timeValue, value ) );
        }
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

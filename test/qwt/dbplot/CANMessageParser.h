
#ifndef SAMPLING_THREAD
#define SAMPLING_THREAD

// --- QWT Includes --- //
#include <qwt_sampling_thread.h>

// --- DB Plot Includes --- //
#include "typehandlers/CAN_Signal.h"

class SensorData;

// --- POCO Includes --- //
#include <Poco/Tuple.h>
#include <Poco/SharedPtr.h>
#include <Poco/Data/Common.h>

// --- STL Includes --- //
#include <map>
#include <fstream>
#include <sstream>

class CANMessageParser : public QwtSamplingThread
{
    ///
    Q_OBJECT

public:
    ///
    CANMessageParser( QObject* parent = NULL );

    ///
    virtual ~CANMessageParser();

//Q_SIGNALS:

protected:
    ///
    virtual void sample( double elapsed );

private:
    ///
    double m_timeValue;

    ///
    std::ifstream m_infile;

    ///
    Poco::SharedPtr< Poco::Data::Session > m_canDB;

    typedef std::multimap< std::string, CAN_Signal > CAN_SCHEMA;
    CAN_SCHEMA m_canSchema;

};

#endif //SAMPLING_THREAD

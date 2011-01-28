
#ifndef SAMPLING_THREAD
#define SAMPLING_THREAD

// --- QWT Includes --- //
#include <qwt_sampling_thread.h>

// --- DB Plot Includes --- //
class SensorData;

// --- Boost Includes --- //
#include <boost/tokenizer.hpp>

// --- STL Includes --- //
#include <fstream>

class SamplingThread : public QwtSamplingThread
{
    ///
    Q_OBJECT

public:
    ///
    SamplingThread( SensorData* sensorData, QObject* parent = NULL );

    ///
    virtual ~SamplingThread();

//public Q_SLOTS:

protected:
    ///
    virtual void sample( double elapsed );

private:
    ///
    typedef boost::tokenizer< boost::escaped_list_separator< char > > Tokenizer;

    ///
    SensorData* m_sensorData;

    ///
    double m_timeValue;

    ///
    std::ifstream m_infile;

};

#endif //SAMPLING_THREAD

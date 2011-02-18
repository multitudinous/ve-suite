
#ifndef SAMPLING_THREAD
#define SAMPLING_THREAD

// --- QWT Includes --- //
#include <qwt_sampling_thread.h>

// --- DB Plot Includes --- //
class SensorData;

// --- STL Includes --- //
#include <fstream>

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

};

#endif //SAMPLING_THREAD

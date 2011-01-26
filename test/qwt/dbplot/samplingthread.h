
#ifndef SAMPLING_THREAD
#define SAMPLING_THREAD

// --- QWT Includes --- //
#include <qwt_sampling_thread.h>

// --- DB Plot Includes --- //
class SensorData;

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
    double value( double timeStamp ) const;

    ///
    SensorData* m_sensorData;

};

#endif //SAMPLING_THREAD

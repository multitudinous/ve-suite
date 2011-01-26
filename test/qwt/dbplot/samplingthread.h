
#ifndef SAMPLING_THREAD
#define SAMPLING_THREAD

// --- QWT Includes --- //
#include <qwt_sampling_thread.h>

class SamplingThread : public QwtSamplingThread
{
    ///
    Q_OBJECT

public:
    ///
    SamplingThread( QObject* parent = NULL );

    ///
    virtual ~SamplingThread();

//public Q_SLOTS:

protected:
    ///
    virtual void sample( double elapsed );

private:
    ///
    double value( double timeStamp ) const;

};

#endif //SAMPLING_THREAD

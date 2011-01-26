
// --- QWT Includes --- //
#include <qwt_math.h>

// --- DB Plot Includes --- //
#include "samplingthread.h"
#include "signaldata.h"

// --- STL Includes --- //
#include <math.h>
#include <iostream>

////////////////////////////////////////////////////////////////////////////////
SamplingThread::SamplingThread( QObject* parent )
    :
    QwtSamplingThread( parent )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
SamplingThread::~SamplingThread()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void SamplingThread::sample( double elapsed )
{
    std::cout << elapsed << std::endl;
    //This is where we would query the database at some interval and
    //append points to SignalData
    if( 1 )
    {
        const QPointF s( elapsed, value( elapsed ) );
        SignalData::instance().append( s );
    }
}
////////////////////////////////////////////////////////////////////////////////
double SamplingThread::value( double timeStamp ) const
{
    const double period = 1.0 / 2.0;

    const double x = fmod( timeStamp, period );
    const double v = 20.0 * qFastSin( x / period * 2 * M_PI );

    return v;
}
////////////////////////////////////////////////////////////////////////////////

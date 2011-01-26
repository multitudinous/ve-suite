
#ifndef SENSOR_DATA_H
#define SENSOR_DATA_H

// --- QT Includes --- //
#include <QtCore/qpoint.h>
#include <QtCore/qmutex.h>
#include <QtCore/qreadwritelock.h>

// --- QWT Includes --- //
#include <qwt_series_data.h>

// --- DB Plot Includes --- //
#include "samplingthread.h"

// --- STL Includes --- //
#include <vector>
#include <deque>

class SensorData : public QwtSeriesData< QPointF >
{
public:
    ///
    SensorData();

    ///
    SensorData( SensorData const& sensorData );

    ///
    SensorData &operator=( SensorData const& sensorData );

    ///
    virtual ~SensorData();

    ///
    void append( QPointF const& sample );

    ///
    virtual QRectF boundingRect() const;

    ///
    void clearStaleValues( double minimum );

    ///
    unsigned int const& GetNumPaintedPoints() const;

    ///
    unsigned int GetNumPoints();

    ///
    SamplingThread& GetSamplingThread();

    ///
    void lock();

    ///
    virtual QPointF sample( size_t i ) const;

    ///
    void SetNumPaintedPoints( unsigned int const& numPoints );

    ///
    virtual size_t size() const;

    ///
    void unlock();

    ///
    QPointF const& value( int index ) const;

protected:

private:
    ///
    unsigned int m_paintedPoints;

    ///
    SamplingThread m_samplingThread;

    ///Protecting values
    QReadWriteLock m_lock;
    std::deque< QPointF > m_values;

    ///Protecting pending values
    QMutex m_mutex;
    std::vector< QPointF > m_pendingValues;

};

#endif //SENSOR_DATA_H
